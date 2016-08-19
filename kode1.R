rm(list=ls()) ## Sletter alle variable og funktioner
cat("\014") # Sletter consol teksten

install.packages("freqweights")
library("freqweights")
library("rio")
library("dplyr")
library("ggplot2")
library("purrr")
library("modelr")

setwd("/Users/simonharmat/Dropbox/Studie/Data_science/Gruppe4" ) ## Skriv stien


data <- import("optagelse3.csv") # læser csv-filen 
lon <- import("londata1.csv") # læser csv-filen

data <- data %>%
  mutate(Kvotient = as.numeric(gsub(",",".",data$Kvotient))) %>%
  filter(!is.na(AngMaend) | !is.na(AngKvinder) )

data.1 <- data %>%
  mutate(OptMK = round(OptMaend / OptKvinder,2),
         TotalOpt = OptMaend + OptKvinder,
         KvindeAndOpt = round(OptKvinder /(OptMaend + OptKvinder),2),
         AngMK = round(AngMaend / AngKvinder,2),
         TotalAng = AngMaend + AngKvinder,
         KvindeAndAng = round(AngKvinder /(AngMaend + AngKvinder),2))

data2 <- data.1 %>% left_join(lon, by = "Placering")

data3 <-  data2 %>%
  filter(!is.na(GnsIndkomst)) %>%
  mutate(GnsIndT = GnsIndkomst/100000
  )




add_rmse = function(i){ data3 %>%
          mutate(sq.error = (KvindeAndOpt - i)^2) %>% 
          summarise(mse = mean(sq.error),rmse = sqrt(mse), guess = i)
}

  
df.rmse = 63:83 %>% map_df(add_rmse)

df.rmse %>%
  filter(rmse == min(rmse))

data3 %>% summarise(round(mean(KvindeAndOpt), 3))

my_model = function(pol, data = data3){ lm(KvindeAndOpt ~ poly(GnsIndT, pol), data = data)
}

model.1 <- my_model(pol = 1)

summary(model.1)

summary(lm(KvindeAndOpt ~ poly(GnsIndT,1), data = data3))


summary(lm(KvindeAndOpt ~ GnsIndT, data = data3))


add_pred = function(mod, data = data3){
  data %>% add_predictions(mod, var = "pred")
}

df.1 = add_pred(model.1)

models = 1:9 %>%
  map(my_model) %>% map_df(add_pred, .id = "poly")

p = ggplot(data = models,
           aes(GnsIndT, pred)) +
  geom_segment(aes(x=GnsIndT, 
                   xend=GnsIndT, 
                   y=KvindeAndOpt, yend=pred), 
               color="red") +
  geom_point(data = data3,
             aes(GnsIndT, KvindeAndOpt),
             color = "grey50",
             fill = "white",
             shape = 21) +
  geom_line(aes( color = poly == 4),
            size = 1) +
  facet_wrap(~ poly, ncol = 3) +
  scale_color_manual(values = c("black", "blue")) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL)


models.rmse = models %>% mutate(error = KvindeAndOpt - pred,
                                sq.error = error^2) %>% group_by(poly) %>%
  summarise(
    mse = mean(sq.error), rmse = sqrt(mse)
  ) %>% arrange(rmse)



gen_crossv = function(pol, data = data3){
  data %>%
    crossv_kfold(10) %>% mutate(
      mod = map(train, ~ lm(KvindeAndOpt ~ poly(GnsIndT, pol), data = .)),
      rmse.test = map2_dbl(mod, test, rmse),
      rmse.train = map2_dbl(mod, train, rmse) )
}

set.seed(3000) 
df.cv = 1:10 %>%
  map_df(gen_crossv, .id = "degree")


df.cv.sum = df.cv %>% group_by(degree) %>% summarise(
  m.rmse.test = mean(rmse.test),
  m.rmse.train = mean(rmse.train) )

df.cv.sum = df.cv.sum %>% 
  mutate(degree = as.numeric(degree)) %>% 
  gather(var, value, -degree) %>% 
  arrange(degree)

p = ggplot(df.cv.sum, 
           aes(x = degree, y = value,
               color = var))
p + geom_point() +
  geom_line() +
  scale_color_viridis(discrete = TRUE,
                      name = NULL,
                      labels = c("RMSE (test)",
                                 "RMSE (train)")) +
  theme(legend.position = "bottom") +
  labs(x = "Degree", y = "RMSE") +
  scale_x_continuous(breaks = 1:11)





trainDTM <- data3 %>%
  filter(Aar != '2016')

testDTM <- data3 %>%
  filter(Aar == '2016')

library("rpart.plot")
library("rpart")
set.seed(1)
model = rpart(KvindeAndOpt ~ GnsIndT  + InstNavn + Retning, data = trainDTM )
KvindeAndOpt = predict(model, newdata = testDTM)

rpart.plot(model)

sqrt(mean(testDTM$KvindeAndOpt-KvindeAndOpt)^2)

model.1
library("ISLR")
df = Carseats



lin.model.1 = lm(Sales ~ Price + Urban + US, 
               data = df)


summary(lin.model)


Sales=a+bPrice+bD_u+bD_US+e


lin.model.2 = lm(Sales ~ Price  + US, 
               data = df)

fit <- fitted(lin.model.1)

fv <- fitted.values(lin.model.1)

predict(lm(Sales ~ Price  + US, 
           data = df))

sales <- 
  
rmse.1 <-  sqrt( mean((df$Sales - fitted(lin.model.1))^2))
 rmse.2 <- sqrt( mean((df$Sales - fitted(lin.model.2))^2))

rmse.1/rmse.2

install.packages("devtools")
devtools::install_github("hadley/modelr")