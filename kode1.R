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

my_model = function(num, data = data3){ 
  if (num == 1) { 
    glm(KvindeAndOpt ~ GnsIndkomst, family=binomial(link=logit), data = data,weights =TotalOpt, x=TRUE)
  } else if (num == 2) {
    glm(KvindeAndOpt ~ GnsIndkomst + Retning, family=binomial(link=logit), data = data,weights =TotalOpt, x=TRUE)
  } else if (num == 3) {
    glm(KvindeAndOpt ~ GnsIndkomst+ Retning + Kvotient, family=binomial(link=logit), data = data3,weights =TotalOpt, x=TRUE)
  } else 
    glm(KvindeAndOpt ~ GnsIndkomst +TotalOpt + Retning + Kvotient, family=binomial(link=logit), data = data3,weights =TotalOpt, x=TRUE)
}



model.1 <- my_model(pol = 1)
models <- data.frame(mod1, mod2, mod3, mod4)
summary(mod1)

summary(lm(KvindeAndOpt ~ poly(GnsIndT,1), data = data3))


summary(lm(KvindeAndOpt ~ GnsIndT, data = data3))


add_pred = function(mod, data = data3){
  data %>% add_predictions(mod, var = "pred")
}

df.1 = add_pred(mod1)

models = 1:4 %>%
  map(my_model) %>% map_df(add_pred, .id = "Model")

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
                                sq.error = error^2) %>% group_by(Model) %>%
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

dim(data3)[1]

trainDTM <- data3[sample(nrow(data3), dim(data3)[1]/2), ]

testDTM <- anti_join(data3,trainDTM, by = c("Aar" = "Aar", "OptNr" = "OptNr"))

trainDTM <- data3 %>%
  filter( Aar == '2013' )

testDTM <- data3 %>%
  filter(Aar == '2014'  | Aar == '2015' | Aar == '2016')

library("rpart.plot")
library("rpart")
library("purrr")
library("tidyr")
library("ggplot2")

300 ser godt ud 




set.seed(300)
trainDTM <- data3[sample(nrow(data3), dim(data3)[1]/2), ]
testDTM <- anti_join(data3,trainDTM, by = c("Aar" = "Aar", "OptNr" = "OptNr"))

model = rpart(KvindeAndOpt ~ GnsIndT  + InstNavn + Retning, data = trainDTM, cp=10^(-6) )
cptablenodes <- model$cptable[1:12, ]
nodes <- cptablenodes[,2]
treenodes = function(nodes){ prune(model, model$cptable[model$cptable[, 2] == nodes, 1])
}
add.pred.train = function(treenodes, data = trainDTM){
  data %>% predict(treenodes,. )
}
add.pred.test = function(treenodes, data = testDTM){
  data %>% predict(treenodes,. )
}
models.train = nodes %>%
  map(treenodes) %>% map_df(add.pred.train)

models.test = nodes %>%
  map(treenodes) %>% map_df(add.pred.test)

models.train.mse = colMeans((models.train-trainDTM$KvindeAndOpt)**2)
models.test.mse = colMeans((models.test-testDTM$KvindeAndOpt)**2)

models.train.rmse = sqrt(models.train.mse)
models.test.rmse = sqrt(models.test.mse)


rmse <- data.frame(cbind(nodes, models.train.rmse, models.test.rmse))


rmse.graf <- rmse %>% gather(key = rmse, value = frequency, -nodes )

ggplot(rmse.graf, aes(x=nodes, y =frequency, group = rmse )) + geom_line(aes(colour=rmse)) + geom_point(aes(colour=rmse))

model = rpart(KvindeAndOpt ~ GnsIndT  + InstNavn + Retning, data = trainDTM)
rpart.plot(model)
p = ggplot(rmse, aes(nodes)) + geom_line(aes(y = models.train.rmse, colour = "var0")) 
+ geom_line(aes(y = models.test.rmse, colour = "var0"))

p = ggplot(rmse, 
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

KvindeAndOpt = predict(model, newdata = testDTM)


names(model)



 

nodes + 1

for (i in nodes) {
  cp.i = which(model$cptable[, 2] == i)
  }










models = models**2

models.mse = colMeans(models)

models.rmse = sqrt(models.mse)

names(models) <- nodes

cbind(nodes, models.mse)
models.rmse <- models %>%
  mutate_each()


sqrt(mean((models.rmse[,1])^2))
 
models.rmse <- mututa()

test <- as.vector(models[,2])

dim(models[,2])
length(trainDTM$KvindeAndOpt)
treenodes.0 = treenodes(nodes = 0)

model$cptable[dim(model$cptable)[1] - 9:0, ]


cp8 = which(model$cptable[, 2] == 8)
cp12 = which(model$cptable[, 2] == 12)
model8 = prune(model, model$cptable[cp8, 1])
model12 = prune(model, model$cptable[cp12, 1])
print(model8)
summary(model8)
rpart.plot(treenodes.0)
KvindeAndOpt = predict(model8, newdata = trainDTM)
KvindeAndOpt = predict(model8, newdata = testDTM)
KvindeAndOpt = predict(model12, newdata = testDTM)

plotcp(model)
printcp(model)
sqrt(mean(trainDTM$KvindeAndOpt-KvindeAndOpt)^2)


which.min(model$cptable[, 4])

cpstat = dim(trainDTM)[1] * model$cptable[, 3] + 2 * (model$cptable[, 2] + 1)

round(model$cptable[which.min(cpstat), ], 3)


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