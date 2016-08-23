library("readr")
library("rio")
library("magrittr")
library("rvest")
library("stringr")


### ANSOGERE
css.selector = ".factbox:nth-child(3) p:nth-child(2) .internal-link"
link = "http://ufm.dk/uddannelse-og-institutioner/statistik-og-analyser/sogning-og-optag-pa-videregaende-uddannelser/grundtal-om-sogning-og-optag/ansogere-og-optagne-fordelt-pa-kon-alder-og-adgangsgrundlag"

## Links
Filer = link %>% 
  read_html() %>% 
  html_nodes(css = css.selector) %>% 
  html_attr(name = 'href')

Filer = Filer[!is.na(Filer)]
View(Filer)

## Årstal
Navne = link %>%
  read_html() %>%
  html_nodes(css = css.selector) %>%
  html_text() %>%
  str_trim() %>% 
  str_sub(start = 1, end = 4)

Navne = unique(Navne)
View(Navne)

## Samler data
data = cbind(Navne, Filer)

data.sub <- tail(data, n = 4)

for ( i in 1:nrow(data.sub)) {
  name = paste("Ansoger", data.sub[i,1], sep = "")
  assign(name, import(data.sub[i,2]))
}



### OPTAGNE
css.selector2 = ".factbox:nth-child(6) p:nth-child(2) .internal-link"

## Links
Filer.Optaget = link %>% 
  read_html() %>% 
  html_nodes(css = css.selector2) %>% 
  html_attr(name = 'href')

Filer.Optaget = Filer.Optaget[!is.na(Filer.Optaget)]
View(Filer.Optaget)

## Årstal
Navne.Optaget = link %>%
  read_html() %>%
  html_nodes(css = css.selector2) %>%
  html_text() %>%
  str_trim() %>% 
  str_sub(start = 1, end = 4)

Navne.Optaget = unique(Navne.Optaget)
View(Navne.Optaget)

## Samler data
data.Optaget = cbind(Navne.Optaget, Filer.Optaget)

data.Optaget = tail(data.Optaget, 4)

for ( i in 1:nrow(data.Optaget)) {
  name = paste("Optaget", data.Optaget[i,1], sep = "")
  assign(name, import(data.Optaget[i,2]))
}



### ADGANGSKVOTIENTER 
link.kvo = "http://ufm.dk/uddannelse-og-institutioner/statistik-og-analyser/sogning-og-optag-pa-videregaende-uddannelser/grundtal-om-sogning-og-optag/kot-hovedtal"

### Links
Filer.kvotient = link.kvo %>% 
  read_html() %>% 
  html_nodes(css = "p:nth-child(10) .internal-link") %>% 
  html_attr(name = 'href')

Filer.kvotient = Filer.kvotient[!is.na(Filer.kvotient)]
Filer.kvotient = unique(Filer.kvotient)

View(Filer.kvotient)


## Aarstal
Aarstal.kvotienter = link.kvo %>%
  read_html() %>%
  html_nodes(css = "p:nth-child(10) .internal-link") %>%
  html_text() %>%
  str_trim() %>% 
  str_sub(start = 1, end = 4)

Aarstal.kvotienter = unique(Aarstal.kvotienter)
Aarstal.kvotienter[Aarstal.kvotienter == ""] = NA
Aarstal.kvotienter = Aarstal.kvotienter[!is.na(Aarstal.kvotienter)]

View(Aarstal.kvotienter)

## Samler data
data.kvotienter = cbind(Aarstal.kvotienter, Filer.kvotient)

data.kvotienter = tail(data.kvotienter, 4)

for ( i in 1:nrow(data.kvotienter)) {
  name = paste("Kvotienter", data.kvotienter[i,1], sep = "")
  assign(name, import(data.kvotienter[i,2]))
}




### Funktion der kan gøre ovenstående, hvor n er antal år fra 2016 og tilbage og Data er
### det ønskede data 

ScrapeFun = function(n, Data) {
  if (Data == "Optaget") {
    link = "http://ufm.dk/uddannelse-og-institutioner/statistik-og-analyser/sogning-og-optag-pa-videregaende-uddannelser/grundtal-om-sogning-og-optag/ansogere-og-optagne-fordelt-pa-kon-alder-og-adgangsgrundlag"
    Filer = link %>% 
      read_html() %>% 
      html_nodes(".factbox:nth-child(6) p:nth-child(2) .internal-link") %>% 
      html_attr(name = 'href')
    Filer = Filer[!is.na(Filer)]
    Filer = unique(Filer)
    Aarstal = link %>%
      read_html() %>%
      html_nodes(".factbox:nth-child(6) p:nth-child(2) .internal-link") %>%
      html_text() %>%
      str_trim() %>% 
      str_sub(start = 1, end = 4)
    Aarstal = unique(Aarstal)
    Aarstal[Aarstal == ""] = NA
    Aarstal = Aarstal[!is.na(Aarstal)]
    data = cbind(Aarstal, Filer)
  }
  else if (Data == "Ansoger") {
    link = "http://ufm.dk/uddannelse-og-institutioner/statistik-og-analyser/sogning-og-optag-pa-videregaende-uddannelser/grundtal-om-sogning-og-optag/ansogere-og-optagne-fordelt-pa-kon-alder-og-adgangsgrundlag"
    Filer = link %>% 
      read_html() %>% 
      html_nodes(".factbox:nth-child(3) p:nth-child(2) .internal-link") %>% 
      html_attr(name = 'href')
    Filer = Filer[!is.na(Filer)]
    Filer = unique(Filer)
    Aarstal = link %>%
      read_html() %>%
      html_nodes(".factbox:nth-child(3) p:nth-child(2) .internal-link") %>%
      html_text() %>%
      str_trim() %>% 
      str_sub(start = 1, end = 4)
    Aarstal = unique(Aarstal)
    Aarstal[Aarstal == ""] = NA
    Aarstal = Aarstal[!is.na(Aarstal)]
    data = cbind(Aarstal, Filer)
  }
  else {
    link = "http://ufm.dk/uddannelse-og-institutioner/statistik-og-analyser/sogning-og-optag-pa-videregaende-uddannelser/grundtal-om-sogning-og-optag/kot-hovedtal"
    Filer = link %>% 
      read_html() %>% 
      html_nodes("p:nth-child(10) .internal-link") %>% 
      html_attr(name = 'href')
    Filer = Filer[!is.na(Filer)]
    Filer = unique(Filer)
    Aarstal = link %>%
      read_html() %>%
      html_nodes("p:nth-child(10) .internal-link") %>%
      html_text() %>%
      str_trim() %>% 
      str_sub(start = 1, end = 4)
    Aarstal = unique(Aarstal)
    Aarstal[Aarstal == ""] = NA
    Aarstal = Aarstal[!is.na(Aarstal)]
    data = cbind(Aarstal, Filer)
  } 
  data.sub <- tail(data, n)
}


data = ScrapeFun(4, "Kvotient")

## Data = Ansoger
for ( i in 1:nrow(data)) {
  name = paste("Ansoger", data[i,1], sep = "")
  assign(name, import(data[i,2]))
}

## Data = Optaget
for ( i in 1:nrow(data)) {
  name = paste("Optaget", data[i,1], sep = "")
  assign(name, import(data[i,2]))
}

## Data = Kvotient
for ( i in 1:nrow(data)) {
  name = paste("Kvotient", data[i,1], sep = "")
  assign(name, import(data[i,2]))
}