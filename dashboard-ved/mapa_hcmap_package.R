#maps
library(WDI)
library(highcharter)
library(dplyr)
library(maps)

#dados:
GDP <- WDI(
  country = "all",
  indicator = "NY.GDP.MKTP.KD.ZG",
  start = 2019,
  end = 2019,
  extra = FALSE,
  cache = NULL)
GDP

#Para simplificar, renomeamos a coluna do crescimento do PIB (GDP_Growth).
names(GDP)[names(GDP) == "NY.GDP.MKTP.KD.ZG"] <- "GDP_Growth"

#decidir quais paises iremos usar
Countries  <- c("Brazil","Argentina","Chile","Russian Federation","United States","China","Germany","Australia","South Africa","Canada","India","Egypt, Arab Rep.","United Kingdom")

GDP_Filter <- GDP[GDP$country %in% Countries ,]

#precisaremos da lista de códigos ISO.
Countries_iso3  <- c("BRA","ARG","CHL","RUS", "USA","CHN","DEU","AUS","ZAF","CAN","IND","EGY","GBR")

#As linhas abaixo são necessárias para criar o mapa posteriormente. Basicamente, o mapa precisa dos códigos ISO3 para ler os países
dat <- iso3166
dat <- rename(dat, "iso-a3" = a3 )
dat = dat[dat$`iso-a3` %in% Countries_iso3 ,]
GDP_Filter_Integer = as.integer(GDP_Filter$GDP_Growth)

#Observe que a China está duplicada em “dat”. Vamos remover o dado do data.frame.
dat<-dat[!duplicated(dat$sovereignty), ]

#combinar os dados de crescimento do PIB com os códigos ISO3
dat$GDP <- GDP_Filter$GDP_Growth

dat

#Criando o mapa usando o código a seguir.
hc<-hcmap(
  map = "custom/world-highres3", 
  data = dat, 
  joinBy = "iso-a3",
  value = "GDP",
  showInLegend = FALSE, 
  nullColor = "#DADADA",
  download_map_data = TRUE) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_legend(align = "center",
            verticalAlign = "top",
            layout = "horizontal",
            x = 100,
            y = 0) %>%
  hc_title(text = "GDP Growth in 2019 for selected Countries")

hc



