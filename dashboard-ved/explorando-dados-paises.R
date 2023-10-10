#Explorando meus dados Letterboxd: 2016 - 2022

#PAÍSES
paises <- read.csv("C:/Users/Amanda/Documents/dashboard-ved/country_freq.csv") 
View(paises)
attach(paises)

library(ggplot2)

df_paises <- data.frame(paises)
df_paises

#Deletando a coluna 'n' (não informativa):
df_paises[1] <- NULL
View(df_paises)

#Mudando nome das colunas de df
colnames(df_paises) <- c('País', 'Frequência')

#Representação gráfica: mapa-mundi
library(WDI)
library(highcharter)
library(dplyr)
library(maps)

#coutries:
df_paises$País

#lista códigos ISO:
#Disponível em: https://www.iban.com/country-codes

iso3 <- c("USA", "DEU", "FRA", "ESP", "POL", "GBR", "PRT", "CHE", "HUN",
          "MEX", "BRA", "CAN", "IRL", "CHN", "HKG", "TWN", "BEL", "BHS",
          "CZE", "ARG", "KOR", "NLD", "NOR", "SWE", "NZL", "TUR", "JPN",
          "MAR", "ITA", "DNK", "MLT", "SRB", "AUS", "ZAF", "PRI", "GRC",
          "ARE", "MYS", "KEN", "AUT", "SVK", "RUS", "IRN")
#ps: SUN (URSS) -> RUS
length(iso3)

#Combinar dados país-frequência com os códigos ISO3:
df_paises$ISO <- iso3
df_paises

#Mapa:
hc <- hcmap(
  map = "custom/world-highres3", 
  data = df_paises, 
  joinBy = "ISO",
  value = "Frequência",
  showInLegend = FALSE, 
  nullColor = "#DADADA",
  download_map_data = TRUE) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_legend(align = "center",
            verticalAlign = "top",
            layout = "horizontal",
            x = 100,
            y = 0) %>%
  hc_title(text = "País de produção")

hc #nadinha :(

#Tentando com outro pacote...
library(rworldmap)

#gera info
mapa <- joinCountryData2Map(df_paises
                    , joinCode = "ISO3" 
                    , nameJoinColumn = "ISO"
                    , nameCountryColumn = "País"
                    , suggestForFailedCodes = FALSE 
                    , mapResolution="coarse" 
                    , projection=NA #DEPRECATED
                    , verbose = FALSE   
)

#plota mapa
mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                , numCats=60 
                , mapTitle= "País de produção"
                , catMethod ="fixedWidth"
                , colourPalette="palette"
)

mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                #, numCats=10
                , mapTitle= "País de produção"
                , catMethod=c(0:629)
                , colourPalette="palette"
)

mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                #, numCats=10
                , mapTitle= "País de produção heat"
                , catMethod=c(0:629)
                , colourPalette="heat"
)

mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                #, numCats=10
                , mapTitle= "País de produção terrain"
                , catMethod=c(0:629)
                , colourPalette="terrain"
)

mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                #, numCats=10
                , mapTitle= "País de produção negpos8"
                , catMethod=c(1:9)
                , colourPalette="negpos8"
)

#esse aqui: melhores cores
mapCountryData( mapa
                , nameColumnToPlot="Frequência"
                , numCats=10
                , mapTitle= "País de produção: intervalos na mão"
                , catMethod=c(1, 5, 10, 20, 30, 50, 70, 125, 629)
                , colourPalette="negpos8"
)

#Curva de Lorenz
library(ineq)

paises <- read.csv("C:/Users/Amanda/Documents/dashboard-ved/country_freq.csv") 
attach(paises)

df_paises2 <- data.frame(paises)

#Deletando a coluna 'n' (não informativa):
df_paises2[1] <- NULL
View(df_paises2)

#Mudando nome das colunas de df
colnames(df_paises2) <- c('País', 'Frequência')

lorenz_pais = Lc(df_paises2$Frequência)

#Índice de Gini
G = Gini(df_paises2$Frequência)
G = round(G, 2) #2 casas decimais

lc <- lorenz_pais

plot(lorenz_pais, 
     main='Curva de Lorenz País de produção'
     , col=1
     , xaxt='n' #retira valores do eixo x
     , xlab = 'País de produção'
     , ylab = 'Filmes assistidos'
     , lwd = 2)
segments(lc$p[43], 0, lc$p[43], lc$L[43], lty=2, col="#8b0000") #USA
segments(lc$p[42], 0, lc$p[42], lc$L[42], lty=2, col="red") #UK
segments(lc$p[41], 0, lc$p[41], lc$L[41], lty=2, col="orange") #FRA
segments(lc$p[40], 0, lc$p[40], lc$L[40], lty=2, col="yellow") #ALE
segments(lc$p[39], 0, lc$p[39], lc$L[39], lty=2, col="yellow") #BRA yellow
segments(lc$p[38], 0, lc$p[38], lc$L[38], lty=2, col="#90ee90") #ESP
segments(lc$p[37], 0, lc$p[37], lc$L[37], lty=2, col="#90ee90") #CAN

legend(x=0.05,y=0.9, legend = c("43 países de produção",
                                "EUA: 629",
                                "UK: 125",
                                "França: 62",
                                "Alemanha: 47",
                                "Brasil: 32",
                                "Espanha: 29",
                                "Canada: 22"),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2, 2, 2, 2, 2, 2, 2), col = c(1, "#8b0000", "red", "orange", "yellow", "yellow", "#90ee90", "#90ee90"), lwd = 2)

#"total"
len = length(df_paises2$Frequência)

#Somatória freq: total filmes
total <- 0
for (i in 1:len)
  total <- total + df_paises2$Frequência[i]
total #1077
