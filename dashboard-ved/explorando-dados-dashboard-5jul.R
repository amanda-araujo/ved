#Explorando meus dados Letterboxd: 2016 - 2022

#GÊNEROS
generos <- read.csv("C:/Users/Amanda/Documents/dashboard-ved/genre_freq.csv") 
View(generos)
attach(generos)

library(ggplot2)

df_generos <- data.frame(generos)
df_generos

#Mudando nome das colunas de df
colnames(df_generos) <- c('n', 'Gênero', 'Frequência')
df_generos

#Acessando cada variável de df
df_generos$Gênero
df_generos$Frequência

len = length(df_generos$Frequência)
df_generos$Gênero[2]

#Somatória freq: total filmes
total <- 0
for (i in 1:len)
  total <- total + df_generos$Frequência[i]
total

#Deletando a coluna 'n' (não informativa):
df_generos[1] <- NULL
View(df_generos)

df_generos[1]

df_generos[2]

#Representação tabular
#já tenho uma tabela de frequências!

#Representação variáveis qualitativas em R

# GRÁFICO DE BARRAS
barplot(height = df_generos$Frequência, 
        names = df_generos$Gênero,
        xlab = "Frequência",
        ylab = "Gênero",
        xlim = c(0,400),
        col = rainbow(19),
        horiz = TRUE,
        las = 1) #orientação argumento 

#Sorting data (by freq *-: descendente)
df_generos <- df_generos[order(df_generos$Frequência),]

# Increase margin size
par(mar=c(6,8,4,4)) #bottom, left, top, right
#The default is c(5, 4, 4, 2) + 0.1.

barplot(height = df_generos$Frequência, 
        names = df_generos$Gênero,
        main = 'Gêneros',
        xlab = "Frequência",
        ylab = NULL,
        xlim = c(0,400),
        col = rainbow(19),
        horiz = TRUE,
        las = 1) #orientação argumento 

#the orientation of the axis labels: las
#0: always parallel to the axis
#1: always horizontal
#2: always perpendicular to the axis
#3: always vertical.
#This is specially helpful for horizontal bar chart.

# TREEMAP (quadrados: área)
install.packages('treemap')
library(treemap)

treemap(dtf = df_generos, #data
        index = "Gênero",
        vSize = "Frequência",
        type = "index",
        title = "Gêneros")

treemap(dtf = df_generos, #data
        index = "Gênero",
        vSize = "Frequência",
        type = "index",
        title = "Gêneros",
        palette = ("Spectral")
        )

treemap(dtf = df_generos, #data
        index = "Gênero",
        vSize = "Frequência",
        type = "index",
        title = "Gêneros",
        palette = ("Dark2")
)

treemap(dtf = df_generos, #data
        index = "Gênero",
        vSize = "Frequência",
        type = "index",
        title = "Gêneros",
        palette = ("Set1")
)
