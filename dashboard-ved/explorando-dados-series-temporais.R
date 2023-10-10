#time series: my films letterboxd June 2016 - July 2022

dados <- read.csv("filmes.csv", header=T)
View(dados)
attach(dados)

df_diario <- data.frame(dados)
df_diario

#Deletando colunas não informativas: 
df_diario[1] <- NULL #'X'
View(df_diario)

df_diario[4] <- NULL #Letterboxd.url
df_diario[7] <- NULL #ID
df_diario[12] <- NULL #voteavg
df_diario[12] <- NULL #votecount
df_diario[1] <- NULL #Date (data arbitrária)

#Mudando nome das colunas de df
#colnames(df_diario) <- c('n', 'Gênero', 'Frequência')
#df_generos

#Gráfico: filmes x data assistida (Watched.Date)
#Data format: 2016-06-18 :: year-month-day

library(ggplot2)
ggplot(data = df_diario, aes(x = Watched.Date, y = Name)) +
  geom_point() +
  labs(x = "Data",
       y = "Filmes", 
       title = "Diário de Filmes") #nops!

#Watched.Date: chr
#need to convert date column to a date class: function as.Date()

# convert date column to date class
df_diario$Watched.Date <- as.Date(df_diario$Watched.Date,
                                  format = "%Y-%m-
                                  %d")
#   ps: %Y - 4 digit year, %y - 2 digit year, %m - month, %d - day

# view R class of data
class(df_diario$Watched.Date)
## [1] "Date" OK

# view results
head(df_diario$Watched.Date)
## [1] "2016-06-18" "2016-06-28" "2016-06-28" "2016-06-29" "2016-06-30"
## [6] "2016-06-30" 

#agora R entende que são datas e não tenta plotar cada data indivudualmente: aglutina
ggplot(data = df_diario, aes(x = Watched.Date, y = Name)) +
  geom_point() +
  labs(x = "Data",
       y = "Filmes", 
       title = "Diário hehe") +
  theme(axis.ticks.y = element_blank(), #retira valores do eixo y
        axis.text.y = element_blank()) +
  scale_x_date(breaks="year")#, labels = c("2017", "2018", "2019", "2020", "2021", "2022"))
#seria legal passar o mouse por cima e ver qual o nome do filme
#cores para diferentes gêneros (!filmes com mais de 1 gênero -> pega o primeiro indicado)

#interativo! :D
library(plotly)
p <- ggplot(data = df_diario, aes(x = Watched.Date, y = Name)) +
  geom_point() +
  labs(x = "Data",
       y = "Filmes", 
       title = "Diário+") +
  theme(axis.ticks.y = element_blank(), #retira valores do eixo y
        axis.text.y = element_blank()) +
  scale_x_date(breaks="year",
               labels = date_format(format="%Y"))
ggplotly(p)

#interativo + size + color NÃO TÁ FUNCIONANDO
library(gapminder)
q <- gapminder %>%
  ggplot(data = df_diario, aes(x = Watched.Date, y = Name, size = Rating, color = Rating)) +
  geom_point() +
  theme_bw() +
  labs(x = "Data",
       y = "Filmes", 
       title = "Diário +") +
  theme(axis.ticks.y = element_blank(), #retira valores do eixo y
        axis.text.y = element_blank()) +
  scale_x_date(breaks="year")
ggplotly(q)

#interativo! tentativa + variáveis com COR >>> N TÁ COLORINDO
library(plotly)
r <- ggplot(data = df_diario, aes(x = Watched.Date, y = Name, fill = Rating)) +
  geom_point() +
  scale_fill_viridis_c() + 
  labs(x = "Data",
       y = "Filmes", 
       title = "Diário+") +
  theme(axis.ticks.y = element_blank(), #retira valores do eixo y
        axis.text.y = element_blank()) +
  scale_x_date(breaks="year")
ggplotly(r)


####
#Ano lançamento do filme
library(ggplot2)
library(plotly)
library(gapminder)
library(scales)

# Ano lançamento x ano assistido
s <- ggplot(data = df_diario, aes(x = Watched.Date, y = Year)) +
  geom_point() +
  labs(x = "Data assitida",
       y = "Ano lançamento", 
       title = "Ano lançamento x ano assistido") +
  theme(axis.ticks.y = element_blank(), #retira valores do eixo y
        axis.text.y = element_blank()) +
  scale_x_date(breaks="year")
ggplotly(s)

# 
# convert date column to date class
df_diario$releasedate <- as.Date(df_diario$releasedate,
                                  format = "%Y-%m-%d")
#   ps: %Y - 4 digit year, %y - 2 digit year, %m - month, %d - day

# view R class of data
class(df_diario$releasedate)
# view results
head(df_diario$releasedate)



t <- ggplot(data = df_diario, aes(x = releasedate, y = Watched.Date)) +
  geom_point() +
  labs(x = "Data lançamento",
       y = "Data assistida", 
       title = "Ano assistido x Data de lançamento") +
  #theme(axis.ticks.y = element_blank(), #retira valores do eixo y
  #      axis.text.y = element_blank()) +
  scale_x_date(breaks="10 years",
               labels = date_format("%Y")) + #,
               #limits = as.Date(c('2016-01-01', '2022-07-01'))) 
  scale_y_date(breaks="year",
               labels = date_format("%Y"),
               limits = as.Date(c('2016-01-01', '2022-07-30')))
ggplotly(t)


# 
#falta carregar o NOME do filme!!!
u <- gapminder %>%
  ggplot(data = df_diario, aes(x = releasedate, y = Watched.Date, color = Rating, size = runtime)) +
  geom_point() +
  labs(x = "Data lançamento",
       y = "Data assistida", 
       title = "Ano assistido x Data de lançamento") +
  #theme(axis.ticks.y = element_blank(), #retira valores do eixo y
  #      axis.text.y = element_blank()) +
  scale_x_date(breaks="10 years",
               labels = date_format("%Y")) + #,
  #limits = as.Date(c('2016-01-01', '2022-07-01'))) 
  scale_y_date(breaks="year",
               labels = date_format("%Y"),
               limits = as.Date(c('2016-01-01', '2022-07-30'))) +
  theme_bw()
ggplotly(u)

# Com nome!
v <- ggplot(data = df_diario, aes(x = releasedate, y = Watched.Date, label = Name, color = Rating, size = runtime)) +
  geom_point() +
  labs(x = "Data lançamento",
       y = "Data assistida", 
       title = "Ano assistido x Data de lançamento") +
  scale_x_date(breaks="10 years",
               labels = date_format("%Y")) + #,
  #limits = as.Date(c('2016-01-01', '2022-07-01'))) 
  scale_y_date(breaks="year",
               labels = date_format("%Y"),
               limits = as.Date(c('2016-01-01', '2022-07-30'))) +
  theme_bw()
ggplotly(v)

#Mudando nome das colunas de df
colnames(df_diario) <- c('Name', 'Year', 'Rating', 'Rewatch', 'Watched Date', 'Languages', 'Production Country', 'Genres', 'Duration', 'Release Date')


#######
#gráfico linha: filmes x datas

linha <- df_diario$`Release Date`

#frequency table
tabela_diario <- table(releasedate)
tabela_diario


barplot(tabela_diario) +
  scale_x_date(breaks="10 years",
               labels = date_format("%Y")) #nops

# Tem que contar n por ano, mas por década!
library(plyr)
a = na.omit(as.Date(df_diario$releasedate, format = "%Y-%m-%d"))
a = format(a, "%Y-%m")
a = count(a)
a$x = as.Date(paste(a$x,"-01",sep=""), "%Y-%m-%d")

plot(a$x, a$freq, "l")




b = na.omit(as.Date(df_diario$Watched.Date, format = "%Y-%m-%d"))
b = format(b, "%Y-%m")
b = count(b)
b$x = as.Date(paste(b$x,"-01",sep=""), "%Y-%m-%d")

  
d <- ggplot(data = b, aes(x = x, y = freq)) +
  geom_line(color = "pink") +
  labs(x = "Data lançamento",
       y = "Filmes", 
       title = "assistido") +
  scale_x_date(breaks="12 month",
               labels = date_format("%Y-%m"),
               limits = as.Date(c("2016-01-01", "2022-07-01"), "%Y-%m-%d") ) +
theme_bw()
ggplotly(d)




# convert date column to date class
a$releasedate <- as.Date(a$x,
                         format = "%Y-%m-%d")
class(a$releasedate)

b <-ggplot(data=a, aes(x = releasedate, y = freq)) +
  geom_bar(stat="identity") 
b 


#Soma tempo total assistindo filmes
tempo_total <- 0
len = length(df_diario$runtime)

df_diario[is.na(df_diario$runtime)] = 0

for (i in 1:len)
  tempo_total <- tempo_total + df_diario$runtime[i]
tempo_total 

