#Ratings

dados <- read.csv("filmes.csv", header=T)
View(dados)
attach(dados)

#Histograma: Rating (estrelas)
hist(Rating, 
     xlim = c(0,5))

h <- hist(Rating,
          main = 'Distribuição da avaliação dos filmes',
          xlim = c(0,5),
          ylim = c(0,200),
          xlab = 'Rating',
          ylab = 'Frequência',
          col="darkmagenta",
          ) 
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
legend(x =0.05, y =195, 
       legend = c("727 filmes avaliados"),
       border = NULL
       )

#outro Histograma (interativo)
hist <- plot_ly(x = ~ratings,
                type = "histogram") %>% 
  layout(title = "Histograma das avaliações",
         xaxis = list(title = "Estrelas",
                      zeroline = FALSE),
         yaxis = list(title = "Filmes",
                      zeroline = FALSE))
hist

#Boxplot
library(plotly)

ratings <- dados$Rating
#NaN: filmes assistidos, mas não avaliados

#Omitir entradas com NaN em Rating
ratings <- na.omit(ratings)
length(ratings) #727 filmes avaliados

ratings <- data.frame(ratings)
View(ratings)

fig <- plot_ly(data=ratings, y=~ratings, type = "box",
               name = "Estrelas",
               marker = list(color = 'rgb(0,0,0)',
                             outliercolor = 'rgba(219, 64, 82, 0.6)'),
               line = list(color = 'rgb(0,0,0)'))
fig

#Medidas resumo
summary(Rating)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.500   3.000   3.500   3.574   4.000   5.000      35 

# Desvio-padrão (sem NaN)
sd(ratings) # 0.8363413

#Variância
var(ratings) # 0.6994668

#Criar tabela
library(gridExtra)
library(grid)
