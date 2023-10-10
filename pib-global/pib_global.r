#Medidas de Concentração e Desigualdade
#Por Amanda Araujo & Alex Prestes

#install.packages("ineq")
library(ineq)

# Dados FMI
#link: https://www.imf.org/imf/weodatabase/downloadreport?c=512,914,612,171,614,311,213,911,314,193,122,912,313,419,513,316,913,124,339,638,514,218,963,616,223,516,918,748,618,624,522,622,156,626,628,228,924,233,632,636,634,238,662,960,423,935,128,611,321,243,248,469,253,642,643,939,734,644,819,172,132,646,648,915,134,652,174,328,258,656,654,336,263,268,532,944,176,534,536,429,433,178,436,136,343,158,439,916,664,826,542,967,443,917,544,941,446,666,668,672,946,137,546,674,676,548,556,678,181,867,682,684,273,868,921,948,943,686,688,518,728,836,558,138,196,278,692,694,962,142,449,564,565,283,853,288,293,566,964,182,359,453,968,922,714,862,135,716,456,722,942,718,724,576,936,961,813,726,199,733,184,524,361,362,364,732,366,144,146,463,528,923,738,578,537,742,866,369,744,186,925,869,746,926,466,112,111,298,927,846,299,582,487,474,754,698,&s=NGDPD,PPPGDP,&sy=2020&ey=2021&ssm=0&scsm=0&scc=0&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1&wsid=f0fb76c5-f774-4759-92bf-316f8381540e
df = read.csv("../Downloads/WEO_Data.csv", header = TRUE, sep = ";")
View(df)

df2 = df[df$Units == 'U.S. dollars', ]$X2020
df2 = as.numeric(df2)
df2=df2[2:196]

pib <- sort(df2)

# Curva de Lorenz
clorenz_pib = Lc(pib)

#índice de Gini
G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais


# Curva de Lorenz + Gini + Br
plot(clorenz_pib, main='Curva de Lorenz PIB global 2020', col=1,  xlab = 'Países', ylab = 'PIB global')
segments(lc$p[184], 0, lc$p[184], lc$L[184], lty=2, col="green") #Brasil
legend(x=0.05,y=0.9, legend = c("Global", "Brasil"),
                 title = paste('Índice de Gini: G =', G),  # Title
                 title.adj = 0.5,         # Horizontal adjustment of the title
                 title.col = "#b30000",   # Color of the title
                 lty = c(1, 2), col = c(1, 3), lwd = 2)

# colorindo o globo...
plot(clorenz_pib, main='Curva de Lorenz PIB global 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

dfcolorcode = data.frame(
  continent = c("Global", "SouthAmerica", "Africa",   "Asia", "CentralAmerica", "Europe", "NorthAmerica", "Oceania"),
  color =     c(       1,        'green',        6, 'orange',                8,   'blue',          'red', '#ffff00')
) 
  
df3c = df3c[order(df3c$X2020), ]
n=1
for (cont in df3c$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 3, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

dfcolorcodeleg = data.frame(
continentleg = c("Global", 'América do Norte', 'Ásia', 'Europa', 'América do Sul', 'Oceania', "África", "América Central"),
colorleg = c(1, 'red', 'orange', 'blue', 'green', '#ffff00', 6, 8)
)

legend(x=0.02,y=0.98, legend = dfcolorcodeleg$continentleg,
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 1), col = dfcolorcodeleg$colorleg, lwd = 2)

# Zoom...
plot(clorenz_pib, main='Curva de Lorenz PIB global 2020', col=1,  xlab = 'Países', ylab = 'PIB global', xlim=c(0,0.5),ylim=c(0,0.02))

n=1
for (cont in df3c$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 3, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

legend(x=0.05,y=0.018, legend = dfcolorcodeleg$continentleg,
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 1), col = dfcolorcodeleg$colorleg, lwd = 2)

#Top50% 
pib_top = pib[99:195]

clorenz_pib = Lc(pib_top)
G = Gini(pib_top)
G = round(Gini(pib_top), 2) #2 casas decimais

plot(clorenz_pib, main='Curva de Lorenz PIB global 2020 - Top 50%', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
for (cont in df3c$Continent[99:195]) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 3, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

legend(x=0.02,y=0.98, legend = dfcolorcodeleg$continentleg,
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 1), col = dfcolorcodeleg$colorleg, lwd = 2)

#Bottom50%
pib_bottom = pib[1:98]

clorenz_pib = Lc(pib_bottom)
G = Gini(pib_bottom)
G = round(Gini(pib_bottom), 2) #2 casas decimais

plot(clorenz_pib, main='Curva de Lorenz PIB global 2020 - Bottom 50%', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
for (cont in df3c$Continent[1:98]) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 3, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

legend(x=0.02,y=0.98, legend = dfcolorcodeleg$continentleg,
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 1), col = dfcolorcodeleg$colorleg, lwd = 2)

#Reunião G20 *na verdade, Top20, leves alterações 
pib_G20 = pib[176:195]

clorenz_pib = Lc(pib_G20)
G = Gini(pib_G20)
G = round(Gini(pib_G20), 2) #2 casas decimais

plot(clorenz_pib, main='Curva de Lorenz PIB global 2020 - G20', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
for (cont in df3c$Continent[176:195]) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 30, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

legend(x=0.02,y=0.98, legend = dfcolorcodeleg$continentleg,
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 1), col = dfcolorcodeleg$colorleg, lwd = 2)
#Juntos, os integrantes do G20 representam cerca de 80% da produção econômica mundial,
#dois terços da população global e três quartos do comércio internacional.
#Os integrantes do G20 são a Argentina, Austrália, Brasil, Canadá, China, França, Alemanha, Índia, Indonésia,
#Itália, Japão, México, República da Coreia, Rússia, Arábia Saudita, África do Sul, Turquia, Reino Unido, Estados Unidos
#e União Europeia. Ao longo do ano, representantes dos países se reúnem para discutir questões financeiras e socioeconômicas.
# Curva de Lorenz + Gini + Br

# Curva de Lorenz
clorenz_pib = Lc(pib)

#índice de Gini
G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB global 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=176
for (cont in df3c$Continent[176:195]) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 1, col=dfcolorcode[dfcolorcode$continent==cont, ]$color)
  n=n+1
}

polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(clorenz_pib$p[176], 0, clorenz_pib$p[176], clorenz_pib$L[176], lty=2, col="#6600cc") #G20
segments(0, clorenz_pib$L[176], clorenz_pib$p[176], clorenz_pib$L[176], lty=2, col="#6600cc") #G20 riqueza 

riqueza_G20 = round((1 - clorenz_pib$L[176])*100, 1)

legend(x=0.05,y=0.9, legend = c("Global", paste("Top20:", riqueza_G20, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2), col = c(1, '#6600cc'), lwd = 2)

#South America
pib = df3c[df3c$Continent=="SouthAmerica", ]$X2020
# Curva de Lorenz
clorenz_pib = Lc(pib)

#índice de Gini
G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB América do Sul 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
for (cont in df3c[df3c$Continent=="SouthAmerica", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb((12-n)/12, n/12, 0))
  n=n+1
}

polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[12], clorenz_pib$p[12], clorenz_pib$L[12], lty=2, col=3) #br riqueza 

riqueza_br = round((1 - clorenz_pib$L[12])*100, 1)
legend(x=0.05,y=0.9, legend = c("América do Sul", paste("Brasil:", riqueza_br, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2), col = c(1, rgb(0, 1, 0)), lwd = 2)

#Asia
pib = df3c[df3c$Continent=="Asia", ]$X2020
# Curva de Lorenz
clorenz_pib = Lc(pib)

#índice de Gini
G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB Ásia 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
len_df = length(clorenz_pib$p)-1
for (cont in df3c[df3c$Continent=="Asia", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb(n/len_df, n/len_df, (len_df-n)/len_df))
  n=n+1
}

polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[len_df], clorenz_pib$p[len_df], clorenz_pib$L[len_df], lty=2, col='#ffff00') #china riqueza 

riqueza_ch = round((1 - clorenz_pib$L[len_df])*100, 1)
legend(x=0.05,y=0.9, legend = c("Ásia", paste("China:", riqueza_ch, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2), col = c(1, rgb(1, 1, 0)), lwd = 2)

#Africa
pib = df3c[df3c$Continent=="Africa", ]$X2020
clorenz_pib = Lc(pib)

G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB África 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
len_df = length(clorenz_pib$p)-1
for (cont in df3c[df3c$Continent=="Africa", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb(n/len_df, (len_df-n)/len_df, n/len_df))
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[len_df], clorenz_pib$p[len_df], clorenz_pib$L[len_df], lty=2, col='#ff00ff') # riqueza 

riqueza_ch = round((1 - clorenz_pib$L[len_df])*100, 1)
legend(x=0.05,y=0.9, legend = c("África", paste("Nigéria:", riqueza_ch, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2), col = c(1, rgb(1, 0, 1)), lwd = 2)

#Europa
pib = df3c[df3c$Continent=="Europe", ]$X2020
clorenz_pib = Lc(pib)

G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB Europa 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
len_df = length(clorenz_pib$p)-1
for (cont in df3c[df3c$Continent=="Europe", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb((len_df-n)/len_df, (len_df-n)/len_df, n/len_df))
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[len_df], clorenz_pib$p[len_df], clorenz_pib$L[len_df], lty=2, col='#0000ff') # riqueza 
n = len_df-2
segments(0, clorenz_pib$L[len_df-1], clorenz_pib$p[len_df-1], clorenz_pib$L[len_df-1], lty=2, 
         col=rgb((len_df-n)/len_df, (len_df-n)/len_df, n/len_df)) # riqueza 


riqueza_al = round((1 - clorenz_pib$L[len_df])*100, 1)
riqueza_ru = round((1 - clorenz_pib$L[len_df-1])*100 - riqueza_al, 1)
legend(x=0.05,y=0.9, legend = c("Europa", paste("Alemanha:", riqueza_al, '%'), paste("Reino Unido:", riqueza_ru, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2, 2), col = c(1, rgb(0, 0, 1), rgb((len_df-n)/len_df, (len_df-n)/len_df, n/len_df)), lwd = 2)

#oceania
pib = df3c[df3c$Continent=="Oceania", ]$X2020
clorenz_pib = Lc(pib)

G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB Oceania 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
len_df = length(clorenz_pib$p)-1
for (cont in df3c[df3c$Continent=="Oceania", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb(n/len_df, n/len_df, (len_df-n)/len_df))
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[len_df], clorenz_pib$p[len_df], clorenz_pib$L[len_df], lty=2, col='#ffff00') #riqueza 
n=n-2
segments(0, clorenz_pib$L[len_df-1], clorenz_pib$p[len_df-1], clorenz_pib$L[len_df-1], lty=2, col=rgb(n/len_df, n/len_df, (len_df-n)/len_df)) #china riqueza 

riqueza_au = round((1 - clorenz_pib$L[len_df])*100, 1)
riqueza_nz = round((1 - clorenz_pib$L[len_df-1])*100 - riqueza_au, 1)
       
legend(x=0.05,y=0.9, legend = c("Oceania", paste("Austrália:", riqueza_au, '%'), paste("Nova Zelândia:", riqueza_nz, '%')),
       title = paste('Índice de Gini: G =', '0.90'),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2, 2), col = c(1, rgb(1, 1, 0), rgb(n/len_df, n/len_df, (len_df-n)/len_df)), lwd = 2)

#CentralAmerica
pib = df3c[df3c$Continent=="CentralAmerica", ]$X2020
clorenz_pib = Lc(pib)
G = Gini(pib)
G = round(Gini(pib), 2) #2 casas decimais
plot(clorenz_pib, main='Curva de Lorenz PIB América Central 2020', col=1,  xlab = 'Países', ylab = 'PIB global')

n=1
len_df = length(clorenz_pib$p)-1
for (cont in df3c[df3c$Continent=="Africa", ]$Continent) {
  segments(clorenz_pib$p[n], 0, clorenz_pib$p[n], clorenz_pib$L[n],
           lty=1, lwd = 2, col= rgb(n/len_df, 0.5*n/len_df, 0))
  n=n+1
}
polygon(x = clorenz_pib$p,  # X-Coordinates of polygon 
        y = clorenz_pib$L,    # Y-Coordinates of polygon
        col = '#f2f2f2') 
lines(clorenz_pib, lwd=3)

segments(0, clorenz_pib$L[len_df], clorenz_pib$p[len_df], clorenz_pib$L[len_df], lty=2, col='#ff8000') # riqueza 

riqueza_ch = round((1 - clorenz_pib$L[len_df])*100, 1)
legend(x=0.05,y=0.9, legend = c("América Central", paste("Porto Rico:", riqueza_ch, '%')),
       title = paste('Índice de Gini: G =', G),  # Title
       title.adj = 0.5,         # Horizontal adjustment of the title
       title.col = "#b30000",   # Color of the title
       lty = c(1, 2), col = c(1, rgb(1, 1/2, 0)), lwd = 2)

