install.packages("tidyverse") #instalação do tidyverse

library("tidyverse") #carreamento do pacote

#Importando os dados em csv

outter300W<-read.csv("1.7outer-300watt.csv", header = TRUE)
outter100W<-read.csv("1.7outer-100watt.csv", header = TRUE)
outter200W<-read.csv("1.7outer-200watt.csv", header = TRUE)


healthy <- read.csv("healthy with pulley.csv", header = TRUE)

#visualizando os datasets

View(outter300W)
View(healthy)

str(outter300W)
str(healthy)

help(rename)

#alterando os nomes das variáveis

healthy <- rename (healthy,
                          Tempo = "Time.Stamp",
                          Eixox = "X.axis",
                          Eixoy = "Y.axis",
                          Eixoz = "Z.axis")

outter300W <- rename(outter300W,
                         Tempo = "Time.Stamp",
                         Eixox = "X.axis",
                         Eixoy = "Y.axis",
                         Eixoz = "Z.axis")

outter200W <- rename(outter200W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")
outter100W <- rename(outter100W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")


#retirando os dados do primeiro e ultimos segundos de aquisição dos dados 


healthy <- healthy[row.names(healthy) %in% 133:118480,]

outter300W <-  outter300W[row.names(outter300W) %in% 153:136958,]


#incluindo uma coluna indice

indice <- c(1:118348)

indice <- as.numeric(indice)
view(indice)

healthy<- mutate(healthy, indice)


indice <- c(1:136806)

indice <- as.numeric(indice)
view(indice)

outter300W <- mutate(outter300W, indice)


#divindo o dataset para uma janela de 1s e realocando as colunas

healthy <- select(healthy, indice, Tempo, Eixox, Eixoy, Eixoz)

summary(healthy)

n1 <- 448 + 132 # 1 segundo + dados removidos 

healthywindow <- healthy[row.names(healthy) %in% 1:n1,]

n2 <- 448 + 152 # 1 segundo + dados removidos 

outter300Wwindow <- outter300W[row.names(outter300W) %in% 1:n2,]

outter300W <- select(outter300W, indice, Tempo, Eixox, Eixoy, Eixoz)

#visualizando os dados em forma de gráfico

ggplot() + geom_line(data = healthywindow, mapping = aes(x = indice, y= sinal, color="red"))


#gráfico de comparação do sinal healthy x vs ouuter  x

ggplot() + geom_line(data = healthywindow, mapping = aes(x=indice, y=Eixox), color="blue")+
       geom_line(data =outter300Wwindow, mapping=aes(x=indice, y=Eixox), color="red")

#vetor do sinal do rolamento bom 

Signaloutter1.7 <- outter300Wwindow$Eixoy

#criando vetores pra frequencia de amostragem e tempo

fs = 443.0871
tempo = 1
t <- seq(0, tempo, length.out = 448)

view(FT)

FT <- fft(t, Signaloutter1.7)

help("spec.fft")


summary(FT)

par(mfrow = c(2, 1))
plot(t, Signaloutter1.7, type = "l", main = "Signal")

plot(
  FT,
  ylab = "Amplitude",
  xlab = "Frequency",
  type = "l",
  xlim = c(0, 10),
  main = "Spectrum"
)

FS <- spectral::


view(T)

spec.fft()

install.packages("spectral")
library("spectral")
help("spec.fft")
