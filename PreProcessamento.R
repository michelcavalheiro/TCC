install.packages("tidyverse") 

#instalação do tidyverse

install.packages("spectral")  

#instalação do spectral

library("tidyverse")

#carreamento do pacote

library("spectral") 

#carreamento do pacote

#Importando os dados em csv

outter300W<-read.csv("1.7outer-300watt.csv", header = TRUE)
outter100W<-read.csv("1.7outer-100watt.csv", header = TRUE)
outter200W<-read.csv("1.7outer-200watt.csv", header = TRUE)

inner300W<-read.csv("1.7inner-300watt.csv", header = TRUE)
inner100W<-read.csv("1.7inner-100watt.csv", header = TRUE)
inner200W<-read.csv("1.7inner-200watt.csv", header = TRUE)
                        
inner11100W<-read.csv("1.1inner-100watt.csv", header = TRUE)
inner11200W<-read.csv("1.1inner-200watt.csv", header = TRUE)
inner11300W<-read.csv("1.1inner-300watt.csv", header = TRUE)

outter11100W<-read.csv("1.1outer-100watt.csv", header = TRUE)
outter11200W<-read.csv("1.1outer-200watt.csv", header = TRUE)
outter11300W<-read.csv("1.1outer-300watt.csv", header = TRUE)


healthypulley <- read.csv("healthy with pulley.csv", header = TRUE)

#visualizando os datasets

View(outter300W)
View(healthy)

str(outter300W)
str(healthy)

help(rename)

#alterando os nomes das variáveis

healthypulley <- rename (healthypulley,
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


inner300W <- rename(inner300W,
                    Tempo = "Time.Stamp",
                    Eixox = "X.axis",
                    Eixoy = "Y.axis",
                    Eixoz = "Z.axis")

inner200W <- rename(inner200W,
                    Tempo = "Time.Stamp",
                    Eixox = "X.axis",
                    Eixoy = "Y.axis",
                    Eixoz = "Z.axis")
inner100W <- rename(inner100W,
                    Tempo = "Time.Stamp",
                    Eixox = "X.axis",
                    Eixoy = "Y.axis",
                    Eixoz = "Z.axis")


inner11100W<- rename(inner11100W,
                    Tempo = "Time.Stamp",
                    Eixox = "X.axis",
                    Eixoy = "Y.axis",
                    Eixoz = "Z.axis")


inner11200W<- rename(inner11200W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")

inner11300W<- rename(inner11300W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")

outter11100W<-rename(outter11100W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")
outter11200W<-rename(outter11200W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")
outter11300W<-rename(outter11300W,
                     Tempo = "Time.Stamp",
                     Eixox = "X.axis",
                     Eixoy = "Y.axis",
                     Eixoz = "Z.axis")


#criando os vetores pra fazer a fft no eixo x em 444 observações (1 segundo)

vecxhealthypulley <- healthypulley[row.names(inner100W) %in% 1:444,]
vecxhealthypulley <- vecxhealthypulley$Eixox

vecxinner100w <- inner100W[row.names(inner100W) %in% 1:444,]
vecxinner100w <- vecxinner100w$Eixox

vecxinner200w <- inner200W[row.names(inner200W) %in% 1:444,]
vecxinner200w <- vecxinner200w$Eixox

vecxinner300w <- inner300W[row.names(inner300W) %in% 1:444,]
vecxinner300w <- vecxinner300w$Eixox

vecxoutter100w <- outter100W[row.names(inner100W) %in% 1:444,]
vecxoutter100w <- vecxoutter100w$Eixox

vecxoutter200w <- outter200W[row.names(inner200W) %in% 1:444,]
vecxoutter200w <- vecxoutter200w$Eixox

vecxoutter300w <- outter300W[row.names(outter300W) %in% 1:444,]
vecxoutter300w <- vecxoutter300w$Eixox

vecxinner11100w <- inner11100W[row.names(inner11100W) %in% 1:444,]
vecxinner11100w<- vecxinner11100w$Eixox

vecxinner11200w <- inner11200W[row.names(inner11200W) %in% 1:444,]
vecxinner11200w<- vecxinner11200w$Eixox

vecxinner11300w <- inner11300W[row.names(inner11300W) %in% 1:444,]
vecxinner11300w<- vecxinner11300w$Eixox

vecxoutter11100w <- outter11100W[row.names(outter11100W)%in% 1:444,]
vecxoutter11100w<- vecxoutter11100w$Eixox

vecxoutter11200w <- outter11200W[row.names(outter11200W)%in% 1:444,]
vecxoutter11200w<- vecxoutter11200w$Eixox

vecxoutter11300w <- outter11300W[row.names(outter11300W)%in% 1:444,]
vecxoutter11300w<- vecxoutter11300w$Eixox


#criando vetor de tempo 

t <- seq(0, 1, length.out = 444)

#calculando todas as fft para os eixos x

FT1 <- spec.fft(vecxhealthypulley , t)

FT2 <- spec.fft(vecxinner100w , t)

FT3 <- spec.fft(vecxinner200w, t)

FT4 <- spec.fft(vecxinner300w, t)

FT5 <- spec.fft(vecxoutter100w, t)

FT6 <- spec.fft(vecxoutter200w, t)

FT7 <- spec.fft(vecxoutter300w, t)


FT8 <- spec.fft(vecxinner11100w  , t)
FT9 <- spec.fft(vecxinner11200w  , t)
FT10<- spec.fft(vecxinner11300w  , t)

FT11<- spec.fft(vecxoutter11100w  , t)
FT12<- spec.fft(vecxoutter11200w  , t)
FT13<- spec.fft(vecxoutter11300w  , t)


# plotando as sinal para rolamento saudavel

plot(t,vecxhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy ")

# plotando as sinal para com falha no inner de 1.7

par(mfrow = c(4, 1))

plot(t,vecxhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy")

plot(t,vecxinner100w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Inner100W ")

plot(t,vecxinner200w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Inner200W ")http://127.0.0.1:36317/graphics/plot_zoom_png?width=1536&height=794

plot(t,vecxinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Inner300W ")

# plotando sinal para com falha no outter de 1.7

par(mfrow = c(4,1))
plot(t,vecxhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy")


plot(t,vecxoutter100w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Outter100W ")

plot(t,vecxoutter200w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Outter200W ")

plot(t,vecxoutter300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "1.7 Outter300W ")

# plotando fft para rolamento saudavel

plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT Rolamento Saudável")

# plotando fft para rolamentos com falha no inner 1,7

par(mfrow = c(4, 1))
plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Rolamento Saudável")

plot(
  FT2,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 100W"
)
plot(
  FT3,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 200W"
)
plot(
  FT4,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 300W"
)


# plotando fft para rolamentos com falha no outter 1,7

par(mfrow = c(4, 1))
plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Rolamento Saudável")
plot(
  FT5,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 100W"
)
plot(
  FT6,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 200W"
)
plot(
  FT7,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 300W"
)


# plotando fft para rolamentos com falha no inner 1,1

par(mfrow = c(4, 1))
plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Rolamento Saudável")

plot(
  FT8,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 1.1 100w"
)
plot(
  FT9,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 1.1 200w"
)
plot(
  FT10,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha Inner 1.1 300w"
)

# plotando fft para rolamentos com falha no outter 1,1

par(mfrow = c(4, 1))
plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Rolamento Saudável")
plot(
  FT11,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 1,1 100w"
)
plot(
  FT12,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 1,1 200w"
)
plot(
  FT13,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha Outter 1,1 300w"
)


# plotando fft para rolamentos com falha no inner 1,1 x 1,7 300w

par(mfrow = c(2, 1))
plot(
  FT4,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT - Falha inner 1,7 300w"
)
plot(
  FT10,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0, 200),
  main = "FFT - Falha inner 1,1 300w"
)





