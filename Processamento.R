install.packages("tidyverse") 

#instalação do tidyverse

library("tidyverse")

#carreamento do pacote


inner300W<-read.csv("1.7inner-300watt.csv", header = TRUE)

healthypulley <- read.csv("healthy with pulley.csv", header = TRUE)

#alterando os nomes das variáveis

healthypulley <- rename (healthypulley,
                         Tempo = "Time.Stamp",
                         Eixox = "X.axis",
                         Eixoy = "Y.axis",
                         Eixoz = "Z.axis")

inner300W <- rename(inner300W,
                    Tempo = "Time.Stamp",
                    Eixox = "X.axis",
                    Eixoy = "Y.axis",
                    Eixoz = "Z.axis")

#criando os vetores pra fazer a fft no eixo x em 444 observações (1 segundo)

vechealthypulley <- healthypulley[row.names(healthypulley) %in% 1:444,]
vecxhealthypulley <- vechealthypulley$Eixox
vecyhealthypulley <- vechealthypulley$Eixoy
veczhealthypulley <- vechealthypulley$Eixoz

vecinner300w <- inner300W[row.names(inner300W) %in% 1:444,]
vecxinner300w <- vecinner300w$Eixox
vecyinner300w <- vecinner300w$Eixoy
veczinner300w <- vecinner300w$Eixoz


#criando vetor de tempo 

t <- seq(0, 1, length.out = 444)

#calculando todas as fft para rolamento saudavel

FT1 <- spec.fft(vecxhealthypulley , t)

FT2 <- spec.fft(vecyhealthypulley , t)

FT3 <- spec.fft(veczhealthypulley , t)

#calculando todas as fft para rolamento inner 300 1,7

FT4 <- spec.fft(vecxinner300w , t)

FT5 <- spec.fft(vecyinner300w , t)

FT6 <- spec.fft(veczinner300w , t)

# plotando as sinal rolamento saudavel

par(mfrow = c(3, 1))

plot(t,vecxhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy eixo x")

plot(t,vecyhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy eixo y")

plot(t,veczhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy eixo z")

# plotando as sinal rolamento inner 300w

par(mfrow = c(3, 1))

plot(t,vecxinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo x")

plot(t,vecyinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo y")

plot(t,veczinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo z")


# plotando  sinal vs fft rolamento saudavel

par(mfrow = c(2, 1))
plot(
  FT1,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Healthy Eixo x")

plot(t,vecxinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo x")

par(mfrow = c(2, 1))


plot(
  FT2,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Healthy Eixo y"
)

plot(t,vecyhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy eixo y")

par(mfrow = c(2, 1))

plot(
  FT3,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT Healthy Eixo z"
)


plot(t,veczhealthypulley , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Healthy eixo z")


# plotando  sinal vs fft rolamento inner 300

par(mfrow = c(2, 1))
plot(
  FT4,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT inner 300 Eixo x")

plot(t,vecxinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo x")


par(mfrow = c(2, 1))
plot(
  FT5,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT inner 300 Eixo y")

plot(t,vecyinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo y")



par(mfrow = c(2, 1))
plot(
  FT6,
  ylab = "Vibration Signal",
  xlab = "Frequency",
  type = "h",
  xlim = c(0,220),
  main = "FFT inner 300 Eixo z")
plot(t,veczinner300w , 
     ylab = "Vibration Signal",
     xlab = "Time (sec)",
     type = "l", main = "Inner 300w eixo z")
