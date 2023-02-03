# exemplo de utilizacao do modelo BARMA com covariavel

source("scripts_barma/analise_barmax/barma.r")

#Lendo dados com tendencia (dados reais de credito consignado)
data<-read.table("consig.txt") # ler os dados
attach(data)
y<-V3 #
y<-ts(y,start=c(2007,3),frequency=12) # transforma em uma serie temporal
n<-length(y)

plot(y) # note que tem forte tendencia
acf(y)
pacf(y)

h1<-12 # numero de previsoes passos a frente

# ajustando sem covariaveis
barma1<-barma(y,ar=c(1,2),ma=c(1,2),h=h1,diag=1)

# ajustanto com covariaveis
mX <- 1:n # covariavel para modelar a tendencia (dentro do intervalo)
mX_hat <- (n+1):(n+h1) # ocorrencias da covariavel usadas para previsoes

barma2<-barma(y,ar=c(1,2),ma=c(1,2),h=h1,diag=1, X = mX, X_hat = mX_hat,resid=1)


print(" ")
print("SEM covariavel")
print(barma1$model)
print(" ")
print("COM covariavel")
print(barma2$model)

# valores previstos passos a frente
print(barma1$forecast)
print(barma2$forecast)

# plot dos valores previstos passos a frente
plot(barma1$forecast,type="p",col="red",pch=1)
points(barma2$forecast,col="blue",pch=3)

# usando best.barma.r
source("best.barma.r")

# usando covariavel
best.barma(y,sf=c(start=c(2007,3),frequency=12),pmax=6, qmax=6,
           X = mX , X_hat = mX_hat)

# Teste Ljung Box
teste<-Box.test(barma2$resid1, lag = 5, type = c("Ljung-Box"), fitdf = 0)
teste$p.value

###### Simulando
source("simu_barma.r")
y<-simu.barma(50,phi=c(0.45,0.3),theta=c(0.5),prec=100)#
plot(y)

fit<-barma(y,ar=c(1,2),ma=c(1),h=h1,diag=1)
teste<-Box.test(fit$resid1, lag = 5, type = c("Ljung-Box"), fitdf = 0)
teste$p.value

