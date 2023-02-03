dados<-read.table("dados2.txt",header=T,sep="")
names(dados)
dados[1,]
attach(dados)
require(forecast)
require(tseries)
require(TSA)
require(descomponer)
require(moments)
require(stats)
require(astsa)

## Série Original
xv=(mean(prop[522:523]))
z=ts(c(prop[366:521], xv,prop[524:643]),start=c(2010, 1), frequency=52) 
z_total=ts(c(prop[366:521], xv,prop[524:658]),start=c(2010, 1), frequency=52) 
plot.ts(z, xlab="Tempo", ylab="Taxa de ILI", type="l")
stats::acf(z, lag.max = NULL,,  main = "", xlab="Lag", ylab="ACF")
stats::pacf(z, lag.max = NULL,  main = "", xlab="Lag", ylab="ACF Parcial")
zz=as.vector(z)

##################### Gráfico da Série ############

nome2<-paste("graf1a", ".pdf",sep="")
pdf(file = nome2,width =5.7, height = 4) 
plot.ts(z, xlab="Tempo", ylab="Taxa de ILI", type="l")
dev.off()

nome2<-paste("graf1a", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot(z, col=1, type="l", axes = T, main="", xlab="Tempo", ylab="Taxa de ILI")
axis(1)
axis(2)
box()
dev.off()

# Análise descritiva dados originais
summary(z)
kurtosis(z)
skewness(z)
adf.test(z)
shapiro.test(z)

# Box.test null hypothesis of independence in a given time series.
teste<-Box.test(z, lag = 100, type = c("Ljung-Box"), fitdf = 0)
teste$p.value

############# PDF ACF_Série e ACFP_Série #########
nome2<-paste("acf_serie", ".pdf",sep="")
pdf(file = nome2,width =5.7, height = 4) 
stats::acf(z, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC", type = c("correlation"))
dev.off()

nome2<-paste("pacf_serie", ".pdf",sep="")
pdf(file = nome2,width =5.7, height = 4) 
stats::pacf(z, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

# ajustanto com covariaveis
h1=15
n<-length(z)

mX1 <-cbind( sin(2*pi*(1:n)/52), cos(2*pi*(1:n)/52)) 
mX_hat1 <- cbind( sin(2*pi*((n+1):(n+h1))/52), cos(2*pi*((n+1):(n+h1))/52))# ocorrencias da covariavel usadas para previsoes

mX2 <-cbind( 1:n, sin(2*pi*(1:n)/52), cos(2*pi*(1:n)/52)) # covariavel para modelar a tendencia (dentro do intervalo)
mX_hat2 <- cbind((n+1):(n+h1), sin(2*pi*((n+1):(n+h1))/52), cos(2*pi*((n+1):(n+h1))/52))# ocorrencias da covariavel usadas para previsoes

modelo=auto.arima(z, max.p=5, max.q=5, max.P=5, max.Q=5, max.order=5, max.d=2, max.D=1, 
                  start.p=1, start.q=1, start.P=1, start.Q=1, stationary=T )

modelo1=auto.arima(z, max.p=5, max.q=5, max.P=0, max.Q=0, max.order=5, max.d=2, max.D=0, 
                  start.p=1, start.q=1, start.P=0, start.Q=0, stationary=T, xreg=mX1 )

modelo2=auto.arima(z, max.p=5, max.q=5, max.P=0, max.Q=0, max.order=5, max.d=2, max.D=0, 
                   start.p=1, start.q=1, start.P=0, start.Q=0, stationary=T, xreg=mX2 )


modelo1=arima(z, order = c(2, 0, 2), seasonal = list(order = c(0, 0, 0)), xreg = mX1)

modelo2=arima(z, order = c(1, 0, 1), seasonal = list(order = c(0, 0, 0)), xreg = mX2)

# Modelo é SARIMA(2,0,1)(1,0,0)[52]
resi=as.vector(residuals(modelo))
resi_padrao=as.vector((modelo$residuals)/(sd(resi)))
stats::acf(resi_padrao, main = "", xlab="Defasagem", ylab="ACF")
stats::pacf(resi_padrao,   main = "", xlab="Defasagem", ylab="ACF Parcial")

# Resíduo modelo ARMA com covariáveis 
resi1=as.vector(residuals(modelo1))
resi_padrao1=as.vector((modelo1$residuals)/(sd(resi1)))
acf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="ACF", type = c("correlation"))
pacf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="ACF Parcial")


# Análise descritiva resíduos Modelo sarima
summary(resi_padrao)
kurtosis(resi_padrao)
skewness(resi_padrao)
adf.test(resi_padrao) # teste de estacionariedade
shapiro.test(resi_padrao)
# Box.test null hypothesis of independence in a given time series.
teste<-Box.test(resi_padrao, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste$p.value

# Modelo ARMA com covariáveis
summary(resi_padrao1)
kurtosis(resi_padrao1)
skewness(resi_padrao1)
adf.test(resi_padrao1)
shapiro.test(resi_padrao1)
teste<-Box.test(resi_padrao1, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste$p.value

############################ MODELO SARIMA ###################################
# PDF ACF_Série e ACFP_Série
nome2<-paste("acf_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(resi_padrao, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC")
dev.off()

nome2<-paste("pacf_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(resi_padrao, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos resíduos SARIMA
nome2<-paste("qqnorm_res_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(resi_padrao,na.rm=T)
min_r<- min(resi_padrao,na.rm=T)
qqnorm(resi_padrao, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis emp?ricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Resíduos vs. índices SARIMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(resi_padrao, main=" ",xlab="indices",ylab="residuos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()

############################ MODELO ARMA ###################################
# PDF ACF_Série e ACFP_Série
nome2<-paste("acf_arma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC")
dev.off()

nome2<-paste("pacf_arma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos resíduos ARMA
nome2<-paste("qqnorm_res_arma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(resi_padrao1,na.rm=T)
min_r<- min(resi_padrao1,na.rm=T)
qqnorm(resi_padrao1, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis empíricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Resíduos vs. índices ARMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_arma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(resi_padrao1, main=" ",xlab="índices",ylab="resíduos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()


############################ MODELO BARMA ###################################
source("barma.r")
source("best.barma.r")
x1=c(rep(1,207), rep(0,157))
# ajustanto com covariaveis
mX <-cbind(1:n, sin(2*pi*(1:n)/52), cos(2*pi*(1:n)/52)) # covariavel para modelar a tendencia (dentro do intervalo)

mX_hat <- cbind( (n+1):(n+h1), sin(2*pi*((n+1):(n+h1))/52), cos(2*pi*((n+1):(n+h1))/52))# ocorrencias da covariavel usadas para previsoes

best <- best.barma(z,sf=c(start=c(2010, 1),frequency=52),pmax=5, qmax=5, X = mX, X_hat = mX_hat)

barma2<-barma(z,ar=c(1,2), ma=c(1), h=h1,diag=2,X = mX, X_hat = mX_hat,resid=1)

dados=data.frame(cbind(z, mX))
colnames(dados)=c("y", "t", "sin", "cos")
library(tidyverse)
view(dados)

write.table(dados, file = "dados.txt", sep = " ", row.names = FALSE, na = "", quote = FALSE)


res=barma2$resid1
pref=barma2$forecast
zp=pref
za=prop[644:658]
teste<-Box.test(res, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste$p.value
shapiro.test(res)

# PDF ACF_Série e ACFP_Série
nome2<-paste("acf_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(res, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC", type = c("correlation"))
dev.off()

nome2<-paste("pacf_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(res, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos resíduos BARMA
nome2<-paste("qqnorm_res_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(res,na.rm=T)
min_r<- min(res,na.rm=T)
qqnorm(resi_padrao, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis emp?ricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Resíduos vs. índices BARMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(res, main=" ",xlab="?ndices",ylab="res?duos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()


################ Previsão ###############
fit <- arima(z, order = c(2,0,1), seasonal = list(order = c(1,0,0)))
zs=predict(modelo, n.ahead=15)
zs1=predict(modelo1, n.ahead=15, newxreg = mX_hat1)

pr=sarima.for(z, 15, 2,0,1, 1,0,0,52)
za=prop[644:658]

########## Gráfico de Previsão ###########
nome2<-paste("previsao", ".pdf",sep="")
pdf(file = nome2,width =5.7, height = 4) 
plot( za, col=1, type="l", ylim=c(0.0015, 0.0035) , axes = F, main="", xlab="Semanas", ylab="Taxa de ILI")
lines(zp, lty = 2, lwd = 1)
lines(as.vector(zs$pred), lty = 3, lwd = 1,)
#lines(as.vector(zs1$pred), lty = 4, lwd = 1,)
legend("topright",legend=c( "Valores reais", expression(paste("Valores previstos (", beta, "ARMA)") ), "Valores previstos (SARMA)"  ),
                    pt.bg="white", lty=c(1,2,3), bty="n" )
axis(1, 1:15, c(18,19,20,21,22,23,24,25,26,27,28,29,30,31,32))
axis(2)
box()
dev.off()

###################### EQM e MAPE período de ajuste#######################

############### EQM #################
residuos_b = as.vector(z-barma2$fitted)
residuos_beta = residuos_b[4:277]
residuos_sarima = as.vector(modelo$residuals)
residuos_arma = as.vector(modelo1$residuals)


(eqm_beta = (sum(residuos_beta^2))/length(residuos_beta))
(eqm_sarima = (sum(residuos_sarima^2))/length(residuos_sarima))
(eqm_arma = (sum(residuos_arma^2))/length(residuos_arma))

############### MAPE #################

(maple_beta = sum( abs(residuos_beta)/abs(z[4:277]) )/ length(residuos_beta))
(maple_sarima = sum( abs(residuos_sarima)/abs(z[1:277]) )/ length(residuos_sarima))
(maple_arma = sum( abs(residuos_arma)/abs(z[1:277]) )/ length(residuos_arma))



###################### EQM e MAPE período previsao #######################

############### EQM #################
residuos_beta_prev = (za-zp)
residuos_sarima_prev = (za-as.vector(zs$pred))
residuos_arma_prev = (za-as.vector(zs1$pred))

(eqm_beta_prev = (sum(residuos_beta_prev^2))/length(residuos_beta_prev))
(eqm_sarima_prev = (sum(residuos_sarima_prev^2))/length(residuos_sarima_prev))
(eqm_arma_prev = (sum(residuos_arma_prev^2))/length(residuos_arma_prev))

############### MAPE #################

(maple_beta_prev = sum( abs(residuos_beta_prev)/abs(za) )/ length(residuos_beta_prev))
(maple_sarima_prev = sum( abs(residuos_sarima_prev)/abs(za) )/ length(residuos_sarima_prev))
(maple_ar_prev = sum( abs(residuos_arma_prev)/abs(za) )/ length(residuos_arma_prev))


################# Gráfico da série original e ajustada ########################


########## Beta  ###########
nome2<-paste("serie_ajustada_beta", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot( z, col=1, type="l", axes = T, main="", xlab="Tempo", ylab="Taxa de ILI")
lines((barma2$fitted), lty = 1, lwd = 1, col=2)
legend("topleft",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,1),  col=c(1,2), bty="n" )
axis(1)
axis(2)
box()
dev.off()

########## Sarima ###########
val_ajus_sarima = z-modelo$residuals
nome2<-paste("serie_ajustada_sarima", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot( z, col=1, type="l", axes = T, main="", xlab="Tempo", ylab="Taxa de ILI")
lines((val_ajus_sarima), lty = 1, lwd = 1, col=2)
legend("topleft",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,1),  col=c(1,2), bty="n" )
axis(1)
axis(2)
box()
dev.off()


########## Arma ###########
val_ajus_arma = z-modelo1$residuals
nome2<-paste("serie_ajustada_arma", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot( z, col=1, type="l", axes = T, main="", xlab="Tempo", ylab="ILI percentual")
lines((val_ajus_arma), lty = 2, lwd = 1, col=1)
legend("topleft",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,2),  col=c(1,1), bty="n" )
axis(1)
axis(2)
box()
dev.off()

teste<-Box.test(res, lag = 274, type = c("Ljung-Box"), fitdf = 0)
teste$p.value





