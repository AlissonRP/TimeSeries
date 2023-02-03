require(moments)
require(tseries)
require(descomponer)
library(forecast) # useful package

# Created by Fabio M. Bayer (bayer@ufsm.br)
# July 2018
#
# It is an example of the KARMA model (BAYER, BAYER AND PUMI, 2017) application. 
# 
# In this application we consider the Relative Humidity data in Santa Maria, Rs, Brazil. 
# It is the same data used in Bayer, Cintra and Cribari-Neto (2018). 
#
# References:
#
# BAYER, F.M.; BAYER, D.M. ; PUMI, G. 
# Kumaraswamy autoregressive moving average models for double bounded environmental data. 
# Journal of Hydrology
# 2017
# DOI: 10.1016/j.jhydrol.2017.10.006


# read the data
ur<-scan(file="ur-sm-mensal.txt")
y<-ur/100 # percentage
Y<-ts(y,start=c(2003,1),frequency=12) # time series

# sample size
n<-length(Y)

# Descriptive analysis
summary(Y,na.rm=T) 
var(Y)

# number of forecast steps
h1 <- 10

# Taking off the last 12 observations
n<-n-h1

y<-ts(Y[1:n], start=c(2003,1), frequency = 12)

# some graphics
plot(y)
monthplot(y)
acf(y)
pacf(y)

## covariates 

# # tendency
t <- 1:n # in sample
t_hat <- (n+1):(n+h1) # out of sample

# deterministic seazonality
C<-cos(2*pi*t/12) # in sample 
C_hat<-cos(2*pi*t_hat/12) # out of sample

# S<-sin(2*pi*t/12) # in sample
# S_hat<-sin(2*pi*t_hat/12) # out of sample

# More than on covariate
# mX<-cbind(S,C) # in sample
# mX_hat<-cbind(S_hat,C_hat) # out of sample

# read the karma function
source("karma.r")

# Example 1: fit the model KARMA(1,1) with covariates
fit1k <- karma(y,ar=c(1),ma=c(1),h=h1,diag=1,X = C,X_hat=C_hat)
residq<-fit1k$resid3
Box.test(residq, lag = 20, type =  "Ljung-Box", fitdf = 0)
acf(residq)
pacf(residq)

# Example 2: fit the model KARMA(2,2) without covariates
fit2k <- karma(y,ar=c(1,2),ma=c(1,2),h=h1,diag=1)
residq<-fit2k$resid3
Box.test(residq, lag = 20, type =  "Ljung-Box", fitdf = 0)

resk=fit1k$resid3

summary(resk)
# Avaliando a normalidade dos resíduos
kurtosis(resk)
skewness(resk)
shapiro.test(resk) # Ho: normalidade
## teste de estatcionaridade 
adf.test(resk) 
# teste de ausência de autocorrelação
teste<-Box.test(resk, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste # H0: as correlações entre diferentes lags são nulas (independencia)


# PDF ACF_residuos e ACFP_residuo
nome2<-paste("acf_k_res", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(resk, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC", type = c("correlation"))
dev.off()

nome2<-paste("pacf_k_res", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(resk, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos residuos KARMA
nome2<-paste("qqnorm_res_karma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(resk,na.rm=T)
min_r<- min(resk,na.rm=T)
qqnorm(resk, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis empíricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Residuos vs. indices KARMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_karma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(resk, main=" ",xlab="Índices",ylab="resíduos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()

# para acessar a previsão
prev_karma=fit1k$forecast

################# Aplicação Beta Sazonal ##################
# Application explored in 
# BAYER, F. M.; CINTRA, R. J.; CRIBARI-NETO, F.. 
# Beta seasonal autoregressive moving average models. 
# Journal of Statistical Computation and Simulations, 
# v. 88, p. 2961-2981, 2018.

source("bsarma.r") # read the code with barma function

seasonplot(y) 
monthplot(y) 
plot(decompose(y)) 

# correlogram
acf(y,lag.max = 24) 
pacf(y,lag.max = 24) 

# zera as opcoes graficas
# op <- par(no.readonly = TRUE) # the whole list of settable par's.
# par(op)

## Fit Beta SARMA model
# Input arguments:
# ar: AR orders
# AR: seasonal AR orders
# ma: MA orders
# MA: seasonal MA orders
# h: forecast horizon
# diag: diagnostic graphs:
#   diag = 0 : do not plot any graph (useful for simulations)
#   diag = 1 : plot graphs 
#   diga = 2 : save graphs on ps files
# resid: there are three types of residuals: 
#   resid = 1 : standardized residual
#   resid = 2 : standardized residual 2
#   resid = 3 : standardized weighted residual

# fit the model
fit1b<-barma(y,ar=c(1),AR=c(1), MA=c(1), h=10,diag=1,resid=3)

resb=fit1b$resid1

summary(resb)
# Avaliando a normalidade dos resíduos
kurtosis(resb)
skewness(resb)
shapiro.test(resb) # Ho: normalidade
## teste de estatcionaridade 
adf.test(resb) 
# teste de ausência de autocorrelação
teste<-Box.test(resb, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste # H0: as correlações entre diferentes lags são nulas (independencia)


# PDF ACF_residuos e ACFP_residuo
nome2<-paste("acf_b_res", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(resb, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC", type = c("correlation"))
dev.off()

nome2<-paste("pacf_b_res", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(resb, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos residuos BSARMA
nome2<-paste("qqnorm_res_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(resb,na.rm=T)
min_r<- min(resb,na.rm=T)
qqnorm(resb, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis empíricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Residuos vs. indices BSARMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_beta", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(resb, main=" ",xlab="Índices",ylab="resíduos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()

prev_bsarma=fit1b$forecast

###############################################
############## Modelo SARIMA ##################
###############################################
modelo1=auto.arima(y, max.p=2, max.q=3, max.P=2, max.Q=3, 
                   max.order=5, max.d=2, max.D=2, 
                   start.p=1, start.q=1, start.P=1, start.Q=1, 
                   stationary=T)
summary(modelo1)
modelo1<-arima(y,c(2,0,1),seasonal=list(order=c(2,0,0),period=12))
summary(modelo1)

resi1=as.vector(residuals(modelo1))
resi_padrao1=as.vector((modelo1$residuals)/(sd(resi1)))
acf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="ACF", type = c("correlation"))
pacf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="ACF Parcial")

# Analise descritiva residuos

summary(resi_padrao1)
# Avaliando a normalidade dos resíduos
kurtosis(resi_padrao1)
skewness(resi_padrao1)
shapiro.test(resi_padrao1)
## teste de estatcionaridade 
adf.test(resi_padrao1) 
# teste de ausência de autocorrelação
teste<-Box.test(resi_padrao1, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste # H0: as correlações entre diferentes lags são nulas (independencia)

############################ MODELO SARIMA ###################################

# PDF ACF_residuo e ACFP_residuo
nome2<-paste("acf_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::acf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FAC")
dev.off()

nome2<-paste("pacf_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
stats::pacf(resi_padrao1, lag.max = NULL,  main = "", xlab="Defasagem", ylab="FACP")
dev.off()

## Q-Q plot dos residuos SARIMA
nome2<-paste("qqnorm_res_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
max_r<- max(resi_padrao1,na.rm=T)
min_r<- min(resi_padrao1,na.rm=T)
qqnorm(resi_padrao1, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis empíricos")
lines(c(-10,10),c(-10,10),lty=2)
dev.off()

## Residuos vs. indices SARIMA
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_sarima", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4) 
plot(resi_padrao1, main=" ",xlab="índices",ylab="resíduos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)
dev.off()


################ Previsao ###############
yprev_sarima=predict(modelo1, n.ahead=10)
m=length(Y)

# y reservado para avaliar as previsões fora do período
# de ajuste
y_obs=Y[(n+1):m] 

########## Grafico de Previsao ###########
nome2<-paste("previsao", ".pdf",sep="")
pdf(file = nome2,width =5.7, height = 4) 
plot(y_obs, col=1, type="l", ylim=c(0.0, 1) , axes = F, main="", xlab="Meses", ylab="Umidade relativa do ar")
lines(as.vector(prev_bsarma), lty = 2, lwd = 1,)
lines(as.vector(prev_karma), lty = 3, lwd = 1,)
lines(as.vector(yprev_sarima$pred), lty = 4, lwd = 1,)
legend("right",legend=c( "Valores reais", 
      expression(paste("Valores previstos (", beta, "ARMA)") ), 
      "Valores previstos (KARMA)", "Valores previstos (SARMA)"  ),
       pt.bg="white", lty=c(1,2,3, 4), bty="n" )
axis(1, 1:10, c("Jan", "Fev", "Mar", "Abr","Mai", "Jun", "Jul", "Ago", "Set", "Out"))
axis(2)
box()
dev.off()

###################### EQM e MAPE periodo previsao #######################
 
############### EQM #################
residuos_beta_prev =(y_obs-prev_bsarma)
residuos_karma_prev = (y_obs-prev_karma)
residuos_sarima_prev = (y_obs-as.vector(yprev_sarima$pred))


eqm_bsarma_prev = (sum(residuos_beta_prev^2))/length(residuos_beta_prev)
eqm_karma_prev = (sum(residuos_karma_prev^2))/length(residuos_karma_prev)
eqm_sarima_prev = (sum(residuos_sarima_prev^2))/length(residuos_sarima_prev)

############### MAPE #################

mape_bsarma_prev = sum( abs(residuos_beta_prev)/abs(y_obs) )/ length(residuos_beta_prev)
mape_karma_prev = sum( abs(residuos_karma_prev)/abs(y_obs) )/ length(residuos_karma_prev)
mape_sarima_prev = sum( abs(residuos_sarima_prev)/abs(y_obs) )/ length(residuos_sarima_prev)

eqm=c(eqm_bsarma_prev, eqm_karma_prev, eqm_sarima_prev)
mape=c(mape_bsarma_prev, mape_karma_prev, mape_sarima_prev)

matriz=cbind(eqm, mape)
rownames(matriz)=c("BSARMA", "KARMA", "SARIMA")
matriz


###### O mesmo pode ser feito dentro do período de ajuste considerando
## o y e o fit1b$fitted para o bsarma, fit1k$fitted

nome2<-paste("serie_ajustada_bsarma", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot(y, col=1, type="l", axes = T, main="", xlab="tempo", ylab="Umidade relativa do ar")
lines((fit1b$fitted), lty = 1, lwd = 1, col=2)
legend("bottomright",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,1),  col=c(1,2), bty="n" )
axis(1)
axis(2)
box()
dev.off()

nome2<-paste("serie_ajustada_karma", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot(y, col=1, type="l", axes = T, main="", xlab="tempo", ylab="Umidade relativa do ar")
lines((fit1k$fitted), lty = 1, lwd = 1, col=2)
legend("bottomright",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,1),  col=c(1,2), bty="n" )
axis(1)
axis(2)
box()
dev.off()

########## Sarima ###########
val_ajus_sarima = y-modelo1$residuals
nome2<-paste("serie_ajustada_sarima", ".pdf",sep="")
pdf(file = nome2,width =8, height = 5.6) 
plot(y, col=1, type="l", axes = T, main="", xlab="Tempo", ylab="Umidade Relativa do ar")
lines((val_ajus_sarima), lty = 1, lwd = 1, col=2)
legend("bottomright",c("Valores reais","Valores ajustados" ),#pch=vpch,
       pt.bg="white", lty=c(1,1),  col=c(1,2), bty="n" )
axis(1)
axis(2)
box()
dev.off()


