library(tidyverse)
library(forecast)

df = read_csv("data/rates.csv")

df = df |>
  mutate(Time_submitted  = as.Date(Time_submitted, format = "%Y-%m-%d"))

df
df = df |> group_by(
  Time_submitted) |>
  summarise(rate = mean(Rating))

df = df |>  bind_cols(index = 1:nrow(df))

#df[67, 2] = 2.7
df |>
  ggplot(aes(x = Time_submitted, y = rate)) +
  geom_line()



y = df$rate / 5

library(lubridate)

y <- ts(y, start = decimal_date(as.Date("2022-01-01")), frequency = 365)


acf(df$rate)
pacf(df$rate)


tseries::adf.test(y)

modelo=auto.arima(y, max.p=5, max.q=5, max.P=5, max.Q=5, max.order=5, max.d=2, max.D=1,
                  start.p=1, start.q=1, start.P=1, start.Q=1, stationary=F)


1:nrow(df) |> cbind(modelo$residuals) |> as.tibble() |> rename(index = `1:nrow(df)`,
                                                               y = `modelo$residuals`) |>
  ggplot(aes(index, y)) + geom_point()

# Modelo é
resi=as.vector(residuals(modelo))
resi_padrao=as.vector((modelo$residuals)/(sd(resi)))
stats::acf(resi_padrao, main = "", xlab="Defasagem", ylab="ACF")
stats::pacf(resi_padrao,   main = "", xlab="Defasagem", ylab="ACF Parcial")


tseries::adf.test(resi_padrao) # teste de estacionariedade
shapiro.test(resi_padrao)
# Box.test null hypothesis of independence in a given time series.
teste<-Box.test(resi_padrao, lag = 120, type = c("Ljung-Box"), fitdf = 0)
teste$p.value



resi_padrao |> hist()




## Q-Q plot dos resíduos ARMA

max_r<- max(resi_padrao,na.rm=T)
min_r<- min(resi_padrao,na.rm=T)
qqnorm(resi_padrao, pch = "+",
       xlim=c(0.95*min_r,max_r*1.05),
       ylim=c(0.95*min_r,max_r*1.05),
       main="",xlab="quantis normais",ylab="quantis empíricos")
lines(c(-10,10),c(-10,10),lty=2)


## Resíduos vs. índices ARMA
n<-length(modelo$residuals)
t<-seq(-5,n+6,by=1)
nome2<-paste("res_ind_arma", ".pdf",sep="")
pdf(file = nome2,width =4, height = 4)
plot(resi_padrao, main=" ",xlab="índices",ylab="resíduos", pch = "+",ylim=c(-4,4))
lines(t,rep(-3,n+12),lty=2,col=1)
lines(t,rep(3,n+12),lty=2,col=1)
lines(t,rep(-2,n+12),lty=3,col=1)
lines(t,rep(2,n+12),lty=3,col=1)


## BARMA


source("scripts_barma/analise_barmax/barma.r")

h1<-12 # numero de previsoes passos a frente

# ajustando sem covariaveis
fit = barma(y,ar=c(1),ma=c(1),h=h1,diag=1)


fit$aic



