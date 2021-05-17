###CODIGO ARIMA DESCARGANDO DATOS DE INTERNET###

####ARIMA###

rm(list=ls(all=TRUE)) #Borra la memoria

# Código para instalar paquetes

#install.packages("xts")


#Llama las librerias
#install.packages('knitr', dependencies = TRUE)
library(zoo)
library(quantmod)
library(xts)
library(tseries)
library(forecast)
library(timeSeries)
library(tframePlus)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(ggthemes)
library(tidyverse)
library(dygraphs)
library(gridExtra)
library(GGally)
library(MASS)
library(timeDate)
library(astsa)
library(rugarch)
library(aTSA)
library(FinTS)


###SELECCIONAR LOS DATOS BAJÁNDOLOS DE INTERNET###

TickerList<- c("SSO") # hat = gorrito, LOS INDICE SIEMPRE LLEVAN HAT

LosPreciosQueQuiero<- NULL
for (Ticker in TickerList)
  LosPreciosQueQuiero <- cbind(LosPreciosQueQuiero,
                               getSymbols(Ticker, from="2013-01-01", to="2021-01-15", auto.assign =F ,src='yahoo') [,4]) #Mantiene solo el precio de cierre

View(LosPreciosQueQuiero)


# Quita todos los NA (en caso de que no haya cotizado el activo)

PreciosCierre <- LosPreciosQueQuiero[apply(LosPreciosQueQuiero ,1,function(x) all(!is.na(x))),]
View(PreciosCierre)

colnames(PreciosCierre)<- c("SSO")
View(PreciosCierre)


# Convierte los precios en rendimientos

rendimientos <- as.timeSeries(tail(PreciosCierre,-1) / as.numeric(head(PreciosCierre,-1))-1)
rendimientos<- as.xts(rendimientos)
View(rendimientos)


# O bien, se pueden convertir en rendimientos logarítmicos


precios_log <-diff(log(PreciosCierre))
precios_log <-precios_log[-1,]




#Asigna Nombre a cada Columna de "PreciosCierre" y "rendimientos"

Ultra<- PreciosCierre[,1]

Ultra_R2<- rendimientos[,1]

Ultra_R<- precios_log[,1]



# Gráficas a niveles (precios de cierre)
par(mfrow=c(1,1))

ggplot(Ultra, aes(x=Index, y=Ultra)) +
  ggtitle("Precio de Cierre ProShares Ultra: enero 2013 - enero 2021") +
  geom_line(color="deepskyblue4") +
  xlab("Fecha")+
  ylab("Precio de cierre")



#Grafica en rendimientos


R1<-ggplot(Ultra_R, aes(x=Index, y=Ultra_R)) +
  ggtitle("ProShares Ultra en rendimientos: enero 2013 - enero 2021") +
  geom_line(color="deepskyblue4") +
  xlab("Fecha")+
  ylab("Rendimiento")

dygraph(Ultra_R,main="ProShares Ultra en rendimientos: enero 2013 - enero 2021") %>% dyRangeSelector() #%>%


#dyOptions(colors = RColorBrewer::brewer.pal(3, "Paired"))

#dygraph(rendimientos) %>%
#dySeries("SP500", label = "SP500") %>%
#dySeries("IPC", label = "IPC") %>%
#dySeries("GDX", label = "GDX") %>%
#dyOptions(stackedGraph = TRUE) %>%
#dyRangeSelector()

# Correlación entre activos a niveles (precio de cierre)
# Rango de correlaciones


ggcorr(PreciosCierre,
       nbreaks = 4,
       label = T,
       label_size = 5
)


cor(PreciosCierre)

# Correlación entre activos en rendimientos

ggcorr(rendimientos,
       nbreaks = 4,
       label = T,
       label_size = 5
)


cor(rendimientos)

min(Ultra)

#Histogramas y descripción de los datos

SSO_h<-ggplot(data=Ultra, aes(x=Ultra),aes(y=..density..)) +
  geom_histogram(breaks=seq(15.71,94.93, by =1),
                 col="purple",
                 aes(fill=..count..)) +
  scale_fill_gradient("", low = "navyblue", high = "violetred1") +
  labs(title="Histograma de cierre del ProShares Ultra") +
  labs(x="Price", y="")

grid.arrange(SSO_h, ncol=1)



# Histogramas en rendimientos
Ultra_R_h<-ggplot(data=Ultra_R, aes(x=Ultra_R),aes(y=..density..)) +
  geom_histogram(breaks=seq(-0.02335, 0.01889, by =0.001),
                 col="purple",
                 aes(fill=..count..)) +
  scale_fill_gradient("", low = "navyblue", high = "violetred1") +
  labs(title="Histograma de rendimientos del ProShares Ultra") +
  labs(x="Price", y="")



grid.arrange(Ultra_R_h, ncol=1)
min(Ultra_R)
max(Ultra_R)

#Gráfico QQ a niveles

q1<-ggplot(PreciosCierre,aes(sample = SSO)) +
  geom_qq() + geom_qq_line(color = "blue")+
  ggtitle("QQ-Plot del ProShares Ultra: niveles")


grid.arrange(q1, ncol=1)


#Gráfico QQ enrendimientos


qr1<-ggplot(rendimientos, aes(sample = SSO)) +
  geom_qq() + geom_qq_line(color = "blue") +
  ggtitle("QQ-Plot del ProShares Ultra: rendimientos")


grid.arrange(qr1, ncol=1)



### PRUEBAS DE RAICES UNITARIAS

## PRUEBA DFA y PP

# A niveles (precio de cierre)

adf.test(Ultra)

PP.test(Ultra, lshort = TRUE)


# En rendimientos

adf.test(Ultra_R)


PP.test(Ultra_R, lshort = TRUE)



## PRUEBA KPSS

# A niveles (precio de cierre)

kpss.test(Ultra)


# En rendimientos

kpss.test(Ultra_R)



### MODELOS ARIMA ###

#Verifica los componentes de autocorrelación ACF y PACF

Ultra %>% diff() %>% ggtsdisplay(main="Función de Autocorrelación (MA) y Función de Autocorrelación parcial (AR)")

#-------------------------autoarima--------------------------#

fit1<-auto.arima(Ultra, seasonal=FALSE) #NOS DA EL MEJOR MODELO ARIMA SEGúN R
fit1
#Revisa autocorrelación en residuales y aplica prueba de Ljung Box
#H0: Los residuales se distribuyen normalmente
checkresiduals(fit1)

#Gráfico del pronóstico
fit1%>%forecast(h=12)%>%autoplot(2)


#Muestra los valores obtenidos por el pronóstico
forecast(fit1, h=12)

#h incorpora el n?mero de d?as que quiero pron?sticas
#en autoplot pongo a partir de que datos quiero que grafique la muestra

#Comprobar estabilidad y estacionariedad del modelo con raíces invertidas
autoplot(fit1, title = "Raices invertidas sobre AR y MA")

#Muestra las estadísticas sobre los residuales
summary(fit1)



#-------------------------propuesta 1--------------------------#

fit_chido = arima(Ultra, order=c(1,1,1))
fit_chido

#H0: Los residuales se distribuyen normalmente
checkresiduals(fit_chido)

fit_chido %>% forecast(h=12) %>% autoplot(200)
forecast(fit_chido, h=12)

autoplot(fit_chido, title = "Raices invertidas sobre AR y MA")

#Muestra las estadísticas sobre los residuales
summary(fit_chido)


#-------------------------propuesta 2--------------------------#


fit_chidote = arima(Ultra, order=c(1,1,15))
fit_chidote

checkresiduals(fit_chidote)

fit_chidote %>% forecast(h=12) %>% autoplot(200)
forecast(fit_chidote, h=12)

autoplot(fit_chidote, title = "Raices invertidas sobre AR y MA")



#Muestra las estadísticas sobre los residuales
summary(fit_chidote)





# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
# MASE: Mean Absolute Scaled Error
# ACF1: Autocorrelation of errors at lag 1.


# ARCH GARCH


#7. Verificar la autocorrelación de los rendimientos al cuadrado

# 7.1 Autocorrelacion de los rendimientos al cuadrado

par(mfrow=c(2,1))
acf((Ultra_R)^2)
pacf((Ultra_R)^2)

par(mfrow=c(1,1))

# 7.2 Prueba ARCH   -----# H0: Los residuos son homocedásticos
####Si valor p < 0.05 Rechazo  H0

ArchTest(Ultra_R, lags=12, demean=TRUE)


#8. Modelos de volatilidad

#8.1 ARCH (1)


ARCH1 = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

arch1_fit = ugarchfit(spec=ARCH1, data =Ultra_R)
arch1_fit
plot(arch1_fit,which=3)




###8.2 ARCH (2)


ARCH2 = ugarchspec(variance.model = list(garchOrder=c(2,0)),
                   mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

arch2_fit = ugarchfit(spec=ARCH2, data =Ultra_R)
arch2_fit
plot(arch2_fit,which=3)

#8.3 ARCH (3)


ARCH3 = ugarchspec(variance.model = list(garchOrder=c(3,0)),
                   mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

arch3_fit = ugarchfit(spec=ARCH3, data =Ultra_R)
arch3_fit
plot(arch3_fit,which=3)

#8.4 ARCH (4)


ARCH4 = ugarchspec(variance.model = list(garchOrder=c(4,0)),
                   mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

arch4_fit = ugarchfit(spec=ARCH4, data =Ultra_R)
arch4_fit
plot(arch2_fit,which=3)

#8.5 GARCH (1,1)


GARCH11 = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                     mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

garch11_fit = ugarchfit(spec=GARCH11, data =Ultra_R)
garch11_fit
plot(garch11_fit,which=3)

ugarchforecast(garch11_fit,n.ahead=20)

#8.6 GARCH (1,2)


GARCH12 = ugarchspec(variance.model = list(garchOrder=c(1,2)),
                     mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

garch12_fit = ugarchfit(spec=GARCH12, data =Ultra_R)
garch12_fit
plot(garch12_fit,which=3)

ugarchforecast(garch12_fit,n.ahead=20)

#8.7 GARCH (2,1)


GARCH21 = ugarchspec(variance.model = list(garchOrder=c(2,1)),
                     mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

garch21_fit = ugarchfit(spec=GARCH21, data =Ultra_R)
garch21_fit
plot(garch21_fit,which=3)

ugarchforecast(garch21_fit,n.ahead=20)

#8.7 GARCH (2,2)


GARCH22 = ugarchspec(variance.model = list(garchOrder=c(2,2)),
                     mean.model = list(armaOrder=c(0,0),include.mean=F,archm=F))

garch22_fit = ugarchfit(spec=GARCH22, data =Ultra_R)
garch22_fit
plot(garch22_fit,which=3)

ugarchforecast(garch22_fit,n.ahead=20)

## 9. Simula los rendimientos con los resultados

# Realizar simulación con ARCH (3)

BuenModelo = ugarchsim(arch3_fit,
                       n.sim=nrow(Ultra_R),
                       rseed=123,
                       startMethod="unconditional")
# Realizar simulación con GARCH (1,1)

BuenoModelo2 = ugarchsim(garch11_fit,
                         n.sim=nrow(Ultra_R),
                         rseed=123,
                         startMethod="unconditional")


# Rendimientos reales contra rendimientos simulados

par(mfrow=c(3,1))
plot(Ultra_R, main="Ultra Rendimientos reales", col = "darkred", lwd = 1)
plot(as.xts(BuenModelo@simulation$seriesSim,
            order.by=index(Ultra_R)),
     main="IPC Rendimientos simulados ARCH(3)",
     col = "darkblue", lwd = 1)
plot(as.xts(BuenoModelo2@simulation$seriesSim,
            order.by=index(Ultra_R)),
     main="IPC Rendimientos simulados GARCH(1,1)",
     col = "darkgreen", lwd = 1)

