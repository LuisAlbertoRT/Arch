library(TSA)
library(forecast)
library(rugarch)

TickerList<- c("SSO") # hat = gorrito, LOS INDICE SIEMPRE LLEVAN HAT

LosPreciosQueQuiero<- NULL
for (Ticker in TickerList)
td <- cbind(LosPreciosQueQuiero,
                               getSymbols(Ticker, from="2013-01-01", to="2021-01-15", auto.assign =F ,src='yahoo') [,4]) #Mantiene solo el precio de cierre

View(LosPreciosQueQuiero)


Cotización <- ts(td[,1])

plot(Cotización)

title (main="FTSE 100",sub="25 de Abril de 1996 - 28 de Abril de 2016 Fuente:
Datastream")


Rentabilidad <- ts(diff(log(Cotización),lag=1)*100)
plot(Rentabilidad)
title (main="Rentabilidad FTSE 100", sub="25 de Abril de 1996 - 28 de Abril de 2016
Fuente: Datastream")
tsdisplay(Rentabilidad)
Rentabilidad2<- Rentabilidad^2
tsdisplay(Rentabilidad2)

-------------------------------------------------------------


modelo.garch11 <- ugarchspec(variance.model=list(garchOrder=c(1,0)),mean.model =
                               list(armaOrder=c(0,0)))
modelo.garch11.fit <- ugarchfit(spec=modelo.garch11,data=Rentabilidad,solver.control
                                = list(trace=1))
summary(modelo.garch11.fit)
modelo.garch11.fit
plot(modelo.garch11.fit)

ugarchforecast(modelo.garch11.fit,n.ahead=5)

---------------------------------------------------------------------

modelo.egarch11 <-
  ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model =
               list(armaOrder=c(0,0)))
modelo.egarch11.fit <- ugarchfit(spec=modelo.egarch11,data=Rentabilidad)
summary(modelo.egarch11.fit)
modelo.egarch11.fit
plot(modelo.egarch11.fit)

ugarchforecast(modelo.egarch11.fit,n.ahead=5)
--------------------------------------------------------------------------------

modelo.egarch11.fore<- ugarchforecast(modelo.egarch11.fit,data=Rentabilidad,n.roll =
                                         0,n.ahead =5)
fitted(modelo.egarch11.fore)
sigma(modelo.egarch11.fore)
plot(modelo.egarch11.fore)