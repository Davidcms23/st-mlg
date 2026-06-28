library(tsibble)
library(dplyr)
library(urca)
library(rbcb)
library(tseries)

dados_ipca <- rbcb::get_series(
  c(IPCA = 433), 
  start_date = "2015-01-01"
)

serie_ipca <- dados_ipca |> 
  mutate(MesAno = tsibble::yearmonth(date)) |> 
  select(MesAno, IPCA) |>
  as_tsibble(index = MesAno)

ipca <- ts(serie_ipca$IPCA, frequency = 12)

ndiffs(ipca) # n de diferenças simples
nsdiffs(ipca) # n de diferenças sazonais

autoplot(ipca)

summary(ur.kpss(ipca)) # H0: estacionários
# não rejeitou H0
adf.test(ipca) # H0: não-estacionários
# reijeitou H0

serie_ipca |> gg_tsdisplay(IPCA, plot_type = 'partial')

acf(ipca)
# decaimento gradual no primeiros lags, sugere compoentne AR

pacf(ipca)
# corte no lag 1, sugere AR(1)

arima(ipca, order = c(1,0,0)) 
arima(ipca, order = c(2,0,0))
arima(ipca, order = c(1,0,1))
arima(ipca, order = c(2,0,1))
# (1, 0, 0) tem o menor ICF = 95.18

(fit <- Arima(ipca, order = c(2, 0, 1)))
autoplot(fit)

(fit <- Arima(ipca, order = c(1, 0, 0)))
autoplot(fit) 

checkresiduals(fit)

fit |> forecast(h=10) |> autoplot()

# comparando com seleção automática
auto.arima(ipca)







  