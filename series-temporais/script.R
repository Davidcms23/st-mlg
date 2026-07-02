library(tidyverse)
library(fpp3)
library(tsibble)
library(dplyr)
library(urca)
library(tseries)
library(forecast)

dados <- read_delim(
  "dados/Dados_Estatisticos.csv",
  delim = ";",
  skip = 1
)

voos_serie <- dados |> 
  select(ANO, MES, AEROPORTO_DE_ORIGEM_PAIS) |>  
  filter(AEROPORTO_DE_ORIGEM_PAIS == "BRASIL") |>  
  mutate(ANOMES = paste0(ANO, "/", MES)) |>  
  mutate(ANOMES = yearmonth(ANOMES)) |>  
  group_by(ANOMES) |>  
  count() |> 
  as_tsibble(index = ANOMES)

voos <- ts(voos$n, frequency = 12)

ndiffs(voos)  # diferenças simples necessárias
nsdiffs(voos) # diferenças sazonais necessárias

autoplot(voos)

# Testar estacionaridade
summary(ur.kpss(voos))
adf.test(voos)

voos_serie |> gg_tsdisplay(n, plot_type = 'partial')

acf(voos)
pacf(voos)

arima(voos, order = c(1,0,0)) 
arima(voos, order = c(2,0,0))
arima(voos, order = c(1,0,1))
arima(voos, order = c(2,0,1))

fit1 <- Arima(voos, order = c(2, 0, 1))
autoplot(fit1)

fit2 <- Arima(voos, order = c(1, 0, 0))
autoplot(fit2) 

checkresiduals(fit2)

fit2 |> forecast(h = 10) |> autoplot()

auto.arima(voos)
