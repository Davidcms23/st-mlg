library(tidyverse)
library(fpp3)
library(fpp2)
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

voos <- dados |> 
  select(ANO, MES, AEROPORTO_DE_ORIGEM_PAIS) |>  
  filter(AEROPORTO_DE_ORIGEM_PAIS == "BRASIL") |>  
  mutate(ANOMES = paste0(ANO, "/", MES)) |>  
  mutate(ANOMES = yearmonth(ANOMES)) |>  
  group_by(ANOMES) |>  
  count() |> 
  as_tsibble(index = ANOMES)

# voos |> autoplot(log(n) |>  difference(12))

voos20 <- voos |> 
  filter(ANOMES < yearmonth("2020 jan"))
  
autoplot(voos20)

ndiffs(ts(voos20))
nsdiffs(ts(voos20, frequency = 12))

voos20 |> autoplot(log(n) |> difference(12))

# Testar estacionaridade
summary(
  ur.kpss(
    diff(log(voos20$n), lag = 12)
  )
)
adf.test(
  diff(log(voos20$n), lag = 12)
)

voos20 |> gg_tsdisplay(
  log(n) |> difference(12),
  plot_type = 'partial',
  lag = 36
  )
# Pela analise da ACF e PACF temos um SARIMA(2, 0, 2)(2, 1, 2)

auto.arima(voos) # (1, 1, 2)(2, 0, 0)

fit <- voos20 |> 
  model(
    best2 = ARIMA(log(n) ~ 0 + pdq(2, 0, 2) + PDQ(2, 1, 2,)),
    best = ARIMA(log(n) ~ 0 + pdq(2, 0, 1) + PDQ(2, 1, 2,)),
    auto = ARIMA(log(n))
  )

report(fit)
accuracy(fit)


# 1. Introdução
# 
# Contextualizar o problema, justificativa do tema
# 
# Incluir referências
# 
# Objetivos
# 
# 2. Metodologia
# 
# Como os objetivos serão atendidos?
# 
#   Quais modelos serão utilizados (ETS e ARIMA)
# 
# Como os modelos serão ajustados
# 
# Como será feita a seleção do modelo (AIC e REQM)
# 
# 3. Resultados
#       
# Análise descritiva dos dados
# 
# Ajuste dos modelos ETS e ARIMA
# 
# Seleção do modelo
# 
# Análise de diagnóstico
# 
# Previsão
# 
# 4. Conclusões
# 
# O que foi feito neste trabalho? Quais resultados foram obtidos?
# 
#   Os objetivos da introdução foram atingidos?
# 
#   Qual a importância dos resultados obtidos?
# 
#   Sugestões para trabalhos futuros.
# 
# 5. Referências
