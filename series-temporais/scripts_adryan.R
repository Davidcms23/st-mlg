# Preparação -------------------------------------------------------------------

rm(list = ls()); gc(full = TRUE)
library(tidyverse); library(fpp3); library(fpp2); library(urca); library(tseries)

library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(fabletools::report)

dados <- readRDS("dados/Dados_Estatisticos.rds")
#write_csv(dados, "~/Downloads/Dados_Estatisticos2.csv")
dados <- dados |> filter(ANOMES < yearmonth("2020 jan"))
dados <- dados |> 
  dplyr::ungroup()

# ------------------------------------------------------------------------------

# Descritiva -------------------------------------------------------------------

dados |> autoplot()
dados |> ACF(lag_max = 42) |> autoplot()

dados |> gg_season()
dados |> gg_subseries()

# ------------------------------------------------------------------------------

# Transformação ----------------------------------------------------------------

dados |> 
  features(n, features = guerrero) # sem transformação

# ------------------------------------------------------------------------------

# Modelos ETS ------------------------------------------------------------------

treinamento <- dados |> head(216)
teste <- dados |> tail(nrow(dados) - 216)

ajustes_todos <- treinamento |>
  model(
    mna = ETS(n ~ error("M") + trend("N") + season("A")),
    mnm = ETS(n ~ error("M") + trend("N") + season("M")),
    mma = ETS(n ~ error("M") + trend("M") + season("A")),
    auto = ETS(),
    ana = ETS(n ~ error("A") + trend("N") + season("A")),
    maa = ETS(n ~ error("M") + trend("A") + season("A")),
  )

ajustes_todos |> 
  report() |>
  select(.model, AICc, BIC, sigma2) |> 
  arrange(AICc)

ajustes_todos |> select(auto) # = mna

ajustes_todos <- treinamento |>
  model(
    mna = ETS(n ~ error("M") + trend("N") + season("A")),
    mnm = ETS(n ~ error("M") + trend("N") + season("M")),
    mma = ETS(n ~ error("M") + trend("M") + season("A")),
    ana = ETS(n ~ error("A") + trend("N") + season("A")),
    maa = ETS(n ~ error("M") + trend("A") + season("A")),
  )

ajustes_todos |> 
  report() |>
  select(.model, AICc, BIC, sigma2) |> 
  arrange(AICc)

fc_todos <- ajustes_todos |> 
  forecast(h = "2 years")

accuracy(fc_todos, dados) |> arrange(RMSE)

ajustes_reduzidos <- treinamento |>
  model(
    mna = ETS(n ~ error("M") + trend("N") + season("A")),
    mnm = ETS(n ~ error("M") + trend("N") + season("M")),
    ana = ETS(n ~ error("A") + trend("N") + season("A")),
  )

# Modelos ARIMA ----------------------------------------------------------------
## Estacionaridade

#nsdiffs(ts(dados, frequency = 12))
#ndiffs(ts(dados))

dados |> 
  features(n, list(unitroot_nsdiffs, unitroot_ndiffs))

dados |> autoplot(n |> difference(1)) # Bem melhor
dados |> autoplot(n |> difference(6)) # Má interpretabilidade, foi só pra ver
dados |> autoplot(n |> difference(12))
dados |> autoplot(difference(difference(n, 1), 6)) # Má interpretabilidade, foi só pra ver
dados |> autoplot(difference(difference(n, 1), 12))

#dados |> 
#  features(n, list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))

#dados |> 
#  features(n |> difference(1), list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))

#dados |> 
#  features(n |> difference(1) |> difference(12), list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))

## Testes ADF

adf.test(
  dados$n # Série base não é estacionária
)

adf.test(
  diff(dados$n, lag = 1) # Série com 1 diferença simples é estacionária
)

adf.test(
  diff(dados$n, lag = 12) # Série com 1 diferença sazonal não é estacionária
)

adf.test(
  diff(diff(dados$n, lag = 12), lag = 1) # Série com 2 diferenças é estacionária
)

## Testes KPSS

summary( # Série não é estacionária
  ur.kpss(
    dados$n
  )
)

summary( # Com 1 diferença simples é estacionária
  ur.kpss(
    diff(dados$n, lag = 1) 
  )
)

summary( # Com 1 diferença sazonal diz que é estacionária
  ur.kpss(
    diff(dados$n, lag = 12) 
  )
)

summary(
  ur.kpss(
    diff(diff(dados$n, lag = 12), lag = 1) # Com 2 diferenças é estacionária
  )
)

## Definição dos modelos

dados |> 
  gg_tsdisplay(
    difference(n, 1) |> difference(12), # Segui com 2 diferenças
    plot_type = "partial",
    lag_max = 36
  )

auto.arima(dados) # (1,1,1)(2, 0, 0)
auto.arima(dados |> mutate(n = difference(n, 1))) # (1, 1, 1)(2, 0, 0)
auto.arima(dados |> mutate(n = difference(difference(n, 1), 12))) # (0, 1, 2)(2, 1, 2)

modelos_arima_todos <- treinamento |> 
  model(
    airline   = ARIMA(n ~ pdq(0,1,1) + PDQ(0,1,1)),
    airlinear = ARIMA(n ~ pdq(0,1,1) + PDQ(1,1,1)),
    auto1 = ARIMA(n ~ pdq(1, 1, 1) + PDQ(2, 0, 0)),
    auto2 = ARIMA(n),
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    arima2 = ARIMA(n ~ pdq(0,1,1) + PDQ(2, 1, 1)),
    best2 = ARIMA(n ~ 0 + pdq(2, 0, 2) + PDQ(2, 1, 2,)),
    best = ARIMA(n ~ 0 + pdq(2, 0, 1) + PDQ(2, 1, 2,)),
    best2ad = ARIMA(n ~ 0 + pdq(2, 1, 2) + PDQ(2, 1, 2)),
    auto3 = ARIMA(n ~ pdq(0, 1, 2) + PDQ(2, 1, 2))
  )

modelos_arima_todos <- treinamento |> # Sem airline, use esse nos slides
  model(
    auto1 = ARIMA(n ~ pdq(1, 1, 1) + PDQ(2, 0, 0)),
    auto2 = ARIMA(n),
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    arima2 = ARIMA(n ~ pdq(0,1,1) + PDQ(2, 1, 1)),
    best2 = ARIMA(n ~ 0 + pdq(2, 0, 2) + PDQ(2, 1, 2,)), # De christian
    best = ARIMA(n ~ 0 + pdq(2, 0, 1) + PDQ(2, 1, 2,)), # De christian
    best2ad = ARIMA(n ~ 0 + pdq(2, 1, 2) + PDQ(2, 1, 2)), # De christian modificado com 2 diferenças
    auto3 = ARIMA(n ~ pdq(0, 1, 2) + PDQ(2, 1, 2)) # auto.arima com 2 diferenças
  )

modelos_arima_todos |> report() |> arrange(AICc)
# ATENÇÃO: A gente tem que usar o RMSE para comparar modelos com quantidade de diferenças diferentes. Por exemplo: (1, 0, 1)(1, 1, 1) e (1, 1, 1)(1, 1, 1)
modelos_arima_todos |> select(auto2) # (0, 1, 2)(1, 0, 0)

fc_arimas_todos <- modelos_arima_todos |> 
  forecast(h = "2 years")

accuracy(fc_arimas_todos, dados) |> arrange(RMSE) # esse modelo faz total sentido quando a gente observa o PACF e o ACF, a professora vai querer que a gente tente justificar ele por meio da interpretação dos gráfico. Então, ta ótimo.

modelos_reduzidos <- treinamento |> 
  model(
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    arima2 = ARIMA(n ~ pdq(0,1,1) + PDQ(2, 1, 1)),
    best2ad = ARIMA(n ~ 0 + pdq(2, 1, 2) + PDQ(2, 1, 2)), # De christian modificado com 2 diferenças
    auto3 = ARIMA(n ~ pdq(0, 1, 2) + PDQ(2, 1, 2)) # auto.arima com 2 diferenças
  )

modelos_reduzidos |> report() |> arrange(AICc)

fc_arimas <- modelos_reduzidos |> 
  forecast(h = "2 years")

accuracy(fc_arimas, dados) |> arrange(RMSE) # Melhor é o arima1

modelos_reduzidos |> 
  forecast(h = "2 years") |> 
  autoplot(dados |> tail(36))

# Resíduos ---------------------------------------------------------------------

ajustes_reduzidos |> 
  select(mna) |> 
  gg_tsresiduals(lag_max = 36) # falhou, todos falham

ajustes_reduzidos |> 
  select(ana) |> 
  gg_tsresiduals(lag_max = 36) # falhou, todos falham

ajustes_reduzidos |> 
  select(mnm) |> 
  gg_tsresiduals(lag_max = 36) # falhou, todos falham

ajustes_reduzidos |> 
  augment() |> 
  features(.innov, ljung_box, lag = 24, dof = 3) # dof varia conforme os parâmetros do modelo

modelos_reduzidos |> 
  select(arima1) |> 
  gg_tsresiduals(lag_max = 36)

modelos_reduzidos |> 
  select(arima2) |> 
  gg_tsresiduals(lag_max = 36)

modelos_reduzidos |> 
  select(best2ad) |> 
  gg_tsresiduals(lag_max = 36)

modelos_reduzidos |> 
  select(auto3) |> 
  gg_tsresiduals(lag_max = 36)

modelos_reduzidos |> augment() |>
  filter(.model == "arima1") |> 
  features(.innov, ljung_box, lag = 24, dof = 5)

modelos_reduzidos |> augment() |> 
  filter(.model == "arima2") |> 
  features(.innov, ljung_box, lag = 24, dof = 4)

modelos_reduzidos |> augment() |>
  filter(.model == "best2ad") |> 
  features(.innov, ljung_box, lag = 24, dof = 8)

modelos_reduzidos |> augment() |>
  filter(.model == "auto3") |> 
  features(.innov, ljung_box, lag = 24, dof = 6)

melhor_modelo_arima <- modelos_reduzidos |> select(arima1)
melhor_modelo_ets <- ajustes_reduzidos |> select(mna)

shapiro.test((melhor_modelo_arima |> augment())$.innov)
shapiro.test((melhor_modelo_ets |> augment())$.innov)

# Comparação final -------------------------------------------------------------

ajuste_final <- treinamento |> 
  model(
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    mna = ETS(n ~ error("M") + trend("N") + season("A"))
  )

fc_final <- ajuste_final |> 
  forecast(h = "2 years")

ajuste_final |> 
  report() |>
  select(.model, AICc, BIC, sigma2) |> 
  arrange(AICc)

accuracy(fc_final, dados) |> arrange(RMSE)

ajuste_final |> 
  forecast(h = "2 years") |> 
  autoplot(dados |> tail(36))

melhor_modelo_arima |> 
  forecast(h = "2 years") |> 
  autoplot(dados |> tail(36))

melhor_modelo_ets |> 
  forecast(h = "2 years") |> 
  autoplot(dados |> tail(36))

melhor_modelo_geral <- dados |> 
  model(
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1))
  )

# Previsão ---------------------------------------------------------------------

melhor_modelo_geral |> 
  forecast(h = "4 years") |> 
  autoplot(dados)

# Extra, ignore ----------------------------------------------------------------

ajustes_todos <- treinamento |>
  model(
    aaa = ETS(n ~ error("A") + trend("A") + season("A")),
    aan = ETS(n ~ error("A") + trend("A") + season("N")),
    aam = ETS(n ~ error("A") + trend("A") + season("M")),
    ana = ETS(n ~ error("A") + trend("N") + season("A")),
    ann = ETS(n ~ error("A") + trend("N") + season("N")),
    anm = ETS(n ~ error("A") + trend("N") + season("M")),
    ama = ETS(n ~ error("A") + trend("M") + season("A")),
    amn = ETS(n ~ error("A") + trend("M") + season("N")),
    amm = ETS(n ~ error("A") + trend("M") + season("M")),
    
    maa = ETS(n ~ error("M") + trend("A") + season("A")),
    man = ETS(n ~ error("M") + trend("A") + season("N")),
    mam = ETS(n ~ error("M") + trend("A") + season("M")),
    mna = ETS(n ~ error("M") + trend("N") + season("A")),
    mnn = ETS(n ~ error("M") + trend("N") + season("N")),
    mnm = ETS(n ~ error("M") + trend("N") + season("M")),
    mma = ETS(n ~ error("M") + trend("M") + season("A")),
    mmn = ETS(n ~ error("M") + trend("M") + season("N")),
    mmm = ETS(n ~ error("M") + trend("M") + season("M")),
    auto = ETS()
  )

adf.test(
  diff(diff(dados$n, lag = 6), lag = 1) # Série com 2 diferenças é estacionária
)

summary(
  ur.kpss(
    diff(diff(dados$n, lag = 6), lag = 1) # Com 2 diferenças é estacionária
  )
)

modelos_teste <- treinamento |> 
  model(
    arima1 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    arima2 = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1, period = 6)),
    arima3 = ARIMA(n ~ 0 + pdq(1,1,6) + PDQ(2, 1, 1))
  )

modelos_teste |> report() |> arrange(AICc)

fc_teste <- modelos_teste |> 
  forecast(h = "2 years")

accuracy(fc_teste, dados) |> arrange(RMSE)

ajuste_final <- dados |> 
  model(
    arima1  = ARIMA(n ~ pdq(1,1,1) + PDQ(2, 1, 1)),
    mnm = ETS(n ~ error("M") + trend("N") + season("M"))
  )

ajuste_final |> augment() |>
  dplyr::filter(.model == "arima1") |> 
  features(.innov, ljung_box, lag = 24, dof = 5)

melhor_modelo_arima <- ajuste_final |> select(arima1)

shapiro.test((melhor_modelo_arima |> augment())$.innov)

ajuste_final |> augment() |>
  dplyr::filter(.model == "mnm") |> 
  features(.innov, ljung_box, lag = 24, dof = 2)

melhor_modelo_ets <- ajuste_final |> select(mnm)

shapiro.test((melhor_modelo_ets |> augment())$.innov)

dados |> autoplot(box_cox(n, dados |> 
                            features(n, features = guerrero)))

dados |> autoplot(difference(difference(box_cox(n, dados |> 
                                                  features(n, features = guerrero)), 1), 12))
