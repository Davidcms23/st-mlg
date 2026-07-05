rm(list = ls()); gc(full = TRUE)
library(tidyverse); library(fpp3); library(fpp2); library(urca); library(tseries)

dados <- readRDS("dados/Dados_Estatisticos.rds")
#write_csv(dados, "~/Downloads/Dados_Estatisticos2.csv")
dados <- dados |> filter(ANOMES < yearmonth("2020 jan"))
dados <- dados |> 
  dplyr::ungroup()
dados |> autoplot()
dados |> ACF(lag_max = 42) |> autoplot()

dados |> gg_season()
dados |> gg_subseries()

dados |> 
  features(n, features = guerrero) # sem transformação

treinamento <- dados |> head(216)
teste <- dados |> tail(nrow(dados) - 216)

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

ajustes_todos |> 
  report() |>
  select(.model, AICc, BIC, sigma2) |> 
  arrange(AICc)

ajustes_todos |> select(auto) # = mna

ajustes_reduzidos <- treinamento |>
  model(
    mna = ETS(n ~ error("M") + trend("N") + season("A")),
    mnm = ETS(n ~ error("M") + trend("N") + season("M")),
    mma = ETS(n ~ error("M") + trend("M") + season("A"))
  )

fc_mna <- ajustes_reduzidos |> 
  forecast(h = "2 years")

accuracy(fc_mna, dados)

ajustes_reduzidos |> 
  select(mna) |> 
  gg_tsresiduals() # falhou, todos falham

ajustes_reduzidos |> 
  augment() |> 
  features(.innov, ljung_box, lag = 24, dof = 3) # dof varia conforme os parâmetros do modelo

nsdiffs(ts(dados, frequency = 12))
ndiffs(ts(dados))

dados |> autoplot(n |> difference(1)) # Bem melhor
dados |> autoplot(n |> difference(6))
dados |> autoplot(n |> difference(12))
dados |> autoplot(difference(difference(n, 1), 12))
dados |> autoplot(difference(difference(n, 1), 6))

dados |> 
  features(n, list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))

dados |> 
  features(n |> difference(1), list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))

dados |> 
  features(n |> difference(1) |> difference(12), list(unitroot_kpss, unitroot_nsdiffs, unitroot_ndiffs))


adf.test(
  diff(diff(dados$n, lag = 12), lag = 1) # Passa
)

summary(
  ur.kpss(
    diff(diff(dados$n, lag = 12), lag = 1) # Passa
  )
)

adf.test(
  diff(dados$n, lag = 1) # Passa
)

adf.test(
  diff(dados$n, lag = 12) # Não passa
)

dados |> 
  gg_tsdisplay(
    difference(n, 1) |> difference(12), 
    plot_type = "partial",
    lag_max = 26
  )
