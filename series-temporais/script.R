library(tidyverse)
library(fpp3)
dados <- read.table("dados/Dados_Estatisticos.csv", sep = ";", header = TRUE, fill = TRUE)

dados <- dados |> select(ANO, MES, AEROPORTO_DE_ORIGEM_PAIS) |> 
  filter(AEROPORTO_DE_ORIGEM_PAIS == "BRASIL") |> 
  mutate(ANOMES = paste0(ANO, "/", MES)) |> 
  mutate(ANOMES = yearmonth(ANOMES)) |> 
  group_by(ANOMES) |> 
  count()

dados <- as_tsibble(dados)