library(tidyverse)
library(MASS)
library(car)
library(faraway)
library(drc)

# O ensaio contamina um compartimento do solo com diferentes níveis de substância nociva e observa quantas minhocas resistem e permanecem no local, em oposição às que migram para o solo vizinho limpo.

data(earthworms)
str(earthworms) 
summary(earthworms) 

head(earthworms)

dados_plot <- earthworms %>%
  mutate(proporcao = number / total)

ggplot(dados_plot, aes(x = dose, y = proporcao)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(
    itle = "Proporção de Minhocas Retidas vs Dose da Substância Tóxica",
    x = "Dose",
    y = "Proporção Empírica (Ficaram/Total)"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")

earthworms |> 
  filter(dose > 0) |> 
  ggplot(aes(x = log(dose), y = number/total)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") + 
  theme_minimal() +
  labs(
    title = "Proporção de Retenção vs Logaritmo da Dose Ativa",
    x = "Log(Dose) (Excluindo o Controle)",
    y = "Proporção Empírica"
  )

earthworms |> 
  filter(dose > 0) |> 
  # logito empírico: log((sucessos + 0.5) / (fracassos + 0.5))
  mutate(elogit = log((number + 0.5) / (total - number + 0.5))) %>%
  ggplot(aes(x = log(dose), y = elogit)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Logitos Empíricos vs Log(Dose)",
    x = "Log(Dose) (Excluindo o Controle)",
    y = "Logito Empírico"
  )
