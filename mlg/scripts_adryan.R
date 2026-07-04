# Bibliotecas ---------------------------------------------------------------

rm(list = ls()); gc(full = TRUE)

library(tidyverse)
library(faraway)
library(MASS)
library(drc)
library(DHARMa)
library(easystats)

# Dados ---------------------------------------------------------------------

data(earthworms)
dados <- earthworms

# Testes -------------------------------------------------------------------

earthworms.m1 <- drm(number/total~dose, weights = total, data = earthworms,
                     fct = LL.2(), type = "binomial")
modelFit(earthworms.m1)

rm(earthworms.m1)

mod_cloglog1 <- glm(number/total ~ dose, weights = total,
                   family = binomial(link = "cloglog"), data = earthworms)
mod_cloglog <- glm(cbind(number, total - number) ~ dose,
                   family = binomial(link = "cloglog"), data = earthworms)
summary(mod_cloglog1)
summary(mod_cloglog2)
rm(mod_cloglog1)

# Modelos ------------------------------------------------------------------

mod_cloglog2 -> mod_cloglog
rm(mod_cloglog2)

modelo_logit1 <- glm(cbind(number, total - number) ~ log(dose + 1), data = earthworms, family = binomial(link = "logit"))


modelo_logit2 <- glm(cbind(number, total - number) ~ log(dose + 0.1), data = earthworms, family = binomial(link = "logit"))

mod_logit <- glm(cbind(number, total - number) ~ dose,
                 family = binomial(link = "logit"), data = earthworms)

dados$is_control <- ifelse(dados$dose == 0, 1, 0) # Definição do dummy
dados$log_dose_active <- ifelse(dados$dose > 0, log(dados$dose), 0) # Se é 0 não faz nada, se é maior faz o log

# 2. Criar a matriz de sucessos e fracassos
sucessos <- dados$number
fracassos <- dados$total - dados$number

# 3. Ajustar o GLM usando as duas novas variáveis
mod_glm_dummy <- glm(cbind(sucessos, fracassos) ~ is_control + log_dose_active, 
                     data = dados, 
                     family = binomial(link = "logit"))

# Comparação de modelos

AIC(mod_cloglog, modelo_logit1, modelo_logit2, mod_logit, mod_glm_dummy)

# Selecionar mod_cloglog, modelo_logit2 e mod_glm_dummy
rm(modelo_logit1, mod_logit)
# Comparar o deviance

deviance(mod_cloglog)
deviance(modelo_logit2)
deviance(mod_glm_dummy)

# Comparar o pseudo-r2

r2_mcfadden <- 1 - (summary(mod_cloglog)$deviance / summary(mod_glm_dummy)$null.deviance)
cat("Pseudo-R2 de McFadden:", r2_mcfadden, "\n")

r2_mcfadden <- 1 - (summary(modelo_logit2)$deviance / summary(modelo_logit2)$null.deviance)
cat("Pseudo-R2 de McFadden:", r2_mcfadden, "\n")

r2_mcfadden <- 1 - (summary(mod_glm_dummy)$deviance / summary(modelo_logit2)$null.deviance)
cat("Pseudo-R2 de McFadden:", r2_mcfadden, "\n")

# Comparar o RMSE

performance::rmse(mod_cloglog)
performance::rmse(modelo_logit2)
performance::rmse(mod_glm_dummy)

# Métricas DE50 e DL50

## Mod_cloglog

# 1. Calcular a DE50 na escala original
res_cloglog <- MASS::dose.p(mod_cloglog, p = 0.5)
de50_cloglog <- as.numeric(res_cloglog)

# 2. Calcular o Intervalo de Confiança de 95%
se_cloglog <- attr(res_cloglog, "SE")
ic_inf_cloglog <- de50_cloglog - 1.96 * se_cloglog
ic_sup_cloglog <- de50_cloglog + 1.96 * se_cloglog

# Exibir resultados
cat("DE50 (cloglog):", de50_cloglog, "[95% IC:", ic_inf_cloglog, ";", ic_sup_cloglog, "]\n") # Dá negativo, já remove

## modelo_logit2

# 1. Calcular o valor estimado na escala do modelo (log)
res_logit2 <- MASS::dose.p(modelo_logit2, p = 0.5)
x50_logit2 <- as.numeric(res_logit2)
se_logit2  <- attr(res_logit2, "SE")

# 2. Criar o Intervalo de Confiança ainda na escala log
ic_inf_log2 <- x50_logit2 - 1.96 * se_logit2
ic_sup_log2 <- x50_logit2 + 1.96 * se_logit2

# 3. Destransformar TUDO para a escala de dose real (exp(x) - 0.1)
de50_logit2   <- exp(x50_logit2) - 0.1
ic_inf_logit2 <- exp(ic_inf_log2) - 0.1
ic_sup_logit2 <- exp(ic_sup_log2) - 0.1

# Exibir resultados
cat("DE50 (logit2):", de50_logit2, "[95% IC:", ic_inf_logit2, ";", ic_sup_logit2, "]\n") # Dá tudo certo, positivo

## mod_glm_dummy

# 1. Calcular o valor estimado na escala log (usando apenas Intercepto e log_dose_active)
# cf = c(1, 3) pula a variável dummy do controle
res_dummy <- MASS::dose.p(mod_glm_dummy, cf = c(1, 3), p = 0.5)
x50_dummy <- as.numeric(res_dummy)
se_dummy  <- attr(res_dummy, "SE")

# 2. Criar o Intervalo de Confiança na escala log
ic_inf_log_d <- x50_dummy - 1.96 * se_dummy
ic_sup_log_d <- x50_dummy + 1.96 * se_dummy

# 3. Destransformar para a escala de dose real (apenas aplicando exp(x))
de50_dummy   <- exp(x50_dummy)
ic_inf_dummy <- exp(ic_inf_log_d)
ic_sup_dummy <- exp(ic_sup_log_d)

# Exibir resultados
cat("DE50 (Dummy):", de50_dummy, "[95% IC:", ic_inf_dummy, ";", ic_sup_dummy, "]\n") # Dá tudo certo, positivo

# Selecionar apenas mod_glm_dummy
# Qualidade do ajuste

dev_residual <- deviance(mod_glm_dummy)
graus_liberdade <- summary(mod_glm_dummy)$df.residual

# Calcular o p-valor do teste
p_valor_ajuste <- pchisq(dev_residual, graus_liberdade, lower.tail = FALSE)
cat("P-valor da Bondade de Ajuste (Desvio):", p_valor_ajuste, "\n")

# Análise de resíduos

# Simular resíduos
residuos_simulados <- simulateResiduals(fittedModel = mod_glm_dummy)

# Plotar diagnósticos
plot(residuos_simulados)

check_model(mod_glm_dummy)

performance::check_overdispersion(mod_glm_dummy)

# Testes específicos

# 1. Teste de Wald para os Coeficientes Individuais
summary(mod_glm_dummy)

# 2. Análise de Desviação
anova(mod_glm_dummy, test = "Chisq")

# 5. Coeficientes e valores

coefficients(mod_glm_dummy, exponentiate = T)

cat("DE50 (Dummy):", de50_dummy, "[95% IC:", ic_inf_dummy, ";", ic_sup_dummy, "]\n") # Dá tudo certo, positivo
