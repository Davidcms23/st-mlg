library(tidyverse)
library(faraway)
library(MASS)
library(drc)
library(DHARMa)

data(earthworms)

earthworms.m1 <- drm(number/total~dose, weights = total, data = earthworms,
                     fct = LL.2(), type = "binomial")
modelFit(earthworms.m1)

rm(earthworms.m1)

mod_cloglog1 <- glm(number/total ~ dose, weights = total,
                   family = binomial(link = "cloglog"), data = earthworms)
mod_cloglog2 <- glm(cbind(number, total - number) ~ dose,
                   family = binomial(link = "cloglog"), data = earthworms)
summary(mod_cloglog1)
summary(mod_cloglog2)

mod_cloglog2 -> mod_cloglog
rm(mod_cloglog2)

modelo_logit1 <- glm(cbind(number, total - number) ~ log(dose + 1), data = earthworms, family = binomial(link = "logit"))
summary(modelo_logit1)
AIC(mod_cloglog, modelo_logit1)

deviance(mod_cloglog)
deviance(modelo_logit1)

modelo_logit2 <- glm(cbind(number, total - number) ~ log(dose + 0.1), data = earthworms, family = binomial(link = "logit"))

mod_logit <- glm(cbind(number, total - number) ~ dose,
                 family = binomial(link = "logit"), data = earthworms)

dados <- earthworms

dados$is_control <- ifelse(dados$dose == 0, 1, 0)
dados$log_dose_active <- ifelse(dados$dose > 0, log(dados$dose), 0)

# 2. Criar a matriz de sucessos e fracassos
sucessos <- dados$number
fracassos <- dados$total - dados$number

# 3. Ajustar o GLM usando as duas novas variáveis
mod_glm_dummy <- glm(cbind(sucessos, fracassos) ~ is_control + log_dose_active, 
                     data = dados, 
                     family = binomial(link = "logit"))
AIC(mod_glm_dummy)

AIC(mod_cloglog, modelo_logit1, modelo_logit2, mod_logit, mod_glm_dummy)

# Substitua pelo nome do seu modelo preferido
dev_residual <- summary(mod_glm_dummy)$deviance
graus_liberdade <- summary(mod_glm_dummy)$df.residual

# Calcular o p-valor do teste
p_valor_ajuste <- pchisq(dev_residual, graus_liberdade, lower.tail = FALSE)
cat("P-valor da Bondade de Ajuste:", p_valor_ajuste, "\n")

dev_residual <- summary(modelo_logit2)$deviance
graus_liberdade <- summary(modelo_logit2)$df.residual

p_valor_ajuste <- pchisq(dev_residual, graus_liberdade, lower.tail = FALSE)
cat("P-valor da Bondade de Ajuste:", p_valor_ajuste, "\n")

dev_residual <- summary(mod_logit)$deviance
graus_liberdade <- summary(mod_logit)$df.residual

p_valor_ajuste <- pchisq(dev_residual, graus_liberdade, lower.tail = FALSE)
cat("P-valor da Bondade de Ajuste:", p_valor_ajuste, "\n")

# Cálculo manual do R2 de McFadden
r2_mcfadden <- 1 - (summary(mod_glm_dummy)$deviance / summary(mod_glm_dummy)$null.deviance)
cat("Pseudo-R2 de McFadden:", r2_mcfadden, "\n")

r2_mcfadden <- 1 - (summary(modelo_logit2)$deviance / summary(modelo_logit2)$null.deviance)
cat("Pseudo-R2 de McFadden:", r2_mcfadden, "\n")

# Simular resíduos
residuos_simulados <- simulateResiduals(fittedModel = mod_glm_dummy)

# Plotar diagnósticos
plot(residuos_simulados)

residuos_simulados <- simulateResiduals(fittedModel = modelo_logit2)

plot(residuos_simulados)

summary(mod_glm_dummy)
summary(modelo_logit2)
