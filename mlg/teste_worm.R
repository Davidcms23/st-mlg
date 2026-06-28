library(tidyverse)
library(MASS)
library(car)
library(faraway)
library(drc)

# O ensaio contamina um compartimento do solo com diferentes níveis de substância nociva e observa quantas minhocas resistem e permanecem no local, em oposição às que migram para o solo vizinho limpo.

data(earthworms)

mod_logit <- glm(cbind(number, total - number) ~ dose,
                 family = binomial(link = "logit"), data = earthworms)
mod_probit <- glm(cbind(number, total - number) ~ dose,
                  family = binomial(link = "probit"), data = earthworms)
mod_cloglog <- glm(cbind(number, total - number) ~ dose,
                   family = binomial(link = "cloglog"), data = earthworms)

AIC(mod_logit, mod_probit, mod_cloglog) # critério para escolher o melhor modelo
# cloglog tem o menor AIC. Portanto, Cloglog é o vencedor 

dev_res <- deviance(mod_cloglog)
gl_res <- df.residual(mod_cloglog)

p_valor <- 1 - pchisq(dev_res, gl_res)

tibble(Desvio=dev_res, gl = gl_res, "p-valor" = p_valor)

