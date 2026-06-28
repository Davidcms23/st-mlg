library(tidyverse)
library(MASS)
library(faraway)
library(drc)

# O ensaio contamina um comartimento do solo com diferentes níveis de substância nociva e observa quantas minhocas resistem e permanecem no local, em oposoção às que migram para o solo vizinho limpo.

data(earthworms)

mod_logit <- glm(cbind(number, total - number) ~ dose,
                 family = binomial(link = "logit"), data = earthworms)
mod_probit <- glm(cbind(number, total - number) ~ dose,
                  family = binomial(link = "probit"), data = earthworms)
mod_cloglog <- glm(cbind(number, total - number) ~ dose,
                   family = binomial(link = "cloglog"), data = earthworms)

AIC(mod_logit, mod_probit, mod_cloglog) # critério para escolher o melhor modelo


