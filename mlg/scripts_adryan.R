library(tidyverse)
library(faraway)
library(MASS)
library(drc)

data(earthworms)

earthworms.m1 <- drm(number/total~dose, weights = total, data = earthworms,
                     fct = LL.2(), type = "binomial")
modelFit(earthworms.m1)

mod_cloglog1 <- glm(number/total ~ dose, weights = total,
                   family = binomial(link = "cloglog"), data = earthworms)
mod_cloglog2 <- glm(cbind(number, total - number) ~ dose,
                   family = binomial(link = "cloglog"), data = earthworms)
summary(mod_cloglog1)
summary(mod_cloglog2)

modelo <- glm(cbind(number, total - number) ~ log(dose + 1), data = earthworms, family = binomial(link = "logit"))
summary(modelo)
AIC(mod_cloglog2, modelo)
