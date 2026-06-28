# k subgrupos experimentais (níveis de dose). para cada grupo i
# Váriável resposta Y_i representa o número de sucessos em n_i ensaios de Bernoulli independentes.

# Objetivo
# modelar a probabilidade de sucesso p_i em função da dose (ou log-dose) x_i
# serão criados 3 modelos baseados nas seguintes funções de ligação
# $g(p_i) = \eta_i = \beta_0 + \beta_1 x_i$

# A. Ligaçção Logito (logit)
# $$\eta_i = \ln\left(\frac{p_i}{1-p_i}\right) \implies p_i = \frac{\exp(\beta_0 + \beta_1 x_i)}{1 + \exp(\beta_0 + \beta_1 x_i)}$$

# B. Ligação Probito (probit)
# $$\eta_i = \Phi^{-1}(p_i) \implies p_i = \Phi(\beta_0 + \beta_1 x_i)$$

# C. Ligação Complemento Log-Log (Clog-log)
# $$\eta_i = \ln(-\ln(1 - p_i)) \implies p_i = 1 - \exp(-\exp(\beta_0 + \beta_1 x_i))$$

# Estrutura do relatório --------------------------------------------------

# Slide 16 a 24
# Modelos para Respostas Binomiais e Dados Agrupados

# Slide 7-9 e 11-12
# Ajuste e Diagnóstico do Modelo

# Slide 27-28
# Investigações de Superdispersão


library(tidyverse)
library(MASS)
library(faraway)

# ------------------------------------------------------------------------------
# 1. INTRODUÇÃO E ANÁLISE DESCRITIVA
# ------------------------------------------------------------------------------
# Importação e tratamento inicial da base de dados 'bliss'

dados_bliss <- faraway::bliss |>
  rename(dose = conc,          
         mortos = dead,        
         vivos = alive) |>     
  mutate(total = mortos + vivos, 
         prop_observada = mortos / total)

# Gráfico Descritivo Inicial (Comportamento empírico da resposta)
ggplot(dados_bliss, aes(x = dose, y = prop_observada)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Análise Descritiva: Proporção de Mortes vs Dose",
       x = "Dose (Concentração do Inseticida)",
       y = "Proporção Empírica de Insetos Mortos") +
  theme_minimal()

# ------------------------------------------------------------------------------
# 2. SEÇÃO 2: MODELOS PARA RESPOSTAS BINOMIAIS E DADOS AGRUPADOS (Slides 16-24)
# ------------------------------------------------------------------------------
# Ajuste dos três modelos competitivos com diferentes funções de ligação
mod_logit   <- glm(cbind(mortos, total - mortos) ~ dose, 
                   family = binomial(link = "logit"), data = dados_bliss)

mod_probit  <- glm(cbind(mortos, total - mortos) ~ dose, 
                   family = binomial(link = "probit"), data = dados_bliss)

mod_cloglog <- glm(cbind(mortos, total - mortos) ~ dose, 
                   family = binomial(link = "cloglog"), data = dados_bliss)

# Critério de Informação de Akaike (AIC) para Seleção de Modelos (Slide 10) 
tibble(
  `Função de Ligação` = c("Logito (Logit)", "Probito (Probit)", "Complemento Log-Log (Cloglog)"),
  AIC = c(AIC(mod_logit), AIC(mod_probit), AIC(mod_cloglog))
)

# Avaliação da Qualidade Global do Ajuste via Desvio Residual (Slide 23)
dev_res <- deviance(mod_logit) 
gl_res  <- df.residual(mod_logit) 
p_valor <- 1 - pchisq(dev_res, gl_res)

tibble(Desvio = dev_res, GL = gl_res, `p-valor` = p_valor)

summary(mod_logit) |> coef()

# ------------------------------------------------------------------------------
# 3. REQUISITO ESPECÍFICO: ESTIMAÇÃO DE DOSES CRÍTICAS (DL50) VIA MÉTODO DELTA
# ------------------------------------------------------------------------------
# Estimação da Dose Letal Mediana (DL50) para probabilidade p = 0.5
dl50_logit   <- MASS::dose.p(mod_logit, p = 0.5)
dl50_probit  <- MASS::dose.p(mod_probit, p = 0.5)
dl50_cloglog <- MASS::dose.p(mod_cloglog, p = 0.5)

# Consolidação dos resultados em uma tabela comparativa
tibble(
  Modelo = c("Logito", "Probito", "Complemento Log-Log"),
  `Estimativa DL50` = c(as.numeric(dl50_logit), as.numeric(dl50_probit), as.numeric(dl50_cloglog)),
  `Erro Padrao (Método Delta)` = c(attr(dl50_logit, "SE"), attr(dl50_probit, "SE"), attr(dl50_cloglog, "SE"))
)

# ------------------------------------------------------------------------------
# 4. SEÇÃO 1: AJUSTE E DIAGNÓSTICO DO MODELO (Slides 7-9 e 11-12)
# ------------------------------------------------------------------------------
# Extração de componentes analíticos de resíduos e matriz de projeção H 
dados_diagnostico <- dados_bliss %>%
  mutate(
    res_pearson = residuals(mod_logit, type = "pearson"),
    res_desvio  = residuals(mod_logit, type = "deviance"),
    alavanca    = hatvalues(mod_logit),                      
    res_desvio_pad = res_desvio / sqrt(1 - alavanca)         
  )

# Gráfico de Diagnóstico Fino: Resíduos de Desvio vs Alavanca (Slide 13)
ggplot(dados_diagnostico, aes(x = alavanca, y = res_desvio)) +
  geom_point(alpha = 0.8, size = 3) + # [cite: 153]
  geom_hline(yintercept = c(-2, 0, 2), linetype = "dashed", color = "red") + 
  labs(title = "Análise de Diagnóstico: Resíduos de Desvio vs. Alavanca", 
       x = "Alavanca (h_ii)", 
       y = "Resíduo de Desvio") + 
  theme_minimal() 

# ------------------------------------------------------------------------------
# 5. SEÇÃO 3: INVESTIGAÇÕES DE SUPERDISPERSÃO (Slides 27-28 e 32)
# ------------------------------------------------------------------------------
# Cálculo empírico do parâmetro de dispersão (Phi) baseado na estatística Chi2
chi2_pearson <- sum(dados_diagnostico$res_pearson^2)
phi_estimado <- chi2_pearson / gl_res 

# Ajuste por Quase-Verossimilhança para correção de Erros Padrão (Slide 33) 
mod_quasi_binomial <- glm(cbind(mortos, total - mortos) ~ dose, 
                          family = quasibinomial(link = "logit"), data = dados_bliss)

# Extração e comparação dos erros padrão conforme estrutura do Slide 35 
ep_binomial_puro <- summary(mod_logit)$coefficients["dose", "Std. Error"]
ep_quasibinomial <- summary(mod_quasi_binomial)$coefficients["dose", "Std. Error"]

tibble(
  Modelo = c("1 Binomial Puro", "2 Quase-Binomial"),
  `Parametro Dispersao (Phi)` = c(1, phi_estimado),
  `Erro Padrao (Dose)` = c(ep_binomial_puro, ep_quasibinomial)
)

# ------------------------------------------------------------------------------
# 6. GRÁFICO REQUISITO: CURVA AJUSTADA SOBREPOSTA AOS PONTOS REAIS
# ------------------------------------------------------------------------------
# Criação de malha fina de dados para gerar curvas preditas suaves
grid_predicao <- tibble(dose = seq(min(dados_bliss$dose), max(dados_bliss$dose), length.out = 300))

grid_curvas <- grid_predicao %>%
  mutate(
    Logito = predict(mod_logit, newdata = ., type = "response"),
    Probito = predict(mod_probit, newdata = ., type = "response"),
    `Clog-log` = predict(mod_cloglog, newdata = ., type = "response")
  ) %>%
  pivot_longer(cols = -dose, names_to = "Ligacao", values_to = "Probabilidade")

# Geração do gráfico comparativo final para inclusão no relatório
ggplot() +
  geom_point(data = dados_bliss, aes(x = dose, y = prop_observada), size = 4, color = "black") +
  geom_line(data = grid_curvas, aes(x = dose, y = Probabilidade, color = Ligacao), linewidth = 1) +
  geom_vline(xintercept = as.numeric(dl50_logit), linetype = "dotted", color = "darkgreen", linewidth = 0.9) +
  labs(title = "Curvas de Resposta Dose-Efeito Sobrepostas",
       subtitle = "Linha pontilhada indica a DL50 estimada via ligação Logito",
       x = "Dose / Concentração do Inseticida",
       y = "Probabilidade / Proporção Ajustada de Mortes",
       color = "Função de Ligação") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")





