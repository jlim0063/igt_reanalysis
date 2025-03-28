# Load packages
require(lme4)
require(lmerTest)
require(blme)
require(emmeans)
require(dplyr)
require(data.table)

# Read csv data with modelled parameters
igt_data.pars <- read.csv(here::here("data/actual/Killgore2007_GUM4", "Killgore2007_GUM4_with_parameters.csv")) %>% as.data.table()

# VSE model ---------------------------------------------------------------

## Factorise session and drug
igt_data.pars$session <- factor(igt_data.pars$session, levels = c(1, 2, 3), labels = c("Baseline", "51h TSD", "75h TSD"))
igt_data.pars$drug    <- as.factor(igt_data.pars$drug)

## Descriptives
summary(igt_data.pars$VSE_theta); sd(igt_data.pars$VSE_theta)
summary(igt_data.pars$VSE_gamma); sd(igt_data.pars$VSE_gamma)
summary(igt_data.pars$VSE_phi); sd(igt_data.pars$VSE_phi)
summary(igt_data.pars$VSE_eta); sd(igt_data.pars$VSE_eta)
summary(igt_data.pars$VSE_C); sd(igt_data.pars$VSE_C)


## Build linear mixed models
m.VSE.theta <- lmer(VSE_theta ~ 1 + session*drug + (1|PtID), data = igt_data.pars) # Value sensitivity
m.VSE.gamma <- lmer(VSE_gamma ~ 1 + session*drug + (1|PtID), data = igt_data.pars) # Decay Rate
m.VSE.eta   <- lmer(VSE_eta ~ 1 + session*drug + (1|PtID), data = igt_data.pars  )   # Exploration Update
m.VSE.phi   <- lmer(VSE_phi ~ 1 + session*drug + (1|PtID), data = igt_data.pars  )   # Exploration Bonus
m.VSE.C     <- lmer(VSE_C ~ 1 + session*drug + (1|PtID), data = igt_data.pars    )     # Consistency

## Interactions weren't significant, drop interaction term and just regress on session
m.VSE.theta <- lmer(VSE_theta ~ 1 + session + (1|PtID), data = igt_data.pars) # Value sensitivity
m.VSE.gamma <- lmer(VSE_gamma ~ 1 + session + (1|PtID), data = igt_data.pars) # Decay Rate
m.VSE.eta   <- lmer(VSE_eta ~ 1 + session + (1|PtID), data = igt_data.pars  )   # Exploration Update
m.VSE.phi   <- lmer(VSE_phi ~ 1 + session + (1|PtID), data = igt_data.pars  )   # Exploration Bonus
m.VSE.C     <- lmer(VSE_C ~ 1 + session + (1|PtID), data = igt_data.pars    )     # Consistency

## View results
sjPlot::tab_model(
  m.VSE.theta, m.VSE.gamma, 
  show.se = T, 
  digits = 3, string.ci = "95% CI", title = "Value plus Sequential Exploration Model: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Value Sensitivity", "Decay Rate")
)
sjPlot::tab_model(
   m.VSE.phi, m.VSE.eta, m.VSE.C,
   show.se = T, 
  digits = 3, string.ci = "95% CI", title = "Value plus Sequential Exploration Model: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Exploration Bonus","Exploration Update (Learning Rate)", "Consistency")
)
sjPlot::tab_model(
  m.VSE.theta, m.VSE.gamma, 
  m.VSE.phi, m.VSE.eta, m.VSE.C,
  digits = 3, string.ci = "95% CI", title = "Value plus Sequential Exploration Model: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Value Sensitivity", "Decay Rate", "Exploration Bonus","Exploration Update (Learning Rate)", "Consistency")
)

## Contrasts
emm.theta <- emmeans(m.VSE.theta, specs = pairwise ~ session, adjust = "bonferroni")
emm.gamma <- emmeans(m.VSE.gamma, specs = pairwise ~ session, adjust = "bonferroni")
emm.phi   <- emmeans(m.VSE.phi,   specs = pairwise ~ session, adjust = "bonferroni")
emm.eta   <- emmeans(m.VSE.eta,   specs = pairwise ~ session, adjust = "bonferroni")
emm.C     <- emmeans(m.VSE.C,     specs = pairwise ~ session, adjust = "bonferroni")

## Daw et al., 2006: Cortical substrates for exploratory decisions in human

## Write contrasts to csv
VSE_contrasts <- rbind(
  tibble::as_tibble(emm.theta$contrasts),
  tibble::as_tibble(emm.gamma$contrasts),
  tibble::as_tibble(emm.phi$contrasts),
  tibble::as_tibble(emm.eta$contrasts),
  tibble::as_tibble(emm.C$contrasts)
  )

write.csv(contrasts, file = here::here("output", "contrasts.csv"))


# Exploratory correlations ------------------------------------------------

## Make correlation matrix
cor(igt_data.pars[, .(VSE_theta, VSE_C, b1.score, b2.score, b3.score, b4.score, b5.score, total.score)], method = c("kendall"))
cor(igt_data.pars[,.(VSE_theta, VSE_gamma, VSE_phi, VSE_eta, VSE_C)])
igt_data.matrix <- as.matrix(igt_data.pars[,.(VSE_theta, VSE_gamma, VSE_phi, VSE_eta, VSE_C)])
# igt_data.matrix <- as.matrix(igt_data.pars[, .(VSE_theta, VSE_C, VSE_gamma, VSE_phi, VSE_eta, b1.score, b2.score, b3.score, b4.score, b5.score, total.score)])
Hmisc::rcorr(igt_data.matrix, type = "spearman")


# PVLD model (SUPPLEMENTARY)-----------------------------------------------

## Build models
m.PVLD.rho    <- lmer(PVLD_rho ~ 1 +    session + (1|PtID), data = igt_data.pars)
m.PVLD.lambda <- lmer(PVLD_lambda ~ 1 + session + (1|PtID), data = igt_data.pars)  
m.PVLD.eta    <- lmer(PVLD_eta ~ 1 +    session + (1|PtID), data = igt_data.pars)
m.PVLD.C      <- lmer(PVLD_C ~ 1 +      session + (1|PtID), data = igt_data.pars) 
## View results
sjPlot::tab_model(
  m.PVLD.rho,
  m.PVLD.lambda,
  m.PVLD.eta,
  m.PVLD.C,
  show.se = TRUE,
  digits = 3, string.ci = "95% CI", title = "Prospect Valence Learning-Delta: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Risk Seeking", "Loss Aversion", "Updating", "Consistency")
)  

# Contrasts
emm.rho      <- emmeans(m.PVLD.rho, specs = pairwise ~ session, adjust = "bonferroni")
emm.lambda   <- emmeans(m.PVLD.lambda, specs = pairwise ~ session, adjust = "bonferroni")
emm.eta.PVLD <- emmeans(m.PVLD.eta,   specs = pairwise ~ session, adjust = "bonferroni")
emm.C.PVLD   <- emmeans(m.PVLD.C,     specs = pairwise ~ session, adjust = "bonferroni")