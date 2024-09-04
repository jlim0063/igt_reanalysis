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

sjPlot::tab_model(
  m.VSE.theta, m.VSE.gamma, 
  digits = 3, string.ci = "95% CI", title = "Value plus Sequential Exploration Model: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Value Sensitivity", "Decay Rate")
)
sjPlot::tab_model(
   m.VSE.phi, m.VSE.eta, m.VSE.C,
  digits = 3, string.ci = "95% CI", title = "Value plus Sequential Exploration Model: Model Parameters regressed on Drug/Session Interactions",
  dv.labels = c("Exploration Bonus","Exploration Update (Learning Rate)", "Consistency")
)

## Contrasts
emm.theta <- emmeans(m.VSE.theta, specs = pairwise ~ session, adjust = "bonferroni")
emm.gamma <- emmeans(m.VSE.gamma, specs = pairwise ~ session, adjust = "bonferroni")
emm.phi   <- emmeans(m.VSE.phi,   specs = pairwise ~ session, adjust = "bonferroni")
emm.eta   <- emmeans(m.VSE.eta,   specs = pairwise ~ session, adjust = "bonferroni")
emm.C     <- emmeans(m.VSE.C,     specs = pairwise ~ session, adjust = "bonferroni")

## Write contrasts to csv
contrasts <- rbind(
  tibble::as_tibble(emm.theta$contrasts),
  tibble::as_tibble(emm.gamma$contrasts),
  tibble::as_tibble(emm.phi$contrasts),
  tibble::as_tibble(emm.eta$contrasts),
  tibble::as_tibble(emm.C$contrasts)
  )

write.csv(contrasts, file = here::here("output", "contrasts.csv"))

# PVLD model --------------------------------------------------------------

m.PVLD.rho    <- blmer(PVLD_rho ~ 1 + session*drug + (1|PtID), data = igt_data.pars, fixef.prior = normal)
m.PVLD.lambda <- blmer(PVLD_lambda ~ 1 + session*drug + (1|PtID), data = igt_data.pars, fixef.prior = normal)  
m.PVLD.eta    <- blmer(PVLD_eta ~ 1 + session*drug + (1|PtID), data = igt_data.pars, fixef.prior = normal)
m.PVLD.C      <- blmer(PVLD_C ~ 1 + session*drug + (1|PtID), data = igt_data.pars, fixef.prior = normal)   
sjPlot::tab_model(
  m.PVLD.rho,
  m.PVLD.lambda,
  m.PVLD.eta,
  m.PVLD.C
)  
