###########################################################################################################################
# Value plus Sequential Exploration (Ligneul, 2019)
###########################################################################################################################
# Parameters:
#   - theta : VALUE SENSITIVITY
#   - gamma : DECAY RATE
#   - phi   : EXPLORATION BONUS
#   - eta   : LEARNING RATE (EXPLORATION UPDATE)
#   - beta  : INVERSE TEMPERATURE
#   - C     : CONSISTENCY

model_VSE <- list(
  model_name      = "Value plus Sequential Exploration",
  stan_file_noext = "VSE_sep_session",
  stan_file       = "VSE_sep_session.stan",
  group_pars      = c(
    # "theta_pr",
    # "gamma_pr",
    # "phi_pr",
    # "eta_pr",
    # "beta_pr"
  ),
  indiv_pars      = c(
    "theta",
    "gamma",
    "phi",
    "eta",
    # "beta",
    "C"
  ),
  init            = NULL
)

###########################################################################################################################
# Prospect Valence Learning - Delta Model (Steingroever et al., 2013)
###########################################################################################################################
# Parameters:
#   - rho    : SHAPE (RISK SEEKING)
#   - lambda : LOSS AVERSION
#   - eta    : LEARNING RATE
#   - beta   : INVERSE TEMPERATURE
#   - C      : CONSISTENCY

model_PVLD <- list(
  model_name      = "Prospect Valence Learning - Delta",
  stan_file_noext = "PVL-D_sep_session",
  stan_file       = "PVL-D_sep_session.stan",
  group_pars      = c(
    # "rho_pr",
    # "lambda_pr",
    # "eta_pr",
    # "beta_pr"
  ),
  indiv_pars      = c(
    "rho",
    "lambda",
    "eta",
    "C"
  ),
  init            = NULL
)