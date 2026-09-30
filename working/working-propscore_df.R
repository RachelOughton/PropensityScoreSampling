load("data/eg_data.rda")


## After merging functions into props_fun

eg_j_est1 = propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "joint",
  pz1_fn = pz1_estimate(1)
)

eg_m_resc15 = propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "marginal",
  pz1_fn = pz1_rescale(1.5)
)

eg_mix_exp100 = propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = list(c("risk_ass", "category")),
  pz1_fn = pz1_expectedN(100)
)

## Checking mixed covdist function - need to do some that make errors, and
## some with more combinations of variables (and higher orders of combinations)

propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = mixed_covdist(list(c("risk_ass", "category"))),
  pz1_fn = pz1_expectedN(100)
)

## Should give an error because of victim_age being numerical

propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "suspect_age"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = joint_covdist(),
  pz1_fn = pz1_estimate(1)
)

## Next steps
# Add in more of the WY covariates so I can test it with more than two
# Get to data frame with ratio in for other methods (sub-joint)
# code up the pz1 methods (should this have a separate function?)

## Find some datasets in R for observational studies, to test with also

