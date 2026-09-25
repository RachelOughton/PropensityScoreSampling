load("data/eg_data.rda")


## Using closure approach

propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = props_joint_fn(),
  pz1_fn = pz1_estimate(1)
)

propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = props_marg_fn(),
  pz1 = "rescacle",
  pz1_n = 1
)

## Should give an error because of victim_age being numerical

propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "victim_age"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "joint",
  pz1 = "rescacle",
  pz1_n = 1
)

## Next steps
# Add in more of the WY covariates so I can test it with more than two
# Get to data frame with ratio in for other methods (sub-joint)
# code up the pz1 methods (should this have a separate function?)

## Find some datasets in R for observational studies, to test with also
