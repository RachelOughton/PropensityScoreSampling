load("data/eg_data.rda")
propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "joint",
  pz1 = "rescacle",
  pz1_n = 1
  )


propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "victim_age"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "joint",
  pz1 = "rescacle",
  pz1_n = 1
)
