## Prepare some propscore df objects

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

## Put into matched_comparison

match_j_est1_replace = matched_comparison(
  df = eg_data,
  propscore_df = eg_j_est1,
  cov_cols = c("category", "risk_ass", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  seed = 20,
  replace = T,
  downsample = F,
  drop_int = F
)

match_m_resc15_down = matched_comparison(
  df = eg_data,
  propscore_df = eg_m_resc15,
  cov_cols = c("category", "risk_ass", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  seed = 20,
  replace = F,
  downsample = T,
  drop_int = F
)

match_j_est1_down_di = matched_comparison(
  df = eg_data,
  propscore_df = eg_j_est1,
  cov_cols = c("category", "risk_ass", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  seed = 20,
  replace = F,
  downsample = T,
  drop_int = T
)

