## Tests for matched_comparison

## Do one with replace adn downsample both TRUE
## One with replace and downsample both FALSE


eg_j_est1 = propscore_df(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention",
  cov_dist = "joint",
  pz1_fn = pz1_estimate(0.1)
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

## Check it can't handle both true or both false

expect_error(
  matched_comparison(
    df = eg_data,
    propscore_df = eg_j_est1,
    cov_cols = c("category", "risk_ass", "sus_age_bin"),
    arm_col = "Arm",
    intervention_level = "Intervention",
    seed = 20,
    replace = T,
    downsample = T,
    drop_int = F
  )
)

expect_error(
  matched_comparison(
    df = eg_data,
    propscore_df = eg_j_est1,
    cov_cols = c("category", "risk_ass", "sus_age_bin"),
    arm_col = "Arm",
    intervention_level = "Intervention",
    seed = 20,
    replace = F,
    downsample = F,
    drop_int = F
  )

)

test_that("With replacement, not dropping", {
  expect_snapshot(
    matched_comparison(
      df = eg_data,
      propscore_df = eg_mix_exp100,
      cov_cols = c("category", "risk_ass", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention",
      seed = 20,
      replace = T,
      downsample = F,
      drop_int = F
    )
 )})


test_that("With replacement, not dropping", {
  expect_snapshot(
    matched_comparison(
      df = eg_data,
      propscore_df = eg_m_resc15,
      cov_cols = c("category", "risk_ass", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention",
      seed = 20,
      replace = T,
      downsample = F,
      drop_int = F
    )
 )})


test_that("downsampling, dropping", {
  expect_snapshot(
    matched_comparison(
      df = eg_data,
      propscore_df = eg_m_resc15,
      cov_cols = c("category", "risk_ass", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention",
      seed = 20,
      replace = F,
      downsample = T,
      drop_int = T
    )
    )})




