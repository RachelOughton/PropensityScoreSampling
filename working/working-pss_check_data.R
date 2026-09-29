# Should work

pss_check_data(
  df = eg_data,
  cov_cols = c("risk_ass", "category", "sus_age_bin"),
  arm_col = "Arm",
  intervention_level = "Intervention"
  )



# Giving numerical argument
  pss_check_data(
    df = eg_data,
    cov_cols = c("risk_ass", "category", "suspect_age"),
    arm_col = "Arm",
    intervention_level = "Intervention"
  )

# Giving a cov col name that isn't in the dataset
  pss_check_data(
    df = eg_data,
    cov_cols = c("risk_ass", "category1", "sus_age_bin"),
    arm_col = "Arm",
    intervention_level = "Intervention"
  )


pss_check_data(
    df = eg_data,
    cov_cols = c("risk_ass", "category", "sus_age_bin"),
    arm_col = "Arm",
    intervention_level = "treatment"
  )


test_that("pss_check_data", {
  expect_snapshot(
    pss_check_data(
      df = eg_data,
      cov_cols = c("risk_ass", "category", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention"
    )
  )

})

