## Expected errors

# Giving numerical argument
expect_error(
  pss_check_data(
    df = eg_data,
    cov_cols = c("risk_ass", "category", "suspect_age"),
    arm_col = "Arm",
    intervention_level = "Intervention"
    )
  )

# Giving a cov col name that isn't in the dataset
expect_error(
  pss_check_data(
    df = eg_data,
    cov_cols = c("risk_ass", "category1", "sus_age_bin"),
    arm_col = "Arm",
    intervention_level = "Intervention",
    cov_dist = props_joint_fn()
  )
)

test_that("my output looks right", {
  expect_snapshot(
    pss_check_data(
      df = eg_data,
      cov_cols = c("risk_ass", "category", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention"
    )
  )})


