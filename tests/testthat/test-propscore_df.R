## Tests for propscore_df


## Test it gives correct error message when arm_col is wrong in various ways



## Use snapshot testing for actual output


test_that("Joint, rescale to 1.5", {
  expect_snapshot(
    propscore_df(
       df = eg_data,
       cov_cols = c("risk_ass", "category", "sus_age_bin"),
       arm_col = "Arm",
       intervention_level = "Intervention",
       cov_dist = "joint",
       pz1_fn = pz1_rescale(1)
       )
  )})

test_that("Marginal, estimate of 0.01", {
    expect_snapshot(
      propscore_df(
        df = eg_data,
        cov_cols = c("risk_ass", "category", "sus_age_bin"),
        arm_col = "Arm",
        intervention_level = "Intervention",
        cov_dist = "marginal",
        pz1_fn = pz1_estimate(0.01)
      )
    )})


test_that("List, expected N of 300", {
  expect_snapshot(
    propscore_df(
      df = eg_data,
      cov_cols = c("risk_ass", "category", "sus_age_bin"),
      arm_col = "Arm",
      intervention_level = "Intervention",
      cov_dist = list(c("risk_ass", "category")),
      pz1_fn = pz1_expectedN(300)
    )
  )})


