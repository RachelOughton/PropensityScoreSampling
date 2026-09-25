#' Matched comparison group
#'
#' Generate a matched comparison group using the propensity score sampling approach
#' @name matched_comparison
#' @param df A data frame containing treatment group and comparison cohort data
#' @param propscore_df A data frame with a column for each variable in `cov_cols` and a `PropScore` column giving the estimated propensity score for each combination of their levels. This should have been returned by `propscore_df`.
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases

#'
#' @return `matched_comparison` returns a data frame containing the (candidate) matched treatment and comparison groups.
#' @export
#'
# @examples
#' @importFrom rlang .data
#'

matched_comparison = function(
    df,
    propscore_df,
    cov_cols,
    arm_col,
    intervention_level,
    replace = T,
    downsample = F
){
  df = pss_check_data(
    df = df,
    cov_cols = cov_cols,
    arm_col = arm_col,
    intervention_level = intervention_level
  )

## Check that propscoare_df has the right parts

  ps_df_names = c(cov_cols, "PropScore")
  if(any(!(ps_df_names %in% names(propscore_df)))){
    stop(
      sprintf("propscore_df should contain the columns %s. It does not contain %s",
              paste(ps_df_names, collapse = ", "),
              paste(ps_df_names[!(ps_df_names %in% names(propscore_df))], collapse = ", ")
              )
    )
  }



}
