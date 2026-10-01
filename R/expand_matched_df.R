#' Expand a matched comparison data frame
#'
#' Expand the data frame output by [matched_comparison()] using the `include` column, so that row i is included `include[i]` times.
#' @name expand_matched_df
#' @param matched_df A data frame output by [matched_comparison()]. This should contain columns for `arm_col`, all elements of `cov_cols` and `include`. It can also contain other columns.
#' @return `expand_matched_df` returns a data frame (more detail here, including the columns!).
#' @export
#' @seealso [matched_comparison()]
#'
#' @examples
#' ## First use `propscore_df` to create a propensity score data frame
#' ## This example uses the joint approach and specifies p(Z=1) such
#' ## that the expected size of the comparison group is 300
#'
#' eg_j_exp300 = propscore_df(
#'    df = eg_data,
#'    cov_cols = c("risk_ass", "category", "sus_age_bin"),
#'    arm_col = "Arm",
#'    intervention_level = "Intervention",
#'    cov_dist = "joint",
#'    pz1_fn = pz1_expectedN(300)
#'    )
#'
#' ## Now we can use `eg_j_exp300` to find a matched comparison group
#' ## In this version we sample from the comparison group with replacement
#' ## and do not drop any intervention cases for which there are no
#' ## comparison cases to sample
#'
#' match_j_exp300_replace = matched_comparison(
#'    df = eg_data,
#'    propscore_df = eg_j_exp300,
#'    cov_cols = c("category", "risk_ass", "sus_age_bin"),
#'    arm_col = "Arm",
#'    intervention_level = "Intervention",
#'    seed = 20,
#'    replace = TRUE,
#'    downsample = FALSE,
#'    drop_int = FALSE
#'    )
#'
#' ## Finally we use `expand_matched_df` to expand this so that each row is
#' ## included `include` times
#'
#' match_j_exp300_replace |> expand_matched_df()
#'
#' @importFrom rlang .data


expand_matched_df = function(
    matched_df
){
  include <- NULL
  ## Check there is an 'include' column and that it includes non-negative integers only
  if(!("include" %in% names(matched_df)))
    stop("There must be an 'include' column, indicating how many times each row should be included.")
  include_vec = matched_df$include
  if(!is.numeric(include_vec))
    stop("The 'include' column must be numeric, and include only non-negative integers.")
  remainder_include = include_vec %% 1
  if((any(remainder_include != 0)) | any(include_vec < 0))
    stop("The 'include' column must include only non-negative integers.")

  out_df = matched_df |> tidyr::uncount(include)
  return(out_df)
}
