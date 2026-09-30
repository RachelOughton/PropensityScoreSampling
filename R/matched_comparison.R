#' Matched comparison group
#'
#' Generate a matched comparison group using the propensity score sampling approach
#' @name matched_comparison
#' @param df A data frame containing treatment group and comparison cohort data
#' @param propscore_df A data frame with a column for each variable in `cov_cols` and a `PropScore` column giving the estimated propensity score for each combination of their levels. This should have been returned by `propscore_df`.
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases
#' @param seed The random seed to be used, for reproducibility
#' @param replace logical. Should the comparison cases be sampled with replacement?
#' @param downsample logical. Should the treatment cases be downsampled when there aren't enough comparison cases? Only one of `replace` and `downsample` should be TRUE.
#' @param drop_int logical. Should cases in the intervention/treated group be dropped if there are no equivalent cases in the comparison cohort to sample from?

#'
#' @return `matched_comparison` returns a data frame that is an expanded form of `df`, with the same number of rows, in the same order, but some extra columns:
#' - `PropScore`: the propensity score for that row, from `propscore_df`
#' - `include`: how many copies of this row are included in the matched dataset. Zero means the row has been dropped. Values greater than one mean the row will be duplicated.
#' - `seed`: the random seed that was used. This will be the same for all rows and is included in the output for reproducibility.
#' @export
#' @importFrom stats runif
#'
#' @examples
#'
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
#' @importFrom rlang .data
#'

matched_comparison = function(
    df,
    propscore_df,
    cov_cols,
    arm_col,
    intervention_level,
    seed,
    replace = T,
    downsample = F,
    drop_int = F
){
  set.seed(seed)
  ## Check this is doing what I want!
  if((replace & downsample)|!(replace | downsample)){
    stop("Exactly one of replace and downsample should be TRUE")
  }

  df = pss_check_data(
    df = df,
    cov_cols = cov_cols,
    arm_col = arm_col,
    intervention_level = intervention_level
  )

## Check that propscore_df has the right parts

  ps_df_names = c(cov_cols, "PropScore")
  if(any(!(ps_df_names %in% names(propscore_df)))){
    stop(
      sprintf("propscore_df should contain the columns %s. It does not contain %s",
              paste(ps_df_names, collapse = ", "),
              paste(ps_df_names[!(ps_df_names %in% names(propscore_df))], collapse = ", ")
              )
    )
  }
  # Not sure I need these indices
  match_col_indices = (1:ncol(df))[names(df) %in% cov_cols]
  cov_cols = names(df)[match_col_indices] # reorders so that names match indices
  n_covs = length(cov_cols)
  arm_col_index = (1:ncol(df))[names(df) == arm_col]
  ## New column for sampling results
  ## Label by 0, 1 etc. so I can include multiples this way
  df$include = 0

  df = dplyr::left_join(df, propscore_df, by = cov_cols)
  if(replace){
    for (i in 1:nrow(df)){
      arm_i = df[[arm_col]][i]
      prob_samp_i = as.numeric(df$PropScore[i])

      if(arm_i == "Comparison"){
        ran_i = runif(1)
        if(prob_samp_i > 1){
          df$include[i] = floor(prob_samp_i)
          prob_remainder_i = prob_samp_i - floor(prob_samp_i)
          if(ran_i < prob_remainder_i){
            df$include[i] = df$include[i]+1
          }
        } else {
          if(ran_i <= prob_samp_i){
            df$include[i] = 1
          }
        }
      } else if (arm_i == "Intervention"){
        ## Keep all of intervention cases (for now!)
        df$include[i] = 1
      }
    }

  } else if (downsample){
    for (i in 1:nrow(df)){
      arm_i = df[[arm_col]][i]
      prob_samp_i = as.numeric(df$PropScore[i])

      if(arm_i == "Comparison"){
        ran_i = runif(1)
        if(ran_i <= prob_samp_i){ ## ran_i<=1, so this includes any with prob_150_i>1 with certainty
          df$include[i] = 1
        }
      } else if (arm_i == "Intervention"){
        ran_i = runif(1)
        if((prob_samp_i>1)&(ran_i <= 1/prob_samp_i)){ # If we need to downsample the intervention
          df$include[i] = 1
        } else if (prob_samp_i <= 1){   # If we are instead downsampling the comparison
          df$include[i] = 1
        }
      }
    }
  }

  if(drop_int){
    ## Drop the intervention cases where there are no comparison cases to sample from
    df$include[is.infinite(df$PropScore)] = 0
  }

  df$seed = seed
  df

}


