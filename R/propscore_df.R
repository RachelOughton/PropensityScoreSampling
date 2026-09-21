#' Generate data frame of propensity score estimates to be used in sampling
#' @param df A data frame containing treatment group and comparison cohort data
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases
#' @param cov_dist Should be either "joint" for fully joint distribution, "marginal" for fully marginal distribution, or a list of character vectors containing each element of `cov_col` exactly once, showing which covariates should be treated jointly.
#' @param pz1 The method used to specify p(Z=1). Should be one of "rescale", "estimate" or "expectedN"
#' @param pz1_n A number relating to `pz1`. If `pz1` == "rescale", `pz1_n` is the number to rescale to. If `pz1` == "estimate", `pz1_n` is the estimate of p(Z=1) to be used. If `pz1` == "expectedN", `pz1_n` is the expected size of the comparison cohort to be aimed for.
#'
#' @return A data frame (more detail here!).
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")


propscore_df = function(
    df,
    cov_cols,
    arm_col,
    intervention_level,
    cov_dist,
    pz1,
    pz1_n
){
  # Check the names exist
  # Check the variables are factor variables

  arm_levels = unlist(unique(df[ ,names(df) == arm_col]))
  if(length(arm_levels)!=2){
    stop(
      sprintf("The arm_col variable %s should have two unique values, but yours has %g: %s" ,
              arm_col, length(arm_levels), paste(arm_levels, collapse=", " ))
    )
  }

  if(!any(arm_levels== intervention_level)){
    stop(
      sprintf("One of the values in the column %s that you have supplied as arm_col should be %s",
              arm_col, intervention_level)
    )
  }
  comparison_level = arm_levels[arm_levels!=intervention_level]
  ## Create the correspondence between my default labels and theirs
  df$arm_given = df[ ,names(df) == arm_col]
  df$Arm = NA
  df$Arm[df$arm_given == intervention_level] = "Intervention"
  df$Arm[df$arm_given != intervention_level] = "Comparison"

  ## Check everything for covariates in cov_cols.
  ## Are they factor variables?
  ## Should I check for missing data?

  n_covcols = length(cov_cols)

  for (col_i in cov_cols){
    col_i_vec = df[[col_i]]
    if(any(is.na(col_i_vec))){
      warning(
        sprintf("There are %g NA values in the %s column. These rows will be lost.",
                sum(is.na(col_i_vec)), col_i)
      )
    }
    ## Check that it is factor (or could be treated as such)
    if(!is.factor(col_i_vec)){
      if(is.character(col_i_vec)){
        col_i_fac = as.factor(col_i_vec)
        warning(
          sprintf("Column %s has been coerced to a factor with %g levels",
                  col_i, nlevels(col_i_fac))
        )
      } else {
        stop(
          sprintf("Covariates should be factors, but %s is %s",
                  col_i, class(col_i_vec))
        )
      }
   }
  }

  ## Need to actually lose the NAs (but only from cov_cols or arm_col)

  df_cols = df |> ungroup() |> select(all_of(c(cov_cols, arm_col)))
  any_NA = apply(df_cols, 1, anyNA)

  df = df[!any_NA, ]

  ## Split dataset

  df_int = df |> filter(Arm == "Intervention")
  df_comp = df |> filter(Arm == "Comparison")
  n_int = nrow(df_int)
  n_comp = nrow(df_comp)

  # Getting to data frame with ratios

  ## Joint approach

  if(tolower(cov_dist) == "joint"){

    props_int = df_int |>
      group_by(pick(cov_cols), .drop=F) |>
      summarise(
        count = n(),
        px = count / n_int
        )

    props_comp = df_comp |>
      group_by(pick(cov_cols), .drop=F) |>
      summarise(
        count = n(),
        px = count / n_comp
      )

    props_both = full_join(
      props_int,
      props_comp,
      by = cov_cols,
      suffix = c("_int", "_comp")
    ) |>
      mutate_if(
        is.numeric, coalesce, 0
      )

    props_both = props_both |>
      mutate(
        ratio = px_int / px_comp
      )


  } else if (tolower(cov_dist) == "marginal") {

  } else {
    ## Check it is a list of vectors that (unlisted and reordered) is exactly cov_cols
  }

  props_both

}
