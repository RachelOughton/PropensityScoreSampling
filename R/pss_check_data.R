#' Check data
#'
#' `pss_check_data` checks that the data frame provided is suitable. Will return either a very slightly cleaned up version, or an error message.
#' The function does the following:
#' - checks the `arm_col` column has two unique values, one of which is `intervention_level`
#' - adds an `Arm` column, with two levels ("Intervention" and "Comparison")
#' - checks the columns named in `cov_cols` are present and in a suitable form (ideally factor, but if character it will coerce to factor and give a warning)
#' - Removes rows for which any of `cov_cols` or `arm_col` is missing.
#'
#' @name pss_check_data
#' @param df A data frame containing treatment group and comparison cohort data
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases
#' @return `pss_check_data` returns a (possibly slightly tidied up) data frame or an error message. The data frame will have an `Arm` column with levels `Intervention` and `Comparison`.
#' @export
#'
# @examples
#' @importFrom rlang .data

pss_check_data = function(
    df,
    cov_cols,
    arm_col,
    intervention_level
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
  if(!any(arm_levels == intervention_level)){
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
      message(
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
  ## Ungroup is because my dataset was previously grouped by one of the ID variables.
  ## Hopefully testing with another dataset will show if this causes a problem
  df_cols = df |> dplyr::ungroup() |> dplyr::select(tidyselect::all_of(c(cov_cols, arm_col)))
  any_NA = apply(df_cols, 1, anyNA)

  df = df[!any_NA, ]

  return(df)
}
