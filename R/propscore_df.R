#' Propensity score data frame
#'
#' Generate data frame of propensity score estimates to be used in sampling
#' @name propscore_df
#' @param df A data frame containing treatment group and comparison cohort data
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases
#' @param cov_dist_fn One of the cov_dist functions: `joint_covdist()` for fully joint distribution, `marginal_covdist()` for marginal distribution. If you want to treat some subset(s) of covariates jointly, use `marginal_covdist(cov_list)`, where `cov_list` is a list of character vectors containing the covariates that should be treated jointly.
#' @param pz1_fn One of the functions used to specify p(Z=1): `pz1_rescale()`, `pz1_estimate()` or `pz1_expectedN()`. Each of these has one numerical argument, `n_pz1`, whose meaning depends on the function being used.
#' @param n_pz1 An argument to `pz1_fn`. Its meaning depends on the function used for `pz1_fn`.
#'
#' @return `propscore_df` returns a data frame (more detail here!).
#' @export
#'
# @examples
#' @importFrom rlang .data


propscore_df = function(
    df,
    cov_cols,
    arm_col,
    intervention_level,
    cov_dist_fn,
    pz1_fn
){
  df = pss_check_data(
    df = df,
    cov_cols = cov_cols,
    arm_col = arm_col,
    intervention_level = intervention_level
  )
  # Getting to data frame with ratios
  ## Using closures

  ## Still need to add in partially joint method
  ## df_out needs to include count_comp, for the expectedN method

  props_fun = cov_dist_fn
  df_out = props_fun(df=df, cov_cols=cov_cols)

  ## Estimating propensity score given chosen method

  pz1_fun = pz1_fn
  pz1 = pz1_fun(df_out)
  df_out$PropScore = pz1*df_out$ratio

  df_out

}

#' @describeIn propscore_df Find table of estimates of p(x), p(x|Z=1) and their ratio using fully joint approach
#' @return `props_joint_fn` returns a function that creates a data frame containing `px_int` (p(x|Z=1)), `px_comp` (p(x) and `ratio` (`px_int / px_comp`) for every combination of the levels of the covariates in `cov_cols`, using the fully joint approach.

#' @export

props_joint_fn = function(
){
  # Try to solve no visible binding
  Arm <- count <- comb_int <- comb_comp <- NULL

  ## Split dataset
  fn = function(df, cov_cols){
    df_int = df |> dplyr::filter(Arm == "Intervention")
    df_comp = df |> dplyr::filter(Arm == "Comparison")
    n_int = nrow(df_int)
    n_comp = nrow(df_comp)

    props_int = df_int |>
      dplyr::group_by(dplyr::pick(cov_cols), .drop=F) |>
      dplyr::summarise(
        count = dplyr::n(),
        px = count / n_int
      )

    props_comp = df_comp |>
      dplyr::group_by(dplyr::pick(cov_cols), .drop=F) |>
      dplyr::summarise(
        count = dplyr::n(),
        px = count / n_comp
      )

    props_both = dplyr::full_join(
      props_int,
      props_comp,
      by = cov_cols,
      suffix = c("_int", "_comp")
    ) |>
      dplyr::mutate_if(
        is.numeric, dplyr::coalesce, 0
      )

    props_both = props_both |>
      dplyr::mutate(
         ratio = .data$px_int / .data$px_comp
#         ratio = px_int / px_comp
      )

    df_out = props_both
    return(df_out)
  }
  return(fn)
}

## Can I include an argument for this where it can treat some jointly?
## Maybe conditional, eg. if(is.null(cov_joint){do what's below} else if(is.list(cov.joint){do the partially joint thing}) )

#' @describeIn propscore_df Find table of estimates of p(x), p(x|Z=1) and their ratio using marginal approach
#' @return `props_marg_fn` returns a function that creates a data frame containing `px_int` (p(x|Z=1)), `px_comp` (p(x) and `ratio` (`px_int / px_comp`) for every combination of the levels of the covariates in `cov_cols`, using the marginal approach.
#' @export

props_marg_fn = function(){
  fn = function(
    df,
    cov_cols
  ){
    # Try to solve no visible binding
    Arm <- count <- comb_int <- comb_comp <- NULL

    ## Split dataset
    df_int = df |> dplyr::filter(Arm == "Intervention")
    df_comp = df |> dplyr::filter(Arm == "Comparison")
    n_int = nrow(df_int)
    n_comp = nrow(df_comp)

    ## Do this by covariate
    df_marg_list = list()
    for (cov_i in cov_cols){
      df_i_marg = df |>
        dplyr::group_by(dplyr::pick(cov_i)) |>
        dplyr::summarise(
          comb_int = sum(Arm == "Intervention"),
          comb_comp = sum(Arm == "Comparison"),
          prop_int = comb_int / n_int,
          prop_comp = comb_comp / n_comp
        ) |>
        dplyr::select(tidyselect::all_of(c(cov_i, "prop_int", "prop_comp")))
      names(df_i_marg) = c(cov_i, sprintf("p_int_%s", cov_i), sprintf("p_comp_%s", cov_i))
      df_marg_list[[cov_i]] = df_i_marg
    }
    ## This is probably quite ugly but it works
    inner_text = paste(sprintf("levels(df$%s)", cov_cols), collapse = ", ")
    full_text = paste0("df_probs_marg = expand.grid(", inner_text, ")")
    eval(parse(text = full_text))
    names(df_probs_marg) = cov_cols
    df_out = df_probs_marg

    for (cov_i in cov_cols){
      df_out = dplyr::left_join(df_out, df_marg_list[[cov_i]], by=cov_i)
    }
    ## Now we need to multiply all the rows starting 'p_int' and all the rows starting 'p_comp'

    p_int_df = df_out |>
      dplyr::select(tidyselect::starts_with("p_int"))
    df_out$px_int = apply(p_int_df, 1, prod)

    p_comp_df = df_out |>
      dplyr::select(tidyselect::starts_with("p_comp"))
    df_out$px_comp = apply(p_comp_df, 1, prod)

    ## Find ratio
    df_out$ratio = df_out$px_int / df_out$px_comp

    ## Find count_comp, which we will need later
    df_comp = df |> dplyr::filter(Arm == "Comparison")

    df_count_comp = df_comp |>
      dplyr::group_by(dplyr::pick(cov_cols), .drop=F) |>
      dplyr::summarise(
      count_comp = dplyr::n()
      )
    df_out = dplyr::left_join(df_out, df_count_comp, by = cov_cols)
    df_out
  }
  return(fn)
}

### PZ1 functions

#' @describeIn propscore_df Specify p(Z=1) using the estimate method. The argument given is the estimate used
#' @return `pz1_estimate` returns the estimate of p(Z=1) using the 'estimate' method (eg. a value provided by an expert). The argument is used as the estimate.
#' @export

pz1_estimate = function(
    n_pz1 # the estimate to be used
    ){
  pz1_fn = function(
    df){
    pz1 = n_pz1
    df$PropScore = n_pz1*df$ratio
    return(pz1)
  }
  return(pz1_fn)
}


#' @describeIn propscore_df Specify p(Z=1) using the rescale method. The argument given is the value the largest estimated propensity score should take
#' @return `pz1_rescale` finds the value of p(Z=1) such that the largest propensity score will be `n_pz1` (the argument given to `pz1_fn`)
#' @export

pz1_rescale = function(
    n_pz1 # the value the largest propensity score should take
){
  pz1_fn = function(
    df
    ){
    df_finite = df |> dplyr::filter(!is.infinite(.data$ratio))
    max_ratio = max(df_finite$ratio)
    pz1 = n_pz1/max_ratio
    return(pz1)
  }
  return(pz1_fn)
}

#' @describeIn propscore_df Specify p(Z=1) using the expectedN method. The argument given should be the expected size of the matched comparison group
#' @return `pz1_expectedN` finds the value of p(Z=1) such that the expected size of the matched comparison group will be `n_pz1` (the argument to `pz1_fn`).
#' @export

pz1_expectedN = function(
    n_pz1 # the expected size of the matched comparison group
){
  pz1_fn = function(
    df
  ){
    ## should already have count_comp for every combination
    df_noninf = df |> dplyr::filter(!is.infinite(.data$ratio))
    nj_rat = df_noninf$count_comp*df_noninf$ratio
    sum_comp = sum(nj_rat)

    pz1 = n_pz1 / sum_comp
    return(pz1)
  }
  return(pz1_fn)
}





