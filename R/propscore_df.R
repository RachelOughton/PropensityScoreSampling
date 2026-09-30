#' Propensity score data frame
#'
#' Generate data frame of propensity score estimates to be used in sampling
#' @name propscore_df
#' @param df A data frame containing treatment group and comparison cohort data
#' @param cov_cols A vector of strings, the column names of the covariates to be matched on. These should all be factor / categorical data.
#' @param arm_col The name of the column indicating which rows are treated cases and which are comparison cases. These should be factor or character, with only two levels / options.
#' @param intervention_level The value in the `arm_col` for the treatment cases
#' @param cov_dist Either "joint" for fully joint distribution, "marginal" for fully marginal distribution, or a list of of character vectors indicating groups of covariates that should be considered jointly.
#' @param pz1_fn One of the functions used to specify p(Z=1): `pz1_rescale()`, `pz1_estimate()` or `pz1_expectedN()`. Each of these has one numerical argument, `n_pz1`, whose meaning depends on the function being used.
#' @param n_pz1 An argument to `pz1_fn`. Its meaning depends on the function used for `pz1_fn`.
#' @param cov_list An argument to `props_fun`. This should be a list of character vectors. The elements of the list determine which groups of covariates are treated jointly. If `cov_dist` is "marginal" or "joint" then this is created automatically from `cov_cols`. If `cov_dist` is a list of groups of covariates to be treated jointly then the covariates to be treated marginally are filled in automatically.
#'
#' @return `propscore_df` returns a data frame (more detail here, including the columns!).
#' @export
#'
#' @examples
#' # Creates a propensity score data frame from `eg_data`
#' # using the joint approach, and rescaling so that the
#' # highest propensity score is 1.
#' propscore_df(
#'   df = eg_data,
#'   cov_cols = c("risk_ass", "category", "sus_age_bin"),
#'   arm_col = "Arm",
#'   intervention_level = "Intervention",
#'   cov_dist = "joint",
#'   pz1_fn = pz1_rescale(1)
#'   )
#'
#'   # Creates a propensity score data frame from `eg_data`
#'   # using the fully marginal approach, and setting p(Z=1)
#'   # so that the expected size of the matched comparison
#'   # group will be 300
#' propscore_df(
#'   df = eg_data,
#'   cov_cols = c("risk_ass", "category", "sus_age_bin"),
#'   arm_col = "Arm",
#'   intervention_level = "Intervention",
#'   cov_dist = "marginal",
#'   pz1_fn = pz1_expectedN(300)
#'   )
#'
#'   # Creates a propensity score data frame from `eg_data`
#'   # for which "risk_ass" and "category" are considered
#'   # jointly and "sus_age_bin" marginally, and setting p(Z=1)
#'   # so that the expected size of the matched comparison
#'   # group will be 300
#' propscore_df(
#'   df = eg_data,
#'   cov_cols = c("risk_ass", "category", "sus_age_bin"),
#'   arm_col = "Arm",
#'   intervention_level = "Intervention",
#'   cov_dist = "marginal",
#'   pz1_fn = pz1_expectedN(300)
#'   )
#' @importFrom rlang .data


propscore_df = function(
    df,
    cov_cols,
    arm_col,
    intervention_level,
    cov_dist,
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

  ## Will need to sort this out in order to combine the three functions into the mixed one.
  ## create cov_dist_list for marginal and joint cases
  if(tolower(cov_dist) == "marginal"){
    cov_dist_list = as.list(cov_cols)
  } else if (tolower(cov_dist) == "joint"){
    cov_dist_list = list()
    cov_dist_list[[1]] = cov_cols
  } else if (is.list(cov_dist)){
    cov_dist_list = cov_dist
  }
  df_out = props_fun(df, cov_cols, cov_dist_list)

  ## Estimating propensity score given chosen method

  pz1_fun = pz1_fn
  pz1 = pz1_fun(df_out)
  df_out$PropScore = pz1*df_out$ratio

  df_out

}

#' @describeIn propscore_df Find table of estimates of p(x), p(x|Z=1) and their ratio using fully joint approach
#' @return `props_fun` returns a function that creates a data frame containing `px_int` (p(x|Z=1)), `px_comp` (p(x) and `ratio` (`px_int / px_comp`) for every combination of the levels of the covariates in `cov_cols`, using the approach specified via `cov_dist`.

#' @export



props_fun = function(
    df,
    cov_cols,
    cov_list
){
  # Try to solve no visible binding
  Arm <- count <- comb_int <- comb_comp <- cov_list_unlist <- NULL

  ## Check that all names in cov_list are in cov_cols, and none appears more than once
  cov_list_unlisted = unlist(cov_list)
  if(length(cov_list_unlisted) != length(unique(cov_list_unlisted))){
    stop("At least one covariate appears twice in cov_list")
  }
  if(any(!(cov_list_unlisted %in% cov_cols))){
    stop(
      sprintf("%s appears in cov_list but not in cov_cols",
              cov_list_unlist[(cov_list_unlisted %in% cov_cols)])
    )
  }

  ## Split dataset by arm

  df_int = df |> dplyr::filter(Arm == "Intervention")
  df_comp = df |> dplyr::filter(Arm == "Comparison")
  n_int = nrow(df_int)
  n_comp = nrow(df_comp)


  ## Need to find the covariates still being treated marginally
  ## and add them into cov_list. Then find each probability table. Then combine them.

  marg_covs = cov_cols[!(cov_cols %in% cov_list_unlisted)]
  for (i in marg_covs){
    cov_list[[length(cov_list)+1]] = i
  }

  df_cov_list = list()
  for (i in 1:length(cov_list)){
    df_cov_list_i = df |>
      dplyr::group_by(dplyr::pick(tidyselect::all_of(cov_list[[i]]))) |>
      dplyr::summarise(
        comb_int = sum(Arm == "Intervention"),
        comb_comp = sum(Arm == "Comparison"),
        prop_int = comb_int / n_int,
        prop_comp = comb_comp / n_comp
      ) |>
      dplyr::select(tidyselect::all_of(c(cov_list[[i]], "prop_int", "prop_comp")))
    names(df_cov_list_i) = c(
      cov_list[[i]],
      sprintf("p_int_%s", paste(cov_list[[i]], collapse = ".")),
      sprintf("p_comp_%s", paste(cov_list[[i]], collapse = ".")))
    df_cov_list[[i]] = df_cov_list_i

  }

  ## This is probably quite ugly but it works
  inner_text = paste(sprintf("levels(df$%s)", cov_cols), collapse = ", ")
  full_text = paste0("df_probs_cov_list = expand.grid(", inner_text, ")")
  eval(parse(text = full_text))
  names(df_probs_cov_list) = cov_cols
  df_out = df_probs_cov_list

  for (i in 1:length(cov_list)){
    df_out = dplyr::left_join(df_out, df_cov_list[[i]], by=cov_list[[i]])
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
    dplyr::group_by(dplyr::pick(tidyselect::all_of(cov_cols)), .drop=F) |>
    dplyr::summarise(
      count_comp = dplyr::n()
    )
  df_out = dplyr::left_join(df_out, df_count_comp, by = cov_cols)
  df_out

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





