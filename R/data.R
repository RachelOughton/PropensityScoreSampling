#' Example forensic marking study data
#'
#' An anonymised and slightly altered version of data from a study investigating forensic marking to protect victims of domestic abuse in the UK
#'
#' @format ## `eg_data`
#' A data frame with 999 rows and 20 columns:
#' \describe{
#'   \item{FM_equip}{Whether the victim was given forensic marking equipment}
#'   \item{warning_YN}{Whether the perpetrator was warned about the use of forensic marking equipment (can only be `Y` if `FM_equip` is TRUE)}
#'   \item{princ_crime}{The principal crime committed at the reference incident, according to the home office lookup table}
#'   ...
#' }
"eg_data"
