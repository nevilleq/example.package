#' Title
#'
#' @param lm_data simulated data from lm_data
#'
#' @return lm model
#' @export
#'
#' @examples
#' df <- lm_data(10)
#' fit_lm(df)
fit_lm <- function(lm_data) {
  #Fit lm from lm_data generator
  lm.mod <- lm(y ~ x_1 + x_2, data = lm_data)
  #return lm model object
  return(lm.mod)
}

#' Title
#'
#' @param lm_data simulated data from lm_data
#'
#' @return lm model
#' @export
#'
#' @examples
#' df <- lm_data(10)
#' fit_lm_int(df)
fit_lm_int <- function(lm_data) {
  #Fit lm from lm_data generator w/ interaction
  lm.mod <- lm(y ~ x_1 * x_2, data = lm_data)
  #return lm model object
  return(lm.mod)
}
