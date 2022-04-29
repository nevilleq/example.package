#' Title
#'
#' @param n Sample size for simulated lm data
#'
#' @return tibble, n rows and 3 columns: y, x_1, x_2
#' @export
#'
#' @examples
#' lm_data(10)
lm_data <- function(n = 10) {
#Create lm data
  tibble::tibble(
    x   = runif(n), #Sample
    e   = rnorm(n, 0, 1), #random error
    x_1 = ifelse(x > 0.5, #continuous predictor
               rnorm(n, 0, 2),
               rnorm(n, 3, 4)),
    x_2 = ifelse(x > 0.5, "Control", "Treatment"), #treatment
    y   = ifelse(x_2 %in% "Treatment",
                 x_1 + 5 + e,
                 x_1 + e)
  ) |> #base R pipe (new)
  dplyr::select(-c(x, e))
}

#Create example package data in right form
#lm_example <- lm_data(100)
#usethis::use_data(lm_example)
#Then add roxygon2 to /R/data.R





