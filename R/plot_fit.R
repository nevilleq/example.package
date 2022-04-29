#' Title
#'
#' @param lm.mod lm object
#' @param lm_data n x 3 tibble, output from lm_data
#' @param new_data new data.frame or tibble with columns y, x_1, x_2
#' @param interaction logical, changes plot title according to fit
#'
#' @return ggplot
#' @export
#'
#' @examples
#' data <- lm_data(10)
#' plot_fit(lm_fit(data), data, data, interaction = FALSE)
plot_fit <- function(lm.mod, lm_data, new_data, interaction = FALSE) {
  #Create string for title of plot
  string <- ifelse(interaction, "w/ Interaction", "")
  r2     <- summary(lm.mod)$adj.r.squared
  preds  <- predict(lm.mod, new_data)
  rmse   <- sqrt(mean((lm_data$y - preds)^2))

  #Predict based on new data
  lm_data |>
    dplyr::mutate(
      predicted_val = preds
    ) |> #Plot it
    ggplot2::ggplot(ggplot2::aes(x = x_1, y = y, colour = x_2)) +
    ggplot2::geom_point(shape = 16, size = 2, alpha = 0.6) +
    ggplot2::geom_smooth(ggplot2::aes(x = x_1, y = predicted_val),
                         inherit.aes = FALSE,
                         method = "lm",
                         colour = "orange",
                         fill   = "azure2") +
    ggplot2::scale_colour_manual(values = c("#440154FF", "#FDE725FF")) +
    ggplot2::labs(title = sprintf("2 Class Bivariate Normal LM%s (R^2 = %g, RMSE = %g)",
                                  string, round(r2, 2), round(rmse, 2))) +
    ggplot2::theme_minimal()
}
