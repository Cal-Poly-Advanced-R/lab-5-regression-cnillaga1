#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here

  diff_x <- x_bar - x                     # calculating difference between each x from the mean
  sd_x <- sum(diff_x**2)                  # summing difference to get sum of squares for x
  diff_y <- y_bar - y                     # calculating difference between each y from the respective mean
  sd_y <- sum(diff_x*diff_y)              # calculating covariance of x and y

  beta_1 <- sd_y / sd_x                   # calculating slope coefficient
  beta_0 <- y_bar - (beta_1 * x_bar)      # calculating intercept

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
multiple_linear_regression <- function(dat, response, method = NULL) {

  y <- dat %>%
    select({{response}})                     # selects response variable

  y1 <- data.matrix(y)                       # converts column into matrix

  p <- dat %>%
    select(-{{response}})                   # removes response variable from data frame

  x1 <- matrix(1:1, nrow = nrow(dat))       # creating 1's matrix
  x2 <- data.matrix(p)                      # converting data frame without response into matrix
  x3 <- cbind(x1, x2)                       # binding 1's and matrix created above
  x4 <- data.matrix(x3)                     # making matrix with 1's and data
  x5 <- t(x4) %*% x4                        # x'x calculation

  coeff <- solve(x5) %*% (t(x4) %*% y1)    # (x'x)^-1 x'y calculation

  results <- data.frame(t(coeff))         # creating data frame with coefficients
  colnames(results)[1] <- "Intercept"     # renaming first column to Intercept

  return(results)
}
