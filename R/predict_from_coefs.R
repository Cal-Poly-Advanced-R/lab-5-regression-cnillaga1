#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){

  y <- dat %>%
    select({{response}})                           # only selects response variable

  y1 <- data.matrix(y)                            # creates a matrix of response variable

  coefs1 <- coefs[,1]                             # grabs the intercept from coefs
  coefs2 <- data.matrix(coefs[,2:ncol(coefs)])    # grabs slope variables

  p1 <- dat %>%
    select(-{{response}})                         # removing response variable from data frame

  x1 <- data.matrix(p1)                           # creating matrix of resulting data frame

  pred <- coefs2 %*% t(x1)                       # multiplying slope coefs by observed results
  preds2 <- t(pred) + coefs1                    # adding intercept to get predicted results

  res <- cbind(y, preds2)                       # creating the final data with both true and predicted results
  results <- data.frame(res)
  colnames(results) <- c("True Results", "Predicted Results")

  return(results)

}
