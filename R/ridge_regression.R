#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  y <- dat %>% select({{response}})                 # pulling response variable

  y1 <- data.matrix(y)                              # creating matrix of response variable


  p1 <- dat %>%
    select(-{{response}})                          # removing response variable
  x1 <- matrix(1:1, nrow = nrow(dat))              # creating 1's column
  x2 <- data.matrix(p1)                            # creating matrix of predictor variables
  x3 <- cbind(x1, x2)                              # combining 1's matrix and predictor variables matrix
  x4 <- data.matrix(x3)                            # making into matrix



  x5 <- t(x4) %*% x4                                # x'x calculation

  i_matrix <- diag(nrow = nrow(x5))               # creating identity matrix

  results <- data.frame()

  for(i in length(lambda)){
  x6 <- x5 + (lambda[i] * i_matrix)                 # x'x + lambdaI calculation

  coeff <- (solve(x6)) %*% (t(x4) %*% y1)        # final calculation

  results1 <- data.frame(t(coeff), lambda[i])      # creating into dataframe
  results <- rbind(results, results1)
  results <- data.frame(results)
  }
  colnames(results)[1] <- "Intercept"          # renaming column for intercept

  colnames(results)[ncol(results)] <- "lambda"

  return(results)

}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {


  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}
