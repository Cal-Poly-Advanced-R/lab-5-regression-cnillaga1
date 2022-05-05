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
  x2 <- data.matrix(scale(p1))                            # creating matrix of predictor variables
  x3 <- cbind(x1, x2)                              # combining 1's matrix and predictor variables matrix
  x4 <- data.matrix(x3)                            # making into matrix

  x5 <- t(x4) %*% x4                                # x'x calculation
  i_matrix <- diag(nrow = nrow(x5))               # creating identity matrix

  results <- data.frame()

  for(i in 1:length(lambda)){
  x6 <- x5 + (lambda[i]) * i_matrix                 # x'x + lambdaI calculation

  coeff <- solve(x6) %*% (t(x4) %*% y1)        # final calculation

  results1 <- data.frame(t(coeff), lambda[i])      # creating into dataframe
  results <- rbind(results, results1)
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

    y <- test_dat %>% select({{response}})                                   # pulling response variable

    model <- ridge_regression(train_dat, {{response}}, lambdas)              # creating model from train_dat

    new_model <- model[,1:ncol(model)-1]                                     # grabbing everything but the lambdas to test model

    int_coeff <- new_model[,1]                   # grabbing intercept
    beta_coeffs <- data.matrix(new_model[,2:ncol(new_model)])       # grabbing betas

    observed_test <- test_dat %>%
      select(-{{response}})                       # removing response variable from testing data frame

    X1 <- data.matrix(scale(observed_test))      # making into matrix

    pred <- beta_coeffs %*% t(X1)                # getting predicted values
    preds2 <- t(pred) + int_coeff

    error_df <- cbind(preds2, y)                # binding predicted and observed values

    new_error <- (error_df[,ncol(error_df)] - error_df[,1:ncol(error_df)-1] )**2      # calculating sum of squares error

    new_error1 <- data.frame(t(new_error))                                          # transposing to get final calculation for sum of squares error

    new_error1$sum <- rowSums(new_error1)                  # final calculation for sum of squares of error

    final_df <- cbind(lambdas, new_error1$sum)             # creating final data frame
    colnames(final_df) <- c("lambda", "error")

    lambda_errors <- data.frame(final_df)

    return(lambda_errors)

}
