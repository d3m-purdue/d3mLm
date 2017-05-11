
#' @export
#' @name LmModelResponse
#' @title Linear Model Response
#' @description R6 object containing all necessary information of a linear model object
#' @section Usage:
#' \preformatted{
#' radon_model <- lm(Uppm ~ typebldg + basement + dupflag, data = radon_mn)
#' result <- extract_lm(radon_model)
#' result$as_json()
#' }
#' @section Initialize:
#' This should only be called by methods within the package
#' @section Details:
#' \code{$model_type}: short description of the model information. "linear model"
#'
#' \code{$model_call}: character value of the call made to produce the model
#'
#' \code{$response_variable}: character value of the response in the linear model
#'
#' \code{$beta_variables}: character vector of beta variables used in the linear model
#'
#' \code{$diag_model}: single row data.frame information about the linear model
#' Contains \describe{
#'     \item{\code{'r.squared'}}{The percent of variance explained by the model}
#'     \item{\code{'adj.r.squared'}}{r.squared adjusted based on the degrees of freedom}
#'     \item{\code{'sigma'}}{Estimate of residual standard deviation when corresponding observation is dropped from model}
#'     \item{\code{'statistic'}}{F-statistic}
#'     \item{\code{'p.value'}}{p-value from the F test, describing whether the full regression is significant}
#'     \item{\code{'df'}}{Degrees of freedom used by the coefficients}
#'     \item{\code{'logLik'}}{the data's log-likelihood under the model}
#'     \item{\code{'AIC'}}{the Akaike Information Criterion}
#'     \item{\code{'BIC'}}{the Bayesian Information Criterion}
#'     \item{\code{'deviance'}}{deviance}
#'     \item{\code{'df.residual'}}{residual degrees of freedom}
#'   }
#'
#' \code{$diag_coefs}: 'p' row data.frame information about the linear model coefficients.
#' \describe{
#'   \item{\code{'term'}}{The term in the linear model being estimated and tested}
#'   \item{\code{'estimate'}}{The estimated coefficient}
#'   \item{\code{'std.error'}}{The standard error from the linear model}
#'   \item{\code{'statistic'}}{t-statistic}
#'   \item{\code{'p.value'}}{two-sided p-value}
#' }
#'
#' \code{$diag_data}: 'n' row data.frame information about the linear model data.
#' \describe{
#'   \item{\code{'.fitted'}}{Fitted values of model}
#'   \item{\code{'.se.fit'}}{Standard errors of fitted values}
#'   \item{\code{'.resid'}}{Residuals}
#'   \item{\code{'.hat'}}{Diagonal of the hat matrix}
#'   \item{\code{'.sigma'}}{Estimate of residual standard deviation when corresponding observation is dropped from model}
#'   \item{\code{'.cooksd'}}{Cooks distance, ‘cooks.distance’}
#'   \item{\code{'.std.resid'}}{Standardised residuals. (Some unusual "lm" objects, such as "rlm" from MASS, may omit ‘.cooksd’ and ‘.std.resid’. "gam" from mgcv omits ‘.sigma’)}
#' }
NULL


LmModelResponse <- R6::R6Class(
  "LmModelResponse",
  public = list(
    model_type = "linear model",

    model_call = NULL,

    ###################
    # linear model specific diagnostics
    ###################
    response_variable = NULL,
    beta_variables = NULL,

    # single row data.frame info about model
    diag_model = data.frame(), # broom::glance(x)
    # p row data.frame info about coefficients
    diag_coefs = data.frame(), # broom::tidy(x)
    # n row data.frame info containing all data used and diagnostics that correspond with the data
    # the first column to not start with a '.' is the response variable
    diag_data = data.frame(), # broom::augment(x)

    as_json = function(..., pretty = TRUE) {
      ret <- list(
        model_type = self$model_type,
        model_call = self$model_call,
        response_variable = self$response_variable,
        beta_variables = as.list(self$beta_variables),
        diag_model = as.list(self$diag_model),
        diag_coefs = self$diag_coefs,
        diag_data = self$diag_data
      )
      jsonlite::toJSON(ret, ..., dataframe = "columns", pretty = pretty, auto_unbox = TRUE)
    }
  )
)


#' Extract linear model diagnostics
#'
#' Extract all information possible with broom::tidy, broom::glance, and broom::augment.
#'
#' @param x linear model object of class 'lm'
#' @return object of class 'LmModelResponse'
#' @export
#' @examples
#' radon_model <- lm(Uppm ~ typebldg + basement + dupflag, data = radon_mn)
#' result <- extract_lm(radon_model)
#' result$as_json()
extract_lm <- function(x) {
  if (!inherits(x, "lm")) {
    stop("'x' must inherit a linear model 'lm'")
  }

  response <- LmModelResponse$new()

  response$model_call <- as.character(as.expression(x$call))

  # set diagnostic content
  response$diag_model <- broom::glance(x)
  response$diag_coefs <- broom::tidy(x)
  response$diag_data <- broom::augment(x)

  # response variable
  diag_data_names <- names(response$diag_data)
  is_response <- min(which(!grepl("^\\.", diag_data_names, perl = TRUE)))
  response$response_variable <- diag_data_names[is_response]

  # betas
  response$beta_variables <- response$diag_coefs$term

  response
}
