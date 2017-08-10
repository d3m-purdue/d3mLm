
#' @export
#' @name ModelResponse
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


ModelResponse <- R6::R6Class(
  "ModelResponse",
  public = list(
    # model_call = NULL,
    #
    # response = NULL,
    # predictors = NULL,

    ###################
    # model specific diagnostics
    ###################
    key_val = list(),

    add_key_val = function(key, val) {
      if (!is.null(self$key_val[[key]])) {
        stop("key already set for : ", key)
      }
      self$key_val[[key]] <- val
      invisible(self)
    },

    # # single row data.frame info about model
    # diag_model = NULL, # broom::glance(x)
    # # p row data.frame info about coefficients
    # diag_coefs = NULL, # broom::tidy(x)
    # # n row data.frame info containing all data used and diagnostics that correspond with the data
    # # the first column to not start with a '.' is the response variable
    # diag_data = NULL, # broom::augment(x)

    as_json = function(..., pretty = TRUE) {
      ret <- self$key_val
      jsonlite::toJSON(ret, ..., dataframe = "columns", pretty = pretty, auto_unbox = TRUE)
    }
  )
)


#' Extract linear model diagnostics
#'
#' Extract all information possible with broom::tidy, broom::glance, and broom::augment.
#'
#' @param x linear model object of class 'lm'
#' @return object of class 'ModelResponse'
#' @export
#' @examples
#' radon_model <- lm(Uppm ~ typebldg + basement + dupflag, data = radon_mn)
#' result <- extract_lm(radon_model)
#' result$as_json()
extract_model <- function(x) {
  response <- ModelResponse$new()

  # set diagnostic content
  update_response(x, response)

  response
}


#' Update model response object
#'
#' @param x model object
#' @param res response object of class \code{ModelResponse}
#' @param ... ignored
#' @export
#' @rdname update_response
#' @import broom
update_response <- function(x, res, ...) {
  UseMethod("update_response")
}
#' @export
#' @rdname update_response
update_response.default <- function(x, res, ...) {
  stop("method not implemented for object of class: ", class(x)[1])
}

#' @export
#' @rdname update_response
update_response.lm <- function(x, res, ...) {
  res$add_key_val("model_type", "linear model")
  res$add_key_val("model_call", as.character(as.expression(x$call)))

  diag_data <- augment(x)

  # response variable
  diag_data_names <- names(diag_data)
  is_response <- min(which(!grepl("^\\.", diag_data_names, perl = TRUE)))
  res$add_key_val("response_variable", diag_data_names[is_response])

  # betas
  tidy_dt <- tidy(x)
  res$add_key_val("predictor_variables", as.list(setdiff(tidy_dt$term, "(Intercept)")))

  # broom
  res$add_key_val("diag_model", as.list(glance(x)))
  res$add_key_val("diag_coefs", tidy_dt)
  res$add_key_val("diag_data", diag_data)

  res
}


#' @export
#' @rdname update_response
update_response.loess <- function(x, res, ...) {
  res$add_key_val("model_type", "loess model")
  res$add_key_val("model_call", as.character(as.expression(x$call)))

  # response variable
  predictors <- x$xnames
  response_variable <- setdiff(attr(attr(x$terms, "dataClasses"), "names"), predictors)
  res$add_key_val("response_variable", response_variable)
  res$add_key_val("predictor_variables", as.list(response_variable))

  # augment
  diag_data <- augment(x)
  diag_data$.weights <- x$weights
  diag_data$.robust <- x$robust

  # glance
  glance_list <- x[c(
    "n", "enp", "s", "trace.hat"
  )]
  glance_list <- append(glance_list, x$pars)
  res$add_key_val("diag_model", glance_list)
  # res$add_key_val("diag_coefs", NULL) # nolint

  res$add_key_val("diag_data", diag_data)

  res
}

#' Run a model and extract features
#'
#' @param data data used. (data.frame)
#' @param response single response variable to be used. (character)
#' @param predictor_variables name of all predictor variables to be used. (character vector)
#' @param model_fn function to create the model object
#' @param quadratic_variables name of quadratic variables. (character vector)
#' @export
#' @rdname run_model
run_model <- function(data, response, predictor_variables, model_fn) {
  if (!is.character(response)) stop("must supply a character type for response")
  if (!is.character(predictor_variables)) {
    stop("must supply a character type vector for predictor_variables")
  }
  form <- as.formula(paste(response, "~", paste(predictor_variables, collapse = "+")))
  mod <- model_fn(form, data = data)
  res <- extract_model(mod)
  res$as_json()
}


#' @export
#' @rdname run_model
#' @examples
#' run_lm(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"))
run_lm <- function(data, response, predictor_variables) {
  run_model(data, response, predictor_variables, model_fn = lm)
}

#' @export
#' @rdname run_model
#' @examples
#' run_quadratic(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"), c("Sepal.Width"))
run_quadratic <- function(data, response, predictor_variables, quadratic_variables) {
  to_name_fn <- function(x) {
    paste(x, "2", sep = "")
  }
  for (term in quadratic_variables) {
    vals <- data[[term]]
    to_name <- to_name_fn(term)
    if (!is.null(data[[to_name]])) {
      stop("found existing column: ", to_name, ". Remove or rename column to run quadratic model")
    }
    data[[to_name]] <- vals * vals
  }
  run_model(data, response, c(predictor_variables, to_name_fn(quadratic_variables)), model_fn = lm)
}


#' @export
#' @rdname run_model
#' @examples
#' run_loess(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"))
run_loess <- function(data, response, predictor_variables) {
  run_model(data, response, predictor_variables, model_fn = loess)
}
