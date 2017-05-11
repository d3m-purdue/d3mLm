


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
        beta_variables = self$beta_variables,
        diag_model = as.list(self$diag_model),
        diag_coefs = self$diag_coefs,
        diag_data = self$diag_data
      )
      jsonlite::toJSON(ret, ..., dataframe = "columns", pretty = pretty, auto_unbox = TRUE)
    }
  )
)



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
