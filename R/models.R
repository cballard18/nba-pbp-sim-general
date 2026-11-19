#' Load the simulator's models
#'
#' Reads the fitted models from a directory. You normally don't need this: every simulation
#' function uses [nbasim_models()], which loads the models shipped with the package once and
#' caches them.
#'
#' @param dir Directory holding `timeout.ubj`, `outcome.ubj`, `duration.ubj` and `rates.rds`.
#' @return A list of models and league rates.
#' @export
load_models <- function(dir = system.file("models", package = "nbasim")) {
  if (!nzchar(dir) || !file.exists(file.path(dir, "outcome.ubj"))) {
    stop("No models found in '", dir, "'. Fit them with data-raw/fit_models.R.", call. = FALSE)
  }
  m <- lapply(c(timeout = "timeout", outcome = "outcome", duration = "duration"), function(name) {
    model <- xgb.load(file.path(dir, paste0(name, ".ubj")))
    xgb.model.parameters(model) <- list(nthread = parallel::detectCores())
    model
  })
  c(m, readRDS(file.path(dir, "rates.rds")))
}

.cache <- new.env(parent = emptyenv())

#' The packaged models (loaded once per session)
#' @return A list of models and league rates.
#' @export
nbasim_models <- function() {
  if (is.null(.cache$m)) .cache$m <- load_models()
  .cache$m
}
