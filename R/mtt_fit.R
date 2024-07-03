#' Fit an curve through MTT data
#'
#' @param x An object of class `gplate::gp`, `data.frame`, or `mop::spectramax`.
#'   See details.
#' @param conditions A named list of numerics of length 4. Contains doses per
#'   quadrant. Names are the drug. If quadrant should be omitted, supply NA.
#'   Quadrants go from left to right, top to bottom. Identical names are
#'   allowed.
#' @param model A character vector of models that should be used to fit the
#'   data. If NULL, will start with a 4-parameter log-logistic model and fall
#'   back using a linear model. Options for supplying a vector include NA, "lm",
#'   and "ll". See details.
#' @param ... Unused
#'
#' @details If a `data.frame` or `gplate::gp` is supplied, it should have
#'   columns `condition`, `dose`, `nm562` and `nm660`
#'
#' This function will first attempt to fit the data using a 4-parameter
#' log-logistic model, then use a linear model as a fallback if fitting fails.
#'
#' The `model` argument can be used to specify a model manually by supplying a
#' character vector with length equal to the number of *unique* conditions.
#' Valid values for the character vector include:
#' - NA: Fitting will start with log-logistic, fallback on linear
#' - "ll": Only (try to) fit with log-logistic
#' - "lm": Only fit with `lm`
#'
#' @export
mtt_fit <- function(x, model, ...) {
  UseMethod("mtt_fit")
}

#' @export
#' @rdname mtt_fit
mtt_fit.default <- function(x, model = NULL, ...) {
  cli::cli_abort("No method for class {class(x)}")
}

#' @export
#' @rdname mtt_fit
mtt_fit.gp <- function(x, model = NULL, ...) {
  mtt_fit(mtt_tidy(x), model)
}

#' @export
#' @rdname mtt_fit
mtt_fit.data.frame <- function(x, model = NULL, ...) {
  if (!is.null(model) &&
        (length(model) != length(unique(x$condition)))) {
    cli::cli_abort(
      "`model` must be the same length as the number of *unique* conditions"
    )
  }

  # mapply hates NULL things, but NULL signals to user that model is optional
  if (is.null(model)) model <- NA

  x <- mtt_tidy(x)
  splits <- split(x, ~ condition)
  fits <- mapply(.fit, splits, model, SIMPLIFY = FALSE)
  names <- names(fits)
  mapply(\(x, y) {
    x$condition <- y
    x
  },
  fits, names, SIMPLIFY = FALSE)
}

#' @export
#' @rdname mtt_fit
mtt_fit.spectramax <- function(x, conditions, model = NULL, ...) {
  mtt_fit(mtt_tidy(x, conditions), model)
}

# Data: data.frame with div, dose (numeric, sanitized)
.fit <- function(data, model) {
  data <- data |>
    subtract_bg_and_get_mean() |>
    normalize_to_lowest_conc()

  if (is.null(model) || is.na(model) || model == "ll") {
    tryCatch(
      return(drc::drm(
        div ~ dose,
        data = data, fct = drc::LL.4(),
        lowerl = c(-Inf, 0, -Inf, -Inf),
        upperl = c(Inf, Inf, 1, Inf)
      )),
      error = \(x) cli::cli_inform("Couldn't fit, relaxing parameters")
    )
  }

  if (is.null(model) || is.na(model) || model == "ll") {
    tryCatch(
      return(drc::drm(div ~ dose, data = data, fct = drc::LL.4())),
      error = \(x) cli::cli_inform("Couldn't fit with lax params, using lm")
    )
  }

  stats::lm(div ~ dose, data = data)
}

get_curve_eqn <- function(fit) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))

  if (inherits(fit, "drc")) return(fit$curve[[1]])

  if (inherits(fit, "lm")) {
    coeffs <- stats::coef(fit)
    return(\(dose) matrix(coeffs[[1]] + coeffs[[2]] * dose))
  }
}

get_fit_data <- function(fit, fun) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))
  if (inherits(fit, "drc")) {
    data <- fit$data
    data <- data[1:2]
  }
  if (inherits(fit, "lm")) {
    data <- fit$model
    data <- data[2:1]
  }
  colnames(data) <- c("dose", "resp")
  data$condition <- fit$condition
  data
}

# TODO:
# Ensure model is same extent as drug conditions in non-spectramax data?
# Can probably check at fit stage - since all are data.frames then
