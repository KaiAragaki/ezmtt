#' @export
mtt_fit <- function(x, ...) {
  UseMethod("mtt_fit")
}

#' @export
mtt_fit.default <- function(x, ...) {
  cli::cli_abort("No method for class {class(x)}")
}

#' @export
mtt_fit.gp <- function(x, ...) {
  mtt_fit(mtt_tidy(x, ...))
}

#' @export
mtt_fit.data.frame <- function(x, ...) {
  x <- mtt_tidy(x)
  fits <- tapply(x, ~ condition, .fit)
  names <- names(fits)
  mapply(\(x, y) {
    x$condition <- y
    x
  },
  fits, names, SIMPLIFY = FALSE)
}

#' @export
mtt_fit.spectramax <- function(x, conditions, ...) {
  mtt_fit(mtt_tidy(x, conditions, ...))
}

# Data: data.frame with div, dose (numeric, sanitized)
.fit <- function(data) {
  data <- data |>
    subtract_bg_and_get_mean() |>
    normalize_to_lowest_conc()

  tryCatch(
    return(drc::drm(
      div ~ dose, data = data, fct = drc::LL.4(),
      lowerl = c(-Inf, 0, -Inf, -Inf),
      upperl = c(Inf, Inf, 1, Inf)
    )),
    error = \(x) cli::cli_inform("Couldn't fit, relaxing parameters")
  )

  tryCatch(
    return(drc::drm(div ~ dose, data = data, fct = drc::LL.4())),
    error = \(x) cli::cli_inform("Couldn't fit with lax params, using lm")
  )

  stats::lm(div ~ dose, data = data)
}

get_curve_eqn <- function(fit) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))

  if (inherits(fit, "drc")) return(fit$curve[[1]])

  if (inherits(fit, "lm")) {
    coeffs <- stats::coef(fit)
    return(\(x) coeffs[[1]] + coeffs[[2]] * x)
  }
}

get_fit_data <- function(fit, fun) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))
  if (inherits(fit, "drc")) data <- fit$data
  if (inherits(fit, "lm")) data <- fit$model
  colnames(data) <- c("dose", "resp")
  data$condition <- fit$condition
  data
}
