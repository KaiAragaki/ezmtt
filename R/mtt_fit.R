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
  mtt_tidy(x) |>
    mtt_fit()
}

#' @export
mtt_fit.data.frame <- function(x, ...) {
  x <- mtt_tidy(x)
  fits <- tapply(x, ~ condition, .fit)
  names <- names(fits)
  mapply(
    \(x, y) {
      x$condition <- y
      x
    },
    fits, names,
    SIMPLIFY = FALSE
  )
}

#' @export
mtt_fit.spectramax <- function(x, conditions, ...) {
  mtt_tidy(x, conditions) |>
    mtt_fit()
}

# Data: dataframe with div, drug (numeric, sanitized)
.fit <- function(data) {
  data <- data |>
    subtract_bg_and_get_mean() |>
    normalize_to_lowest_conc()

  fit <- tryCatch(
    drc::drm(
      div ~ drug, data = data, fct = drc::LL.4(),
      lowerl = c(-Inf, 0, -Inf, -Inf),
      upperl = c(Inf, Inf, 1, Inf)
    ),
    error = function(cond) {
      cli::cli_inform(
        "Failed to fit model with strict parameters, relaxing..."
      )
      NULL
    }
  )

  if (is.null(fit)) {
    fit <- tryCatch(
      drc::drm(
        div ~ drug, data = data, fct = drc::LL.4()
      ),
      error = function(cond) {
        cli::cli_inform(
          "Failed to fit model with lax parameters, using `lm`..."
        )
        NULL
      }
    )
  }

  if (is.null(fit)) fit <- stats::lm(div ~ drug, data = data)

  fit
}

get_curve_eqn <- function(fit) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))

  if (inherits(fit, "drc")) return(fit$curve[[1]])

  if (inherits(fit, "lm")) {
    coeffs <- coef(fit)
    return(\(x) {
      coeffs[[1]] + coeffs[[2]] * x
    })
  }
}

get_fit_data <- function(fit, fun) {
  stopifnot(inherits(fit, "drc") || inherits(fit, "lm"))
  if (inherits(fit, "drc")) {
    data <- fit$data
  }
  if (inherits(fit, "lm")) {
    data <- fit$model
  }
  colnames(data) <- c("dose", "resp")
  data$condition <- fit$condition
  data
}
