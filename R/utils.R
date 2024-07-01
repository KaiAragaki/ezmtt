#' Remove unassigned wells
#'
#' @param df a `data.frame`
#'
#' @return a `tibble` without any conditions that are `NA`
#' @noRd
rm_unassigned_wells <- function(df) {
  dplyr::filter(df, !is.na(as.character(.data$condition)))
}

#' Subtract background and calculate mean per drug and condition
#'
#' @param df a `data.frame` containing columns `condition`, `drug`, `nm562`, and
#'   `nm660`.
#'
#' @return a `tibble`
#' @noRd
subtract_bg_and_get_mean <- function(df) {
  dplyr::group_by(df, .data$condition, .data$drug) |>
    dplyr::mutate(
      diff = .data$nm562 - .data$nm660,
      mean = mean(.data$diff)
    ) |>
    dplyr::ungroup()
}

#' Divide all differences by lowest concentration
#'
#' @param df a `data.frame` containing at `diff` (A562-A660), `drug` (numeric
#'   conc of drug), and `mean` (mean of `diff` per condition and conc)
#'
#' @return a `tibble`
#' @noRd
normalize_to_lowest_conc <- function(df) {
  dplyr::group_by(df, .data$condition) |>
    dplyr::mutate(div = .data$diff / .data$mean[which(.data$drug == min(.data$drug))]) |>
    dplyr::ungroup()
}

#' Model curve, produce plots, and calculate IC per MTT condition
#'
#' @param df a `data.frame` containing a `condition`, `div`, and `drug` column
#' @param drug_conc a numeric vector containing drug concentrations of the
#'   conditions, from left to right
#' @param ic_pct numeric. The %IC desired, where 25 would represent the
#'   concentration at which growth was reduced by 25% vs baseline
#'
#' @return A `tibble`
#' @noRd
fit_mtt <- function(df, drug_conc, ic_pct) {
  dplyr::group_by(df, .data$condition) |>
    tidyr::nest() |>
    dplyr::mutate(
      fit = purrr::map(.data$data, mtt_model),

      ic = purrr::map(.data$fit, get_ic, ic_pct = ic_pct)
    ) |>
    tidyr::unnest(cols = c("ic", "data")) |>
    dplyr::ungroup()
}

#' Try to fit a logistic curve to MTT data
#'
#' Function will try to fit a 4 parameter log-logistic function. Constraints: Y
#' is bounded between 0 and 1.
#'
#' If the model fails to fit the first time, it will try again without
#' constraints
#'
#' @param data a `data.frame` containing a `div` (dep var.) and `drug` (indep.
#'   var) column.
#'
#' @return  A `drc` object
mtt_model <- function(data) {

  model_strict <- function(data) {
    drc::drm(
      div ~ drug, data = data, fct = drc::LL.4(),
      lowerl = c(-Inf, 0, -Inf, -Inf),
      upperl = c(Inf, Inf, 1, Inf)
    )
  }
  model_lax <- function(data) {
    drc::drm(
      div ~ drug, data = data, fct = drc::LL.4()
    )
  }
  safe_model_strict <- purrr::safely(model_strict)
  model <- safe_model_strict(data)
  if (!is.null(model$result)) {
    return(model$result)
  }
  safe_model_lax <- purrr::safely(model_lax)
  model <- safe_model_lax(data)
  model$result
}

#' Make points for plotting based off a fit
#'
#' @param fit a `drc` object
#' @param drug_conc numeric vector of drug concentrations. Assumes no 0s.
#' @param length_out number of points to generate. More points = smoother curve.
#'
#' @return A `data.frame` of x and y coordinates for a given fit
#' @noRd
make_curve <- function(fit, drug_conc, length_out = 1000) {
  x <- exp(seq(log(min(drug_conc)), log(max(drug_conc)), length.out = length_out))
  if (!is.null(fit)) {
    curve <- fit$curve[[1]](x)
  } else {
    curve <- NA
  }
  data.frame(x = x, y = curve)
}

#' Get the IC value of a fit at a given percent
#'
#' @param fit A `drc` object
#' @param ic_pct numeric. The %IC desired, where 25 would represent the
#'   concentration at which growth was reduced by 25% vs baseline
#'
#' @return A `tibble` with three columns - `ic_value`, `ic_std_err`, and
#'   `ic_pct`
get_ic <- function(fit, ic_pct) {
  if(!is.null(fit)) {
    drc::ED(fit, respLev = ic_pct, display = FALSE) |>
      dplyr::as_tibble() |>
      dplyr::rename(
        ic_value = "Estimate",
        ic_std_err = "Std. Error"
      ) |>
      dplyr::mutate(ic_pct = ic_pct)
  } else {
    NA
  }
}

#' Convert 0 to a small enough equivalent
#'
#'
#' @details 0 doesn't behave well with the fitting algorithm. This takes the
#' user-supplied drug concentrations and converts the 0 value (if it exists) to
#' a value low enough to approximate 0 without scaring the fitting algorithm too
#' much. Specifically, it converts 0 to:
#'
#' \eqn{\frac{SecondSmallest}{ThirdSmallest^4}}
#'
#' @param drug_conc numeric vector of drug concentrations, in the order which
#'   they appear in the section (left to right)
#'
#' @param quiet Should conversion from 0 to some small number be done silently?
#'
#' @return a numeric vector of the same length and order as input
#' @noRd
sanitize_drug_conc <- function(drug_conc, quiet = FALSE) {
  if (min(drug_conc) == 0) {
    sorted <- unique(sort(drug_conc))
    new_low <- (sorted[2]/sorted[3])^4
    drug_conc <- ifelse(drug_conc == 0, new_low, drug_conc)
    if (!quiet) {
      message("Lowest drug concentration is 0, converting to ", new_low)
    }
  }
  drug_conc
}
