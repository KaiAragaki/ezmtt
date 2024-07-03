#' Remove unassigned wells
#'
#' @param df a `data.frame`
#'
#' @return a `data.frame` without any conditions or doses that are `NA`
#' @noRd
rm_unassigned_wells <- function(df) {
  na_cond <- is.na(as.character(df$condition))
  na_dose <- is.na(as.character(df$dose))
  df[!(na_cond | na_dose), ]
}

#' Subtract background and calculate mean per dose and condition
#'
#' @param df a `data.frame` containing columns `condition`, `dose`, `nm562`, and
#'   `nm660`.
#'
#' @return a `tibble`
#' @noRd
subtract_bg_and_get_mean <- function(df) {
  df |>
    dplyr::mutate(
      diff = .data$nm562 - .data$nm660,
      mean = mean(.data$diff),
      .by = "dose"
    )
}

#' Divide all differences by lowest concentration
#'
#' @param df a `data.frame` containing at `diff` (A562-A660), `dose` (numeric
#'   conc of drug), and `mean` (mean of `diff` per condition and dose)
#'
#' @return a `tibble`
#' @noRd
normalize_to_lowest_conc <- function(df) {
  df |>
    dplyr::mutate(
      div = .data$diff / .data$mean[which(.data$dose == min(.data$dose))]
    )
}

#' Get the IC value of a fit at a given percent
#'
#' @param fit A `drc` object
#' @param ic_pct numeric. The %IC desired, where 25 would represent the
#'   concentration at which growth was reduced by 25% vs baseline
#'
#' @return A `data.frame` with three columns - `ic_value`, `ic_std_err`, and
#'   `ic_pct`
get_ic <- function(fit, ic_pct) {
  if (is.null(fit)) return(NA)

  ic <- drc::ED(fit, respLev = ic_pct, display = FALSE) |>
    as.numeric()

  data.frame(
    ic_value = ic[1],
    ic_std_err = ic[2],
    ic_pc = ic_pct
  )
}

#' Convert 0 to a small enough equivalent
#'
#' @details 0 doesn't behave well with the fitting algorithm. This takes the
#' user-supplied dose and converts the 0 value (if it exists) to
#' a value low enough to approximate 0 without scaring the fitting algorithm too
#' much. Specifically, it converts 0 to:
#'
#' \eqn{\frac{SecondSmallest}{ThirdSmallest^4}}
#'
#' @param dose A numeric vector of doses
#'
#' @param quiet Should conversion from 0 to some small number be done silently?
#'
#' @return The smallest value, or if 0, a new small value
#' @noRd
calc_new_min <- function(dose, quiet = FALSE) {
  if (min(dose, na.rm = TRUE) != 0) return(min(dose, na.rm = TRUE))

  sorted <- unique(sort(dose))
  new_low <- (sorted[2] / sorted[3])^4
  if (!quiet)
    cli::cli_inform("Lowest dose is 0, converting to {new_low}")

  new_low
}
