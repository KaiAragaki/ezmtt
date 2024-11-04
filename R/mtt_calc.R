#' Calculate normalized ODs without fitting
#'
#' It can be useful to calculate normalized ODs without fitting, particularly if
#' you would like to use `geom_mtt`, or need to manipulate your data downstream.
#'
#' @param data A `data.frame` of absorbances, containing at least the `signal`,
#'   `background`, and `dose` columns (see below)
#' @param signal The name of the column that contains the signal absorbances
#' @param background The name of the column that contains the background
#'   absorbances. If NULL, will just use signal.
#' @param dose The name of the column containing the concentrations of the drug.
#'   Will use the lowest concentration as the baseline.
#' @param out The name of the output column
#' @param .by Character vector of columns to summarize by. Other columns will be
#'   dropped
#' @export
mtt_calc <- function(data,
                     signal = "nm562",
                     background = "nm660",
                     dose = "dose",
                     out = "div",
                     .by = NULL) {
  required_names <- c(signal, background, dose, .by)
  if (!all(required_names %in% names(data))) {
    cli::cli_abort("Not all required columns are in data")
  }

  minimal <- data[, match(required_names, names(data))]

  # If background is NULL, just use raw signal
  if (is.null(background)) {
    minimal$diff <- minimal[[signal]]
  } else {
    minimal$diff <- minimal[[signal]] - minimal[[background]]
  }

  group_cols <- c(dose, .by)
  rest_cols <- setdiff(names(minimal), group_cols)

  group_data <- minimal[, match(group_cols, names(minimal))]
  minimal$diff_mean <- stats::ave(minimal$diff, group_data)
  group_cols <- setdiff(group_cols, dose)

  # Need to find an easy way to mutate with groups
  # tapply or split might do the trick
  if (length(group_cols) == 0) {
    data <- .calc_div(minimal, dose, out)
  } else {
    group_data <- group_data[, match(group_cols, names(group_data))]
    no_group_data <- minimal[, -match(group_cols, names(minimal))]
    data <- tapply(
      no_group_data, group_data, .calc_div,
      out = out, dose = dose
    ) |>
      array2DF()
  }
  data
}

.calc_div <- function(data, dose, out) {
  # tapply runs all combinations of factors, even ones that have no combinations
  # This gets ahead of that by returning a NULL `data.frame` of appropriate
  # size. Otherwise, it'll freak out when trying to take the min of NULLy
  if (nrow(data) == 0) {
    data[[out]] <- numeric(0)
    return(data)
  }
  smallest_dose_index <- which(data[[dose]] == min(data[[dose]]))[1]
  div <- data$diff / data$diff_mean[smallest_dose_index]
  data[[out]] <- div
  data
}
