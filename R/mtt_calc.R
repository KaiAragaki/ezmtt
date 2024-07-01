#' Calculate and fit MTT model from absorbance data
#'
#' @param x A `data.frame`, `spectramax`, or `gp` object. See Details.
#' @param ... Arguments passed to their respective methods
#'
#' @details
#'
#' If supplied with a `data.frame`, `mtt` will expect columns `condition`,
#' `drug`, `nm562`, and `nm660`.
#'
#' If supplied with a `spectramax` object, everything should be in place.
#'
#' Using a `gp` object (NOT CURRENTLY SUPPORTED) is a good idea if you have a
#' 'non-standard' plate layout (standard being each quarter of the plate is a
#' condition)
#'
#' @return a `tibble`
#' @export
mtt_calc <- function(x, ...) {
  UseMethod("mtt_calc")
}

#' @export
#' @rdname mtt_calc
mtt_calc.data.frame <- function(x, ic_pct = 50, ...) {
  drug_conc <- sanitize_drug_conc(x$drug)

  x$drug <- ifelse(x$drug == 0,  min(drug_conc), x$drug)

  x |>
    rm_unassigned_wells() |>
    subtract_bg_and_get_mean() |>
    normalize_to_lowest_conc() |>
    fit_mtt(drug_conc, ic_pct)
}

#' @param condition_names What to name each 'sector' of the plate
#' @param drug_conc a numeric vector containing drug concentrations of the
#'   conditions, from left to right
#' @param ic_pct numeric. The %IC desired, where 25 would represent the
#'   concentration at which growth was reduced by 25% vs baseline
#' @export
#' @rdname mtt_calc
mtt_calc.spectramax <- function(x, condition_names, drug_conc, ic_pct = 50, ...) {
  drug_conc <- sanitize_drug_conc(drug_conc)

  df <- x$data[[1]]$data |>
    gplate::gp_sec("condition", nrow = 4, ncol = 6, labels = condition_names) |>
    gplate::gp_sec("drug", nrow = 4, ncol = 1, labels = drug_conc, advance = FALSE) |>
    gplate::gp_serve() |>
    dplyr::mutate(drug = as.numeric(levels(.data$drug)[.data$drug]))

  mtt_calc(df, ic_pct = ic_pct)
}

#' @export
#' @rdname mtt_calc
mtt_calc.gp <- function(x, ic_pct = 50, ...) {
  mtt_calc(gplate::gp_serve(x), ic_pct, ...)
}
