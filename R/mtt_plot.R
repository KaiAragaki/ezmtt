#' Plot MTT fits
#'
#' @param fits a list of fits, usually the output from `mtt_fit`
#' @param ic_pct Numeric. If NULL, no IC will be plotted. Otherwise, will plot
#'   `ic_pct`% IC the % IC supplied.
#'
#' @return a `ggplot`
#' @importFrom rlang .data
#' @export
mtt_plot <- function(fits, ic_pct = NULL) {
  curve_data <- do.call(rbind, lapply(fits, get_fit_data))
  plot <- ggplot2::ggplot(
    curve_data,
    ggplot2::aes(.data$dose, .data$resp, color = .data$condition)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    lapply(fits, make_curve_geom)

  if (!is.null(ic_pct)) {
    ic_df <- make_ic_data(fits, ic_pct)
    plot <- plot +
      ggrepel::geom_label_repel(
        data = ic_df,
        ggplot2::aes(.data$x, .data$y, label = .data$label),
        nudge_x = -5,
        show.legend = FALSE,
        inherit.aes = FALSE
      )
  }
  plot
}

make_curve_geom <- function(fit) {
  ggplot2::geom_function(
    ggplot2::aes(color = fit$condition),
    fun = get_curve_eqn(fit)
  )
}

make_ic_data <- function(fits, ic_pct) {
  ics <- lapply(fits, make_ic_datum, ic_pct = ic_pct)
  do.call(rbind, ics)
}

make_ic_datum <- function(fit, ic_pct) {
  if (inherits(fit, "lm")) return(NULL)
  ic <- get_ic(fit, ic_pct = ic_pct)
  ic_x <- ic$ic_value
  ic_y <- predict(fit, data.frame(ic_x))
  ic_label <- fmt_ic_label(ic)
  data.frame(x = ic_x, y = ic_y, label = ic_label)
}

fmt_ic_label <- function(ic) {
  paste0("IC", ic$ic_pct, ": ", round(ic$ic_value, 2))
}
