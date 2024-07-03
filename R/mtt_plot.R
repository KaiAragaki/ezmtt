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
  ics <- lapply(fits, get_ic, ic_pct = ic_pct)
  ic_x <- vapply(ics, \(x) x$ic_value, 1)
  # predict needs data.frames, mapply needs lists
  temp <- lapply(as.list(ic_x), as.data.frame)
  ic_y <- unlist(mapply(stats::predict, fits, temp, SIMPLIFY = FALSE))
  ic_labels <- vapply(ics, fmt_ic_label, "")
  data.frame(x = ic_x, y = ic_y, label = ic_labels)
}

fmt_ic_label <- function(ic) {
  paste0("IC", ic$ic_pct, ": ", round(ic$ic_value, 2))
}
