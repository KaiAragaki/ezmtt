#' Plot MTT results
#'
#' @param mtt a `data.frame` output from  `mtt_calc()`
#' @param plot_ics logical. Should the calculated inhibitor concentrations be
#'   plotted?
#'
#' @return a `ggplot`
#' @export
mtt_plot <- function(mtt, plot_ics = FALSE) {

  drug_conc <- c(min(mtt$drug), max(mtt$drug))

  fit_curve <- mtt |>
    dplyr::group_by(.data$condition, .data$fit) |>
    tidyr::nest() |>
    dplyr::mutate(
      curve = purrr::map(.data$fit, make_curve, drug_conc)
    ) |>
    dplyr::select("condition", "curve") |>
    tidyr::unnest(.data$curve) |>
    dplyr::ungroup()

  plot <- mtt |>
    ggplot2::ggplot(
      ggplot2::aes(
        as.numeric(.data$drug),
        .data$div,
        color = .data$condition
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    ggplot2::geom_line(data = fit_curve, ggplot2::aes(.data$x, .data$y))

  if (plot_ics) {
    ic_annot <- mtt |>
      dplyr::group_by(.data$condition) |>
      dplyr::summarize(
        ic_value = unique(.data$ic_value),
        ic_std_err = unique(.data$ic_std_err),
        ic_pct = unique(.data$ic_pct),
        ic_fit = unique(.data$fit)
      ) |>
      dplyr::mutate(
        label = paste0("IC", ic_pct, ": ", round(ic_value, 2))
      ) |>
      dplyr::mutate(
        ic_y_fn = purrr::map(.data$ic_fit, \(x) x[[3]][[1]])
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(ic_y = .data$ic_y_fn(.data$ic_value)[, 1]) |>
      dplyr::ungroup()
    plot <- plot +
      ggrepel::geom_label_repel(
        data = ic_annot,
        ggplot2::aes(x = .data$ic_value, y = .data$ic_y, label = .data$label),
        nudge_x = -5
      )
  }
  plot
}
