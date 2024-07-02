#' Plot MTT results
#'
#' @param mtt a `data.frame` output from  `mtt_calc()`
#' @param plot_ics logical. Should the calculated inhibitor concentrations be
#'   plotted?
#'
#' @return a `ggplot`
#' @export

# mtt is a list of fits
# will probably need a custom 'condition' list item for each fit
mtt_plot <- function(fits, plot_ics = FALSE) {

  curve_data <- do.call(rbind, lapply(fits, get_fit_data))

  plot <- ggplot2::ggplot(
    curve_data,
    ggplot2::aes(.data$dose, .data$resp, color = .data$condition)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    lapply(fits, make_curve_geom)


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

make_curve_geom <- function(fit) {
  ggplot2::geom_function(
    ggplot2::aes(color = fit$condition),
    fun = get_curve_eqn(fit)
  )
}
