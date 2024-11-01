#' @rdname geom_mtt
#' @export
GeomMtt <- ggplot2::ggproto(
  "GeomMtt", ggplot2::Geom,
  required_aes = c("x", "y"),
  draw_key = ggplot2::draw_key_path,
  draw_group = function(data, panel_params, coord) {
    ranges <- coord$backtransform_range(panel_params)
    # From ggplot2's abline geom:
    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }
    # Can't have negative concentrations anyway
    if (ranges$x[1] <= 0) ranges$x[1] <- sort(unique(data$x))[2] / 10
    start <- ranges$x[1]
    end <- ranges$x[2]
    mtt_path <- create_mtt(data$x, data$y, start = start, end = end, n = 100, log = TRUE)
    coords <- coord$transform(mtt_path, panel_params)
    first_row <- coords[1, , drop = FALSE]
    grid::polylineGrob(
      coords$x, coords$y,
      default.units = "native",
      gp = grid::gpar(
        col = data$colour
      )
    )
  }
)

#' Plot MTT data
#'
#' @param na.rm Remove NA values?
#' @inheritParams ggplot2::layer
#' @export
geom_mtt <- function(mapping = NULL,
                     data = NULL,
                     stat = "identity",
                     position = "identity",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMtt, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

create_mtt <- function(x,
                       y,
                       start,
                       end,
                       n = 100,
                       log = TRUE) {
  if (log) {
    .x <- exp(seq(log(start), log(end), length.out = n))
  } else {
    .x <- seq(start, end, length.out = n)
  }

  data <- data.frame(conc = x, div = y)
  fit <- flexible_model(data)
  eq <- get_curve_eqn(fit)
  data.frame(x = .x, y = eq(.x))
}

flexible_model <- function(data) {
  tryCatch(
    return(
      drc::drm(div ~ conc,
        data = data, fct = drc::LL.4(),
        lowerl = c(-Inf, 0, -Inf, -Inf),
        upperl = c(Inf, Inf, 1, Inf)
      )
    ),
    error = function(x) cli::cli_inform("Couldn't fit, relaxing parameters")
  )
  tryCatch(
    return(
      drc::drm(div ~ conc,
        data = data,
        fct = drc::LL.4()
      )
    ),
    error = function(x) cli::cli_inform("Couldn't fit with lax params, using lm")
  )
  lm(div ~ conc, data = data)
}
