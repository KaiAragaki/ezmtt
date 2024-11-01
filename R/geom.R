#' @rdname geom_mtt
#' @export
GeomMtt <- ggplot2::ggproto(
  "GeomMtt", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 1,
    alpha = 1,
    linetype = 1L
  ),
  draw_key = ggplot2::draw_key_path,
  draw_group = function(data, panel_params, coord, n = 1000) {
    ranges <- coord$backtransform_range(panel_params)

    # From ggplot2's abline geom:
    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }

    # Can't have negative concentrations anyway
    if (ranges$x[1] <= 0) ranges$x[1] <- sort(unique(data$x))[2] / 10

    model <- unique(data$model)
    if (length(model) > 1) {
      cli::cli_warn(
        "Found more than one unique model per group, using the first"
      )
      model <- model[1]
    }

    start <- ranges$x[1]
    end <- ranges$x[2]
    mtt_path <- create_mtt(
      data$x,
      data$y,
      model,
      start = start,
      end = end,
      n = n,
      log = TRUE
    )
    coords <- coord$transform(mtt_path, panel_params)
    first_row <- coords[1, , drop = FALSE]
    grid::polylineGrob(
      coords$x, coords$y,
      default.units = "native",
      gp = grid::gpar(
        col = data$colour,
        lwd = data$linewidth,
        lty = data$linetype,
        alpha = data$alpha
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
                     n = 1000,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMtt, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

create_mtt <- function(x,
                       y,
                       model,
                       start,
                       end,
                       n = 1000,
                       log = TRUE) {
  model <- select_model(model)

  if (log) {
    .x <- exp(seq(log(start), log(end), length.out = n))
  } else {
    .x <- seq(start, end, length.out = n)
  }

  data <- data.frame(conc = x, div = y)
  fit <- model(data)
  eq <- get_curve_eqn(fit)
  data.frame(x = .x, y = eq(.x))
}

flexible_model <- function(data) {
  tryCatch(
    return(ll(data)),
    error = \(x) cli::cli_inform("Couldn't fit, relaxing parameters")
  )
  tryCatch(
    return(ll_lax(data)),
    error = \(x) cli::cli_inform("Couldn't fit with lax params, using lm")
  )
  lm_mtt(data)
}

ll <- function(data) {
  drc::drm(
    div ~ conc,
    data = data, fct = drc::LL.4(),
    lowerl = c(-Inf, 0, -Inf, -Inf),
    upperl = c(Inf, Inf, 1, Inf),
  )
}

ll_lax <- function(data) {
  drc::drm(div ~ conc, data = data, fct = drc::LL.4())
}

lm_mtt <- function(data) {
  stats::lm(div ~ conc, data = data)
}
# As for now, let's not accept any custom functions for simplicity
select_model <- function(model) {
  if (is.null(model) || is.na(model)) {
    return(flexible_model)
  }

  if (model == "ll") {
    return(ll)
  }

  if (model == "ll_lax") {
    return(ll_lax)
  }

  if (model == "lm") {
    return(lm_mtt)
  }
}
