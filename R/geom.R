#' @rdname geom_mtt
#' @export
GeomMtt <- ggplot2::ggproto(
  "GeomMtt", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    model = NA,
    colour = "black",
    linewidth = 1,
    alpha = 1,
    linetype = 1L
  ),
  draw_key = ggplot2::draw_key_path,
  draw_group = function(data, panel_params, coord, n = 1000) {
    ranges <- coord$backtransform_range(panel_params)

    # Can't have negative concentrations anyway
    if (ranges$x[1] <= 0) ranges$x[1] <- sort(unique(data$x))[2] / 10

    model <- select_model(get_one_aes(data$model))
    fit <- fit_mtt_model(data$x, data$y, model)

    start <- ranges$x[1]
    end <- ranges$x[2]
    mtt_path <- create_mtt(fit = fit, start = start, end = end, n = n)
    coords <- coord$transform(mtt_path, panel_params)
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

StatIcMtt <- ggplot2::ggproto(
  "StatIcMtt",
  ggplot2::Stat,
  # X and Y here stand for conc and div, not for the ultimate position of the
  # ICs
  required_aes = c("x", "y"),
  # While the IC can be parameterized using only one row, it needs many rows to
  # fit it. So it actually should be compute_group instead of compute_panel.
  compute_group = function(data, scales, ic = 50) {
    # Turn a model name into an actual model
    model <- select_model(get_one_aes(data$model))

    # Fit the model (in the case of flexible_model, choose and then fit the
    # model)
    fit <- fit_mtt_model(data$x, data$y, model)

    ic <- get_one_aes(ic)
    data <- make_ic_datum(fit, ic)
    data
  },
  default_aes = ggplot2::aes(ic = 50, model = NA)
)

#' Plot MTT data
#'
#' @param ic The IC percentage that should be calculated
#' @param ... Additional arguments passed to ggplot2::layer params
#' @inheritParams ggplot2::layer
#' @export
stat_ic_mtt <- function(mapping = NULL,
                        data = NULL,
                        geom = "point",
                        position = "identity",
                        ...,
                        ic = 50,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatIcMtt,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      ic = ic,
      ...
    )
  )
}

#' Plot MTT data
#'
#' @param na.rm Remove NA values?
#' @param n How many line segments should be used to draw an MTT curve?
#' @param ... Additional arguments passed to ggplot2::layer params
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

fit_mtt_model <- function(x, y, model) {
  model(data.frame(conc = x, div = y))
}

create_mtt <- function(fit,
                       start,
                       end,
                       n = 1000) {
  x <- exp(seq(log(start), log(end), length.out = n))
  eq <- get_curve_eqn(fit)
  data.frame(x = x, y = eq(x))
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

get_one_aes <- function(aes) {
  aes <- unique(aes)
  if (length(aes) > 1) {
    cli::cli_warn("Found >1 unique aes per group, using first")
    aes <- aes[1]
  }
  aes
}
