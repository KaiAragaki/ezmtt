# Data: dataframe with div and drug (numeric, sanitized)
mtt_fit <- function(data) {
  fit <- tryCatch(
    drc::drm(
      div ~ drug, data = data, fct = drc::LL.4(),
      lowerl = c(-Inf, 0, -Inf, -Inf),
      upperl = c(Inf, Inf, 1, Inf)
    ),
    error = function(cond) {
      cli::cli_inform(
        "Failed to fit model with strict parameters, relaxing..."
      )
      NULL
    }
  )

  if (is.null(fit)) {
    fit <- tryCatch(
      drc::drm(
        div ~ drug, data = data, fct = drc::LL.4()
      ),
      error = function(cond) {
        cli::cli_inform(
          "Failed to fit model with lax parameters, using `lm`..."
        )
        NULL
      }
    )
  }

  if (is.null(fit)) fit <- stats::lm(div ~ drug, data = data)

  fit  
}
