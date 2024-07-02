#' @export
mtt_tidy <- function(x, ...) {
  UseMethod("mtt_tidy")
}

#' @export
mtt_tidy.default <- function(x, ...) {
  cli::cli_abort("No method for class {class(x)}")
}

#' @export
mtt_tidy.gp <- function(x, ...) {
  x <- gplate::gp_serve(x)
  mtt_tidy(x)
}

#' @export
mtt_tidy.data.frame <- function(x, ...) {
  # Allow user to map custom cols?
  # Should coerce cols into their proper types
  # Allow for NULL conditions if included in df?
  if (any(!c("condition", "dose", "nm562", "nm660") %in% names(x))) {
    cli::cli_abort(
      "Data is missing one or more of the following:",
      "`condition`, `dose`, `nm562`, `nm660`"
    )
  }
  x
}

#' @export
mtt_tidy.spectramax <- function(x, conditions, ...) {
  dose <- process_conditions(conditions)

  df <- x$data[[1]]$data |>
    gplate::gp_sec(
      "dose",
      nrow = 4, ncol = 1, labels = dose, wrap = TRUE, advance = FALSE
    ) |>
    gplate::gp_sec(
      "condition", nrow = 4, ncol = 6, labels = names(conditions)
    )

  df <- df |>
    gplate::gp_serve() |>
    dplyr::mutate(dose = as.numeric(levels(.data$dose)[.data$dose])) |>
    rm_unassigned_wells()

  # If a condition is NA, needs to be dropped even after removal of values
  df$condition <- droplevels(df$condition)

  df
}

process_conditions <- function(conditions) {
  validate_condition_names(conditions)
  conditions <- process_condition_values(conditions)
}

# Identical names are ok
validate_condition_names <- function(conditions) {
  if (!is.list(conditions))
    cli::cli_abort("Conditions must be supplied as a named list")

  if (length(conditions) != 4) {
    cli::cli_abort(
      "Length of conditions must be 4",
      "i" = "If some quadrants are blank, use `NA`",
      "i" = "For complex layouts, consider using a `gplate::gp` instead"
    )
  }

  if (is.null(names(conditions))) cli::cli_abort("Conditions must be named")

  # Are there any list items that don't have names and AREN'T NAs?
  na_inds <- which(is.na(names(conditions)))
  # We can assume !is.null(names(conditions)) since we checked for the case of
  # NO names above
  noname_inds <- which(names(conditions) == "")
  # Naming NAs is weird but not technically forbidden
  if (length(setdiff(noname_inds, na_inds) > 0)) {
    "All non-NA conditions must be named"
  }
}

# Test:
# Catches NA inds (even named ones):
# which(is.na(list("a", b = NA, NA, a = "d")))
# Does ok with no NAs:
# list(a = "a", "b", c = "d")
# Deals with duplicate names ok

process_condition_values <- function(conditions) {
  conditions <- lapply(conditions, process_single_cond_vals)
  conditions <- process_vals_by_name(conditions)
  conditions <- unname(unlist(conditions))
  conditions
}

process_single_cond_vals <- function(condition) {
  if (!is.numeric(condition) && !is.na(condition))
    cli::cli_abort("Condition values must be numeric.")

  # This only holds if we assume `conditions` can only apply to quadrants
  if (length(condition) > 6) {
    cli::cli_abort(
      "This condition has more than 6 values, the maximum a quadrant can hold",
      "i" = "For complex layouts, consider using a `gplate::gp` instead"
    )
  }

  length(condition) <- 6
  # Remember: two conditions with the same name but different values might
  # 'merge' - consider conditions that wraps/spans quadrants. Conditions with a
  # single value may be intentional.

  # Because of that, it's probably better to sanitize doses
  # outside of a single condition context
  condition
}

process_vals_by_name <- function(conditions) {
  unique_names <- setdiff(unique(names(conditions)), "")
  for (name in unique_names) {
    inds <- which(names(conditions) == name)
    new_min <- calc_new_min(unlist(conditions[inds]))
    # Need to get 0 indices per name
    # Loop through each similarly named condition
    for (ind in inds) {
      zeros <- which(conditions[[ind]] == 0)
      # Need to check if any exists
      if (length(zeros) > 0) {
        # Replace them with the new min
        conditions[[ind]][zeros] <- new_min
      }
    }
  }
  conditions
}
