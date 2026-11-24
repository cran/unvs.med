#' @title Identification of Outcome's type
#'
#' @description
#' This function identifies the outcome's type according to the model
#' if the user does not directly specify it.
#' The type of outcome could be \code{'ordinal'} or any other characters, e.g., 'non-ordinal'.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage ident_Y_type(Y = NULL, data = NULL, model = NULL)
#'
#' @param Y a character variable of the outcome's name.
#' @param data a dataframe used in the analysis.
#' @param model an optional fitted model object. If supplied, the type is identified from the model class first: polr/clm/clmm are treated as ordinal, and a binomial glm is treated as non-ordinal.
#'
#' @returns This function returns a character as the outcome's type.
#' @export
#'
ident_Y_type = function(Y = NULL, data = NULL, model = NULL)
{
  if (is.null(Y) || is.null(data) || is.null(data[[Y]])) {
    stop("Outcome '", Y, "' was not found in the data.", call. = FALSE)
  }

  y <- data[[Y]]
  y <- y[!is.na(y)]

  if (length(y) == 0L) {
    stop("Outcome '", Y, "' contains only missing values.", call. = FALSE)
  }

  # Model class takes priority when available
  if (!is.null(model)) {
    if (inherits(model, c("polr", "clm", "clmm"))) return("ordinal")
    if (inherits(model, "glm") &&
        model$family$family %in% c("binomial", "quasibinomial")) return("non-ordinal")
  }

  if (is.character(y) || is.factor(y)) {
    k <- nlevels(factor(y))
    if (k > 2L) return("ordinal")
    if (k == 2L) return("non-ordinal")
    stop("Outcome '", Y, "' has fewer than two observed levels.", call. = FALSE)
  }

  "non-ordinal"
}
