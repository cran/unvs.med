#' @title Identification of Mediator's type
#'
#' @description
#' This function identifies the mediator's type according to the model
#' if the user does not directly specify it.
#' The type of mediator could be \code{'continuous'}, \code{'binary'} or \code{'ordinal'}.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage ident_M_type(M = NULL, data = NULL, model = NULL)
#'
#' @param M a character variable of the mediator's name.
#' @param data a dataframe used in the analysis.
#' @param model an optional fitted model object. If supplied, the type is identified from the model class first: polr/clm/clmm are treated as ordinal, and a binomial glm is treated as binary.
#'
#' @returns This function returns a character as the mediator's type.
#' @export
#'
ident_M_type = function(M = NULL, data = NULL, model = NULL)
{
  if (is.null(M) || is.null(data) || is.null(data[[M]])) {
    stop("Mediator '", M, "' was not found in the data.", call. = FALSE)
  }

  x <- data[[M]]
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    stop("Mediator '", M, "' contains only missing values.", call. = FALSE)
  }

  # Model class takes priority when available
  if (!is.null(model)) {
    if (inherits(model, c("polr", "clm", "clmm"))) return("ordinal")
    if (inherits(model, "glm") &&
        model$family$family %in% c("binomial", "quasibinomial")) return("binary")
  }

  if (is.logical(x)) return("binary")

  if (is.character(x) || is.factor(x)) {
    k <- nlevels(factor(x))
    if (k == 2L) return("binary")
    if (k > 2L) return("ordinal")
    stop("Mediator '", M, "' has fewer than two observed levels.", call. = FALSE)
  }

  if (is.numeric(x)) {
    ux <- unique(x)
    if (length(ux) < 2L) {
      stop("Mediator '", M, "' has fewer than two distinct values.", call. = FALSE)
    }
    if (length(ux) == 2L && all(ux %in% c(0, 1))) return("binary")
    return("continuous")
  }

  stop("Unsupported mediator type: ", class(x)[1], call. = FALSE)
}
