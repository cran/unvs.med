#' @title Modifications of Data and Models for Moderated Mediation
#'
#' @description
#' This function modifies the mediator and outcome model and the dataset according
#' to the conditions of certain covariates specified by the user.
#' The conditions are constructed by the character parameter \code{cov_val} with multiple string elements.
#' This function then modifies the data and models based on rules of conditional regressions.
#' The amount of sample and model structure are changed correspondingly. The samples and model variables satisfy the conditions
#' are finally remain. Therefore, This function is only involved when moderated mediation effects are considered in the analysis.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage cond_cov (m_model, y_model, data, X, M, cov_val)
#'
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param X a character variable of the exposure's name.
#' @param M a character variable of the mediator's name.
#' @param cov_val a character variable of the conditions of the covariates. Each string (element) in the character may include ==, <, >, <=, >=, etc.
#'
#' @returns This function returns a list of three objects. A conditional dataframe, an updated mediator model and an updated outcome model.
#' @export
#'
cond_cov = function(m_model = NULL, y_model = NULL, data = NULL, X = NULL, M = NULL,
                    cov_val = NULL)
{ #Beginning function
  # Conditional data
  condition <- paste(cov_val, collapse = ' & ')
  data <- subset(data, eval(parse(text = condition), envir = data))
  if (nrow(data) == 0L) stop('No observations satisfy cov_val.')

  eq_conditions <- grep('==', cov_val, value = TRUE)
  cond_vars <- trimws(sub('==.*', '', eq_conditions))

  drop_terms_by_name <- function(fml, vars) {
    tt <- terms(fml)
    tl <- attr(tt, 'term.labels')
    hit <- vapply(tl, function(term) {
      any(all.vars(as.formula(paste('~', term))) %in% vars)
    }, logical(1))
    if (all(hit)) {
      reformulate('1', response = deparse(formula(fml)[[2]]))
    } else {
      formula(drop.terms(tt, which(hit), keep.response = TRUE))
    }
  }

  m_model2 <- update(m_model,
                     formula = drop_terms_by_name(formula(m_model), cond_vars), data = data)
  y_model2 <- update(y_model,
                     formula = drop_terms_by_name(formula(y_model), cond_vars), data = data)

  # Returning results
  return(list(data=data, m_model=m_model2, y_model=y_model2))
} #Ending function
