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
  condition = paste(cov_val, collapse = " & ")
  data = subset(data, eval(parse(text = condition)))

  # Extracting covariates names
  eq_conditions = grep("==", cov_val, value = TRUE)
  cond_cov_names = sub("==.*", "", eq_conditions)

  # Updating mediator model
  new_rhs_m <- deparse(formula(m_model)[[3]])
  for (var in cond_cov_names) {
    new_rhs_m <- gsub(paste0("\\s?\\+?\\s?", var, "\\s?\\+?\\s?"), "+", new_rhs_m)
  }
  new_rhs_m <- gsub("\\*(?=\\+)|\\+(?=\\*)", "", new_rhs_m, perl = TRUE)
  new_formula_m <- reformulate(new_rhs_m, response = formula(m_model)[[2]])
  m_model2 = update(m_model, formula = new_formula_m, data = data)

  # Updating outcome model
  new_rhs_y<- deparse(formula(y_model)[[3]])
  for (var in cond_cov_names) {
    new_rhs_y <- gsub(paste0("\\s?\\+?\\s?", var, "\\s?\\+?\\s?"), "+", new_rhs_y)
  }
  new_rhs_y <- gsub("\\*(?=\\+)|\\+(?=\\*)", "", new_rhs_y, perl = TRUE)
  new_formula_y <- reformulate(new_rhs_y, response = formula(y_model)[[2]])
  y_model2 = update(y_model, formula = new_formula_y, data = data)

  # Returning results
  return(list(data=data, m_model=m_model2, y_model=y_model2))
} #Ending function
