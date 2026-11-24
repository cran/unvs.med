#' @title Estimation of Potential Outcomes Based on the Universal Approach (for Numeric Exposure)
#'
#' @description
#' This function realizes the main algorithm of the universal approach to
#' estimate potential outcomes with observed data. Different potential outcomes
#' can be estimated by different combinations of the input parameters \code{xx} and \code{xm}.
#'
#' This is an internal function, automatically called by the function \code{\link{SingleEstimation}}.
#'
#' @details
#' This function is called when the exposure is a numeric variable.
#' Especially, a numeric exposure would be identified as a binary variable
#' when it has only two values: 0 and 1. However, if the only two values are not 0 and 1,
#' then users should specify it as a factor variable in advance so that function \code{\link{potentialoutcome_facX}}
#' would be called automatically instead. The function still works well if the exposure is a multi-level discrete variable.
#'
#' @usage potentialoutcome_numX (xx, xm, data, X, exp0=NULL, exp1=NULL, M, Y,
#' m_type, y_type, m_model, y_model)
#'
#' @param xx a counterfactual value for exposure, directly affecting the outcome. Equals 1 in the treatment group,
#' equals 0 in the control group.
#' @param xm a counterfactual value for exposure, directly affecting the mediator. Equals 1 in the treatment group,
#' equals 0 in the control group.
#' @param data a dataframe used for the above models in the mediation analysis.
#' @param X a character variable of the exposure's name.
#' @param exp0 a numeric variable of the baseline level of the exposure.
#' @param exp1 a numeric variable of the new level of the exposure.
#' @param M a character variable of the mediator's name.
#' @param Y a character variable of the outcome's name.
#' @param m_type a character variable of the mediator's type.
#' @param y_type a character variable of the outcome's type.
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#'
#' @returns This function returns a value of the potential outcome.
#'
#' @export
#'
potentialoutcome_numX = function(xx = NULL, xm = NULL, data = NULL, X = NULL, exp0 = NULL, exp1 = NULL,
                                 M = NULL, Y = NULL, m_type = NULL, y_type = NULL,
                                 m_model = NULL, y_model = NULL)
{ #beginning main function

#dealing with binary exposure X
  # Constant exposure (Special cases)
if (length(unique(data[[X]])) == 1L)
   {stop("The exposure should not be a constant")}
  # 0-1 binary exposure
if (length(unique(data[[X]])) == 2L && all(unique(data[[X]]) %in% c(0L, 1L)))
  {data[[X]][] = 0L}
  # Binary exposure beyond zero and one value
if (length(unique(data[[X]])) == 2L && !all(unique(data[[X]]) %in% c(0L, 1L))) {
    stop('The exposure has two non-0/1 values. Convert it to a factor (e.g., factor(X)) before fitting the models,
         so that the two levels are treated as a binary exposure.')
  }

# Only exp0 specified
if(!is.null(exp0) && is.null(exp1))
  {data[[X]][] = exp0}

# exp0 and exp1 specified
if(!is.null(exp0) && !is.null(exp1))
{data[[X]][] = exp0
  if(xm==1){xm = exp1-exp0}
  if(xx==1){xx = exp1-exp0}
}

setDT(data)  # Targeting data
# Constructing data_m (X in potential vaule M)
data_m = data[, .SD, .SDcols = setdiff(names(data), c(X, M, Y))]
data_m[, (X) := data[[X]] + xm]

# Forordinal model detecting
predict_probs <- function(model, newdata) {
  type <- if (inherits(model, c('clm', 'clmm'))) 'prob' else 'probs'
  p <- predict(model, newdata = newdata, type = type)
  if (is.list(p) && !is.null(p$fit)) p <- p$fit
  p
}

###### Continuous mediator ############################################################
  if(m_type == "continuous")
  { #beginning if for continuous M

  # Predicting the distribution of M
  model_sigma = sigma(m_model) # Model's standard error
  pred_m = predict(m_model, newdata=data_m, type = "response",se.fit = T)
  mu_m = pred_m$fit

  # Producing distribution M through Monte Carlo simulation
  MC = 100L  # Number of draw in Monte Carlo simulation
  n = as.integer(nrow(data)) # sample's row

  m_samples = matrix(rnorm(n * MC, mean = mu_m, sd = model_sigma), ncol = MC)

  # Constructing data_y (X in potential value Y)
  data_y = data[rep(1:.N, each = MC), !c(X, M, Y), with = FALSE]
  data_y[, (X) := rep(data[[X]] + xx, each = MC)]
  data_y[, (M) := as.vector(t(m_samples))]

  # Predicting Y through matrix calculations
  if (y_type == "ordinal") {
    y_levels = seq_along(levels(factor(data[[Y]])))
    mu_y <- predict_probs(y_model, data_y)
    ey_matrix = matrix(tcrossprod(mu_y, matrix(y_levels, nrow = 1)), ncol = MC, nrow = n, byrow = T)
  } else {
    mu_y = predict(y_model, newdata = data_y, type = "response")
    ey_matrix = matrix(mu_y, ncol = MC, nrow = n, byrow = T)
  }

  EY = rowMeans(ey_matrix)
  ptocY_result = mean(EY)

 #ending if for continuous M

###### Binary mediator ################################################################
  }else if(m_type == "binary")
  { #beginning if

  # P(M = 1 | X, W)
  p_m = predict(m_model, newdata = data_m, type = "response")

  n = as.integer(nrow(data)) # sample's row

  # Constructing data_y (X in potential value Y)
  data_y = data[rep(1:n, 2), !c(Y), with = FALSE]  # Copying every row twice (representing for M=1 and M=0)
  # data_y[, (X) := rep(data[[X]], times = 2) + xx]
  data_temp=rep(data[[X]], times = 2) + xx
  data_y[, (X) := data_temp]

  # Modifying M in data_y
  if (is.factor(data[[M]])) {
    m_levels = levels(data[[M]])
    data_y[, (M) := rep(m_levels[c(2,1)], each = n)]
  } else { data_y[, (M) := rep(c(1L, 0L), each = n)]}

  # Estimation Y
  if (y_type == "ordinal") {
    y_levels = seq_along(levels(factor(data[[Y]])))
    mu_y <- predict_probs(y_model, data_y)
    ey_values = as.vector(mu_y %*% y_levels)
  } else {
    ey_values = predict(y_model, newdata = data_y, type = "response")
  }

  E1 = ey_values[1:n]             # First n rows are the results when M=1
  E0 = ey_values[(n+1):(2*n)]     # Last n rows are the results when M=0
  EY = p_m * E1 + (1 - p_m) * E0  # Final expectation

  ptocY_result = mean(EY) #final potential value result

   #ending if for binary M

###### Ordinal mediator ###############################################################
  }else if(m_type == "ordinal")
  { #beginning if

  # Calculating M's levels
  m_levels = levels(factor(data[[M]]))
  num_m_levels = length(m_levels)

  n = as.integer(nrow(data)) # sample's row

  p_m <- predict_probs(m_model, data_m) # Prediction of M

  # Constructing data_y (X in potential value Y)
  data_y = data[rep(1:n, each = num_m_levels), !c(Y), with = FALSE]
  expanded_X = rep(data[[X]], each = num_m_levels) + xx
  data_y[, (X) := expanded_X]

  # Modifying M in data_y
  if (is.factor(data[[M]])) {
    data_y[, (M) := rep(m_levels, times = n)]
  } else {
    data_y[, (M) := rep(as.numeric(m_levels), times = n)]
  }

  # Estimation Y
  if (y_type == "ordinal") {
    y_levels = seq_along(levels(factor(data[[Y]])))
    mu_y <- predict_probs(y_model, data_y)
    ey_values = as.vector(mu_y %*% y_levels)
  } else {
    ey_values = predict(y_model, newdata = data_y, type = "response")
  }

  # Calculating final expectation
  ey_matrix = matrix(ey_values, ncol = num_m_levels, byrow = TRUE)
  EY = rowSums(ey_matrix * p_m)

  ptocY_result = mean(EY) #final potential value result

  } else {stop("Wrong mediator type!")}#ending if

###### Outputs and returns ############################################################
return(ptocY_result) #returning final potential outcome of nested counterfactuals
} #ending main function
