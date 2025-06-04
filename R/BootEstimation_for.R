#' @title Bootstrapping Estimation for Causal Mediation Effects via
#' Ordinary "for" Loop
#'
#' @description
#' This function obtains the estimates of mediation effects by the ordinary \code{for} loop.
#' Through bootstrap sampling and repeating the algorithm of function \code{\link{SingleEstimation}},
#' This function obtains a number of estimates for each type of effect.
#'
#' This is an internal function, automatically called by the function \code{\link{Statistics}}.
#'
#' @details
#' This function is realized by the ordinary \code{for} loop, therefore may take longer time to proceed.
#' For small amounts of data, e.g., dozens to a hundred samples, with relatively simple models,
#' \code{for} loop is recommended.
#'
#' @usage BootEstimation_for (m_model, y_model, data, X, M, Y,
#' m_type, y_type, boot_num = 100)
#'
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param X a character variable of the exposure's name.
#' @param M a character variable of the mediator's name.
#' @param Y a character variable of the outcome's name.
#' @param m_type a character variable of the mediator's type.
#' @param y_type a character variable of the outcome's type.
#' @param boot_num the times of bootstrapping in the analysis. The default is 100.
#'
#' @returns This function returns a list of three dataframes, i.e.,
#' the bootstrapping results of the mediation effects.
#' This list is also saved in the return of the main function \code{\link{FormalEstmed}}.
#'
#' @export
#'
BootEstimation_for = function(m_model = NULL, y_model = NULL, data = NULL,
                              X = NULL, M = NULL, Y = NULL, m_type = NULL,
                              y_type = NULL, boot_num = 100)
{#beginning function
    # Preparing to save bootstrap result
    Boot_result.RD=data.frame(TE=numeric(), PNDE=numeric(), TNDE=numeric(), PNIE=numeric(),
                              TNIE=numeric(), Prop_PNIE=numeric(), Prop_TNIE=numeric(),
                              Ave_NDE=numeric(), Ave_NIE=numeric(), Ave_Prop_NIE=numeric())
    Boot_result.OR=data.frame(TE=numeric(), PNDE=numeric(), TNDE=numeric(), PNIE=numeric(),
                              TNIE=numeric(), Prop_PNIE=numeric(), Prop_TNIE=numeric(),
                              Ave_NDE=numeric(), Ave_NIE=numeric(), Ave_Prop_NIE=numeric())
    Boot_result.RR=data.frame(TE=numeric(), PNDE=numeric(), TNDE=numeric(), PNIE=numeric(),
                              TNIE=numeric(), Prop_PNIE=numeric(), Prop_TNIE=numeric(),
                              Ave_NDE=numeric(), Ave_NIE=numeric(), Ave_Prop_NIE=numeric())
    # Bootstrapping
    for (BT in 1:boot_num)
    { # beginning for
      #Bootstrap sampling
      boot_indices = sample(nrow(data), size=nrow(data), replace = T)
      data_boot = data[boot_indices, ,drop = F]

      # Updating model's data
      m_model_new = update(m_model, data = data_boot)
      y_model_new = update(y_model, data = data_boot)

      # Run single estimation
      Single_result = SingleEstimation (data=data_boot, X = X, M = M, Y = Y, m_type = m_type,
                        y_type = y_type, m_model = m_model_new, y_model = y_model_new)

      # Saving results for every single time
      Boot_result.RD[BT,]=Single_result$RD
      Boot_result.OR[BT,]=Single_result$OR
      Boot_result.RR[BT,]=Single_result$RR

    } # ending for

    return(list(RD=Boot_result.RD, OR=Boot_result.OR, RR=Boot_result.RR))
  } #ending function
