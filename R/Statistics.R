#' @title Statistics of Estimated Value of Causal Mediation Effects
#'
#' @description
#' This function calculates the statistics of mediation effects
#' risk difference (RD), odds ratio (OR) and risk ratio (RR) scales based on the bootstrapping results. The statistics include
#' the mean value, standard error, t-statistics, p-value and confident interval.
#' The way to realize bootstrapping estimations is also specified in this function,
#' either through the ordinal \code{for} loop, or the multi-threading process.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @details
#' This function also detects the treatment group and control group when the exposure
#' is not a continuous variable. The function deals with o-1 exposure, character and factor exposure and then
#' displays the grouping information for users.
#'
#' @usage Statistics (m_model, y_model, data, X, exp0=NULL, exp1=NULL, M, Y,
#' m_type, y_type, boot_num = 100, MT = TRUE, Cf_lv = 0.95)
#'
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param X a character variable of the exposure's name.
#' @param exp0 a numeric variable of the baseline level of the exposure.
#' @param exp1 a numeric variable of the new level of the exposure.
#' @param M a character variable of the mediator's name.
#' @param Y a character variable of the outcome's name.
#' @param m_type a character variable of the mediator's type.
#' @param y_type a character variable of the outcome's type.
#' @param boot_num the times of bootstrapping in the analysis. The default is 100.
#' @param MT a logical value indicating whether the multi-threading process is activated. If TURE, activating max-1 cores.
#'        If FALSE, use the ordinary 'for' loop. The default is \code{TRUE}.
#' @param Cf_lv a numeric variable of the confidence interval. The value is presented in decimal form, not percentage form.
#'        The default is 0.95.
#'
#' @returns This function returns a list of three dataframes, i.e., statistics of the mediation effects risk difference (RD), odds ratio (OR) and risk ratio (RR) scales respectively.
#' The statistics include the mean value, standard error, t-statistics, p-value and confident interval based on the bootstrapping estimations.
#'
#' @export
#'
Statistics = function (m_model = NULL, y_model = NULL, data = NULL, X = NULL, exp0 = NULL, exp1 = NULL,
                       M = NULL, Y = NULL, m_type = NULL, y_type = NULL, boot_num = 100, MT = TRUE, Cf_lv = 0.95)
{ # beginning

  # Identifying factor X
  if(is.factor(data[[X]]) && nlevels(data[[X]])==2){
    message("Binary factor exposure detected:")
    message("1 (treatment group) stands for exposure: ",levels(as.factor(data[[X]]))[2])
    message("0 (control group) stands for exposure: ",levels(as.factor(data[[X]]))[1])
  }

  # Identifying character X
  if(is.character(data[[X]]) && length(unique(data[[X]]))==2){
    message("Binary character exposure detected:")
    message("1 (treatment group) stands for exposure: ",levels(as.factor(data[[X]]))[2])
    message("0 (control group) stands for exposure: ",levels(as.factor(data[[X]]))[1])
  }

  # Numeric X but only having 0 and 1 value
  if (length(unique(data[[X]])) <= 2L && all(unique(data[[X]]) %in% c(0L, 1L)))
  {message("Numeric exposure is detected as binary variable")}

  # Obtaining bootstrap results

  if (MT == T)
  {message("\nMulti-threading process activated: ",parallel::detectCores() - 1," cores in use")
    suppressMessages({
    Boot_result = BootEstimation_MT(m_model = m_model, y_model = y_model, data = data,
                                    X = X, exp0=exp0, exp1=exp1, M = M, Y = Y, m_type = m_type, y_type = y_type,
                                    boot_num = boot_num)
    })
  }else{
    Boot_result = BootEstimation_for(m_model = m_model, y_model = y_model, data = data,
                                     X = X, exp0=exp0, exp1=exp1, M = M, Y = Y, m_type = m_type, y_type = y_type,
                                     boot_num = boot_num)}


  # Beginning statstics
  row_names = c("Estimate", "Std.Err.", "z-stat", "p-value", "LowerCI", "UpperCI") # Statistics name
  alpha=1-Cf_lv # confidence level

  # ============================================================
  # Point estimate on the original sample (computed once)
  # ============================================================
  Origin_result <- SingleEstimation(
    m_model = m_model, y_model = y_model, data = data,
    X = X, exp0 = exp0, exp1 = exp1, M = M, Y = Y,
    m_type = m_type, y_type = y_type
  )

  row_names <- c("Estimate", "Std.Err.", "z-stat", "p-value", "LowerCI", "UpperCI")
  alpha <- 1 - Cf_lv

  # ------------------------------------------------------------
  # Risk difference (RD) scale
  # ------------------------------------------------------------
  Stat.RD <- data.frame(
    TE = numeric(), PNDE = numeric(), TNDE = numeric(), PNIE = numeric(),
    TNIE = numeric(), Prop_PNIE = numeric(), Prop_TNIE = numeric(),
    Ave_NDE = numeric(), Ave_NIE = numeric(), Ave_Prop_NIE = numeric()
  )

  Stat.RD[1, ] <- as.numeric(Origin_result$RD)                  # original-sample estimate
  Stat.RD[2, ] <- apply(Boot_result$RD, 2, sd, na.rm = TRUE)    # bootstrap SE
  Stat.RD[3, ] <- Stat.RD[1, ] / Stat.RD[2, ]                   # z-statistic
  Stat.RD[4, ] <- 2 * (1 - pnorm(abs(as.numeric(Stat.RD[3, ]))))# two-sided p-value
  Stat.RD[5, ] <- apply(Boot_result$RD, 2, quantile,
                        probs = alpha / 2, na.rm = TRUE)        # lower CI
  Stat.RD[6, ] <- apply(Boot_result$RD, 2, quantile,
                        probs = 1 - alpha / 2, na.rm = TRUE)    # upper CI
  row.names(Stat.RD) <- row_names

  # ------------------------------------------------------------
  # Odds ratio (OR) scale
  # ------------------------------------------------------------
  Stat.OR <- data.frame(
    TE = numeric(), PNDE = numeric(), TNDE = numeric(), PNIE = numeric(),
    TNIE = numeric(), Prop_PNIE = numeric(), Prop_TNIE = numeric(),
    Ave_NDE = numeric(), Ave_NIE = numeric(), Ave_Prop_NIE = numeric()
  )

  Stat.OR[1, ] <- as.numeric(Origin_result$OR)                  # original-sample estimate
  Stat.OR[2, ] <- apply(Boot_result$OR, 2, sd, na.rm = TRUE)    # bootstrap SE
  Stat.OR[3, ] <- Stat.OR[1, ] / Stat.OR[2, ]                   # z-statistic
  Stat.OR[4, ] <- 2 * (1 - pnorm(abs(as.numeric(Stat.OR[3, ]))))# two-sided p-value
  Stat.OR[5, ] <- apply(Boot_result$OR, 2, quantile,
                        probs = alpha / 2, na.rm = TRUE)        # lower CI
  Stat.OR[6, ] <- apply(Boot_result$OR, 2, quantile,
                        probs = 1 - alpha / 2, na.rm = TRUE)    # upper CI
  row.names(Stat.OR) <- row_names

  # ------------------------------------------------------------
  # Risk ratio (RR) scale
  # ------------------------------------------------------------
  Stat.RR <- data.frame(
    TE = numeric(), PNDE = numeric(), TNDE = numeric(), PNIE = numeric(),
    TNIE = numeric(), Prop_PNIE = numeric(), Prop_TNIE = numeric(),
    Ave_NDE = numeric(), Ave_NIE = numeric(), Ave_Prop_NIE = numeric()
  )

  Stat.RR[1, ] <- as.numeric(Origin_result$RR)                  # original-sample estimate
  Stat.RR[2, ] <- apply(Boot_result$RR, 2, sd, na.rm = TRUE)    # bootstrap SE
  Stat.RR[3, ] <- Stat.RR[1, ] / Stat.RR[2, ]                   # z-statistic
  Stat.RR[4, ] <- 2 * (1 - pnorm(abs(as.numeric(Stat.RR[3, ]))))# two-sided p-value
  Stat.RR[5, ] <- apply(Boot_result$RR, 2, quantile,
                        probs = alpha / 2, na.rm = TRUE)        # lower CI
  Stat.RR[6, ] <- apply(Boot_result$RR, 2, quantile,
                        probs = 1 - alpha / 2, na.rm = TRUE)    # upper CI
  row.names(Stat.RR) <- row_names

return(list(Stat.RD=Stat.RD, Stat.OR=Stat.OR, Stat.RR=Stat.RR, Boot_result=Boot_result))
} # ending
