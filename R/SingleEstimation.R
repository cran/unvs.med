#' @title Single-time Estimation for Causal Mediation Effects
#'
#' @description
#' This function obtains the estimates of various types of mediation effects
#' for a single time based on one bootstrap sample. This function calculates the effects through
#' different combinations of potential outcomes by the algorithm we proposed.
#'
#' This is an internal function, automatically called by the function \code{\link{BootEstimation_for}} or \code{\link{BootEstimation_MT}}.
#'
#' @usage SingleEstimation (m_model, y_model, data, X, M, Y, m_type, y_type)
#'
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param X a character variable of the exposure's name.
#' @param M a character variable of the mediator's name.
#' @param Y a character variable of the outcome's name.
#' @param m_type a character variable of the mediator's type.
#' @param y_type a character variable of the outcome's type.
#'
#' @returns This function returns a list of three dataframes of mediation effects on
#' risk difference (RD), odds ratio (OR) and risk ratio (RR) scales.
#' Each dataframe has only one single estimate for each effect.
#'
#' @export
#'
SingleEstimation = function(m_model = NULL, y_model = NULL, data=NULL, X = NULL, M = NULL,
                            Y = NULL, m_type = NULL, y_type = NULL)
{#beginning function

# Calculating potential outcomes
if(is.numeric(data[[X]])){
  # Numeric exposure
  Y00=potentialoutcome_numX (xx = 0, xm = 0, data = data, X = X, M = M, Y = Y,
                            m_type = m_type, y_type = y_type, m_model = m_model,
                            y_model = y_model)
  Y01=potentialoutcome_numX (xx = 0, xm = 1, data = data, X = X, M = M, Y = Y,
                            m_type = m_type, y_type = y_type, m_model = m_model,
                            y_model = y_model)
  Y10=potentialoutcome_numX (xx = 1, xm = 0, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)
  Y11=potentialoutcome_numX (xx = 1, xm = 1, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)

} else {

  # Factor exposure
  Y00=potentialoutcome_facX (xx = 0, xm = 0, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)
  Y01=potentialoutcome_facX (xx = 0, xm = 1, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)
  Y10=potentialoutcome_facX (xx = 1, xm = 0, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)
  Y11=potentialoutcome_facX (xx = 1, xm = 1, data = data, X = X, M = M, Y = Y,
                             m_type = m_type, y_type = y_type, m_model = m_model,
                             y_model = y_model)
}

#mediation effects on risk difference (RD) scales
  TE.RD=Y11-Y00
  PNDE.RD=Y10-Y00
  TNDE.RD=Y11-Y01
  PNIE.RD=Y01-Y00
  TNIE.RD=Y11-Y10
  Prop_PNIE.RD=PNIE.RD/TE.RD
  Prop_TNIE.RD=TNIE.RD/TE.RD
  Ave_NDE.RD=(PNDE.RD+TNDE.RD)/2
  Ave_NIE.RD=(PNIE.RD+TNIE.RD)/2
  Ave_Prop_NIE.RD=Ave_NIE.RD/TE.RD

  # Saving result on risk difference (RD) scale
  Single_result.RD=data.frame(
    TE=TE.RD, PNDE=PNDE.RD, TNDE=TNDE.RD, PNIE=PNIE.RD, TNIE=TNIE.RD,
    Prop_PNIE=Prop_PNIE.RD,  Prop_TNIE= Prop_TNIE.RD, Ave_NDE=Ave_NDE.RD, Ave_NIE=Ave_NIE.RD,
    Ave_Prop_NIE=Ave_Prop_NIE.RD
  )

# mediation effects on odds ratio (OR) scales
  TE.OR=(Y11/(1-Y11)) / (Y00/(1-Y00))
  PNDE.OR=(Y10/(1-Y10)) / (Y00/(1-Y00))
  TNDE.OR=(Y11/(1-Y11)) / (Y01/(1-Y01))
  PNIE.OR=(Y01/(1-Y01)) / (Y00/(1-Y00))
  TNIE.OR=(Y11/(1-Y11)) / (Y10/(1-Y10))
  Prop_PNIE.OR=PNIE.OR/TE.OR
  Prop_TNIE.OR=TNIE.OR/TE.OR
  Ave_NDE.OR=(PNDE.OR+TNDE.OR)/2
  Ave_NIE.OR=(PNIE.OR+TNIE.OR)/2
  Ave_Prop_NIE.OR=Ave_NIE.OR/TE.OR

  # Saving result on odds ratio (OR) scale
  Single_result.OR=data.frame(
    TE=TE.OR, PNDE=PNDE.OR, TNDE=TNDE.OR, PNIE=PNIE.OR, TNIE=TNIE.OR,
    Prop_PNIE=Prop_PNIE.OR,  Prop_TNIE= Prop_TNIE.OR, Ave_NDE=Ave_NDE.OR, Ave_NIE=Ave_NIE.OR,
    Ave_Prop_NIE=Ave_Prop_NIE.OR
  )

# mediation effects on risk ratio (RR) scales
  TE.RR= Y11/Y00
  PNDE.RR= Y10/Y00
  TNDE.RR= Y11/Y01
  PNIE.RR= Y01/Y00
  TNIE.RR= Y11/Y10
  Prop_PNIE.RR=PNIE.RR/TE.RR
  Prop_TNIE.RR=TNIE.RR/TE.RR
  Ave_NDE.RR=(PNDE.RR+TNDE.RR)/2
  Ave_NIE.RR=(PNIE.RR+TNIE.RR)/2
  Ave_Prop_NIE.RR=Ave_NIE.RR/TE.RR

  # Saving result on risk ratio (RR) scale
  Single_result.RR=data.frame(
    TE=TE.RR, PNDE=PNDE.RR, TNDE=TNDE.RR, PNIE=PNIE.RR, TNIE=TNIE.RR,
    Prop_PNIE=Prop_PNIE.RR,  Prop_TNIE= Prop_TNIE.RR, Ave_NDE=Ave_NDE.RR, Ave_NIE=Ave_NIE.RR,
    Ave_Prop_NIE=Ave_Prop_NIE.RR
  )

return(list(RD=Single_result.RD, OR=Single_result.OR, RR=Single_result.RR))
} #ending function
