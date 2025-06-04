#' @title Identification of Mediator's type
#'
#' @description
#' This function identifies the mediator's type according to the model
#' if the user does not directly specify it.
#' The type of mediator could be \code{'continuous'}, \code{'binary'} or \code{'ordinal'}.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage ident_M_type (M, data)
#'
#' @param M a character variable of the mediator's name.
#' @param data a dataframe used in the analysis.
#'
#' @returns This function returns a character as the mediator's type.
#' @export
#'
ident_M_type = function(M = NULL, data = NULL)
{
  # Identifying mediator's type
    if(all(data[[M]] %in% c(0,1))){
      m_type="binary"
    } else if(is.character(data[[M]])&&length(unique(data[[M]]))==2){
      m_type="binary"
    } else if(is.character(data[[M]])&&length(unique(data[[M]]))>2){
      m_type="ordinal"
    } else if(is.factor(data[[M]]) && nlevels(data[[M]])==2){
      m_type="binary"
    } else if(is.factor(data[[M]]) && nlevels(data[[M]])>2){
      m_type="ordinal"
    } else {m_type="continuous"}

  return(m_type)
}
