#' @title Identification of Outcome's type
#'
#' @description
#' This function identifies the outcome's type according to the model
#' if the user does not directly specify it.
#' The type of outcome could be \code{'ordinal'} or any other characters, e.g., 'non-ordinal'.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage ident_Y_type (Y, data)
#'
#' @param Y a character variable of the outcome's name.
#' @param data a dataframe used in the analysis.
#'
#' @returns This function returns a character as the outcome's type.
#' @export
#'
ident_Y_type = function(Y = NULL, data = NULL)
{
  # Identifying outcome's type
  if(is.character(data[[Y]])&&length(unique(data[[Y]]))>2){
    y_type="ordinal"
  } else if(is.factor(data[[Y]]) && nlevels(data[[Y]])>2){
    y_type="ordinal"
  } else {y_type="non-ordinal"}
  return(y_type)
}
