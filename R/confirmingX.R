#' @title Confirmation of the Exposure
#'
#' @description
#' This function checks the specification of the exposure in function \code{\link{FormalEstmed}}
#' specified by users.
#' The name of exposure specified in function \code{\link{FormalEstmed}} should be a character.
#' The exposure should be a numeric and binary factor, otherwise this function warns.
#'
#' This is an internal function, automatically called by the function \code{\link{FormalEstmed}}.
#'
#' @usage confirmingX (X)
#'
#' @param X a character variable of the exposure's name.
#'
#' @returns No return value, called for checking the exposure's name.
#'
#' @export
#'
confirmingX = function(X = NULL)
{
# confirming exposure X
  if(is.null(X)){
   warnings("The expsure's name is NULL!")
    stop("Please specify an exposure (treament variable), otherwise this function cannot proceed!")
  }
  if(!is.character(X)){
    warnings("The expsure's name is not chr type!")
    stop("The exposure's name needs to be specified as chr type, otherwise this function cannot proceed!")
  }
}
