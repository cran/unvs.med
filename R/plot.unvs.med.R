#' @title Visualization of Mediation Effects
#'
#' @description
#' This function is applied to the resulting object from function \code{\link{FormalEstmed}}
#' to make the plot of the mediation effects. The plot is shown in boxplot.
#'
#' @usage
#' \method{plot}{unvs.med} (x, scale = "RD", type = c("PNDE", "TNIE"), ...)
#'
#' @aliases plot.unvs.med
#'
#' @param x a resulting object of class \code{"unvs.med"}
#' from function \code{\link{FormalEstmed}}.
#' @param scale a character variable of the effect scales. It may include more than one element.
#' The default is \code{"RD"}.
#' @param type a character variable of the effect types. The character can contain any types.
#' The default is \code{c("PNDE", "TNIE")}.
#' @param ...  additional parameters passed to "plot".
#'
#' @details
#' (1) For instance, for parameter \code{scale}, users can specify it as \code{"RD"}, \code{c("RD")},
#' \code{c("RD", "OR")}, \code{c("RD", "OR", "RR")}, etc. regardless of the order and quantity.
#' Besides, if \code{scale} equals to \code{"all"}, \code{"ALL"} or \code{"All"}, then effects on three scales will be output
#' (2) For \code{type}, the element is arbitrary, it may include any types of effects users are interested.
#'
#' @examples
#' \donttest{
#' # Running formal estimation
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r1 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp") # Running formal estimation via bootstrapping
#'
#' # Plot examples
#' plot(r1) # Plot of the default settings (plot PNED and TNDE on RD scales).
#' plot(r1,"OR") # Plot PNDE and TNIE on OR scales.
#' plot(r1,c("RD", "OR")) # Plot PNED and TNDE on RD and OR scales.
#' # Plot five types of the effects on RD and OR scales:
#' plot(r1,c("RD", "OR"), c("TE","PNDE", "TNDE", "PNIE", "TNIE"))
#' plot(r1,c("RD", "RR"), "all") # Plot all the effects on RD and RR scales.
#' plot(r1, scale=c("RD", "OR", "RR"), type="all") # Plot all the effects on all scales.
#' }
#'
#' @returns No return value, called for making a plot.
#'
#' @method plot unvs.med
#' @export
plot.unvs.med = function (x = NULL, scale = "RD", type = c("PNDE", "TNIE"), ...)
{ # beginning summary function
op = par(no.readonly = TRUE)
on.exit(par(op))

if (length(scale) == 1L && toupper(scale) == 'ALL') scale <- c('RD', 'OR', 'RR')
scale <- toupper(scale)
if (any(!scale %in% c('RD', 'OR', 'RR'))) stop('scale must be RD, OR, RR, or all.')

par(mfrow = c(1,length(scale)))

if(all(type == "all") || all(type == "All") || all(type == "ALL"))
{type = colnames(x$Boot_result$RD)}

for (var in scale){
  boxplot(subset(x$Boot_result[[var]], select = type),
          main = paste("Mediation effects on", var, "scales"),
          xlab = "Effect type",
          ylab = "Estimated value"
          )
}
} # ending summary function
