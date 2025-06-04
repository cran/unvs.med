#' @title Summary of Formal Estimation for Causal Mediation Effects
#'
#' @description
#' This function presents the refined statistics results of the mediation effects on risk difference (RD),
#' odds ratio (OR) and risk difference (RR) scales
#' from function \code{\link{FormalEstmed}}.
#' This function is applied on the resulting object of class \code{"unvs.med"} from function \code{\link{FormalEstmed}}.
#' The output shows the mean, standard error, t-statistics, p-value and confident interval of the effect estimates based on
#' bootstrapping estimations.
#'
#' @usage
#' \method{summary}{unvs.med} (object, form = "short", scale = "RD", ...)
#'
#' @aliases summary.unvs.med
#'
#' @param object a resulting object of class \code{'unvs.med'}
#' from function \code{\link{FormalEstmed}}.
#' @param form a character variable indicating the output form.
#' It can be \code{"short"} or \code{"long"}. The default is \code{"short"}.
#' @param ...  additional parameters passed to "summary".
#'
#' In addition, The function identifies it as the same string if the first letter is capital or all letters are capital.
#' For example, \code{"short"} equals to \code{"Short"} and \code{"SHORT"}.
#'
#' @param scale a character variable of the effect scales. It can be \code{"RD"}, \code{"OR"} or \code{"RR"}.
#' If \code{scale} equals to string \code{"all"}, \code{"ALL"} or \code{"All"}, then effects on three scales will be displayed.
#' The default is \code{"RD"}.
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
#' # Summary examples
#' summary(r1) # Summary of the default settings (Short form and on RD scales).
#' summary(r1, "long") # Summary of long form and on RD scales.
#' summary(r1, "long", "OR") # Summary of long form and on OR scales.
#' summary(r1, "long", "RR") # Summary of long form and on RR scales.
#' summary(r1, "long", "all") # Summary of long form and on all scales.
#' summary(r1, form="short", scale="all") # Summary of short form and on all scales.
#' }
#'
#' @returns No return value, called for displaying the output of the estimation result.
#'
#' @method summary unvs.med
#' @export
summary.unvs.med = function (object = NULL, form = "short", scale = "RD", ...)
{ # beginning summary function
  # Result forms
  if(form == "short"||form=="Short"||form=="SHORT"){
    ty = 1:5
cat("---------------Short-form results for causal mediation analysis-----------------\n")
cat("(Based on the universal approach)\n")
} else if (form == "long"|| form=="Long"||form=="LONG") {
    ty = 1:10
cat("---------------Long-form results for causal mediation analysis------------------\n")
cat("(Based on the universal approach)\n")
}else{stop("Wrong form specified!")}

  # Results on risk difference (RD) scales
  RD_tab = cbind(
    Estinate = t(object$Stat.RD[1,]),
    Std.Err. = t(object$Stat.RD[2,]),
    t.stat = t(object$Stat.RD[3,]),
    LowerCI = t(object$Stat.RD[5,]),
    UpperCI = t(object$Stat.RD[6,]),
    p.value = t(object$Stat.RD[4,])
  )
  colnames(RD_tab)=c("Estimate", "Std.Err.", "t-stat", "LowerCI", "UpperCI", "p-value")
  RD_tab = RD_tab[ty,]

  # Results on odds ratio (OR) scales
  OR_tab = cbind(
    Estinate = t(object$Stat.OR[1,]),
    Std.Err. = t(object$Stat.OR[2,]),
    t.stat = t(object$Stat.OR[3,]),
    LowerCI = t(object$Stat.OR[5,]),
    UpperCI = t(object$Stat.OR[6,]),
    p.value = t(object$Stat.OR[4,])
  )
  colnames(OR_tab)=c("Estimate", "Std.Err.", "t-stat", "LowerCI", "UpperCI", "p-value")
  OR_tab = OR_tab[ty,]

  # Results on risk ratio (RR) scales
  RR_tab = cbind(
    Estinate = t(object$Stat.RR[1,]),
    Std.Err. = t(object$Stat.RR[2,]),
    t.stat = t(object$Stat.RR[3,]),
    LowerCI = t(object$Stat.RR[5,]),
    UpperCI = t(object$Stat.RR[6,]),
    p.value = t(object$Stat.RR[4,])
  )
  colnames(RR_tab)=c("Estimate", "Std.Err.", "t-stat", "LowerCI", "UpperCI", "p-value")
  RR_tab = RR_tab[ty,]

##############################################################################################

  # Result scales
  if(scale=="RD"||scale=="rd"){
cat("\nResults on RD scales:\n")
    printCoefmat(
      RD_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )

  } else if (scale=="OR"||scale=="or") {
cat("\nResults on OR scales:\n")
    printCoefmat(
      OR_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )

  } else if (scale=="RR"||scale=="rr"){
cat("\nResults on RR scales:\n")
    printCoefmat(
      RR_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )

  } else if (scale=="all"||scale=="All"||scale=="ALL"){
cat("\nResults on RD scales:\n")
    printCoefmat(
      RD_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )

cat("\nResults on OR scales:\n")
    printCoefmat(
      OR_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )
cat("\nResults on RR scales:\n")
    printCoefmat(
      RR_tab,
      digits = getOption("digits"),
      signif.stars = T,
      cs.ind = 1:2,
      has.Pvalue = TRUE
    )
  } else{stop("Wrong scale specified!")}
  cat("\nConfidence interval level:",100*object$Confidence_level,"%\n")
  cat("Bootstrap times:",object$Bootstrap_number,"\n")
  cat("Valid data samples in use:",nrow(object$Data),"\n")
  if(!is.null(object$Covariates_cond)) {cat("Covariates are conditioned on:", paste(object$Covariates_cond, collapse = ", "))}
} # ending summary function
