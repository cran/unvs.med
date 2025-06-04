#' @title Test of Mediation Effects Within One Single Object
#'
#' @description
#' This function tests the difference of various types of effects within
#' one single estimation object from function \code{\link{FormalEstmed}}. It
#' is used to compare whether an effect is distinct from another.
#'
#' @usage
#' um.test1 (obj, scale = "RD",type = c("PNDE", "TNIE"), Cf_lv=0.95, verbose=TRUE)
#'
#' @param obj a resulting object of class \code{"unvs.med"}
#' from function \code{\link{FormalEstmed}}.
#' @param scale a character variable of the effect scales. It can be \code{"RD"}, \code{"OR"} or \code{"RR"}.
#' The default is \code{"RD"}.
#' @param type a character variable of the effect types, containing more than two.
#' The default is \code{c("PNDE", "TNIE")}.
#' @param Cf_lv a numeric variable of the confident interval.
#' The default is 0.95.
#' @param verbose a logical value indicating whether the output is display. The default is TRUE.
#' This is a standard parameter required by CRAN.
#'
#' @returns No return value, called for displaying the output of the test result.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Running formal estimation
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r1 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp") # Running formal estimation
#'
#' # Test examples
#' um.test1(r1) # Test of the default settings (PNDE v.s. TNIE on RD scales).
#' um.test1(r1,"OR") # Test of PNDE v.s. TNIE on OR scales.
#' # Test of PNDE v.s. TNIE on RD and OR scales:
#' um.test1(r1,c("RD", "OR"))
#' # Test of PNDE v.s. TNIE v.s. TE on RD and OR scales:
#' um.test1(r1,c("RD", "OR"), c("PNDE", "TNIE", "TE"))
#' # Test of PNDE v.s. TNIE v.s. TE on RD, OR and RR scales:
#' um.test1(r1, scale=c("RD", "OR", "RR"), type=c("PNDE", "TNIE", "TE"))
#'}
um.test1 = function(obj = NULL, scale = "RD",type = c("PNDE", "TNIE"),
                          Cf_lv=0.95, verbose=TRUE) { # Beginning function
  lci = (1-Cf_lv)/2
  uci = 1-lci

  # Combinition of type
  comb = combn(type,2)

for (var_scale in scale)
{ # Beginning scale for
  for (i in 1:ncol(comb))
  { # Beginning comb for
    # Obtaining Bootstrap samples
    samples1 = obj$Boot_result[[var_scale]][[comb[1,i]]]
    samples2 = obj$Boot_result[[var_scale]][[comb[2,i]]]

    diff_samples = samples1 - samples2 # Difference
    # Statistics of the difference
    diff_mean = mean(diff_samples)
    diff_ci = quantile(diff_samples, c(lci, uci))  # CI
    diff_p = 2 * min(mean(diff_samples > 0), mean(diff_samples < 0))  # p.value

    # Output
    if(verbose){ # Ending verbose if
    cat("\n-----Test of mediation effects based on the universal approach estimation-----\n")
    cat("(Test within one single estimation)\n")
    cat("\nEffect scale: ",var_scale,"\n")
    cat("Effect type: ",comb[1,i],"v.s.", comb[2,i],"\n")
    cat("\n(Alternative hypothesis: The difference between the two effects is not equal to 0)\n")
    cat(sprintf("The difference is %.4f (p-value = %.2f)\n", diff_mean, diff_p))
    cat(100*Cf_lv, "% confidence interval of the difference:\n")
    cat(diff_ci)
    } # Ending verbose if

  } # Ending comb for
} # Ending scale for
}#Ending function
