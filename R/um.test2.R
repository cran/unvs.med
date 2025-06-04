#' @title Test of Mediation Effects Between Two Objects
#'
#' @description
#' This function tests the difference of various types of effects between
#' two estimation objects from function \code{\link{FormalEstmed}}. It
#' is used to compare whether an effect is distinct from another between two
#' different estimation settings.
#'
#' @usage
#' um.test2 (obj1, obj2, scale = "RD", type = c("PNDE", "TNIE"), Cf_lv=0.95, verbose=TRUE)
#'
#' @param obj1 a resulting object of class \code{"unvs.med"}
#' from function \code{\link{FormalEstmed}}.
#' @param obj2 a resulting object of class \code{"unvs.med"}
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
#' med_model=glm(med~exp+exp*C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+exp*C1+C2+C3, data=testdata) # Fitting outcome's model
#' r1 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", cov_val="C1==1") # Conditioning on C1=1
#' r0 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", cov_val="C1==0") # Conditioning on C1=0
#'
#' # Test examples
#' um.test2(r1,r0) # Test of the default settings (PNDE v.s. TNIE on RD scales).
#' um.test2(r1,r0, c("OR", "RR")) # Test of PNDE v.s. TNIE on OR and RR scales.
#' # Test of PNDE v.s. TNIE v.s. TE on OR and RR scales:
#' um.test2(r1,r0, c("OR", "RR"), c("PNDE", "TNIE", "TE"))
#' # Test of PNDE v.s. TNIE v.s. TE on OR and RR scales with 90% CI:
#' um.test2(r1,r0, c("OR", "RR"), c("PNDE", "TNIE", "TE"), Cf_lv=0.9)
#' }
um.test2 = function(obj1 = NULL, obj2 = NULL, scale = "RD",
                    type = c("PNDE", "TNIE"), Cf_lv=0.95, verbose=TRUE){ # Beginning function
  lci = (1-Cf_lv)/2
  uci = 1-lci

for (var_scale in scale){ # Beginning scale for
  for (var_type in type)
  {# Beginning type for

  # Obtaining Bootstrap samples
  samples1 = obj1$Boot_result[[var_scale]][[var_type]]
  samples2 = obj2$Boot_result[[var_scale]][[var_type]]

  # Calculating difference
  diff_samples = samples1 - samples2

  # Statistics
  diff_mean = mean(diff_samples)
  diff_ci = quantile(diff_samples, c(lci, uci))  # 95%置信区间
  diff_p = 2 * min(mean(diff_samples > 0), mean(diff_samples < 0))  # 双侧p值

  # Output
  if(verbose){ # Ending verbose if
  cat("\n-----Test of mediation effects based on the universal approach estimation-----\n")
  cat("(Test between two separate estimations)\n")
  cat("\nEffect scale: ",var_scale,"\n")
  cat("Effect type: ",var_type,"\n")
  cat("\n(Alternative hypothesis: The difference of", var_type, "between the two estimations is not equal to 0)\n")
  cat(sprintf("The difference is %.4f (p-value = %.2f)\n", diff_mean, diff_p))
  cat(100*Cf_lv, "% confidence interval of the difference:\n")
  cat(diff_ci)
  } # Ending verbose if

  } # Ending type for
} #Ending scale for
}#Ending function
