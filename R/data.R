#' Test Dataset for Causal Mediation Analysis
#'
#' A simulated dataset used in examples and vignettes to demonstrate the functionality
#' of the \code{FormalEstmed} function. Contains variables for exposure (\code{exp}),
#' mediator (\code{med}), outcome (\code{out}), and covariates (\code{C1}, \code{C2}, \code{C3}).
#'
#' @format A data frame with 1000 rows and 9 variables:
#' \describe{
#'   \item{exp}{Exposure variable (continuous).}
#'   \item{exp2}{Exposure variable (binary: 0/1).}
#'   \item{exp3}{Exposure variable (binary character: male and female).}
#'   \item{med}{Mediator variable (binary: 0/1).}
#'   \item{out}{Outcome variable (continuous).}
#'   \item{out2}{Outcome variable (ordinal).}
#'   \item{C1}{Covariate 1 (binary).}
#'   \item{C2}{Covariate 2 (discrete).}
#'   \item{C3}{Covariate 3 (continuous).}
#' }
#' @source Simulated data for package demonstration.
#' @examples
#' data(testdata)
#' head(testdata)
#' summary(testdata)
"testdata"

#' A simulated dataset used in examples and vignettes to demonstrate the functionality
#' of the \code{FormalEstmed} function. Contains variables for exposure (\code{X}),
#' mediator (\code{M}), outcome (\code{Y}), and covariates (\code{C1}, \code{C2}, \code{C3}).
#'
#' @format Demodata1
#' \describe{
#'   \item{X}{Exposure variable (continuous).}
#'   \item{M}{Mediator variable (continuous).}
#'   \item{Y}{Outcome variable (binary).}
#'   \item{C1}{Covariate 1 (binary).}
#'   \item{C2}{Covariate 2 (continuous).}
#'   \item{C3}{Covariate 3 (continuous).}
#' }
#' @source Simulated data for package demonstration.
#' @examples
#' data(Demodata1)
#' head(Demodata1)
#' summary(Demodata1)
"Demodata1"

#' A simulated dataset used in examples and vignettes to demonstrate the functionality
#' of the \code{FormalEstmed} function. Contains variables for exposure (\code{X}),
#' mediator (\code{M}), outcome (\code{Y}), and covariates (\code{C1}, \code{C2}, \code{C3}).
#'
#' @format Demodata2
#' \describe{
#'   \item{X}{Exposure variable (continuous).}
#'   \item{M}{Mediator variable (continuous).}
#'   \item{Y}{Outcome variable (binary).}
#'   \item{C1}{Covariate 1 (binary).}
#'   \item{C2}{Covariate 2 (continuous).}
#'   \item{C3}{Covariate 3 (continuous).}
#' }
#' @source Simulated data for package demonstration.
#' @examples
#' data(Demodata2)
#' head(Demodata2)
#' summary(Demodata2)
"Demodata2"
