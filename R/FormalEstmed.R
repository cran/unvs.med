#' @title Formal Estimation for Causal Mediation Effects (The Main Function)
#'
#' @description
#' This is the main function for causal mediation estimations. Users only need to
#' call this function to estimate causal mediation effects, as it can automatically call
#' other internal functions for the algorithm.
#' This function provides estimates of various types of mediation effects on
#' risk difference (RD), odds ratio (OR) and risk ratio (RR) scales in the returned object,
#' in which a wide range of estimation details and model information are also included.
#' This function is applicable to almost any type of mediator and outcome models and data structure,
#' greatly increasing the efficiency of causal mediation analysis.
#'
#' @note
#' The running time of this function depends on the quantity of data samples and
#' the complexity of the mediator and outcome models. For example, the running time
#' in the case of continuous mediator is significantly longer than that in the case of binary
#' and ordinal mediator. For a certain type of mediator, it takes a longer time to proceed in the case
#' of ordinal outcome. The running time in different settings also varies significantly, from a couples of seconds
#' to several minutes. Therefore, we welcome users to provide a more efficient algorithm for the case of continuous mediators and
#' contact our maintainer with no hesitation. We are looking forward to your suggestions and comments.
#'
#' @usage FormalEstmed (med_model, out_model, data, exposure,
#' mediator=NULL, outcome=NULL, med_type=NULL,out_type=NULL, cov_val=NULL,
#' boot_num=100, MT = TRUE, Cf_lv=0.95)
#'
#' @param med_model a fitted model object for the mediator.
#' @param out_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param exposure a character variable of the exposure's name. Must be specified by the user.
#' @param mediator a character variable of the mediator's name. Identified automatically if not specified by the user.
#' @param outcome a character variable of the outcome's name. Identified automatically if not specified by the user.
#' @param med_type a character variable of the mediator's type. Identified automatically if not specified by the user.
#' @param out_type a character variable of the outcome's type. Identified automatically if not specified by the user.
#' @param cov_val a character variable of the conditions of the covariates. Each string (element) in the character variable is a logical statement, e.g.,
#'        \code{'C1==5'}, \code{'C2>1'}, etc.
#' @param boot_num the times of bootstrapping in the analysis. The default is 100.
#' @param MT a logical value indicating whether the multi-threading process is activated. If TURE, activating max-1 cores.
#'        If FALSE, use the ordinary 'for' loop. The default is \code{TRUE}.
#' @param Cf_lv a numeric variable of the confidence interval. The value is presented in decimal form, not percentage form.
#'        The default is 0.95.
#'
#' @details
#' For continuous variables, \code{mediator} and \code{outcome} can be identified automatically by this function.
#' However, when the mediator or outcome variable is not continuous, users should make sure the class of the variable is consistent both in the dataframe and in the models,
#' otherwise, users should specify it manually, not automatically.
#' For example, for a \code{ordinal} type of outcome variable, if users transfer it into a factor variable in the dataframe before building the model, \code{outcome} can be identified automatically.
#' If users do not transfer it into a factor variable in advance, but only specify it as a factor within the model, e.g., \code{polr(as.factor(outcome)~X1+X2+...)},
#' then \code{outcome} can not be identified automatically.
#' Therefore we recommend users transfer the mediator and outcome variable properly in the dataframe before building models.
#'
#' @returns This function returns a list object of class \code{"unvs.med"}. The object encompasses the complete result of
#' the estimates of various types of effects on risk difference (RD), odds ratio (OR) and risk ratio (RR) scales,
#' the results of mom-parametric bootstrapping,
#' model specifications and other detailed information. Users may conduct further analysis based on this object.
#'
#' The function \code{\link{summary.unvs.med}} can be used to obtain the refined result of this returned object,
#' The function \code{\link{plot.unvs.med}} can be used to obtain the visualized result of this returned object.
#' The function \code{\link{um.test1}} can be used to test the statistical difference of effects within one single estimation.
#' The function \code{\link{um.test2}} can be used to test the statistical difference of effects between two separate estimations.
#'
#' \item{Stat.RD, Stat.OR, Stat.RR}{Statistics of the estimates of mediation effects on risk difference (RD), odds ratio (OR) and risk ratio (RR) scales.}
#'
#' \item{Boot_result}{results of the original non-parametric bootstrapping estimations of mediation effects risk difference (RD), odds ratio (OR) and risk ratio (RR) scales.}
#'
#' \item{Function_call}{user's code of calling function FormalEstmed().}
#'
#' \item{Exposure}{exposure in the analysis.}
#'
#' \item{Mediator}{mediator in the analysis.}
#'
#' \item{Medaitor_type}{mediator's type in the analysis.}
#'
#' \item{Medaitor_model}{mediator's model in the analysis. If involving moderated mediation,
#' i.e., \code{Covariates_cond} is not \code{NULL}, then this returned mediator's model is different from the input one.}
#'
#' \item{Outcome}{outcome in the analysis.}
#'
#' \item{Outcome_type}{outcome's type in the analysis.}
#'
#' \item{Outcome_model}{outcome's model in the analysis. If involving moderated mediation,
#' i.e., \code{Covariates_cond} is not \code{NULL}, then this returned outcome's model is different from the input one.}
#'
#' \item{Covariates}{covariates in the analysis.}
#'
#' \item{Covariates_cond}{conditions of the covariates in the analysis.}
#'
#' \item{Data}{dataframe used in the analysis. If involving moderated mediation,
#' i.e., \code{Covariates_cond} is not \code{NULL}, then this returned dataframe is different from the input one.}
#'
#' \item{Bootstrap_number}{times of bootstrapping in the analysis.}
#'
#' \item{Confidence_level}{levels of confident interval in the analysis.}
#'
#' @export
#'
#' @examples
#' \donttest{
#' ############################################################
#' # Example 1.1: Continuous exposure and outcome; Binary mediator
#' ############################################################
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r11 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp") # Running formal estimation via bootstrapping
#' summary(r11) # Viewing results in short form and on RD scales.
#'
#' ############################################################
#' # Example 1.2: Example 1.1 but considering moderated mediation
#' ############################################################
#' data(testdata)
#' med_model=glm(med~exp*C1+C2+exp*C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+exp*C1+C2+exp*C3, data=testdata) # Fitting outcome's model
#' r12 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", cov_val=c("C1==1","C3>7")) # Conditional on C1 and C3.
#' summary(r12)
#'
#' ############################################################
#' # Example 1.3: Example 1.1 with more bootstrapping
#' ############################################################
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r13 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", boot=500) # Running formal estimation via bootstrapping
#' summary(r13) # Viewing results in short form and on RD scales.
#'
#' ############################################################
#' # Example 2.1: Continuous exposure; Binary mediator; Ordinal outcome
#' #############################################################'
#' library("MASS") # For ordinal logistic regression
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' testdata$out2=as.factor(testdata$out2) # out2 is the outcome. Convert it into a factor.
#' out_model=polr(out2~med*exp+C1+C2+C3, data=testdata, method="logistic") # Fitting outcome's model.
#' r21 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", boot=100) # Running formal estimation via bootstrapping.
#' summary(r21)
#'
#' ############################################################
#' # Example 2.2: Example 2.1 but considering moderated mediation
#' #############################################################'
#' library("MASS") # For ordinal logistic regression
#' data(testdata)
#' med_model=glm(med~exp+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' testdata$out2=as.factor(testdata$out2) # out2 is the outcome. Convert it into a factor.
#' out_model=polr(out2~med*exp+C1+C2+C3, data=testdata, method="logistic") # Fitting outcome's model.
#' r22 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp", boot=100, cov_val="C2>=50") # Solely conditioning on C2
#' summary(r22)
#'
#' ############################################################
#' # Example 3.1: Binary exposure (0 and 1); Binary mediator; continue outcome
#' #############################################################'
#' data(testdata)
#' med_model=glm(med~exp2+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp2+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r31 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp2") # Running formal estimation via bootstrapping
#' summary(r31) # Viewing results in short form and on RD scales.
#'
#' ############################################################
#' # Example 3.2: Binary exposure (male and female); Binary mediator; continue outcome
#' #############################################################'
#' data(testdata)
#' med_model=glm(med~exp3+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp3+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r32 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp3") # Running formal estimation via bootstrapping
#' summary(r32) # Viewing results in short form and on RD scales.
#'
#' ############################################################
#' # Example 3.3: Binary exposure (male and female); Binary mediator; continue outcome
#' #############################################################'
#' data(testdata)
#' testdata$exp3=as.factor(testdata$exp3) # The default level is c("female","male").
#' levels(testdata$exp3)=c("male","female") # Factor level: c("male","female").
#' med_model=glm(med~exp3+C1+C2+C3, data=testdata, family=binomial) # Fitting mediator's model
#' out_model=lm(out~med*exp3+C1+C2+C3, data=testdata) # Fitting outcome's model
#' r33 = FormalEstmed (med_model=med_model, out_model=out_model,
#' data=testdata, exposure = "exp3") # Running formal estimation via bootstrapping
#' summary(r33) # Viewing results in short form and on RD scales.
#' }
#'
#' @import data.table
#' @import parallel
#' @import snowfall
#'
#' @import graphics
#' @import stats
#' @import utils
#'
FormalEstmed = function(med_model=NULL, out_model=NULL, data=NULL,
                      exposure=NULL, mediator=NULL, outcome=NULL,
                      med_type=NULL,out_type=NULL, cov_val=NULL,
                      boot_num=100, MT = TRUE, Cf_lv=0.95)
{ #beginning main function

#Changing input names: from function input specifications to original coding proxies
X=exposure
M=mediator
Y=outcome

m_type=med_type
y_type=out_type

m_model=med_model
y_model=out_model

confirmingX(X = X) # confirming exposure X

# identifying mediator M, outcome Y, covariates (confounders/control variables) W
if(is.null(M))
{M=deparse(formula(m_model)[[2]])}
if(is.null(Y))
{Y=deparse(formula(y_model)[[2]])}

Wy=setdiff(all.vars(formula(y_model)),c(Y,X,M))
Wm=setdiff(all.vars(formula(m_model)),c(X,M))
W=union(Wy,Wm)

# identifying mediator type
if(is.null(m_type))
{m_type=ident_M_type(M = M, data = data)}
# identifying outcome type
if(is.null(y_type))
{y_type=ident_Y_type(Y = Y, data = data)}

# Modifying dataset
data = data[, intersect(c(X, M, Y, W), names(data)), drop = FALSE] # Excluding irrevalent variables
data = na.omit(data) #Excluding NA lines

# Dealing with conditional covariates
if(!is.null(cov_val))
{ if(!is.character(cov_val)){
  stop("cov_val should be character variable")
  } else {
    cond_result = cond_cov(m_model, y_model, data, X, M, cov_val)
    data=cond_result$data
    m_model=cond_result$m_model
    y_model=cond_result$y_model
  }
}

# Main estimations begin
message("Function being preceeded! It may take a while...")
message("(Please make sure all the settings are consistent with the instruction)")

# Obtaining results
Com_Est_result = Statistics (data = data, X = X, M = M, Y = Y, m_type = m_type, y_type = y_type,
                            m_model = m_model, y_model = y_model, boot_num=boot_num, MT = MT,
                            Cf_lv=Cf_lv) # Results for mediation effects

# Summary of function's info
info=list(Function_call = match.call(), Exposure = X, Mediator = M, Mediator_type = m_type,
          Mediator_model = m_model, Outcome = Y, Outcome_type = y_type, Outcome_model = y_model,
          Covariates = W, Covariates_cond = cov_val,
          Data = data, Bootstrap_number = boot_num, Confidence_level = Cf_lv)

FormalEstmed_result = c(Com_Est_result, info) # Saving results as a list
class(FormalEstmed_result) = "unvs.med"

message("Function proceeded successfully!")
return(FormalEstmed_result) #Returning final potential outcome of nested counterfactuals

} #ending main function
