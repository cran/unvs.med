#' @title Bootstrapping Estimation for Causal Mediation Effects via
#' Multi-threading Process
#'
#' @description
#' This function obtains the estimates of mediation effects via non-parametric bootstrapping.
#' Through bootstrap sampling and repeating the algorithm of function \code{\link{SingleEstimation}},
#' This function obtains a number of estimates for each type of effect.
#'
#' This is an internal function, automatically called by the function \code{\link{Statistics}}.
#'
#' @details
#' This function activates the multi-threading process through
#' package 'snowfall' in R with \code{max-1} cores (CPU) of the PC.
#'
#' @usage BootEstimation_MT (m_model, y_model, data, X, M, Y,
#' m_type, y_type, boot_num = 100)
#'
#' @param m_model a fitted model object for the mediator.
#' @param y_model a fitted model object for the outcome.
#' @param data a dataframe used in the analysis.
#' @param X a character variable of the exposure's name.
#' @param M a character variable of the mediator's name.
#' @param Y a character variable of the outcome's name.
#' @param m_type a character variable of the mediator's type.
#' @param y_type a character variable of the outcome's type.
#' @param boot_num the times of bootstrapping in the analysis. The default is 100.
#'
#' @returns This function returns a list of three dataframes, i.e.,
#' the bootstrapping results of the mediation effects.
#' This list is also saved in the return of the main function \code{\link{FormalEstmed}}.
#'
#' @export
#'
BootEstimation_MT = function(m_model = NULL, y_model = NULL, data = NULL,
                             X = NULL, M = NULL, Y = NULL, m_type = NULL, y_type = NULL,
                             boot_num = 100)
{ # Beginning function

  # Preparing dataframe
  cols <- c("TE", "PNDE", "TNDE", "PNIE", "TNIE", "Prop_PNIE", "Prop_TNIE",
            "Ave_NDE", "Ave_NIE", "Ave_Prop_NIE")
  Boot_result.RD <- data.frame(matrix(numeric(), nrow = boot_num, ncol = length(cols)))
  colnames(Boot_result.RD) <- cols
  Boot_result.OR <- Boot_result.RD
  Boot_result.RR <- Boot_result.RD


  # sf function
  fun_Boot = function(BT) {
    # Bootstrap sampling
    boot_indices = sample(nrow(data), size = nrow(data), replace = TRUE)
    data_boot = data[boot_indices, , drop = FALSE]

    # Updating model's data
    m_model_new = update(m_model, data = data_boot)
    y_model_new = update(y_model, data = data_boot)

    # Single estimation with bootstrap data and models
    Single_result = SingleEstimation(data = data_boot, X = X, M = M, Y = Y,
                                     m_type = m_type, y_type = y_type,
                                     m_model = m_model_new, y_model = y_model_new)
    return(list(RD = Single_result$RD, OR = Single_result$OR, RR = Single_result$RR))
  }

  # Initiating multi-threading process
  loaded_packages <- search()
  loaded_packages <- loaded_packages[grep("^package:", loaded_packages)]
  loaded_packages <- sub("^package:", "", loaded_packages)
  basic_packages = c("stats", "graphics", "grDevices", "utils", "datasets", "methods",
                     "base")
  #loaded_packages = setdiff(loaded_packages, basic_packages)

  sfInit(parallel = TRUE, cpus = parallel::detectCores() - 1)
  on.exit(sfStop())

  sfExport("loaded_packages")

  # Loading packages on all the nodes.
  sfClusterEval
  ({
    invisible(capture.output(
      for (pkg in loaded_packages)
    { sfLibrary(pkg, character.only = TRUE) }
    ))
  })

  # Exporting sources
  sfExport("boot_num", "data", "X", "M", "Y", "m_type", "y_type")
  invisible(capture.output({
  sfLibrary(data.table)
  sfExportAll()
  }))

  # Obtaining results
  sf_results = sfLapply(1:boot_num, fun_Boot)

  # Combining sf results
  Boot_result.RD = do.call(rbind, lapply(sf_results, `[[`, "RD"))
  Boot_result.OR = do.call(rbind, lapply(sf_results, `[[`, "OR"))
  Boot_result.RR = do.call(rbind, lapply(sf_results, `[[`, "RR"))

  return(list(RD = Boot_result.RD, OR = Boot_result.OR, RR = Boot_result.RR))
} # Ending function
