#' Add a new column with standardized model names
#'
#' @param data The StockSMART data.
#' @param model The assessment model to be checked. Current options are AMAK, ASAP, BAM, and SS
#' @param names The available names for each model
#'
#' @return A new data frame of function \code{standardize_model_names}, containing a new column of Assessment.Model.Standardize.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' standardize_model_names(
#' data = stocksmart_data,
#' model = "ASAP",
#' names = "asap"
#' )
#' }
standardize_model_names <- function(
  data,
  model,
  names
){
  dat <- data

  # Assign a new column to standardize names of assessment models
  # Here we focus on get stats for AMAK, ASAP, BAM, and SS, if names of other models don't have any issues, we can set data$Assessment.Model.adj = data$Assessment.Model
  if (!("Assessment.Model.Standardize" %in% colnames(dat))) dat$Assessment.Model.Standardize <- "Others"

  ## Standardize names for AMAK
  if (model == "AMAK") {

    dat$Assessment.Model.Standardize[dat$Assessment.Model %in% names] <- "AMAK"

    dat$Assessment.Model.Standardize[(dat$Assessment.Model %in% c("Custom SCAA", "custom SCAA in ADMB")) &
                                       !(dat$Science.Center %in% "AFSC")] <- "Others"
  }

  ## Standardize names for ASAP
  if (model == "ASAP") {

    dat$Assessment.Model.Standardize[dat$Assessment.Model %in% names] <- "ASAP"

  }

  ## Standardize names for BAM
  if (model == "BAM") {

    dat$Assessment.Model.Standardize[dat$Assessment.Model %in% names] <- "BAM" # Q: What about Beaufort SCAA?


  }

  ## Standardize names for SS
  if (model == "SS") {

    dat$Assessment.Model.Standardize[dat$Assessment.Model %in% names] <- "SS"

    dat$Assessment.Model.Standardize[((dat$Assessment.Model.Standardize == "SS") &
                                (dat$Model.Category == 4) &
                                (dat$Assessment.Year < 2019))] <- "Others"

    dat$Assessment.Model.Standardize[((dat$Assessment.Model.Standardize == "SS") &
                                (dat$Model.Category == 6) &
                                (dat$Assessment.Year >= 2019))] <- "Others"


  }

  return(dat)
}


