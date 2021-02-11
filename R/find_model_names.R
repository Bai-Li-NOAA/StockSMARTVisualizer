#' Find available names for a stock assessment model
#'
#' @param data The StockSMART data.
#' @param model The assessment model to be checked. Current options are AMAK, ASAP, BAM, and SS
#'
#' @return A list of available model names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' find_model_names(
#' data = stocksmart_data,
#' model = "ASAP"
#' )
#' }
find_model_names <- function(
  data,
  model
){
  dat <- data

  amak_names <- asap_names <- bam_names <- ss_names <- NA

  # Find available names for AMAK
  if ("AMAK" %in% model) {

    amak_names <- unique(c(
      grep('^a', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('amak', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('^custom scaa', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE)
    ))

  }

  # Find available names for ASAP
  if ("ASAP" %in% model) {

    asap_names <- unique(c(
      grep('^a', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('asap', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE)
    ))

  }

  # Find available names for BAM
  if ("BAM" %in% model){

    bam_names <- unique(c(
      grep('^b', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('bam', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE)
    ))

  }

  # Find available names for SS
  if ("SS" %in% model){
    ss_names <- unique(c(
      grep('^s', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('stock synthesis', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('\\<ss\\>', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE),

      grep('^custom scaa', dat$Assessment.Model,
           ignore.case=TRUE, value=TRUE)
    ))

  }

  return(list(amak=amak_names,
              asap=asap_names,
              bam=bam_names,
              ss=ss_names))
}


