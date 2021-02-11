#' Read data downloaded from StockSMART
#'
#' @param filedir Path to the StockSMART csv file.
#' @param filename Name of the StockSMART csv file.
#'
#' @return A dataframe of function \code{read_stocksmart_data}, containing inputs to functions that visualize the data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read_stocksmart_data(
#' filedir = system.file("extdata", package="StockSMARTVisualizer"),
#' filename = "Current_SIS_Records.csv"
#' )
#' }
read_stocksmart_data <- function(
  filedir,
  filename
){

  if (!grepl(".csv", filename, ignore.case = T)) stop("Please convert your file to CSV file first!")

  stocksmart_data <- read.csv(file = file.path(filedir, filename))

    return(stocksmart_data)
}


