% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_stocksmart_data.R
\name{read_stocksmart_data}
\alias{read_stocksmart_data}
\title{Read data downloaded from StockSMART}
\usage{
read_stocksmart_data(filedir, filename)
}
\arguments{
\item{filedir}{Path to the StockSMART csv file.}

\item{filename}{Name of the StockSMART csv file.}
}
\value{
A dataframe of function \code{read_stocksmart_data}, containing inputs to functions that visualize the data.
}
\description{
Read data downloaded from StockSMART
}
\examples{
\dontrun{
read_stocksmart_data(
filedir = system.file("extdata", package="StockSMARTVisualizer"),
filename = "Current_SIS_Records.csv"
)
}
}
