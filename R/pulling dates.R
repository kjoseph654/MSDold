#' @title Pulling Dates
#'
#' @description This function pulls the dates that correspond with the Mid Summer Drought, and is used in conjuction with the duration, minimum, onset, and intensity functions.
#'
#' @description The main input is the length of the data you are working with, which is either the length of a vector or the number of layers of a raster.
#'
#' @usage lpdates(length=20440, date_start="1950-01-01", cal="standard", date1="05-01", date2="07-15", date3="07-16", date4="09-30")
#'
#' @param x          RasterBrick or TimeSeries
#' @param start_date Start Date of the input. NOTE: This MUST be January 1st, but the year can differ.
#' @param cal        "standard" corresponds to a data set that has leap days. "365_day" corresponds to a data set that has 365 days. No matter the input, the function will correct for it.
#' @param date1      Start index to find first onset date
#' @param date2      End index to find first onset date
#' @param date3      Start index to find second onset date
#' @param date4      End index to find second onset date
#'
#' @return A vector of indexes that correspond to the four dates every year, with respect to the start date.
#' @return NOTE: It would be significantly easier to save the output as a variable called 'dated', especially when using rasters so that you do not have to create an intermediate function.
#'
#' @examples
#' # using RasterBricks
#' dated<-dur(length=nlayers(r), date_start="1950-01-01", date1="05-01", date2="07-15", date3="07-16", date4="09-30")
#'
#' # using TimeSeries
#' dated<-dur(length=length(t), ...)
#' @export
lpdates <- function(length=20440, date_start="1950-01-01", cal="standard", date1="05-01", date2="07-15", date3="07-16", date4="09-30"){
  #d=vector, length=nlayers() or nlength()

  #detect 365 day (no leap) calendar, allow 1 day deviation
  cal="standard"
  if ((length %% 365 == 0) | ((length+1) %% 365 == 0) | ((length-1) %% 365 == 0)) {cal="365_day"}

  dated <- seq(from = as.Date(date_start), along.with = c(1:length), by = "day")
  if(cal == "365_day") {
    #cat("using a no-leap calendar")
    leapindex<-which(find_leap(dated))
    nleaps <- length(leapindex)
    dated <- seq(from = as.Date(date_start), length.out=(length+nleaps), by = "day")
    dated <- dated[!find_leap(dated)]
    if ( length != length(dated) ) {stop("dates not correct length in duration function")}
  } else {
    #cat("using a standard calendar")

  }
  i<-year(date_start)
  j=i+round(length/365)-1
  k<-c(0)
  for(year in i:j)
  {
    k[((year-i)*4)+1]<-which(grepl(paste(as.character(year),date1,sep="-"),dated))
    k[((year-i)*4)+2]<-which(grepl(paste(as.character(year),date2,sep="-"),dated))
    k[((year-i)*4)+3]<-which(grepl(paste(as.character(year),date3,sep="-"),dated))
    k[((year-i)*4)+4]<-which(grepl(paste(as.character(year),date4,sep="-"),dated))
  }
  return(k)
}
