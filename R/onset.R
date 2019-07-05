#' @title Onset Value
#'
#' @description Calculate the onset value of the mid summer drought from a RasterBrick or a Time Series.
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' @description If x is a RasterBrick, then the output is a RasterBrick with a data point for each year. If you would like to find a statistical value of the new rasterbrick, it is recommended to use
#'
#' @description r <- raster::calc(x, mean)
#'
#' @usage onset(x, date1="1950-05-01", date2="1950-07-15", date3="1950-07-16", date4="1950-09-30")
#'
#' @param x          RasterBrick or TimeSeries
#' @param date1      Start index to find first onset date
#' @param date2      End index to find first onset date
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{raster}}
#'
#' @examples
#' # using RasterBricks
#' r<-raster::calc(x, onset)
#'
#' # using TimeSeries
#' r<-onset(x)
#' @export
onset <- function (b, date1="1950-05-01", date2="1950-07-15") {
  #b=rasterbrick, date1<-first date, date2<-second date

  date1<-as.numeric(strftime(date1, format="%j"))
  date2<-as.numeric(strftime(date2, format="%j"))

  filtered<-c(x)
  new<-c(0)
  for (year in 1:(length(filtered)/365))
  {
    new[year]<-max(filtered[(date1+(365*year-365)):(date2+(365*year-365))])
  }

  return(new)
}
