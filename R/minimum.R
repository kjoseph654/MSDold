#' @title Minimum Value
#'
#' @description Calculate the minimum value of the mid summer drought from a RasterBrick or a Time Series.
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' @description If x is a RasterBrick, then the output is a RasterBrick with a data point for each year. If you would like to find a statistical value of the new rasterbrick, it is recommended to use
#'
#' @description r <- raster::calc(x, mean)
#'
#' @usage minimum(x, date1="1950-05-01", date2="1950-07-15", date3="1950-07-16", date4="1950-09-30")
#'
#' @param x          RasterBrick or TimeSeries
#' @param date1      Start index to find first onset date
#' @param date2      End index to find first onset date
#' @param date3      Start index to find second onset date
#' @param date4      End index to find second onset date
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{raster}}
#'
#' @examples
#' # using RasterBricks
#' r<-raster::calc(x, minimum)
#'
#' # using TimeSeries
#' r<-minimum(x)
#' @export
minimum<-function(x, date1="1950-05-01", date2="1950-07-15", date3="1950-07-16", date4="1950-09-30"){
  #b=rasterbrick, date1/date2=index to find onset date, date3/date4=index to find "second" onset date

  date1<-as.numeric(strftime(date1, format="%j"))
  date2<-as.numeric(strftime(date2, format="%j"))
  date3<-as.numeric(strftime(date3, format="%j"))
  date4<-as.numeric(strftime(date4, format="%j"))

  filtered<-c(x)
  new<-c(0)

  for(year in 1:(length(filtered)/365))
  {
    max1<-max(filtered[(date1+(365*year-365)):(date2+(365*year-365))])
    max2<-max(filtered[(date3+(365*year-365)):(date4+(365*year-365))])

    date5<-match(max1, filtered)
    date6<-match(max2, filtered)
    new[year]<-min(filtered[date5:date6])
  }

  return(new)
}
