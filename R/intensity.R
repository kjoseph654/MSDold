#' @title Intensity Value
#'
#' @description Calculate the intensity of the mid summer drought from a RasterBrick or a Time Series, or the average of the two onset data points minus the minimum data point.
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' @description If x is a RasterBrick, then the output is a RasterBrick with a data point for each year. If you would like to find a statistical value of the new rasterbrick, it is recommended to use
#'
#' @description r <- raster::calc(x, mean)
#'
#' @usage intensity(x, ind=dated)
#'
#' @param x          RasterBrick or TimeSeries
#' @param ind        Index of Dates from the lpdates function (should be saved as 'dated')
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{raster}}
#'
#' @examples
#' # using RasterBricks
#' r<-raster::calc(x, intensity)
#'
#' # using TimeSeries
#' r<-intensity(x)
#' @export
intensity<-function(x, ind=dated){
  #b=rasterbrick, date1/date2=index to find onset date, date3/date4=index to find "second" onset date

  filtered<-c(x)
  new<-c(0)

  for(year in 1:(length(filtered)/365))
  {
    max1<-max(filtered[ind[(year-1)*4+1]:ind[(year-1)*4+2]])
    max2<-max(filtered[ind[(year-1)*4+3]:ind[(year-1)*4+4]])

    date5<-ind[1]+365*(year)-365+match(max1, filtered[ind[(year-1)*4+1]:ind[(year-1)*4+2]])-1
    date6<-ind[3]+365*(year)-365+match(max2, filtered[ind[(year-1)*4+3]:ind[(year-1)*4+4]])-1
    min1<-min(filtered[date5:date6])

    new[year]<-((max1+max2)/2)-min1
  }

  return(new)
}
