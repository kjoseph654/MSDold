#' @title Onset Value
#'
#' @description Calculate the onset value of the mid summer drought from a RasterBrick or a Time Series.
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' @description If x is a RasterBrick, then the output is a RasterBrick with a data point for each year. If you would like to find a statistical value of the new rasterbrick, it is recommended to use
#'
#' @description r <- raster::calc(x, mean)
#'
#' @description If the index vector from lpdates is not saved as 'dated', a function will have to be created to use it in raster::calc.
#' 
#' @description onset1<-function(x){
#'   onset(x,ind=vector)
#' }
#'
#' @usage onset(x, ind=dated)
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
#' r<-raster::calc(x, onset)
#'
#' # using TimeSeries
#' r<-onset(x)
#' @export
onset <-function(x, ind=dated){
  filtered<-c(x)
  new<-c(0)
  
  for(year in 1:(length(ind)/4))
  {
    max1<-max(filtered[ind[(year-1)*4+1]:ind[(year-1)*4+2]])
    new[year]<-max1
  }
  return(new)
}