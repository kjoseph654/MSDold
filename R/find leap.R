#' @title Leap Day Finder
#'
#' @description Finds any leap days in a vector of dates.
#'
#' @description The Input should be a vector of dates that has at least the month and day attached to it. find_leap returns a logical vector that displays leap days as true.
#'
#' @usage find_leap(x)
#'
#' @param x          Date or Date Vectors
#'
#' @return Logical Vector (Leap Days are TRUE)
#'
#' @seealso \code{\link{lubridate}}
#'
#' @examples
#'
#' dates<-c("date1","date2","date3")
#' date<-find_leap(dates)
#'
#' @export
find_leap = function(x){
  day(x) == 29 & month(x) == 2
}
