##' Residualize a lowess function
##'
##' This function will create an approximate fitted function for a lowess line, then residualize the Y based on the fit of the model
##'	
##' @param x The predictor of interest
##' @param y The outcome of interest
##' @references This function was developed by Glen_b on \href{https://stats.stackexchange.com/questions/126699/residuals-from-lowess-curve}{Stack Overflow}
##' @return A list of residuals
##' @author Dustin Fife
##' @export
##' @examples
##' data(airquality)
##' head(airquality)
##' attach(airquality)
##' plot(Wind, Temp)
##' lines(lowess(Wind, Temp), col="red")
##' residuals = residualize.lowess(Wind, Temp)
##' plot(Wind, residuals)
residualize.lowess = function(x,y, return.fitted){
 lfit = loess(x,y, degree=3)

 # create a functional version of the lowess fit
 lfun = approxfun(lfit)
 fitted = lfun(x)
 resid = y-fitted
 if(return.fitted) {
   return(fitted)
 } else {
   return(resid)
 }
}