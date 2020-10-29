#' Generate polynomial random data
#'
#' @param cor correlation coefficient
#' @param poly strength of polynomial term
#' @param n sample size
#' @param means means of x and y, respectively
#' @param sds sds of x and y, respectively
#' @param names names of x/y, respectively
#' @param plot should a plot be returned?
#' @param ... other arguments passed to flexplot
#'
#' @return either a plot or generated data
#' @export
generate_polynomial = function(cor = .3, poly = -.4, n = 200, means = c(0,0), sds = c(1,1), names=c("x", "y"), plot=TRUE, ...) {
  x = rnorm(n)
  y = cor*x + poly*x^2 + rnorm(n, 0, sqrt(1-(cor^2 + poly^2)))
  d = data.frame(x=x,y=y)
  if (plot) {
    return(flexplot::flexplot(y~x, data=d, ...))
  }
  names(d) = names
  d
}