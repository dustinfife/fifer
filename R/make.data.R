##' Basic function for simulating bivariate data
##'
##' Basic function for simulating bivariate data
##'	
##' This can model either numeric on numeric data or categorical on numeric. 
##' @param cor The correlation coefficient between the Y and X
##' @param means A vector of length two that indicates the mean of Y and X, respectively
##' @param sds A vector of length two that indicates the standard deviations of Y and X, respectively
##' @param n Either a single value or a vector indicating the sample size of each group
##' @param names The names of the Y and X variables, respectively
##' @param groups The names of the groups
##' @param digits The number of digits the numeric variables should be rounded to
##' @param cap The min/max values for each of the variables, to be used for truncating scores, coded as a vector. 
##' @return a simulated dataset
##' @author Dustin Fife
##' @export
##' @examples
##' fake.data = make.data(cor=.6, means=c(50, 100), sds = c(15, 15), n=100, names=c("Exam Score", "IQ"))
##' 
##' fake.data = make.data(means = c(10, 50, 30), sds=c(4, 15, 9), n=c(20, 26, 55), names=c("Depression", "Condition"), groups=c("Control", "Medication + Therapy", "Therapy"))
make.data = function(cor=NULL, means, sds, n, names=c("X","Y"), groups=c("A", "B"), digits=0, cap=NULL){
	if (!is.null(cor)){
		cor.mat = matrix(c(1, cor, cor, 1), nrow=2)
		cov.mat = cor2cov(cor.mat, sds)
		d = data.frame(mvrnorm(n, means, cov.mat))
		names(d) = names
		d = round(d, digits=digits)
	
		if (!is.null(cap)){
			### max for y
			if (!is.null(cap[1])){
				mx.y = which(d[,1]<cap[1])
				if (length(mx.y)>0){
					d[mx.y,1] = cap[1]
				}
			}
	
			### min for y
			if (!is.null(cap[2])){
				mx.y = which(d[,1]>cap[2])
				if (length(mx.y)>0){
					d[mx.y,1] = cap[2]
				}
			}			
			
			### max for y
			if (!is.null(cap[3])){
				mx.y = which(d[,2]<cap[3])
				if (length(mx.y)>0){
					d[mx.y,2] = cap[3]
				}
			}
	
			### min for y
			if (!is.null(cap[4])){
				mx.y = which(d[,2]>cap[4])
				if (length(mx.y)>0){
					d[mx.y,2] = cap[4]
				}
			}
		}
		return(d)
	} else {
		### generate categorical variable
		outcome = 1:(sum(n)); group.vals=rep("NN", times=length(outcome))
		end.row = 1
		i = 2
		for (i in 1:length(n)){
			outcome[end.row:(end.row+n[i]-1)] = rnorm(n[i], means[i], sds[i])
			group.vals[end.row:(end.row+n[i]-1)] = groups[i]
			end.row = end.row + n[i]
		}
		d = data.frame(x=outcome, groups=group.vals); names(d) = names
		d[,1] = round(d[,1], digits=digits)

		if (!is.null(cap)){
			### max for y
			if (!is.null(cap[1])){
				mx.y = which(d[,1]<cap[1])
				if (length(mx.y)>0){
					d[mx.y,1] = cap[1]
				}
			}
	
			### min for y
			if (!is.null(cap[1])){
				mx.y = which(d[,1]>cap[2])
				if (length(mx.y)>0){
					d[mx.y,1] = cap[2]
				}
			}			
		
		return(d)
	} else {
		return(d)
	}
	}
}

cap = c(0,100,0,NULL)