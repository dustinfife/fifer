comp.p = function(variable, response){
	#### if it's a factor, compute chi square test
	if (is.factor(variable)){
		test = chisq.test(variable,response)
		return(test$p.value)
	} else if (length(unique(response))<3){
	##### do a t test
		test = t.test(variable~response)
		return(test$p.value)
	} else {
		k = lm(variable~response)
		return(as.numeric(unlist(anova(k)["Pr(>F)"]))[1])
	}
}

##' Extract the p value from a univariate significance test
##'
##' This function is used to easily extract p value when there are a host of predictor variables. See examples. 
##' @title Extract p values for a data frame
##' @param dataframe a data frame containing both the variables and the response
##' @param exclude.cols a vector indicating which columns should not have a significance test
##' @param response a string with the name of the response variable
##' @return a vector of p values
##' @author Dustin Fife
##' @export
##' @examples
##' k = data.frame(cbind(ID=1:100,
##'				A = rnorm(100),
##'				B = rnorm(100),
##'				C = rnorm(100),
##'				Group = rep(1:2, times=50)))
##' univariate.tests(dataframe = k, exclude.cols=1, response="Group")
univariate.tests = function(dataframe, exclude.cols=NULL, response){
	cl = which(names(dataframe)==response)
	if (is.null(exclude.cols)){
		exclude.cols = cl
	} else {
		exclude.cols = c(exclude.cols, cl)
	}
	p.vals = unlist(lapply(dataframe[,-c(exclude.cols)], comp.p, response=dataframe[,response]))
	return(p.vals)
}