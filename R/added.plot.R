##' Create an added variable plot
##'
##' Create an added variable plot
##'	
##' This function first residualizes the outcome variable based on the first variable listed in the formula.
##' The mean of the outcome variable is then added to the residuals (to maintain the interpretation of the variable),
##' then the function plots the residuals against the second variable. 
##' @param formula A formula with exactly two predictors and an outcome variable
##' @param data The dataset used
##' @param ... Other parameters passed to flexplot
##' @seealso \code{\link{flexplot}}
##' @author Dustin Fife
##' @export
##' @examples
##' data(exercise_data)
##' added.plot(weight.loss~motivation + therapy.type, data=d)
added.plot = function(formula, data, ...){

	#### identify variable types
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	
	#### bark if there's not two variables
	if (length(predictors) != 2){
		stop("You must have exactly two predictor variables")
	}
	
	#### remove missing data
	miss.vals = sapply(data[,variables], function(x) sum(is.na(x)))
	miss.vals = miss.vals[miss.vals>0]
	if (length(miss.vals)!=0){
		warning("Note: I'm removing missing values so the function will work.")
		data[,predictors] = na.omit(data[,predictors])
	}


	#### model the first chosen variable
	new.form = make.formula(outcome, predictors[1])
	data$residuals = residuals(lm(new.form, data=data)) + mean(data[,outcome])
	
	##### now plot that succa
	new.form = make.formula("residuals", predictors[2])
	flexplot(new.form, data=data, ...) + labs(y=paste0(outcome, " | ", predictors[1]))
}