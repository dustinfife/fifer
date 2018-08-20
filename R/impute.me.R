##' Multiple Imputation on a Model
##'
##' Multiple Imputation on a Model
##'	
##' This is a wrapper function for both the mice function in the mice package, as well as for basic models in R (e.g., lm). As input,
##' it takes the model the user wishes to estimate using advanced missing data strategies, as well as a list of variables they wish to use
##' to impute the missing values. The function takes the raw data and performs MI using mice, then re-analyzes the dataset and outputs the
##' multiply imputed parameter estimates. 
##' @param model An R-friendly model. Currently, it only allows lm objects, but will eventually allow other objects (e.g., glm). 
##' @param data The dataset used for analysis. This dataset should contain predictors used to impute the missing values
##' @param predictors A list of predictors (as a character vector) that identify which variables to keep (or drop; see below argument). 
##' @param keep Logical. Should the list of predictors be kept or dropped? Defaults to keep. 
##' @param imputations The number of imputations to be performed. Defaults to 20. 
##' @import mice
##' @author Dustin Fife
##' @export
##' @examples
##' data(exercise_data)
##' d = exercise_data
##' 
##' 		##### create missing data in motivation
##' missing.ld = which(d$motivation<quantile(d$motivation, .25))
##' notmissing = which(!(1:nrow(d) %in% missing.ld))
##' d$weight.change.missing = d$weight.change
##' d$weight.change.missing[missing.ld] = NA
##' 
##' 		#### create model with missing data
##' mod = lm(weight.change.missing~motivation, data=d, model=T)
##' predictors = c("muscle.gain.missing", "weight.change")
##' impute.me(mod, data=d, predictors=predictors, keep=F, imputations=5)
impute.me = function(model, data, predictors=NULL, keep=T, imputations=20, silent=F){

		#### set up data to perform the imputations
		if (!is.null(predictors)){
			data = make.null(predictors, data=data, keep=keep)
		}

		#### figure out missing data pattern
		pattern = md.pattern(data)

		#### do multiple imputations
		if (!silent){
			cat("Performing Imputations. \n\n")
			require(mice)
			imputed.data= mice(data=data, m=imputations)
		} else {
			require(mice)
			imputed.data= mice(data=data, m=imputations, quietly=T)
		}
		
		#### remove dataset in the call
		model$call$data=NULL

		#### pool estimates
		fit = with(data=imputed.data,expr= eval(model$call))
		combined = pool(fit)
		estimates = (summary(combined))
		return(estimates)
}