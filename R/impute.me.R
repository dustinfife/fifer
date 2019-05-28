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
##' @return.mod Should the model be returned?
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
impute.me = function(model, data, predictors=NULL, keep=T, imputations=20, silent=F, return.mod=F){

		#### set up data to perform the imputations
		if (!is.null(predictors)){
			data = make.null(predictors, data=data, keep=keep)
		}

		#### figure out missing data pattern
		pattern = md.pattern(data)

		#### do multiple imputations
		if (!silent){
			cat("Performing Imputations. \n\n")
			imputed.data= mice(data=data, m=imputations)
		} else {
			imputed.data= mice(data=data, m=imputations, quietly=T)
		}
		
		#### remove dataset in the call
		model$call$data=NULL

		#### pool estimates
		fit = with(data=imputed.data,expr= eval(model$call))

		combined = pool(fit)
		mod.combined=pool(fit$analyses)
		if (!return.mod){
			estimates = (summary(combined))
		} else {
			list(estimates=estimates, models=fit)
		}

}

##' Compute Bayes Factor for a Imputed Model
##'
##' Compute Bayes Factor for a Imputed Model
##'	
##' blah blah blah
##' @param model1 The full model
##' @param model2 The reduced model
##' @param data The dataset used for analysis. This dataset should contain predictors used to impute the missing values
##' @param predictors A list of predictors (as a character vector) that identify which variables to keep (or drop; see below argument). 
##' @param keep Logical. Should the list of predictors be kept or dropped? Defaults to keep. 
##' @param imputations The number of imputations to be performed. Defaults to 20. 
##' @return.mod Should the model be returned?
##' @import mice
##' @author Dustin Fife
##' @export
impute.model.comparison = function(model1, model2, data,predictors=NULL, keep=T, imputations=20, silent=F, invert=F){
	impute.me.full = impute.me(model1, data=data, predictors=predictors, keep=keep, imputations=imputations, silent=F, return.mod=T)
	impute.me.reduced = impute.me(model2, data=data, predictors=predictors, keep=keep, imputations=imputations, silent=F, return.mod=T)	
	
	compared = pool.compare(impute.me.full$models, impute.me.reduced$models, method="likelihood")
	bic.full = log(nrow(data))*length(compared$qbar1)-compared$deviances$dev1.M[1]
	bic.reduced = log(nrow(data))*length(compared$qbar0)-compared$deviances$dev0.M[1]
	bf = exp((bic.full-bic.reduced)/2)
	
	### do same for r squared
	rsq.full = pool.r.squared(impute.me.full$models)
	rsq.reduced = pool.r.squared(impute.me.reduced$models)	


	if (invert){
		1/bf
	} else {
		bf
	}


	### create a table
	results.table = data.frame(model=c("Full", "Reduced"), rsq = NA, BIC=NA, BF = NA, p=NA)
	results.table$rsq = c(rsq.full[1], rsq.reduced[1])
	results.table$BIC = c(bic.full, bic.reduced)
	results.table$BF[1] = bf
	results.table$p[1] = compared$pvalue

	return(results.table)	
}