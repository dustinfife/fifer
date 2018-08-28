##' Compare the fits of two models
##'
##' Compare the fits of two models
##'	
##' Compare the fits of two models
##' @param formula A formula that can be used in flexplot. The variables inside must not include variables outside the fitted models. 
##' @param data The dataset containing the variables in formula
##' @param model1 The fitted model object (e.g., lm) containing the variables specified in the formula
##' @param model2 The second fitted model object (e.g., lm) containing the variables specified in the formula
##' @author Dustin Fife
##' @export
##' @examples
##' #not yet
compare.fits = function(formula, data, model1, model2, ...){


	#### extract the terms from each model
	terms.mod1 = attr(model1$terms, "term.labels")
	terms.mod2 = attr(model2$terms, "term.labels")
		
	#### check if models have the same terms
	if (length(which(!(terms.mod1 %in% terms.mod2)))>0 | length(which(!(terms.mod2 %in% terms.mod1)))>0){
		stop("You must have the same predictors in both models to compare them.")
	}

	##### extract variable names
	variables = all.vars(formula)
    outcome = variables[1]
    predictors = variables[-1]
    

	#### create random column just to make the applies work (yeah, it's hacky, but it works)
    data$reject = 1:nrow(data); data$reject2 = 1:nrow(data)
    predictors = c(predictors, "reject", "reject2")

    #### get variable types
    numb = names(which(unlist(lapply(data[,predictors], is.numeric))))
    cat = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))
    
    ##### make "quadriture" points for quant variables
    var.mins = apply(data[, numb], 2, min, na.rm=T)
    var.max = apply(data[, numb], 2, max, na.rm=T)    
    min.max = data.frame(var.mins, var.max)
	f = function(d){seq(from=d[1], to=d[2], length.out=10)}
	min.max = as.list(as.data.frame((apply(min.max, 1, f))))

    #### get unique values for categorical vars
    if (length(cat)==1){
    	un.vars = lapply(data[cat], unique)    	
    } else {
		un.vars =lapply(data[,cat], unique); names(un.vars) = cat
	}

    
    #### combine into one dataset
    all.vars = c(min.max, un.vars)    
    #### get rid of extra variables
    tot.vars = length(predictors)
    rejects = grep("reject", names(all.vars))
	all.vars = all.vars[-rejects]
	pred.values = expand.grid(all.vars)

	##### look for interactions and remove them
	if (length(grep(":", terms.mod1))>0){
		terms.mod1 = terms.mod1[-grep(":", terms.mod1)]
	}
	
	#### if it's not in model 1:
	#### input the mean (if numeric) or a value (if categorical)
	if (length(which(!(terms.mod1 %in% predictors)))>0){
		not.in.there = terms.mod1[which(!(terms.mod1 %in% predictors))]
		for (i in 1:length(not.in.there)){
			if (is.numeric(data[,not.in.there[i]])){
				cat(paste0("Note: You didn't choose to plot ", not.in.there[i], " so I am inputting the median\n"))
				pred.values[,not.in.there[i]] = median(data[,not.in.there[i]], na.rm=T)
			} else {
				val = unique(data[,not.in.there[i]])[1]
				cat(paste0("Note: You didn't choose to plot ", not.in.there[i], " so I am inputting '", val, "'\n"))
				pred.values[,not.in.there[i]] = val
			}
		}
	}
### if it's binned, predict the midpoint of the binned variable

	#### generate predictions
	pred.mod1 = data.frame(prediction = predict(model1, pred.values, type="response"), model= subsetString(as.character(model1$call), "(", position=1)[1])
	pred.mod2 = data.frame(prediction = predict(model2, pred.values, type="response"), model = subsetString(as.character(model2$call), "(", position=1)[1])	
	prediction.model = rbind(pred.mod1, pred.mod2)
	prediction.model = cbind(pred.values, prediction.model)



	#### create flexplot
	flexplot(formula, data=data, prediction=prediction.model, suppress_smooth=T, se=F, ...)

}	