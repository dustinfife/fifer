##' Perform a model comparison
##'
##' Perform a model comparison
##'	
##' This function takes two models (either nested or not nested) and compares them in terms of AIC, BIC,
##' bayes factor, and the p-value
##' @param model1 the first model to be compared
##' @param model2 the second model to be compared
##' @author Dustin Fife
##' @export
model.comparison = function(model1, model2){
	
	#### collect terms
	mod1 = attr(terms(model1), "term.labels")
	mod2 = attr(terms(model2), "term.labels")	
	
	#### check for nested models
	if (all(length(mod1)>length(mod2) & (mod2 %in% mod1))){
		nested = T
	} else if (all(length(mod2)>length(mod1) & mod1 %in% mod2)) {
		nested = T
	} else {
		nested = F
	}
	
	if (nested){
		anova.res = anova(model1, model2)
		p = 1-pchisq( abs(anova.res$Deviance[2]), abs(anova.res$Df[2]))
	} else {
		p = NA
	}
	
	m1.name = deparse(substitute(model1))
	m2.name = deparse(substitute(model2))	
	aic = c(AIC(model1), AIC(model2))
	bic = c(BIC(model1), BIC(model2))
	bayes.factor = bf.bic(model1, model2)
	
	model.table = data.frame(aic=aic, bic=bic, bayes.factor=c(bayes.factor, NA), p.value=c(p, NA))
	row.names(model.table) = c(m1.name, m2.name)

	mod1.predictions = sensitivity.table(model1)
	mod2.predictions = sensitivity.table(model2)	
	
	predictions = data.frame(rbind(unlist(mod1.predictions), unlist(mod2.predictions))); row.names(predictions) = c(m1.name, m2.name)
	if (length(mod1)>length(mod2)){
		difference = unlist(predictions[1,]) - unlist(predictions[2,])
	} else {
		difference = predictions[2,] - predictions[1,]
	}
	predictions = data.frame(rbind(predictions, difference)); row.names(predictions)[3] = "Difference"

	list(statistics=model.table, predictions=predictions)
}

##' Compute sensitivity/specificity/etc. 
##'
##' Compute sensitivity/specificity/etc. 
##'	
##' This function computes sensitivity, specificity, positive/negative predictive value and accuracy and reports
##' them as a list. 
##' @param an object that can be predicted (e.g., glm). Note the thing to be predicted must have only two outcomes
##' @author Dustin Fife
##' @export
sensitivity.table = function(object){
	predmat = table(Observed = object $model[,1], Predicted=round(predict(object, type="response")))
	TP = predmat[2,2]
	FP = predmat[2,1]
	TN = predmat[1,1]
	FN = predmat[1,2]
	sens = TP/(TP+FN)
	spec = TN/(TN + FP)
	ppv = TP/(FP+TP)
	npv = TN/(TN+FN)
	acc = (TP+TN)/(TP+FP+TN+FN)
	list(acc=acc,sens=sens, spec=spec, ppv=ppv, npv=npv)
}