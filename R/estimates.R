#' Report Estimates (effect sizes and parameters)
#'
#' Report object Estimates
#' @aliases estimates.regression estimates estimates.ttest estimates.default
#' @param object a object
#' @export
estimates = function(object,...){
	UseMethod("estimates")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @aliases report.regression report report.ttest report.default
#' @param object a object
#' @export
estimates.default = function(object){
	out = coef(object)
	class(out) = "estimates"
	out
}

#' Report regression object Estimates (effect sizes and parameters)
#'
#' Report regression object Estimates
#' @aliases estimates.regression estimates
#' @param object a regression object
#' @export
estimates.regression = function(object){
	file.name = deparse(substitute(object))
	cat(paste("R squared:\n", round(object$R.squared[1], digits=3), " (", round(object$R.squared[2], digits=2),", ", round(object$R.squared[3], digits=2),")\n\nParameter Estimates:\n",sep=""))
	print(object$Estimates)
	cat(paste("\n\nr = ", round(object$r, digits=4), "\nsigma = ", round(object$Sigma, digits=4)))
}


#' Report glm object Estimates (effect sizes and parameters)
#'
#' Report glm object Estimates
#' @aliases estimates.glm estimates
#' @param object a glm object
#' @export
estimates.glm = function(object){
	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	#### get dataset
	d = object$model
	
	#### identify factors
	if (length(terms)>1){
		factors = names(which(unlist(lapply(d[,terms], is.factor))));
		numbers = names(which(unlist(lapply(d[,terms], is.numeric))));
	} else {
		factors = terms[which(is.factor(d[,terms]))]
		numbers = terms[which(is.numeric(d[,terms]))]
	}

	#### output predictions
	n.func = function(term){anchor.predictions(object, term, shutup=T)$prediction}
	preds = lapply(terms, n.func); names(preds) = terms
	
	
	#### output coefficients
	options(warn=-1)
	coef.matrix = data.frame(raw.coefficients = coef(object), OR = exp(coef(object)), inverse.OR = 1/exp(coef(object)), standardized.OR = exp(standardized.beta(object, sd.y=F)), inverse.standardized.OR = 1/exp(standardized.beta(object, sd.y=F)))
	options(warn=0)
	coef.matrix[numbers,"Prediction Difference (+/- 1 SD)"] = sapply(preds[numbers], function(x){abs(round(x[2]-x[1], digits=2))})
	
	
	#### for those that are factors, put the first prediction in the -1 SD column
	string.round = function(x, digits){
		return.val = ifelse(round(x, digits)==0, paste0("<0.", rep(0, times=digits-1), "1"), round(x, digits=digits))
		return.val
	}


	if (length(factors)>0){
	for (i in 1:length(factors)){
		current.pre = preds[factors[i]]
		levs = levels(d[,factors[i]]); levs = paste0(factors[i], levs)
		coef.matrix[levs[-1],"Prediction Difference (+/- 1 SD)"] = paste0(string.round(unlist(current.pre)[-1] - unlist(current.pre)[1], digits=2), " (relative to ", levs[1], ")")
		
		if (length(factors)==1){
			coef.matrix[1,"Prediction Difference (+/- 1 SD)"] = paste0(string.round(unlist(current.pre)[1], digits=2), " (", levs[1], " prediction)")
			row.names(coef.matrix)[1] = levs[1]
		}
	}}
	coef.matrix
}

#' Report regression object Estimates (effect sizes and parameters)
#'
#' Report regression object Estimates
#' @aliases estimates.lm estimates
#' @param object a lm object
#' @importFrom lsmeans lsmeans
#' @export
estimates.lm = function(object){

	n = nrow(model.frame(object)) 
	
	#### report R squared
	r.squared = summary(object)$r.squared
	t.crit = qt(.975, df=n-2)	
	se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
	r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
	r.squared = round(r.squared, digits=4)



	#### compute change in r squared
	ssr = drop1(aov(object))[-1,"Sum of Sq"]
	if (length(ssr)<(nrow(anova(object))-1)){
		cat("Note: I am not reporting the semi-partial R squared for the main effects because an interaction is present. To obtain main effect sizes, drop the interaction from your model. \n\n")
	}
	sst = sum(anova(object)[,"Sum Sq"])
	semi.p = ssr/sst	
	max = nrow(anova(object))-1
	min = max-length(semi.p)+1
	nms = row.names(anova(object))[min:max]	
	names(semi.p) = nms
	
	# z = 1.96
	# mult0 = r.squared[1] - semi.p 
	# zr <- log((1 + sqrt(semi.p))/(1 - sqrt(semi.p)))/2
	# a <- (r.squared[1]^2 - 2*r.squared[1] + mult0 - mult0^2 + 1)/(1 - sqrt(semi.p)^2)^2
	# se <- sqrt(a/(n - 3))
	# LL0 <- zr - z*se
 	# UL0 <- zr + z*se
 	# LL <- (exp(2*LL0) - 1)/(exp(2*LL0) + 1)
 	# UL <- (exp(2*UL0) - 1)/(exp(2*UL0) + 1)
 	# semi.p = data.frame(semi.partial = semi.p, lower=LL^2, upper=UL^2)


	##### compute ls means
		#### figure out what is numeric
	classes = attr(terms(object), "dataClasses")[-1]

		#### extract which variables are factors and which are numeric
	factors = which(classes=="factor" | classes=="ordered")
	numeric = which(classes=="numeric")
	predictors = attr(terms(object),"term.labels")
	#### extract lsmeans for factors
	if (length(factors)>0){
		lsmeans = lsmeans(object, predictors[factors])	
	}
	
	#### extract parameter estimates
	if (length(numeric)>0){
		rows = c(1, which(row.names(summary(object)$coef)%in%names(numeric)))
		betas = standardized.beta(object)[rows]
		params = data.frame(summary(object)$coef[rows,])
		params$beta =betas
		params = params[,c(1,5,2)]
		names(params) = c("Coef", "Standardized Beta", "SE")
	}	


	#### report R squared
	r.squared = summary(object)$r.squared
	t.crit = qt(.975, df=n-2)	
	se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
	r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
	r.squared = round(r.squared, digits=4)

	#### print summary
	cat(paste("Model R squared:\n", round(r.squared[1], digits=3), " (", round(r.squared[2], digits=2),", ", round(r.squared[3], digits=2),")\n\nSemi-Partial R squared:\n",sep=""))
	print(semi.p)
	if (length(numeric)>0){
		cat(paste("\nCoefficients:\n"))
		print(params)
	}
	if (length(factors)>0){
		cat(paste("\n\nLS Means = \n"))
		print(lsmeans)		
	}
	cat(paste("\nsigma = ", round(summary(object)$sigma, digits=4), "\n\n"))
	
	ret = list(r.squared=r.squared, semi.p=semi.p, params=params, lsmeans=lsmeans)
	return(ret)
}