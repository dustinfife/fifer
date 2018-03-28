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

#' Report regression object Estimates (effect sizes and parameters)
#'
#' Report regression object Estimates
#' @aliases estimates.lm estimates
#' @param object a lm object
#' @importFrom lmSupport modelEffectSizes
#' @importFrom lsmeans lsmeans
#' @export
estimates.lm = function(object){

	#### report R squared
	r.squared = summary(object)$r.squared
	t.crit = qt(.975, df=n-2)	
	se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
	r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
	r.squared = round(r.squared, digits=4)



	#### compute change in r squared
	semi.p = modelEffectSizes(object, Print=F)$Effects[-1,"dR-sqr"]
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
	factors = which(classes=="factor")
	numeric = which(classes=="numeric")
	predictors = attr(terms(object),"term.labels")
	#### extract lsmeans for factors
	if (length(factors)>0){
		lsmeans = lsmeans(object, predictors[factors])	
	}
	
	#### extract parameter estimates
	if (length(numeric)>0){
		rows = c(1, which(row.names(summary(object)$coef)==names(numeric)))
		betas = standardized.beta(object)[rows]
		params = data.frame(summary(object)$coef[rows,])
		params$beta =betas
		params = params[,c(1,5,2)]
		names(params) = c("Coef", "Standardized Beta", "SE")
	}	


	n = nrow(model.frame(object)) 
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
		cat(paste("Coefficients:\n"))
		print(params)
	}
	if (length(factors)>0){
		cat(paste("\n\nLS Means = \n"))
		print(lsmeans)		
	}
	cat(paste("\nsigma = ", round(summary(object)$sigma, digits=4)))
}