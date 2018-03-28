##' Compute standardized betas on a linear model object
##'
##' Compute standardized betas on a linear model object
##'	
##' Compute standardized betas on a linear model object
##' @param object a lm object
##' @return the standardized betas
##' @author Dustin Fife
##' @export
standardized.beta = function(object){
	b <- summary(object)$coef[, 1]
	sx <- apply(model.matrix(object), 2, sd)
    sy <- apply(object$model[1], 2, sd)
    beta <- b * sx/sy
    return(beta)
}