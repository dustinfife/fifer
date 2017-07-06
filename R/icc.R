##' Compute the ICC from a lmer (package lme4) model
##'
##' This function will extract the variances from a mixed model and output the value of the ICC
##'	
##' Recall that the equation for the ICC is var(school)/[var(school)+ var(person)]. This forumla extracts the necessary variances from the mixed model and computes it. 
##' @param model a lmer model
##' @return the ICC
##' @author Dustin Fife
##' @export
icc = function(model){
	var.components = as.data.frame(VarCorr(model))$vcov
	ICC = var.components[1]/sum(var.components)
	ICC
}