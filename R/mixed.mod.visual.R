##' Display a graphic of a mixed model
##'
##' Display a graphic of a mixed model
##'	
##' This function samples the ID variable and plots the bivariate relationship, depending on the mixed model chosen
##' @param formula A flexplot friendly formula, though it can only include one predictor
##' @param data The dataset
##' @param n The number of IDs to sample and put in different panels
##' @param jitter Should the data be jittered?
##' @return A plot
##' @author Dustin Fife
##' @export
mixed.mod.visual = function(formula, data, model, n=6, jitter=F){
	
	##### sample from the IDs
	form = formula(model)
	ID = subsetString(as.character(form)[3], "|", position=2); ID = gsub(" ", "", ID); ID = gsub(")", "", ID)
	all.IDs = as.character(unique(data[,ID]))
	if (n<length(all.IDs)){
		samp = sample(all.IDs, n)
	} else {
		stop(paste0("n can't be larger than the number of unique values of ", ID))
	}
	
	##### subset the data
	samp = sort(as.numeric(samp))
	rows = data[,ID] %in% samp
	d_new = data[rows,]	

	
	##### only allow one X and one Y 
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	if (length(predictors)>1){
		stop("Sorry, I can only plot one predictor variable at a time")
	}
	

	
	##### extract the rows of the random effects
	d2 = data[!duplicated(data$ID),]
	select.rows = which(d2[,ID] %in% samp)
	show.predicted = coef(model)$ID[select.rows,]
	names(show.predicted) = c("intercept", "predictor")
	show.predicted$ID = factor(row.names(show.predicted))

	##### jitter if needed
	if (jitter){
			jit = geom_jitter(width=.2, height=.2)
		} else {
			jit = geom_point()
	}
	fixed.slope = fixef(model)[2]
	fixed.intercept = fixef(model)[1]
		
	##### plot it
	ggplot(data=d_new, aes_string(predictors, outcome)) +
		jit +
		geom_abline(aes(slope=fixed.slope, intercept=fixed.intercept), col="red", size=2) +
		geom_abline(data=show.predicted, aes(slope=predictor, intercept=intercept), col="gray") +
		facet_wrap(ID, labeller = labeller(.cols=label_both)) +
		theme_bw()
		
}