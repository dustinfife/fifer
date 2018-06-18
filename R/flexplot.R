d = read.csv("research/Rowan/Dinzeo/social_functioning/data/social_functioning_cleaned.csv")



formula = formula(social.functioning~schizo + neg.affect | problem.focused + Age )
data = d
labels = list(NULL, c("emotion focused", "neutral", "problem focused"), c("18-19", "20-21", "22+"))
bins = c(4,3,3)
method="lm"
se=F
##' Insert title here
##'
##' Write the description here
##'	
##' Formula takes the form of y~x + a | b + z. Everything on the right of | will occur in panels (in this case, b will be in the columns and z will be in the rows). Any numeric
##' variables between ~ and | will fall on the x axis and categorical predictors will be color-coded/symbol-coded/line-coded (if user specifies). No more than four variables are allow
##' (otherwise, cognitive load is too high). If the user wishes to include another variable, I recommend they create separate plots, one for each level of the new variable (or a binned level
##' of a numeric variable). 
##' @param
##' @param
##' @aliases
##' @seealso \code{\link{MASS}}
##' @references
##' @return \item{}{}
##' @author
##' @export
##' @examples
#flexplot = function(formula, data, color=NULL, symbol=NULL, linetype=NULL, bins = 4, labels=NULL, method="loess", se=T){
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	
		#### remove missing values
	if (length(unlist(apply(data[,c(outcome, predictors)], 2, function(x){(which(is.na(x)))})))>0){
		data = na.omit(data)
	}

	#### make sure outcome is continuous
	if (!is.numeric(data[,outcome])){
		stop("Your outcome variable must be numeric.")
	}
	
	#### identify categorical/numeric predictors
	numbers = names(which(unlist(lapply(data[,predictors], is.numeric))))
	categories = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))
	
	##### identify the "given" variables
	given = unlist(strsplit(as.character(formula[[3L]])[3], " + ", fixed=T))
	if (length(given)>2){
		stop("Only two 'given' variables are allowed.")
	}
	given.as.string = paste0(given, collapse="~")
	
	##### identify the non given variables
	axis = unlist(strsplit(as.character(formula[[3L]])[2], " + ", fixed=T))	
	
	
	#### repeat "bins" the number of times there are given variables
	
	
	#### identify the number of binned variables we need
	if (length(axis)>1 & axis[2] %in% numbers){ 
		binned.vars = c(axis[2], numbers[which((numbers) %in% given)])
	} else {
		binned.vars = numbers[which((numbers) %in% given)]
	}

	msg = paste0("The following variables are going to be binned: ", paste0(binned.vars, collapse=", "))
	cat(msg)
	
	#### repeat the bins the number of bins there are
	if (length(bins) != length(binned.vars) & length(bins)>1){
		warning("You haven't specified enough bins to cover all the binned variables. I'm making a guess for the rest of the variables")
		bins = matrix(bins, nrow=1, ncol=length(binned.vars))
	}
	
	if (length(bins)==1){
		bins = rep(bins, times=length(binned.vars))
	}

	##### bin the binned variables
	for (i in 1:length(binned.vars)){
		if (!is.null(unlist(labels[i])) & length(unlist(labels[i])) != bins[i]){
			stop(paste0("The label vectors (", paste0(unlist(labels[i]), collapse=", "), ") is not the same length as the bin length (", bins[i], ")", sep=""))
		}
		quants = quantile(data[,binned.vars[i]], seq(from=0, to=1, length.out=bins[i]+1))
		data[,binned.vars[i]] = cut(data[,binned.vars[i]], quants, labels= unlist(labels[i]), include.lowest=T, include.highest=T)
		
	}

	
	if (length(axis)>1){
		p = ggplot(data=data, aes_string(axis[1], outcome, symbol=axis[1], linetype=axis[2])) 
	} else {
		p = ggplot(data=data, aes_string(axis[1], outcome)) 
	}
	p + geom_point(alpha=.1) + 
			geom_smooth(method=method, se=se) + 
			facet_grid(given.as.string) +
			theme_bw()
#}
