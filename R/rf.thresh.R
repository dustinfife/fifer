##' Using a set of predictors, this function uses random forests to select the best ones in a stepwise fashion. Both the procedure and
##' the algorithm were borrowed heavily from the \code{VSURF} package with some modifications. These modifications allow for unbiased
##' computation of variable importance via the \code{\link{cforest}} function in the party package. 
##'
##' @details What follows is the documentation for the original algorithm in VSURF: 
##'
#' Three steps variable selection procedure based on random forests for
#' supervised classification and regression problems.  First step
#' ("thresholding step") is dedicated to eliminate irrelevant variables from
#' the dataset.  Second step ("interpretation step") aims to select all
#' variables related to the response for interpretation prupose.  Third step
#' ("prediction step") refines the selection by eliminating redundancy in the
#' set of variables selected by the second step, for prediction prupose.
#' 
#' \itemize{ \item First step ("thresholding step"): first, \code{nfor.thres}
#' random forests are computed using the function \code{randomForest} with
#' arguments \code{importance=TRUE}. Then variables are sorted according to
#' their mean variable importance (VI), in decreasing order.  This order is
#' kept all along the procedure. Next, a threshold is computed:
#' \code{min.thres}, the minimum predicted value of a pruned CART tree fitted
#' to the curve of the standard deviations of VI.  Finally, the actual
#' "thresholding step" is performed: only variables with a mean VI larger than
#' \code{nmin} * \code{min.thres} are kept.
#' 
#' \item Second step ("intepretation step"): the variables selected by the
#' first step are considered. \code{nfor.interp} embedded random forests models
#' are grown, starting with the random forest build with only the most
#' important variable and ending with all variables selected in the first step.
#' Then, \code{err.min} the minimum mean out-of-bag (OOB) error of these models
#' and its associated standard deviation \code{sd.min} are computed.  Finally,
#' the smallest model (and hence its corresponding variables) having a mean OOB
#' error less than \code{err.min} + \code{nsd} * \code{sd.min} is selected.
#' 
#' \item Third step ("prediction step"): the starting point is the same than in
#' the second step. However, now the variables are added to the model in a
#' stepwise manner. \code{mean.jump}, the mean jump value is calculated using
#' variables that have been left out by the second step, and is set as the mean
#' absolute difference between mean OOB errors of one model and its first
#' following model.  Hence a variable is included in the model if the mean OOB
#' error decrease is larger than \code{nmj} * \code{mean.jump}.  }
##' @title Variable Selection Using Random Forests
##' @param formula a formula, such as \code{y~x1 + x2}, where \code{y} is the response variable and anything following \code{~} are predictors. 
##' @param data the dataset containing the predictors and response. 
##' @param nruns How many times should random forests be run to compute variable importance? Defaults to 50.
##' @param silent Should the algorithm talk to you?
##' @param importance Either "permutation" or "gini." 
##' @param nmin Number of times the "minimum value" is multiplied to set
##' threshold value. 
##' @param ... other arguments passed to \code{\link{cforest}} or \code{\link{randomForest}}
#'@return The object returned has the following
#'attributes:
#'@return \item{variable.importance}{A sorted vector of each variable importance measures.}
#'@return \item{importance.sd}{the standard deviation of variable importance, measured across the \code{nruns} iterations. }
#'@return \item{stepwise.error}{The OOB error after each variable is added to the model}
#'@return \item{response}{The response variable that was modeled.}
#'@return \item{variables}{A vector of strings that indicate which variables were included in the initial model.}
#'@return \item{nruns}{How many times the random forest was initially run.}
#'@return \item{formula}{the formula used for the last model.}
#'@return \item{data}{the dataset used to fit the model.}
#'@return \item{oob}{the oob error of the entire model.}
#'@return \item{time}{how long the algorithm ran for}
#'@return \item{rfmodel}{The final model used, a randomForest object.}
#' @importFrom party cforest
#' @importFrom party cforest_control
#' @importFrom party varimp
#' @importFrom randomForest randomForest
#' @importFrom randomForestSRC var.select
#' @importFrom rpart rpart
#' @importFrom rpart rpart.control
#' @importFrom rpart prune
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot, with modifications by Dustin Fife
#' @references 
#' Genuer, R. and Poggi, J.M. and Tuleau-Malot, C. (2010), Variable
#' selection using random forests, Pattern Recognition Letters 31(14),
#' 2225-2236
#' Carolin Strobl, Anne-Laure Boulesteix, Achim Zeileis, and Torsten Hothorn. Bias in random forest variable importance measures: Illustrations, sources and a solution. 
#' BMC Bioinformatics, 8(1): 1-21, 2007. doi: 10.1186/1471-2105-8-25. URL http://dx.doi.org/10.1186/1471-2105-8-25.
#' @rdname rfThresh
#' @export rfThresh
#' @seealso \code{\link{rfInterp}}, \code{\link{rfPred}}
# data(iris); data = iris; formula = as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"); nruns=5; silent=FALSE; importance="gini";nmin=1
rfThresh = function(formula, data, nruns = 50, silent=FALSE, importance="permutation", nmin=1,...){
	
	#### extract varaibles and response from formula
	vars = attr(terms(formula), "term.labels")
	resp = row.names(attr(terms(formula), "factors"))[1]
	
	#### keep us posted
	if (!silent){
		cat(paste("Running Random Forests", nruns, "Times\n"))
	}
	
	start = Sys.time()

	
	##### check importance
	importance = match.arg(importance, c("permutation", "gini", "minDepth",NULL))

	##### loop through and compute variable importance nruns times
	preds = data.frame(matrix(nrow=nruns, ncol=length(vars)))
	names(preds) = vars
	oob = 1:length(nruns)	

	##### compute importance 50 times
	if (importance=="gini"){
		for (i in 1:nrow(preds)){
			rf = randomForest(formula, data=data, importance=TRUE,...)
			preds[i,] = rf$importance[, ncol(rf$importance)-1]
			if (rf$type=="regression"){
				oob[i] =  tail(rf$mse, n=1)
			} else {
				oob[i] =  tail(rf$err.rate[,1], n=1)
			}
		}
	} else if (importance=="permutation"){
		for (i in 1:nrow(preds)){
			


			mt = sqrt(length(vars))
			
			##### add controls based on Strobl et al, 2007 (see http://www.biomedcentral.com/content/supplementary/1471-2105-8-25-S1.R)
			my_cforest_control <- cforest_control(teststat = "quad",
			    testtype = "Univ", mincriterion = 0, ntree = 1000, mtry = mt,
			    replace = FALSE)			
			rf = cforest(formula, data=data, controls= my_cforest_control,...)

				##### compute variable importance
			imp = varimp(rf,...)
			preds[i,] = imp
			oobError = predict(rf, OOB=T)
			oob[i] = 1-length(which(oobError==data[,resp]))/length(data[, resp])
		}
	} else if (importance=="minDepth"){
		thresh = 1:nrow(preds)
		for (i in 1:nrow(preds)){
			rf_mindepth = var.select(formula, data=data, method="md", conservative="high")
			varimp = rf_mindepth$threshold
			
			#### extract order of variables (because the stupid algorithm sorts it by var imp)
			ord.depth = unlist(lapply(row.names(rf_mindepth$varselect), function(x){which(vars==x)}))
			varimp = rf_mindepth$varselect[ord.depth,1]
			thresh[i] = rf_mindepth$md.obj$threshold
			oob[i] = rf_mindepth$err.rate[1]
			preds[i,] = varimp
		}
	}
	
	vimp = colMeans(preds)
	vimp.sd = apply(preds, 2, sd)
	
	#### reorder
	ord = order(vimp, decreasing=T)
	ord.sd = vimp.sd[ord]
	ord.imp = vimp[ord]
	
	#### threshold
	if (importance=="minDepth"){
		s = length(which(vimp<mean(thresh)))
	} else {
	
		s <- NULL
		if (length(vars)==1) {
			s <- 1
		} else {
		
			p <- length(vars)
			u <- 1:p
			u <- as.data.frame(u)
	
			# estimation of the standard deviations curve with CART (using "rpart" package)
	
			# construction of the maximal tree and search of optimal complexity
			tree <- rpart(ord.sd ~., data=u, control=rpart.control(cp=0, minsplit=2))
			d <- tree$cptable
			argmin.cp <- which.min(d[,4])
			
			# pruning
			pruned.tree <- prune(tree, cp=d[argmin.cp, 1])
			pred.pruned.tree <- predict(pruned.tree)
			
			# determination of the y-value of the lowest stair: this is the estimation
			# of the mean standard deviation of IV
			min.pred <- min(pred.pruned.tree)
			
			# thresholding: all variables with IV mean lower than min.pred are discarded
			w <- which(ord.imp < nmin*min.pred)
			
			if (length(w)==0) {
			  s <- p
			}
			else {
			  s <- min(w)-1
			}
		}
	
	}
	
	formula = make.formula(resp, names(ord.imp[1:s]))
	if (importance!="permutation"){
		model = randomForest(formula, data=data, importance=TRUE,...)
	} else {			
		my_cforest_control <- cforest_control(teststat = "quad",
		    testtype = "Univ", mincriterion = 0, ntree = 1000, mtry = mt,
		    replace = FALSE)			
		model = cforest(formula, data=data, controls= my_cforest_control,...)
	}
	
	time = Sys.time()-start

	##### make a list to store important info
	ret = list(importance.mean=ord.imp[1:s], importance.all=ord.imp, importance.sd=ord.sd[1:s], 
			response=resp, remaining.variables=names(ord.imp)[1:s], nruns=nruns, formula = formula,
			data=data, oob=oob, time=time, all.vars=names(ord.imp), model=model)
	
	##### make into proper class
	attr(ret, "class") = c("rfThresh")
	
	return(ret)
}



#' Print rfThesh Summary
#'
#' Print a rfThresh object
#' @aliases print.rfThresh
#' @param x an rfThresh object
#' @param ... ignored
#' @export
print.rfThresh = function(x,...){
	print(names(x))
	cat(paste("\n\nThe best variables (in order of importance) were:\n\n", sep=""))
	print(sort(x$importance.mean, decreasing=TRUE))
		cat(paste("\n\n"))
	print(x$time)
}	

#' rfThesh Summary
#'
#' Plot a rfThresh object
#' @aliases plot.rfThresh
#' @param x an rfThresh object
#' @param y igorned
#' @param ... other parameters passed to plo
#' @export
plot.rfThresh = function(x, y, ...){
	length.vars = length(x$importance.mean)
	var = x$importance.sd
	vimp = x$importance.mean
	labels = list(xlab="", ylab="Variable Importance", pch=16, cex=.8, xlim=range(.5, length.vars+.5), xaxt="n") 
	args = modifyList(labels, list(x=1:length.vars, y=vimp,...))
	do.call("plot", args)	
	text(1:length.vars, vimp, names(vimp), pos=4, cex=1.2)
	segments(1:length.vars, vimp+1.96*var, 1:length.vars, vimp-1.96*var, col="lightgray")
}
