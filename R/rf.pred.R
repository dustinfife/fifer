##' Variable selection for prediction purposes using Random Forest. See \code{\link{rfThresh}} for complete documentation.
##'
##' @title Variable selection in Random Forest
##' @param object an object returned from \code{\link{rfInterp}}
##' @param importance what importance measure should be used? Either "permutation" or "gini."
##' @param nfor.pred number of forests to grow
##' @param nmj a contant used for setting the threshold for variable selection. Higher values indicate a less stringent threshold. 
##' @param outfile The file location where the rfPred object should be stored. Defaults to storing it in rfPred.file in the default directory.
##' @param named.file What should the rfPred object be named when saved? Defaults to "rfPredResults".
##' @param ... other arguments passed to \code{\link{cforest}} or \code{\link{randomForest}}
#'@return \item{varselect.pred}{The variables selected for Prediction (sorted)}
#'@return \item{err.interp}{The error at each stage of the stepwise variable inclusion.}
#'@return \item{mean.jump}{The threshold for variable inclusion.}
#'@return \item{stepwise.error}{The OOB error rate at each iteration of the stepwise procedure.}
#'@return \item{num.varselect.pred}{The number of variables selected for prediction.}
#'@return \item{comput.time}{Computation time of the procedure.}
#'@return \item{model}{The final model, either a \code{randomForest} or \code{cforest} object.}
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot, with modifications by Dustin Fife
#' @seealso \code{\link{rfInterp}}, \code{\link{rfThresh}}
#' @rdname rfPred
#' @export rfPred
##' @examples
#' \dontrun{data(iris); 
#' data = iris; 
#' formula = as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
#' thresh = rfThresh(formula, data=iris, nruns=2, importance="permutation"); 
#' interp = rfInterp(thresh, importance="permutation");
#' predic = rfPred(interp, importance="gini")
#' predic}
rfPred <-function(object, importance="permutation", nfor.pred=25, nmj=1, outfile="rfPred.file", named.file="rfPredResults",...){


	### record system time at beginning
	start = Sys.time()
	formula = object$formula
			
	##### extract variable names and OOB values	
	err.interp = object$err.interp
	varselect.interp = object$varselect.interp
	data = object$data
				
	##### extract IV/DV
	x.lab = attr(terms(formula), "term.labels")
	x = data[,x.lab]
	y.lab = row.names(attr(terms(formula), "factors"))[1]
	y = data[,y.lab]
	
	##### extract other things
	vars = attr(terms(formula), "term.labels")
	

	#### get sample size and number of variables
	nvars = length(vars)
	n = nrow(x)



	k <- length(err.interp)
	l <- length(varselect.interp)

	if (l==1) {
		warning(
		"Unable to perform prediction step, because the interpretation step
		did not eliminate variables")
		varselect.pred <- NULL
		err.pred <- NULL
		mean.jump <- NULL
	} else {

		# mean jump calculation
		s=NULL
		for (i in l:(k-1)){
			s <- c(s, abs(err.interp[i+1] - err.interp[i]) )
		}
		
		mean.jump <- mean(s, na.rm=T)

		# comparison between the error with the variable and the precedent error
		# and test of the addition of the variable
		rf <- rep(NA, nfor.pred)

		##### fit it for only first variable
		if (importance=="permutation"){		
			for (j in 1:nfor.pred) {
				y = row.names(attr(terms(formula), "factors"))[1]
				f = make.formula(y, vars[1])
				rfmod = cforest(f, data=data, controls=cforest_control(mtry=1), ...)
				oob = predict(rfmod, OOB=T); oob = 1-length(which(oob==data[,y]))/length(data[,y], ...)
				rf[j] <- oob
			}
		} else {
			for (j in 1:nfor.pred) {
				y = row.names(attr(terms(formula), "factors"))[1]
				f = make.formula(y, vars[1])
				if (object$model$type=="regression"){
					rf[j] <- tail(randomForest(f, data=data, ...)$mse, n=1)
				} else {
					rf[j] <- tail(randomForest(f, data=data, ...)$err.rate[,1], n=1)					
				}
			}
		}
		err.pred <- mean(rf)


		t <- err.pred
		
		stepwise.error = 1:l
		stepwise.error[1] = t

		### if there's more than one variable
		if (l>1) { 
			
			#### sequentially add variables
			varselect.pred = vars[1]
			
			for (i in 2:l){	
				form.1 = make.formula(y.lab, c(varselect.pred, vars[i]))
				#### preallocate
				rf <- rep(NA, nfor.pred)
				
				#### fit just this set of variables nfor.pred times
				for (j in 1:nfor.pred) {
					if (importance=="permutation"){
						rfor = cforest(form.1, data=data, controls=cforest_control(mtry=sqrt(i)), ...)
						oob = predict(rfor, OOB=T, ...); oob = 1-length(which(oob==data[,y.lab]))/length(data[,y])
						rf[j] = oob
					} else {
						if (object$model$type=="regression"){
							rf[j] <- tail(randomForest(form.1, data=data, ...)$mse, n=1)
						} else {
							rf[j] <- tail(randomForest(form.1, data=data, ...)$err.rate[,1], n=1)					
						}
					}	
				}
				z <- mean(rf)
				
				### record error at this iteration
				stepwise.error[i] = z
			
				if ((t-z) > nmj*mean.jump){
					varselect.pred <- c(varselect.pred, vars[i])
					err.pred <- c(err.pred, z)
					t <- z
				}				

			}
		}
	}
	formula = make.formula(y.lab, varselect.pred)
	if (importance=="gini"){
		model = randomForest(formula, data=data, importance=TRUE,...)
	} else {
		model = cforest(formula, data=data, controls=cforest_unbiased(ntree=1000, mtry=sqrt(length(varselect.pred))),...)
	}
		
	comput.time <- Sys.time()-start

	
	output <- list(
	'varselect.pred'=varselect.pred,
	'vars.considered' = vars,
	 'err.pred'=err.pred,
	 'stepwise.error' = stepwise.error,
	 'mean.jump'=mean.jump,
	 'num.varselect.pred'=length(varselect.pred),
	 'comput.time'=comput.time,
	 'model' = model,
	 importance = importance)
	attr(output, "class") = "rfPred"
	assign(named.file, output)
	save(list = named.file, file=outfile)	
	return(output)

}



#' Print rfPred Summary
#' 
#' Print rfPred Summary
#' @aliases print.rfPred
#' @param x an rfPred object
#' @param ... ignored
#' @export
print.rfPred = function(x,...){
	print(names(x))
	cat(paste("\n\nThe remaining variables (in order of importance) are:\n\n", sep=""))
	print(sort(x$varselect.pred, decreasing=TRUE))
	cat(paste("\n\n"))	
	print(x$comput.time)
}	


#' Prepare xtable Summary
#'
#' Print xtable Summary
#' @aliases xtable.rfPred
#' @param x an rfPred object
#' @param caption Character vector of length 1 or 2 containing the table's caption or title. If length 2, the second item 
#' is the "short caption" used when LaTeX generates a "List of Tables". Set to NULL to suppress the caption. Default value is NULL.	
#' @param label Character vector of length 1 containing the LaTeX label
#'    or HTML anchor. Set to \code{NULL} to suppress the label.  Default
#'    value is \code{NULL}. 
#' @param align Character vector of length equal to the number of columns
#'     of the resulting table indicating the alignment of the corresponding
#'     columns.  Also, \code{"|"} may be used to produce vertical lines
#'     between columns in LaTeX tables, but these are effectively ignored
#'     when considering the required length of the supplied vector.  If a
#'     character vector of length one is supplied, it is split as
#'     \code{strsplit(align, "")[[1]]} before processing. Since the row
#'     names are printed in the first column, the length of \code{align} is
#'     one greater than \code{ncol(x)} if \code{x} is a
#'     \code{data.frame}. Use \code{"l"}, \code{"r"}, and \code{"c"} to
#'     denote left, right, and center alignment, respectively.  Use
#'     \code{"p\{3cm\}"} etc for a LaTeX column of the specified width. For
#'     HTML output the \code{"p"} alignment is interpreted as \code{"l"},
#'     ignoring the width request. Default depends on the class of
#'     \code{x}. 
#' @param digits Numeric vector of length equal to one (in which case it will be
#'    replicated as necessary) or to the number of columns of the
#'    resulting table \bold{or} matrix of the same size as the resulting
#'    table indicating the number of digits to display in the
#'    corresponding columns. Since the row names are printed in the first
#'    column, the length of the vector \code{digits} or the number of
#'    columns of the matrix \code{digits} is one greater than
#'    \code{ncol(x)} if \code{x} is a \code{data.frame}. Default depends
#'    of class of \code{x}. If values of \code{digits} are negative, the
#'    corresponding values of \code{x} are displayed in scientific format
#'    with \code{abs(digits)} digits.
#' @param display Character vector of length equal to the number of columns of the
#'    resulting table indicating the format for the corresponding columns.
#'    Since the row names are printed in the first column, the length of
#'    \code{display} is one greater than \code{ncol(x)} if \code{x} is a
#'    \code{data.frame}.  These values are passed to the \code{formatC}
#'    function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
#'    \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
#'    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
#'    \code{xxx.xxx} format; \code{"e"} and \code{"E"} give
#'    \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format); \code{"g"}
#'    and \code{"G"} put \code{x[i]} into scientific format only if it
#'    saves space to do so.  \code{"fg"} uses fixed format as \code{"f"},
#'    but \code{digits} as number of \emph{significant} digits.  Note that
#'    this can lead to quite long result strings.  Default depends on the
#'    class of \code{x}.
#' @param ... other arguments passed to xtable
#' @export
#' @importFrom xtable xtable
xtable.rfPred = function(x,caption=NULL, label=NULL, align=NULL, digits=NULL, display=NULL,...){
	tab = data.frame(matrix(nrow=length(x$vars.considered), ncol=3))
	names(tab) = c("Current Variable", "OOB Error", "Model Selected")
	tab[,1] = x$vars.considered
	tab[,2] = x$stepwise.error
	selected = x$varselect.pred
	
	#### if only one is selected, populate entire table
	if (length(selected)==1){
		tab[,3] = selected
	} else {

		#### give first row the first variable
		tab[1,3] = selected[1] 
		sel.string = selected[1]
		
		s=2
		#### loop through and add only if it's included
		for (i in 2:nrow(tab)){
			if (s>length(selected)){
				tab[(i:nrow(tab)),3] = sel.string
			} else {
				if (tab[i,1] == selected[s]){
					sel.string = paste(sel.string, "+", selected[s])
					s = s+1
				}
				tab[i,3] = sel.string
			}

		}
	}
	
	xtable(tab, caption=caption, label=label, align=align, digits=digits, display=display, ...)

}

#' Print a Summary Table of rfPred
#'
#' Print a Summary Table of rfPred
#' @description summary.rfPred is best for those non-Latex users to produce a table
#' that shows each stage of the variable selection algorithm.
#' @aliases summary.rfPred
#' @param object an rfPred object
#' @param ... additional arguments affecting the summary produced.
#' @export 
summary.rfPred = function(object, ...){
	tab = data.frame(matrix(nrow=length(object$vars.considered), ncol=3))
	names(tab) = c("Current Variable", "OOB Error", "Model Selected")
	tab[,1] = object$vars.considered
	tab[,2] = object$stepwise.error
	selected = object$varselect.pred
	
	#### if only one is selected, populate entire table
	if (length(selected)==1){
		tab[,3] = selected
	} else {

		#### give first row the first variable
		tab[1,3] = selected[1] 
		sel.string = selected[1]
		
		s=2
		#### loop through and add only if it's included
		for (i in 2:nrow(tab)){
			if (s>length(selected)){
				tab[(i:nrow(tab)),3] = sel.string
			} else {
				if (tab[i,1] == selected[s]){
					sel.string = paste(sel.string, "+", selected[s])
					s = s+1
				}
				tab[i,3] = sel.string
			}

		}
	}
	tab
}
	
	
#' Plot rfPred Summary
#'
#' Plot rfPred Summary
#' @aliases plot.rfPred
#' @param x an rfPred object
#' @param y igorned
#' @param ... other parameters passed to plo
#' @export
plot.rfPred = function(x, y, ...){
	length.vars = length(x$varselect.pred)

	labels = list(ylab="Stepwise OOB Error", xlab="", pch=16, cex=.8,col="gray", type="b", xaxt="n") 
	args = modifyList(labels, list(x=1:length.vars, y=x$err.pred[1:length.vars],...))
	do.call("plot", args)
#	plot(1:length.vars, x$err.pred[1:length.vars], ylab="Stepwise OOB Error", xlab="", pch=16, cex=.8,col="gray", type="b",...)
	text(1:length.vars, x$err.pred[1:length.vars], x$varselect.pred, pos=4, cex=.8)
}

##' Output accuracy, sensitivity, specificity, NPV, and PPV. 
##'
##' @param object An rfPred object
##' @return A list containing the accuracy, sensitivity, etc. 
##' @export
rfSensitivity = function(object){
	
	if (object$importance=="permutation"){
		Observed = attr(object$model@responses,"variables")[,1]
		Predicted = predict(object$model, OOB=T)
	} else {
		Observed = object$model$y
		Predicted = object$model$predicted 
	}
	predmat = table(Observed=Observed, Predicted=Predicted)
	if (nrow(predmat)>2){
		warning("You can only compute sensitivity when groups=3. Computing just accuracy.")
		accuracy = sum(diag(predmat))/sum(predmat)
		list(accuracy=accuracy)
	} else {
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
}