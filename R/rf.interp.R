##' Find best variables using Random Forest (Interpretation Step). Find complete documentation at
##' \code{\link{rfThresh}}
##'
##' @title Variable Selection with Random Forest
##' @param object a rfThresh object
##' @param nruns how many forests should be grown?
##' @param nsd defaults to one. 
##' @param importance method of calculating importance (permutation or gini)
##' @param ... other arguments passed to \code{\link{cforest}} or \code{\link{randomForest}}
#'@return \item{varselect.interp}{The variables selected for Interpretation (sorted)}
#'@return \item{err.interp}{The error at each stage of the stepwise variable inclusion.}
#'@return \item{sd.min}{The standard deviation of the minimum fitted value.}
#'@return \item{num.varselect.interp}{The number of variables selected for interpretation.}
#'@return \item{comput.time}{Computation time of the procedure.}
#'@return \item{data}{The dataset used for fitting the RF algorithm}
#'@return \item{formula}{The formula of all variables included after the interpretation step.}
#' @author Robin Genuer, Jean-Michel Poggi and Christine Tuleau-Malot, with modifications by Dustin Fife
#' @importFrom party cforest
#' @importFrom party cforest_control
#' @importFrom party cforest_unbiased
#' @importFrom party varimp
#' @importFrom randomForest  randomForest
#' @rdname rfInterp
#' @export rfInterp
#' @seealso \code{\link{rfInterp}}, \code{\link{rfPred}}
##' @examples
##' 	#### do threshold step
##' \dontrun{data(iris); 
##' data = iris; 
##' formula = as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
##' object = rfThresh(formula, data=iris, nruns=2, importance="gini"); 
##'	#### run interpretation step
##' rfInterp(object, nruns=10, importance="permutation")}
rfInterp = function(object, nruns=20, nsd=1, importance="permutation",...){

	### record system time at beginning
	start = Sys.time()
		
	### object is a rf.thresh object
	formula = object$formula
	data = object$data
	model = object$model
	y = row.names(attr(terms(formula), "factors"))[1]
	
		#### if levels of y < 3, convert to factor and tell user
	if (length(unique(data[,y]))<3){
		warning("Note: Converting the DV to a factor.")
		data[,y] = factor(data[,y])
	}
	
	#### set function depending on whether permutations are used
	if (importance == "permutation"){
		rfcall = function(form, data, mt){
			y = row.names(attr(terms(formula), "factors"))[1]
			rf = cforest(form, data=data, controls=cforest_control(ntree=1000, mtry=mt),...)
			oob = predict(rf, OOB=T); oob = 1-length(which(oob==data[,y]))/length(data[,y])
			return(oob)
		}
	} else {
		rfcall = function(form, data, mt){
			if (object$model$type=="regression"){
				oob = tail(randomForest(form, data=data, mtry=mt,...)$mse, n=1)				
			} else {
				oob = tail(randomForest(form, data=data, mtry=mt,...)$err.rate[,1], n=1)				
			}
			return(oob)
		}
	}	
	
	#### quit if there's only one variable
	if (length(object$remaining.variables)<=1){
		warning("The threshold step only yielded one variable. I'm returning the results of the threshold step.")
		err.interp = rfcall(formula, data=object$data, mt=1)
		varselect = object$remaining.variables
		vars = varselect
		sd.min = NA
		nvarselect = 1
		final.form = formula
		comput.time = Sys.time()-start
	} else {

		##### extract IV/DV
		x.lab = attr(terms(formula), "term.labels")
		x = data[,x.lab]
		y.lab = row.names(attr(terms(formula), "factors"))[1]
		y = data[,y.lab]
		
		##### extract other things
		vars = object$remaining.variables
	
		#### get sample size and number of variables
		nvars = length(vars)
		n = nrow(x)
		
		#### preallocate error mean and sd
		err.interp = rep(NA, nvars)
		sd.interp = rep(NA, nvars)
		
	
		
	
	
		#### loop through each variable, one at a time
		for (i in 1:nvars){
			rf = rep(NA, nruns)
			u = vars[1:i]
			w = as.matrix(x[,u])
	
		#### repeatedly obtain oob estimate
			if (i <= n) {
				for (j in 1:nruns) {				
					form = make.formula(y.lab, u)
					mt = sqrt(i)
					rf[j] = rfcall(form, data=object$data, mt=mt)
				}
			} else {
	
				for (j in 1:nruns) {
					form = make.formula(y.lab, u)
					mt = i/3
					rf[j] = rfcall(form, data=object$data, mt=mt)
				}
			}
	
	
			err.interp[i] = mean(rf)
			print(mean(rf))
			sd.interp[i] = sd(rf)
		}
	
		var.min = which.min(err.interp)
		sd.min = sd.interp[var.min]
	
		nvarselect = min( which(err.interp <= (err.interp[var.min] + nsd*sd.min)) )
		varselect = vars[1:nvarselect]
	
		comput.time = Sys.time()-start
		
		final.form = make.formula(y.lab, varselect)
	}
	
	### build final model
		
	formula = final.form
	if (importance=="gini"){
		model = randomForest(formula, data=data, importance=TRUE,...)
	} else {
		model = cforest(formula, data=data, controls=cforest_unbiased(ntree=1000, mtry=mt),...)
	}
	
	output = list('varselect.interp'=varselect,
				'vars.considered' = vars,	
				 'err.interp'=err.interp,
				 'sd.min'=sd.min,
				 'num.varselect.interp'=nvarselect,
				 'comput.time'=comput.time,
				 'data' = object$data,
				 'formula' = final.form,
				 'model' = model)
	attr(output, "class") = "rfInterp"
	return(output)
}


#' Print rfInterp Summary
#'
#' Print rfInterp Summary
#' @aliases print.rfInterp
#' @param x an rfInterp object
#' @param ... ignored
#' @export
print.rfInterp = function(x,...){
	print(names(x))
	cat(paste("\n\nThe remaining variables (in order of importance) are:\n\n", sep=""))
	print(x$varselect.interp, decreasing=TRUE)
	cat(paste("\n"))
	print(x$comput.time)
}

#' Prepare xtable Summary
#'
#' Prepare xtable Summary
#' @aliases xtable.rfInterp
#' @param x an rfInterp object
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
xtable.rfInterp = function(x,caption=NULL, label=NULL, align=NULL, digits=NULL, display=NULL,...){
	tab = data.frame(matrix(nrow=length(x$vars.considered), ncol=3))
	names(tab) = c("Current Variable", "OOB Error", "Model Selected")
	tab[,1] = x$vars.considered
	tab[,2] = x$err.interp
	selected = x$varselect.interp
	
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
	

#' Plot rfInterp Summary
#'
#' Plot rfInterp Summary
#' @aliases plot plot.rfInterp
#' @param x an rfInterp object
#' @param y igorned
#' @param ... other parameters passed to plot
#' @export
plot.rfInterp = function(x, y, ...){
	length.vars = length(x$varselect.interp)
	
	labels = list(ylab="Stepwise OOB Error", xlab="", pch=16, cex=.8,col="gray", type="b", xaxt="n") 
	args = modifyList(labels, list(x=1:length.vars, y=x$err.interp[1:length.vars],...))
	do.call("plot", args)
		
# # 	plot(1:length.vars, x$err.interp[1:length.vars], ylab="Stepwise OOB Error", xlab="", pch=16, cex=.8,col="gray", type="b",...)
	text(1:length.vars, x$err.interp[1:length.vars], x$varselect.interp, pos=4, cex=.8)
}

