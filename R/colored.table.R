##' Create a color-coded table in latex
##'
##' Create a color-coded table in latex
##'	
##' @param data the dataset in "long" format, where the conditions are on columns.
##' @param file the location where the file should be exported. 
##' @param dep.var A string naming the dependent variable
##' @param row.factor A vector of one or two elements, each of which is a string indicating the name of the row factor(s)
##' @param col.factor A vector of one or two elements, each of which is a string indicating the name of the col factor(s)
##' @param breaks The number of breaks in color. Defaults to 4.
##' @param round How many digits should the matrix be rounded to? Defaults to 2. 
##' @param rng The range of values used for coloring the plot. 
##' @param ... Other arguments passed to Hmisc
##' @aliases color.table col.table
##' @seealso \code{\link{Hmisc:latex}}
##' @author Dustin Fife
##' @importFrom Hmisc latex
##' @export
##' @examples
##' ## do this later
colored.table = function(data, file, dep.var, row.factors, col.factors, breaks=4, round=2, rng=NULL,...){

	#### average across conditions
	f = make.formula(dep.var, c(row.factors, col.factors))
	agg = aggregate(f, data=data, FUN=mean)

	if (length(row.factors)>2 | length(col.factors)>2){
		stop("Only two factors are allowed in either the rows or columns of the matrix")
	}

	row.vals = unique(agg[,row.factors]); row.vals = row.vals[order(row.vals[,1], row.vals[,2]),]
	col.vals = unique(agg[,col.factors]); col.vals = col.vals[order(col.vals[,1], col.vals[,2]),]	
	
	results = matrix(nrow=nrow(row.vals), ncol=nrow(col.vals))
	for (i in 1:nrow(row.vals)){
		for (j in 1:nrow(col.vals)){
	
			ag.row = matrix(data=unlist(c(agg[,row.factors])), nrow=nrow(agg))
			ag.col = matrix(data=unlist(c(agg[,col.factors])), nrow=nrow(agg))		
			row.match =(apply(ag.row, 1, identical, as.vector(unlist(c(row.vals[i,])))))
			col.match =(apply(ag.col, 1, identical, as.vector(unlist(c(col.vals[j,]))))) 		
			
			k = agg[row.match & col.match,]
			
			
			results[i,j] = k[,dep.var]
		}
	}

	#### set colors
	if (!is.null(rng)){
		max.r = max(abs(results))/max(rng)
	} else {
		max.r = 1
	}

	#### color code results
	cols =(seq(from=1, to = .5+ ((1-max.r)*.5), length.out=breaks)	)
	cols = paste0("cellcolor[gray]{", cols, "}")	
	col.mat = matrix(
			cut(abs(results), breaks=seq(from=rng[1], to=rng[2], length.out=breaks), labels=cols[-8]), 
			nrow=nrow(results))
	results = round(results, digits=round)
	row.major = paste0(row.factors[1], "=", unique(row.vals[,1]))
	if (length(row.factors)==2){
		row.minor = paste0(row.factors[2], "=", unique(row.vals[,2]))
		row.minor = rep(row.minor, times=length(row.minor))
	} 
	
	col.major = paste0(col.factors[1], "=", unique(col.vals[,1]))
	if (length(col.factors)==2){
		col.minor = paste0(col.factors[2], "=", unique(col.vals[,2]))
		col.minor = rep(col.minor, times=length(col.major))
	}
	
	latex(results, file=file,rgroup = row.major, title='', n.rgroup = rep(nrow(results)/length(row.major), times=length(row.major)), 
				cgroup = col.major, n.cgroup = rep(ncol(results)/length(col.major), times=length(col.major)),
				rowname=row.minor, colheads=col.minor, cellTexCmds = latexTranslate(col.mat), numeric.dollar=FALSE,...) 
}


