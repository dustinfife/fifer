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
colored.table = function(data, file, dep.var, row.factors, col.factors, breaks=4, round=2, rng=NULL, FUN=mean, row.prefix=NULL, col.prefix=NULL, custom.breaks=NULL, return.table=F, ...){

	#### average across conditions
	f = make.formula(dep.var, c(row.factors, col.factors))
	agg = aggregate(f, data=data, FUN=FUN)

	if (length(row.factors)>2 | length(col.factors)>2){
		stop("Only two factors are allowed in either the rows or columns of the matrix")
	}

	row.vals = unique(agg[,row.factors]); 
		if (length(row.factors)>1){
			row.vals = row.vals[order(row.vals[,1], row.vals[,2]),]
			nr = nrow(row.vals)
		} else {
			nr = length(row.vals)
		}
	col.vals = unique(agg[,col.factors]); 
		if (length(col.factors)>1){
			col.vals = col.vals[order(col.vals[,1], col.vals[,2]),]	
			nc = nrow(col.vals)
		} else {
			nc = length(col.vals)
		}

	results = matrix(nrow=nr, ncol=nc)
	for (i in 1:nr){
		for (j in 1:nc){
	
			ag.row = matrix(data=unlist(c(agg[,row.factors])), nrow=nrow(agg))
			ag.col = matrix(data=unlist(c(agg[,col.factors])), nrow=nrow(agg))		
			if (length(row.factors)>1){
				row.match =(apply(ag.row, 1, identical, as.vector(unlist(c(row.vals[i,])))))
			} else {
				row.match =(apply(ag.row, 1, identical, as.vector(unlist(c(row.vals[i])))))
			}
			if (length(col.factors)>1){
				col.match =(apply(ag.col, 1, identical, as.vector(unlist(c(col.vals[j,]))))) 		
			} else {
				col.match =(apply(ag.col, 1, identical, as.vector(unlist(c(col.vals[j]))))) 		
			}
	
			
			k = agg[row.match & col.match,]
			
			
			results[i,j] =(k[,dep.var])
		}
	}

	#### set colors
	if (!is.null(rng)){
		max.r = max(abs(results))/max(rng)
	} else {
		max.r = 1
		rng = range(abs(results))
	}


	#### color code results
	if (!is.null(custom.breaks)){
		custom.breaks = c(-Inf, custom.breaks, Inf)
		cols =(seq(from=1, to = .4+ ((1-max.r)*.4), length.out=length(custom.breaks)-1)	)
		cols = paste0("cellcolor[gray]{", round(cols, digits=2), "}")	
		col.mat = matrix(
				cut(abs(results), breaks=custom.breaks, labels=cols, right=F),
				nrow=nrow(results))		
	} else {
		cols =(seq(from=1, to = .2+ ((1-max.r)*.2), length.out=breaks)	)
		cols = paste0("cellcolor[gray]{", round(cols, digits=2), "}")	
		col.mat = matrix(
				cut(abs(results), breaks=seq(from=rng[1], to=rng[2], length.out=breaks+1), labels=cols, include.lowest=T), 
				nrow=nrow(results))
	}			
	results = round(results, digits=round)
	
	if (is.null(row.prefix)){
		row.prefix = paste0(row.factors,"=")
	}
	
	if (is.null(col.prefix)){
		col.prefix= paste0(col.factors, "=")
	}

	if (length(row.factors)==2){
		row.major = paste0(row.prefix[1], sprintf("%.2f", unique(row.vals[,1])))
		row.minor = paste0(row.prefix[2], sprintf("%.2f", unique(row.vals[,2])))
		row.minor = rep(row.minor, times=length(row.major))
	} else {
		row.major = paste0(row.prefix[1], unique(row.vals))
		row.minor=NA
	}
	
	if (length(col.factors)==2){
		if (is.numeric(col.vals[,1])){
			col.major = paste0(col.prefix[1], sprintf("%.2f", unique(col.vals[,1])))
		} else {
			col.major = col.vals[,1]
		}
		if (is.numeric(col.vals[,2])){
			col.minor = paste0(col.prefix[2], sprintf("%.2f", unique(col.vals[,2])))
		} else {
			col.minor = col.vals[,2]
		}
		col.minor = rep(col.minor, times=length(col.major))
	} else {
		col.major = paste0(col.prefix[1], unique(col.vals))	
		col.minor=NA
	}
	
	if (length(col.factors)==2 & length(row.factors)==2){
		latex(results, file=file,rgroup = row.major, title='', n.rgroup = rep(nrow(results)/length(row.major), times=length(row.major)), 
					cgroup = col.major, n.cgroup = rep(ncol(results)/length(col.major), times=length(col.major)),
					rowname=row.minor, colheads=col.minor, cellTexCmds = latexTranslate(col.mat), numeric.dollar=FALSE,...) 
	} else if (length(col.factors)==2){
		latex(results, file=file,title='',  
					cgroup = col.major, n.cgroup = rep(ncol(results)/length(col.major), times=length(col.major)),
					rowname=row.major, colheads=col.minor, cellTexCmds = latexTranslate(col.mat), numeric.dollar=FALSE,...) 
	} else if (length(row.factors)==2){
		latex(results, file=file,rgroup = row.major, title='', n.rgroup = rep(nrow(results)/length(row.major), times=length(row.major)), 
					rowname=row.minor, colheads=col.major, cellTexCmds = latexTranslate(col.mat), numeric.dollar=FALSE,...) 
	} else {
		latex(results, file=file, title='', rowname=row.major, colheads=col.major, cellTexCmds = latexTranslate(col.mat), numeric.dollar=FALSE,...) 		
	}
	return(results)
}


