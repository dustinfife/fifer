##' Plot prism-like Plots
##'
##' Given a factor (e.g., group membership) and a quantitative variable, this function plots a psuedo-scatterplot
##' of the groups on the x axis (jittered) and the DV on the y axis.
##' @title Plot prism-like Plots
##' @param formula a formula object with the quantitative variable as the response variable (e.g., Var~group).
##' @param data a dataset containing the variables indicated in \code{formula}
##' @param centerfunc what function should be used to indicate the center of the distribution. Defaults to median.
##' @param interquartile Should the interquartile range be plotted? Defaults to TRUE.
##' @param spreadfunc what function should be used to calculate the spread of the distribution? If interquartile=TRUE,
##' this argument will be ignored. The default (when not ignored) is to produce a 95\% confidence interval (1.96*sd(x)/sqrt(n)).
##' @param def.axis Logical. Should the default axes be used?
##' @param jitter.y Logical. Should the y values be jittered as well?
##' @param add Should the plot be added to an existing plot?
##' @param start What X value should the plot start at? (defaults to zero)
##' @param col What color should the dots be painted? Defaults to gray.
##' @param ... other arguments passed to plot
##' @author Dustin Fife	
##' @seealso \code{\link{boxplot}}, \code{\link{densityPlotR}}, \code{\link{plotSigBars}}
##' @export
##' @aliases prismPlots prismplots plots.prism
##' @examples
##' prism.plots(count ~ spray, data = InsectSprays, centerfunc=mean)
##' prism.plots(count ~ spray, data = InsectSprays, centerfunc=median)
prism.plots = function(formula, data, centerfunc=median, interquartile=TRUE,spreadfunc=function(x){return(1.96*sd(x)/sqrt(length(x)))},
		def.axis=TRUE, jitter.y=FALSE, add=FALSE, start=0, col="gray", ...){
			
	dv = as.character(formula[[2]])
    iv = as.character(formula[[3]])

	#### make a vector of colors if they didn't supply one
	col = rep(col, times=nrow(data))

    
    #### resort so variables line up
    ord = order(data[,iv])
    data = data[ord,]
    col = col[ord]
    types = unique(data[, iv])
    
    centers = aggregate(formula, data=data, FUN=centerfunc)[,2]    
    spread=matrix(nrow=length(types), ncol=2)
    if (interquartile){
		vals = aggregate(formula, data=data, FUN=quantile, probs=c(.25, .75))    	
    	spread = vals[,2]
    } else {
	    ss = aggregate(formula, data=data, FUN=spreadfunc)[,2]   
	    spread = cbind(centers-ss,centers+ss)
    }	

    
	data = data[order(data[,iv]),]
	un.vals = unique(data[,iv])
	iv.vals = rep(NA, times=nrow(data))
	for (i in 1:length(iv.vals)){
		rws = which(data[,iv]==un.vals[i])
		iv.vals[rws] = i
	}
	
	if (jitter.y){
    	depvar = jitter(data[,dv])
    } else {
    	depvar = data[,dv]
    }
	
	labels = list(xlab="", ylab=dv, ylim=range(depvar, na.rm=T)+c(-.25*sd(depvar, na.rm=T), .25*sd(depvar, na.rm=T)), 
					xlim=c(.5,(length(types)+.5)), xaxt="n", x=NA, y=NA)
	args = modifyList(labels, list(x=NA,...))
	
	

	
	##### compute mean (or median)
    if (def.axis){
	    if (!add){do.call("plot", args)}
	    points(jitter(iv.vals) + rep(start, times=nrow(data)), depvar, col=col, ...)
	    axis(1, at=(1:length(types))+start, labels=unique(data[,iv]))
    } else {
	    if (!add){do.call("plot", args)}
	    points(jitter(iv.vals) + rep(start, times=nrow(data)), depvar, col=col,...)
    }
    
    segments(1:length(unique(data[,iv]))-.25 + start, centers, 1:length(unique(data[,iv]))+.25 + start, centers, lwd=2,...)
    segments(1:length(unique(data[,iv])) + start, spread[,1], 1:length(unique(data[,iv])) + start, spread[,2], lwd=2,...)
    segments(1:length(unique(data[,iv]))-.05 + start, spread[,1], 1:length(unique(data[,iv]))+.05 + start, spread[,1], lwd=2,...)    
    segments(1:length(unique(data[,iv]))-.05 + start, spread[,2], 1:length(unique(data[,iv]))+.05 + start, spread[,2], lwd=2,...)         
}

##' Add significance bars to a prism plot, corrected for multiple comparisons either using Tukey's HSD (parametric),
##' or Dunn's correction for multiple comparison (non-parametric). 
##'
##' @title Add significance bars to a prism plot
##' @param formula a R formula object
##' @param data a dataset containing the variables in formula
##' @param type either "tukey" or "dunn" indicating which multiple comparison should be used
##' @seealso \code{\link{boxplot}}, \code{\link{densityPlotR}}, \code{\link{prism.plots}}
##' @export
##' @author Dustin Fife
##' @examples 
##'	prism.plots(Sepal.Length ~ Species, data = iris, centerfunc=mean)
##' plotSigBars(Sepal.Length ~ Species, data = iris, type="tukey")
##' @note This function should probably only be used when the number of groups is less than four, otherwise the number
##' of pairwise comparisons becomes too large to display. 
##'
##' When p-values are adjusted using Dunn's multiple comparison, this function calls the \code{kruskalmc} function in the
##' \code{pgirmess} package. To avoid having to load the entire package, the function was directly copied into the fifer package. 
##' references Patrick Giraudoux (2013). pgirmess: Data analysis in ecology. R package version 1.5.7. http://CRAN.R-project.org/package=pgirmess
plotSigBars = function(formula, data, type=c("tukey", "dunn")){
	
	type = match.arg(type)
    iv = as.character(formula[[3]])
    dv = as.character(formula[[2]])
    levs = sort(unique(data[,iv]))
    
    #### convert to a factor
    data[,iv] = as.factor(data[,iv])
    
	if (type=="tukey"){
		tuk = data.frame(TukeyHSD(aov(formula, data=data))[iv])
		tuk[,4] = paste0("p=",round(tuk[,4], digits=4))
		tuk[,4][tuk[,4]=="p=0"] = paste0("p<.001")		
	} else {
		tuk = kruskalmc(resp= data[,dv], categ= data[,iv], probs=.05)$dif.com
		names(tuk)[3] = "p adj"
		tuk[tuk[,3],3] = "p<.05"
		tuk[tuk[,3]=="FALSE",3] = "ns"				
	}
	

    
    barlikes = strsplit(row.names(tuk), "-")

    for (i in 1:length(barlikes)){
		rowsofinterest = sort(barlikes[[i]])
		xcoords = which(levs %in% rowsofinterest)
		miny = par("usr")[3:4]
		yheights = (miny[2]-miny[1])/50
		xlengths = par("usr")
		xlengths = (xlengths[2]-xlengths[1])/50
		
				### offset so they don't overlap
		if (i/2 == round(i/2)){
			miny[1] = miny[2]
			yheights = -1*yheights
		}				

		p.text = tuk[i, ncol(tuk)]
		segments(xcoords[1]+xlengths, miny[1]+yheights, xcoords[2]-xlengths, miny[1]+yheights)
		segments(xcoords[1]+xlengths, miny[1]+yheights, xcoords[1]+xlengths, miny[1]+2*yheights)
		segments(xcoords[2]-xlengths, miny[1]+yheights, xcoords[2]-xlengths, miny[1]+2*yheights)		
		text(x=mean(c(xcoords[1], xcoords[2])), y=miny[1]+2*yheights, labels=p.text, cex=.8)

    }
}
