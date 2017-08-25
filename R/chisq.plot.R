##' Plot Expected and Observed Frequencies (Chi Square)
##'
##' This function will plot the expected vs. observed frequencies from a Chi Square Test and plot them in Barplots. 
##'	
##' @param list A table containing the variables. If not included, user can include raw data as v1 or v2
##' @param v1 The first variable (must be a factor)
##' @param v2 The second variable (also must be a factor)
##' @param standardize Logical. Should the results be reported in proportions? (Rather than counts)
##' @seealso \code{\link{barchart}}
##' @author Dustin Fife
##' @export
##' @references Agresti, A. (2007) An Introduction to Categorical Data Analysis, 2nd ed., New York: John Wiley & Sons. Page 38.
##' @examples
##' ##' 
##' # From Agresti(2007) p.39
##' M = as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
##' dimnames(M) = list(gender = c("F", "M"),
##'                     party = c("Democrat","Independent", "Republican"))
##' chisq.plot(list=M, standardize=F)              
chisq.plot = function(list=NULL, v1=NULL, v2=NULL, standardize=F, sqrt.y=F){
	
	if (!is.null(v1) & !is.null(v2)){
	if (!is.factor(v1) | !is.factor(v2)){
		stop("Both variables must be factors")
	}
	}

		###### figure out coordinates of each plot
	if (!is.null(list)){
		chi = chisq.test(list)
		rows = unlist(unique(dimnames(list)[1]))
		cols = unlist(unique(dimnames(list)[2]))
	} else {
		rows = levels(v1)
		cols = levels(v2)	
		chi = chisq.test(v1, v2)
	}

	obs = chi$observed
	exp = chi$expected

	
	if (standardize){
		obs = obs/sum(obs)
		exp = exp/sum(exp)
	}
	
			#### figure out where to start "inner" plots
	inner.num = length(cols)+length(rows)+1
	inner.num = inner.num:((inner.num-1)+(length(cols)*length(rows))) # e.g., 6:(6+6)
	
			#### figure out column labels
	col.labs = 1:length(cols)
		
			#### combine them
	so.far = matrix(c(col.labs, inner.num), ncol=length(cols), byrow=T)
	
			#### now add the first column
	first.col = c(0, (max(col.labs)+1):(min(inner.num)-1))
	so.far = cbind(first.col, so.far)
	
			#### now add the legend
	legend = so.far
	
	heights = c(1, rep(2, times=length(rows)))
	heights = heights/sum(heights)
	widths = c(1, rep(2, times=length(cols)))
	widths = widths/sum(widths)
	layout(legend, heights=heights, widths=widths)
	
	#### now actually plot that sucka!
	
	#### plot the columns labels
	for (i in 1:length(cols)){
		par(mar=c(1,1,3,1))			
		plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
		text(.5, .5, cols[i], cex=1.5)	
	}
	
	##### plot the row labels
	for (i in 1:length(rows)){
		par(mar=c(1,1,1,1))			
		plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
		text(.5, .5, rows[i], cex=1.5)	
		
	}
	
	#### now plot the actual numbers
	for (i in 1:length(rows)){
		for (j in 1:length(cols)){
			par(mar=c(2,3.5,1,1))
			coords = c(obs[i,j], exp[i,j])
			if (sqrt.y){
				coords = sqrt(coords)
				ylab=expression(sqrt('count'))
				ylim = sqrt(c(0, max(obs, exp))				)
			} else {
				ylab=""
				ylim = c(0, max(obs, exp))				
			}
			barplot(coords, names.arg=c("Observed", "Expected"), col=c("gray", "white"), ylim=ylim, ylab=ylab)
		}
	}
}