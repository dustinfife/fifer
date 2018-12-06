##' Function to create a related t test plot
##'
##' Function to create a related t test plot
##'	
##' Function to create a related t test plot
##' @param predictors the character of the predictor variable
##' @param outcome the character of the outcome variable
##' @param data the dataset
##' @param raw.data should raw data be displayed
##' @param spread type of spread to be displayed
##' @param n the number of datapoints to be displayed
##' @author Dustin Fife
##' @export
related.plot = function(predictors, outcome, data, raw.data, ss, spread="quartiles"){
		if (length(predictors)!=1){
			stop("Currently, the 'related' option is only available when there's a single predictor.")
		} 
		
		#### extract levels of the predictors
		levs = levels(data[,predictors])
		
		if (length(levs)!=2){
			stop("Sorry, I can only accept two levels of the grouping variable when related=T.")
		}

		#### create difference scores
		g1 = data[data[,predictors]==levs[1], outcome]
		g2 = data[data[,predictors]==levs[2], outcome]		
		
		if (length(g1) != length(g2)){
			stop("Sorry, the length of the two groups are not the same. I can only create difference scores when the group sizes are identical.")
		}
		
		lab = paste0("Difference (",levs[2], "-", levs[1], ')')
		d2 = data.frame(Difference=g2-g1)
		
		if (spread == "stdev"){ 
			summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
			summary2 = stat_summary(geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, 
					fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.12, position=position_dodge(width=.2))}
		if (spread == "sterr"){ 
			summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
			summary2 = stat_summary(geom='errorbar', fun.data = mean_cl_normal, color=rgb(1,0,0,.25), 
					width=.12, size = 1.25, position=position_dodge(width=.2))}
		if (spread == "quartiles"){ 
			summary1 = stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.2)) 
			summary2 = stat_summary(geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  
					fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.12, position=position_dodge(width=.2))}				


		if (ss!=Inf){
			m = data.frame(d2[sample(1:nrow(d2), size= ss),])
			names(m) = names(d2)
		} else {
			m = d2
		}

		p = ggplot(d2, aes(y=Difference, x=1)) +
			geom_jitter(data=m, alpha=raw.alph.func(raw.data, .15), width=.05) +
			summary1 + summary2 + 
			geom_hline(yintercept=0, col="lightgray") +
			labs(x=lab) +
			theme_bw() +
			theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) +
			coord_cartesian(xlim=c(.75, 1.25))
		p
}


##' Sample the data
##'
##' Sample the data
##'	
##' Sample the data
##' @param ss sample size of raw data to be displayed. Defaults to Inf (for all data)
##' @param data the dataset
##' @author Dustin Fife
##' @export
subset.sample = function(ss, data){
	if (ss!=Inf){
		m = data.frame(data[sample(1:nrow(data), size= ss),])
		names(m) = names(data)
	} else {
		m = data
	}
}		


##' Find transparency of data
##'
##' Find transparency of data
##'	
##' Find transparency of data
##' @param raw.data Should raw data be displayed?
##' @param alpha the transparency level
##' @author Dustin Fife
##' @export
raw.alph.func = function(raw.data,alpha=1){
	if (raw.data){
		alpha.raw = alpha
	} else {
		alpha.raw = 0
	}	
	alpha.raw
}


##' How to display data
##'
##' How to display data
##'	
##' How to display data
##' @param jitter Should data be jittered?
##' @param data the dataset
##' @param ss The size of the sample
##' @param raw.data should raw data be displayed
##' @param alpha the transparency level
##' @author Dustin Fife
##' @export
jit.func = function(jitter, data, ss, raw.data, alpha){
	print(ss)
	if (!is.null(jitter)){
			if (jitter[1]==T){
				jit = geom_jitter(data= subset.sample(ss, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=.2)
			} else if (jitter[1] == F){
				jit = geom_point(data= subset.sample(ss, data), alpha=raw.alph.func(raw.data, alpha=alpha))
			} else {
				jit = geom_jitter(data= subset.sample(ss, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=jitter[1], height=jitter[2])
			}
			
		} else {
			jit = geom_point(data= subset.sample(ss, data), alpha=raw.alph.func(raw.data, alpha=alpha))
	}
	
	jit
}
	

##' Type of spread to use
##'
##' Type of spread to use
##'	
##' Type of spread to use
##' @param spread Should be stdev, sterr, or quartiles
##' @param predictors the character of the predictor variable
##' @param outcome the character of the outcome variable
##' @param data the dataset
##' @author Dustin Fife
##' @export
spread_type = function(spread, predictors, outcome, data){
	spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
	if (spread=="stdev"){
		summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), 
				geom='errorbar', fun.ymin = function(z){mean(z)-sd(z)}, 
				fun.ymax = function(z) {mean(z)+sd(z)}, fun.y=median, size = 1.25, width=.2, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="mean", position=position_dodge(width=.2))
	} else if (spread=="sterr"){	
		summary1 = stat_summary(fun.y='mean', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), geom='errorbar', fun.data = mean_cl_normal, width=.2, size = 1.25, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="mean", position=position_dodge(width=.2)) 	
	} else if (spread == "quartiles"){	
		summary1 = stat_summary(fun.y='median', geom='point', size=3, position=position_dodge(width=.2)) 
		summary2 = stat_summary(aes_string(x=predictors[1], y=outcome), geom='errorbar', fun.ymin = function(z){quantile(z, .25)},size = 1.25,  fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, width=.2, position=position_dodge(width=.2))
		sum.line = stat_summary(aes_string(group=predictors[2]), geom="line", fun.y="median", position=position_dodge(width=.2)) 	
	}
	
	list(summary1=summary1, summary2=summary2, sum.line=sum.line)
}

	
##' Type of x summary to use (line or mean)
##'
##' Type of x summary to use (line or mean)
##'	
##' Type of x summary to use (line or mean)
##' @param predictors the character of the predictor variable
##' @param outcome the character of the outcome variable
##' @param data the dataset
##' @param spread type of spread to be displayed
##' @author Dustin Fife
##' @export
x.summary = function(predictors, outcome, spread, data){
		
		if (is.numeric(data[,predictors[1]])) {

		#### summarize with a line...
			#### identify the correct line
			if (suppress_smooth){
				x_summary = theme_bw()
			} else if (method=="logistic") {
		
				#### make sure there's only two levels
				if (length(unique(data[,outcome]))!=2){
					stop("To fit a logistic curve, you must have only two levels of your outcome variable.")
				}
				
				#### convert outcome to numeric (if necessary)
				if (!is.numeric(data[,outcome])){
					data[,outcome] = as.numeric(data[,outcome])-1
				}
		
				#### specify the curve
				x_summary = geom_smooth(method = "glm", method.args = list(family = "binomial"), se = se)
			} else {
				x_summary = geom_smooth(method=method, se=se)
			}	
			
	} else {
		
		##### summarize with dots
		x = spread_type(spread, predictors, outcome, data)
		x_summary = c(x$summary1,x$summary2)
	}
	
	x_summary
}	


##' Bin them vars
##'
##' Bin them vars
##'	
##' Bin them vars
##' @param given stuff
##' @param bins The number of bins used when putting quantitative variables into categories. A list can be used if different amounts are wanted for different variables
##' @param labels A list of the same length as bins
##' @param breaks The breaks to be used for the bins
##' @param data the dataset
##' @param ghost.reference What should the reference group be (from which to draw the ghost line)? The user can specify up to two values as a vector. See examples. 
##' @param prediction The predicted line
##' @author Dustin Fife
##' @export
bin.variables = function(given, breaks, labels, data, bins, ghost.reference, prediction){
				##### only allow two "given" variables
		if (length(given)>2){
			stop("Only two 'given' variables are allowed.")
		}

		#### modify given (if needed)
		if (length(given)>0 & !is.na(given)){
		if (regexpr("+", given)){
			given = unlist(strsplit(given, " + ", fixed=T))
		}
		}		
		
		#### see if more than two variables are shown at the left of the given sign
		if ((length(predictors) - length(given))>2){
			stop("Only two 'axis' variables are allowed.")
		}
			
		
		
		#### make given variables ggplot friendly (for facet_grid)
		given.as.string = ifelse(length(given)>1 & !is.na(given),paste0(rev(given), collapse="~"), paste0("~",given))

			
		### identify the number of binned variables we need
		if (length(axis)>1 & axis[2] %in% numbers){ 
			binned.vars = c(axis[2], numbers[which((numbers) %in% given)])
		} else {
			binned.vars = numbers[which((numbers) %in% given)]
		}

		if (length(binned.vars)>0){
			msg = paste0("The following variables are going to be binned: ", paste0(binned.vars, collapse=", "), "\n")
			cat(msg)
		}
		
		### repeat the bins the number of bins there are
		if (length(bins) != length(binned.vars) & length(bins)>1){
			warning("You haven't specified enough bins to cover all the binned variables. I'm making a guess for the rest of the variables")
			bins = matrix(bins, nrow=1, ncol=length(binned.vars))
		}
		
		if (length(bins)==1){
			bins = rep(bins, times=length(binned.vars))
		}


		
		#### bin the binned variables
		if (length(binned.vars)>0){
			for (i in 1:length(binned.vars)){
				
				break.current = unlist(breaks[i])
				if (!is.null(unlist(labels[i])) & length(unlist(labels[i])) != bins[i]){
					stop(paste0("The label vectors (", paste0(unlist(labels[i]), collapse=", "), ") is not the same length as the bin length (", bins[i], ")", sep=""))
				}
				
				### if they supply the breaks...
				if (!is.null(break.current)){
					#### give min as breaks, if the user doesn't
					if (min(break.current)>min(data[,binned.vars[i]])){
						break.current = c(-Inf, break.current)
					}
					if (max(break.current,na.rm=T)<max(data[,binned.vars[i]])){
						break.current = c(break.current, Inf)
					}
					
					quants = unlist(break.current)
				} else {
					quants = quantile(data[,binned.vars[i]], seq(from=0, to=1, length.out=bins[i]+1))
				}
	
				data[,paste0(binned.vars[i])] = cut(data[,binned.vars[i]], quants, labels= unlist(labels[i]), include.lowest=T, include.highest=T)

				#### if they're making a ghost reference, bin that too
				if (!is.null(ghost.reference) & binned.vars[i] %in% names(ghost.reference)){
					val = as.numeric(ghost.reference[binned.vars[i]])
					ghost.reference[binned.vars[i]] = as.character(cut(val, quants, labels=unlist(labels[i]), include.lowest=T, include.highest=T))
				}
				
				if (!is.null(prediction)){
					prediction[,paste0(binned.vars[i])] = cut(prediction[,binned.vars[i]], quants, labels= unlist(labels[i]), include.lowest=T, include.highest=T)

				}
				
			}

			if (!is.null(prediction)){
							#### average the predictions within bin
				f = make.formula("prediction", c("model",
														predictors[-which(predictors==binned.vars[i])],
														binned.vars[i]
														)
									)			
				prediction = aggregate(f, data=prediction, FUN=median)

			}	
				#### add code for "given" variable
		} 
		
		if (!is.na(given[1])){
			
			giv = facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both))
		} else {
			giv = theme_bw()
		}
			
		
		list(prediction=prediction, ghost.reference=ghost.reference, data=data, giv=giv)
}		