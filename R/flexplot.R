##' Create a "flexible plot" or flexplot
##'
##' Create a flexible plot 
##'	
##' Formula takes the form of y~x + a | b + z. Everything on the right of | will occur in panels (in this case, b will be in the columns and z will be in the rows). Any numeric
##' variables between ~ and | will fall on the x axis and categorical predictors will be color-coded/symbol-coded/line-coded (if user specifies). If more than one numeric variable is specified
##' after ~, the second (and third) will be binned. No more than four variables are allow (otherwise, cognitive load is too high). If the user wishes to include another variable,
##'  I recommend they create separate plots, one for each level of the new variable (or a binned level of a numeric variable). 
##' @param formula A formula of the form  y~x + a | b + z
##' @param data The dataset
##' @param color Color values to be used for the categorical variables
##' @param symbol The symbols to be used for the categorical variables
##' @param linetype The linetype to be used for the categorical variables
##' @param bins The number of bins used when putting quantitative variables into categories. A list can be used if different amounts are wanted for different variables
##' @param labels A list of the same length as bins
##' @param breaks The breaks to be used for the bins
##' @param method The method to be used to draw the lines. Defaults to loess
##' @param se Should standard errors be drawn?
##' @param spread How should standard errors be drawn? Defaults to quartiles
##' @param jitter Should values be jittered?
##' @param raw.data Should raw data be plotted?
##' @param sample Should a sample of the data be plotted? Defaults to Inf (for all variables). 
##' @author Dustin Fife
##' @export
##' @examples
##' 
flexplot = function(formula, data, 
		color=NULL, symbol=NULL, linetype=NULL, 
		bins = 4, labels=NULL, breaks=NULL,
		method="loess", se=T, spread=c('quartiles', 'stdev', 'sterr'), jitter=FALSE, raw.data=T,
		sample=Inf){
			
		##### extract outcome, predictors, and given variables
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	given = unlist(strsplit(as.character(formula[[3L]])[3], " + ", fixed=T))
	if (is.na(given[1])){given=NULL}
	
	##### identify which variables are numeric and which are factors
	if (length(predictors)>0){
		numbers = names(which(unlist(lapply(data[,predictors], is.numeric))))
		categories = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))
	}
	
		#### remove missing values
	if (length(predictors)>0){
		if (length(unlist(apply(data[,variables], 2, function(x){(which(is.na(x)))})))>0){
			data = na.omit(data)
		}
	}
	
	#### create custom function to sample data
	sample.subset = function(sample, data){
		if (sample!=Inf){
			m = data[sample(1:nrow(data), size=sample),]
		} else {
			m = data
		}
	}

	#### if they don't want raw data, just make alpha = 0
	if (raw.data){
		alpha.raw = .35
	} else {
		alpha.raw = 0
	}	

	#### BEGIN THE MEGA PLOTTING IFS!
	#### PLOT UNIVARIATE PLOTS
		#### if there's no predictors, use the "uni.plot" function
	if (length(outcome)==1 & length(predictors)==0 & length(given)==0){
		#fifer:::uni.plot(outcome)
		p = uni.plot(outcome, d=data)

	##### BIVARIATE PLOTS	
	} else if (length(outcome)==1 & length(predictors)==1 & length(given)==0){			
		p = bivariate.plot(predictors, outcome, d=data, jitter=jitter, raw.data=raw.data, spread=spread, method=method, se=se, sample=sample)


	###### INTERACTION PLOT
	} else if (length(outcome)==1 & length(predictors)==2 & (is.character(data[,predictors[1]]) | is.factor(data[,predictors[1]])) & (is.character(data[,predictors[2]]) | is.factor(data[,predictors[2]]))){		
		p = ggplot(data=data, aes_string(x=predictors[1], y=outcome)) +
			geom_jitter(data=sample.subset(sample, data), alpha = alpha.raw, size=.25, width=.2) +
			facet_wrap(as.formula(paste("~", predictors[2]))) +
			stat_summary(aes_string(x=predictors[1], y=outcome), geom="errorbar", fun.ymin = function(z){mean(z) - 1.96*(sd(z)/length(z))}, fun.ymax=function(z){mean(z) + 1.96*(sd(z)/length(z))}, fun.y = mean, width=.1) +
			stat_summary(fun.y = "mean", geom="line", group=1) +
			labs(x=predictors[1], y=outcome) +
			theme_bw()
		
	##### ANCOVA PLOT		
	} else if (length(predictors)==2 & length(categories)==1){
		p = ggplot(data=d, aes_string(x=numbers, y=outcome, group=categories, linetype=categories)) +
			geom_point(data=sample.subset(sample, data), alpha=alpha.raw) +
			geom_smooth(method=method, se=se) +
			theme_bw()							


	###### FOR VARAIBLES THAT WILL BE BINNED...
	} else {
		
		###### only allow two "given" variables
		if (length(given)>2){
			stop("Only two 'given' variables are allowed.")
		}
		
		##### make given variables ggplot friendly (for facet_grid)
		given.as.string = ifelse(length(given)>1,paste0(given, collapse="~"), paste0("~",given))
		

		##### identify the non given variables
		axis = unlist(strsplit(as.character(formula[[3L]])[2], " + ", fixed=T))	
			
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
			
			break.current = unlist(breaks[i])
			if (!is.null(unlist(labels[i])) & length(unlist(labels[i])) != bins[i]){
				stop(paste0("The label vectors (", paste0(unlist(labels[i]), collapse=", "), ") is not the same length as the bin length (", bins[i], ")", sep=""))
			}
			
			#### if they supply the breaks...
			if (!is.null(break.current)){
				##### give min as breaks, if the user doesn't
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
			
		}

		
		if (length(axis)>1){
			print("here")
			p = ggplot(data=data, aes_string(x=axis[1], y=outcome, symbol=axis[1], linetype=axis[2]))+
					geom_point(alpha=0) +
					geom_smooth(method=method, se=se, col="black") + 
					geom_point(data=sample.subset(sample, data), alpha=.15) +
					facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both)) + 
					theme_bw()
		} else {
			p = ggplot(data=data, aes_string(x=axis[1], y=outcome))+
				geom_point(data=sample.subset(sample, data), alpha=.5) +
				geom_smooth(method=method, se=se)+
				facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both)) +
				theme_bw()			
		}

		
	

	}		
			return(p)	
		
		
		
		
		
		
		
		# if (length(outcome)==1 & length(predictors)==3 & (is.character(data[,predictors[1]]) | is.factor(data[,predictors[1]])) & (is.character(data[,predictors[2]]) | is.factor(data[,predictors[2]])) & (is.character(data[,predictors[3]]) | is.factor(data[,predictors[3]]))){
	# ###### MULTIWAY DOT PLOT FOR THREE CATEGORICAL PREDICTORS
	
		# #### figure out which variable has the largest effect
		# mod = lm(formula, data=data)
		# anova(mod)
		# order = order(anova(mod)[-length(coef(mod)),"F value"], decreasing=F)
		# ordered.predictors = row.names(anova(mod))[order]
		# new.formula = make.formula(outcome, ordered.predictors)
		# #### reorder the means
		# means = aggregate(new.formula, data=data, FUN=mean)
		# names(means)[ncol(means)]="mean"
		# se = aggregate(new.formula, data=data, FUN=function(x){sd(x)/length(x)})
		# means$se = se[,outcome]

		# #### combine with previous dataset
		# d = merge(data, means, by=ordered.predictors)


		# call = paste0("ggplot(data=", deparse(substitute(d)), ", aes(x=", ordered.predictors[1], ", y=", outcome, ", col=", ordered.predictors[1], ")) + geom_point() +
		# geom_errorbar(aes(ymin=mean - 1.96*se, ymax= mean + 1.96*se, col=",ordered.predictors[1], "), width=.2) +
		# coord_flip(ylim=c(0,1)) + 
		# facet_wrap(",ordered.predictors[2],"+", ordered.predictors[3],"~., ncol=1)
		# " )
		# cat(call)
		# eval(parse(text = call))	
		# +
						# geom_errorbar(aes_string(ymin= outcome - 1.96* outcome*(1-outcome)/sqrt(nrow(d)), ymax= outcome + 1.96* outcome*(1-outcome)/sqrt(nrow(d)),col=ordered.predictors[1]), width=.2) + 
						# coord_flip(ylim=c(0,1)) + 
						# facet_wrap(~Condition, ncol=1) + 
						# scale_color_manual(values=c("green", "red", "blue"))+
						# theme_bw() +
						# theme(strip.text = element_text(size=15))							
				
				
				
				# # a = ggplot(data=d[d$Category=="Health",], aes(x=Group, y=Percentage, col=Group)) + geom_point()+ 
						# # geom_errorbar(aes(ymin=Percentage - 1.96*Percentage*(1-Percentage)/sqrt(200), ymax=Percentage + 1.96*Percentage*(1-Percentage)/sqrt(200),col=Group), width=.2) + 
						# # coord_flip(ylim=c(0,1)) + 
						# # facet_wrap(~Condition, ncol=1) + 
						# # scale_color_manual(values=c("green", "red", "blue"))+
						# # theme_bw() +
						# # theme(strip.text = element_text(size=15))
				# # a		
		
		
		
		
		
	
		
		
	# }
	
	### GIVEN VARIABLES
	
	
}


