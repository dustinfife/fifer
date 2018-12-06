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
##' @param related Are variables related? If so, it will show a plot of difference scores, rather than the raw scores (for a related t test). 
##' @param color Color values to be used for the categorical variables
##' @param symbol The symbols to be used for the categorical variables
##' @param linetype The linetype to be used for the categorical variables
##' @param bins The number of bins used when putting quantitative variables into categories. A list can be used if different amounts are wanted for different variables
##' @param labels A list of the same length as bins
##' @param breaks The breaks to be used for the bins
##' @param method The method to be used to draw the lines. Defaults to loess
##' @param se Should standard errors be drawn?
##' @param ghost.line Should a ghost line be drawn? If so, user must specify a color. Default is NULL (in which case, the ghost line isn't shown). 
##' @param ghost.reference What should the reference group be (from which to draw the ghost line)? The user can specify up to two values as a vector. See examples. 
##' @param spread How should standard errors be drawn? Defaults to quartiles
##' @param jitter Should values be jittered?
##' @param raw.data Should raw data be plotted?
##' @param ss Should a sample of the data be plotted? Defaults to Inf (for all variables). 
##' @author Dustin Fife
##' @export
##' @examples
#' clear()
#' data(exercise_data)
#' d = exercise_data
#'
#'	# # #### histograms and barcharts
##' flexplot(income~1, data=d)
##' flexplot(gender~1, data=d)
#'
#'	# ### scatter plot
##' source("research/RPackages/fifer/R/flexplot.R")
##' flexplot(weight.change~motivation, data=d)	
##' flexplot(weight.change~motivation, data=d, method="lm", se=F)	#### with regression line and without standard error	
#'
#'	# ### mean plots
##' flexplot(weight.change~therapy.type, data=d)
##' flexplot(weight.change~therapy.type, data=d, raw.data=F)		## without raw data
#'
#'	# ### CHI SQUARE PLOT
##' flexplot(therapy.type~gender, data=d)	
#'			
#'	# ### INTERACTION PLOT			
##' flexplot(weight.change~therapy.type + gender, data=d)
##' flexplot(weight.change~therapy.type + gender, data=d, sample=50)	#### sampling 50 people instead (to make it less noisy)
#'
#'	# #### ANCOVA PLOT
##' flexplot(weight.change~motivation + gender, data=d, se=F)	### remove se
#'
#'	# #### 2N PLOT (2 NUMERIC VARIABLE PLOTS)
##' flexplot(weight.change~motivation + income, data=d, se=F, method="lm")
##' flexplot(weight.change~motivation + income, data=d, se=F, method="lm", breaks = list(c(95000, 100000, 105000)),labels=list(c("<95K", "<100K", "<105K", ">105K")))		### change labels for income
#'
#'	# #### 3N plot
##' flexplot(weight.change~motivation + income + health, data=d, se=F, method="lm")	## different lines for income
##' flexplot(weight.change~motivation | income + health, data=d, se=F, method="lm")	## different panels for income
##' flexplot(weight.change~motivation | income + health, data=d, se=F, method="lm", breaks = list(c(95000, 100000, 105000)),labels=list(c("<95K", "<100K", "<105K", ">105K")))	## relabel income
#'
#'	# #### three categorical variables (multiway dot plot)
##' flexplot(weight.change~gender + therapy.type + rewards, data=d, raw.data=F)
flexplot2 = function(formula, data, related=F,
		color=NULL, symbol=NULL, linetype=NULL, 
		bins = 4, labels=NULL, breaks=NULL,
		method="loess", se=T, 
		ghost.line=NULL, ghost.reference=NULL,
		spread=c('quartiles', 'stdev', 'sterr'), jitter=FALSE, raw.data=T,
		ss=Inf, 
		prediction = NULL, suppress_smooth=F, alpha=.99977){
			

	##### use the following to debug flexplot
	#formula = formula(weight.loss~therapy.type + rewards); related=T; data=d; color=NULL; symbol=NULL; linetype=NULL; bins = 4; labels=NULL; breaks=NULL; method="loess"; se=T; spread=c('stdev'); jitter=FALSE; raw.data=T; ghost.line="gray"; sample=Inf; prediction = NULL; suppress_smooth=F; alpha=1
		print(ss)
		
	##### IDENTIFICATION
	spread = match.arg(spread, c('quartiles', 'stdev', 'sterr'))
	
	#### extract outcome, predictors, and given variables
	variables = all.vars(formula)
	outcome = variables[1]
	predictors = variables[-1]
	given = unlist(subsetString(as.character(formula)[3], sep=" | ", position=2, flexible=F))

	#### identify which variables are numeric and which are factors
	if (length(predictors)>0){
		numbers = names(which(unlist(lapply(data[,predictors], is.numeric))))
		categories = names(which(!(unlist(lapply(data[,predictors], is.numeric)))))
	}

	### remove missing values
	if (length(predictors)>0){
		if (length(unlist(apply(data[,variables], 2, function(x){(which(is.na(x)))})))>0){
			
			delete.me = as.numeric(unlist(apply(data[,variables], 2, function(x){(which(is.na(x)))})))
			data = data[-delete.me,]
		}
	}
	
		#### create bins
	bin.info = bin.variables(given, breaks, labels, data, bins, ghost.reference, prediction)
	data = bin.info$data
	
	#### identify the non given variables
	axis = unlist(subsetString(as.character(formula)[3], sep=" | ", position=1, flexible=F))
	axis = unlist(strsplit(axis, " + ", fixed=T))	

	#### create x axis variable
	x.raw = jit.func(jitter, data=data, ss =ss, raw.data=raw.data, alpha=alpha)
	x.sum = x.summary(predictors, outcome, spread, data)

	### generic plot
	p = ggplot(data=bin.info$data, aes_string(x=predictors[1], y=outcome)) +
		x.raw +
		x.sum +
		bin.info$giv + 
		theme_bw()

		
	### PLOT UNIVARIATE PLOTS
	if (length(outcome)==1 & length(predictors)==0){
		
		##### reorder according to columns lengths (if it's not an ordered factor)
		if (!is.ordered(data[,outcome])){
			counts = sort(table(data[,outcome]), decreasing=T)
			names(counts)
			data[,outcome] = factor(data[,outcome], levels=names(counts))
		}
		p = uni.plot(outcome, d=data)
		
	### related t plot
	} else if (related){
		p = related.plot(predictors=predictors, outcome=outcome, data=data, ss=ss, raw.data=raw.data, spread=spread)	

	#### SCATTERPLOT	
	} else if (length(outcome)==1 & length(predictors)==1 & is.na(given) & (is.numeric(data[,predictors]) & is.numeric(data[,outcome]))){				
		p = ggplot(data=data, aes_string(x=predictors, y=outcome))+
			jit + #geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
			gm +
			theme_bw()				
	
	##### MEAN PLOT
	} else if (length(outcome)==1 & length(predictors)==1 & is.na(given) & (is.numeric(data[,predictors]) | is.numeric(data[,outcome]))){		
		
		
		#### reorder if it's not already ordered
		if (!is.ordered(data[,outcome])){
			if (spread=="quartiles"){ fn = "median"} else {fn = "mean"}
			ord = aggregate(data[,outcome]~data[,predictors], FUN=fn, na.rm=T)
			ord = ord[order(ord[,2], decreasing=T),]
			data[,predictors] = factor(data[,predictors], levels=ord[,1])
		}

		#### set default alpha
		if(alpha==.99977){
			alpha = .2
		}
		p = ggplot(data=data, aes_string(x=predictors, y=outcome)) +
			geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha), size=.75, width=.05) + 
			summary1 + summary2 + 
			theme_bw()						
	##### logistic regression plot
	# } else if (){
		
	# }
	##### CHI SQUARE PLOT
	} else if (length(outcome) == 1 & length(predictors)==1 & is.na(given) & !is.numeric(data[,predictors]) & !is.numeric(data[,outcome])){
		m = as.data.frame(table(d[,predictors], d[,outcome])); names(m)[1:2] = c(predictors, outcome)
		Freq = 'Freq'
		p = ggplot(data=m, aes_string(x=predictors, y=Freq, fill=outcome)) + geom_bar(stat='identity', position='dodge') + theme_bw()

	##### INTERACTION PLOT
	} else if (length(outcome)==1 & length(predictors)==2 & (is.character(data[,predictors[1]]) | is.factor(data[,predictors[1]])) & (is.character(data[,predictors[2]]) | is.factor(data[,predictors[2]]))){


					#### set default alpha
		if(alpha==.99977){
			alpha = .2	
		}
		
				
		#### identify if given is na
		if (!is.na(given)){

			p = ggplot(data=data, aes_string(x=predictors[1], y=outcome)) +
				geom_jitter(data=sample.subset(sample, data), alpha = raw.alph.func(raw.data, alpha), size = .75, width=.2) +
				facet_wrap(as.formula(paste("~", predictors[2]))) +
				summary1 + summary2 +
				labs(x=predictors[1], y=outcome) +
				theme_bw()
		} else {
			p = ggplot(data=data, aes_string(x=predictors[1], y=outcome, color=predictors[2], linetype=predictors[2], shape=predictors[2])) +
				geom_jitter(data=sample.subset(sample, data), alpha = raw.alph.func(raw.data, alpha), size=.75,  position= position_jitterdodge(jitter.width=.2, dodge.width=.2)) +
				summary1 + summary2 + 
				sum.line + 
				labs(x=predictors[1], y=outcome) +
				theme_bw()			
		}

	#### ANCOVA PLOT		
	# } else if (length(predictors)==2 & length(categories)==1 & length(given)<1){
		# ### if they're supplying a prediction, put the covariate in the "given" area
		# if (!is.null(prediction)){
			# given.as.string = paste0("~", categories)
			# p = ggplot(data=data, aes_string(x=numbers, y=outcome))+
				# geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
				# gm +
				# facet_grid(as.formula(given.as.string),labeller = labeller(.rows = label_both, .cols=label_both)) +
				# theme_bw()	
				
		# } else {	
			# p = ggplot(data=d, aes_string(x=numbers, y=outcome, group=categories, linetype=categories, color=categories)) +
				# geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha)) +
				# gm +
				# theme_bw()							
		# }
	###### MULTIWAY DOT PLOT FOR THREE CATEGORICAL PREDICTORS
	} else if (length(outcome)==1 & length(predictors)==3 & length(categories)==3){

	
		#### figure out which variable has the largest effect
		mod = lm(formula, data=data)

		order = order(anova(mod)[-length(coef(mod)),"F value"], decreasing=F)
		ordered.predictors = row.names(anova(mod))[order]
		new.formula = make.formula(outcome, ordered.predictors)
		
		#### reorder the means
		means = aggregate(new.formula, data=data, FUN=mean)
		names(means)[ncol(means)]="mean"
		ser = aggregate(new.formula, data=data, FUN=function(x){sd(x)})
		means$ser = ser[,outcome]
		means$lower = means$mean - means$ser
		means$upper = means$mean + means$ser

		#### combine with previous dataset
		d = merge(data, means, by=ordered.predictors)
		if (se){
			p = ggplot(data=d, aes_string(x= ordered.predictors[1], y=outcome, col=ordered.predictors[1])) + 
				geom_jitter(data=sample.subset(sample, d), alpha = raw.alph.func(raw.data, .15)) +
				geom_point(aes_string(y="mean")) + 
				geom_errorbar(aes_string(ymin="lower", ymax= "upper", col=ordered.predictors), width=.2) +
				coord_flip() + 
				facet_grid(as.formula(paste0(ordered.predictors[2]," +", ordered.predictors[3],"~."))) +
				theme_bw()
		} else {
			p = ggplot(data=d, aes_string(x= ordered.predictors[1], y=outcome, col=ordered.predictors[1])) + 
				geom_jitter(data=sample.subset(sample, d), alpha = raw.alph.func(raw.data, .15)) +
				geom_point(aes_string(y="mean")) + 
				coord_flip() + 
				facet_grid(as.formula(paste0(ordered.predictors[2]," +", ordered.predictors[3],"~."))) +
				theme_bw()			
		}				
		
	##### FOR VARAIBLES THAT WILL BE BINNED...
	} else {





		
		#### repeat this (otherwise it references the old dataset, before things were binned)
		if (!is.null(jitter)){
				if (jitter[1]==T){
					jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=.2, height=.2)
				} else if (jitter[1] == F){
					jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
				} else {
					jit = geom_jitter(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha), width=jitter[1], height=jitter[2])
				}
				
			} else {
				jit = geom_point(data=sample.subset(sample, data), alpha=raw.alph.func(raw.data, alpha=alpha))
		}

		
		if (length(axis)>1){

			p = ggplot(data=data, aes_string(x=axis[1], y=outcome, shape=axis[2], linetype=axis[2], color=axis[2]))+
					gm + 
					jit + 
					giv + 
					theme_bw()
		} else {
				
			p = ggplot(data=data, aes_string(x=axis[1], y=outcome))+
				jit + 
				gm +
				giv + 
				theme_bw()			
		}


		if (!is.null(ghost.line)){ # with help from https://stackoverflow.com/questions/52682789/how-to-add-a-lowess-or-lm-line-to-an-existing-facet-grid/52683068#52683068

			### quit if they try to do two axes
			if (!is.na(axis[2])){
				stop("Sorry. I can't plot a second variable on the x axis. Try putting it in the given area (e.g., y~ x + z | b should become y~ x | b + z)")
			}
			
			### make sure the reference groups are all in the data
			if (sum(names(ghost.reference) %in%  variables) != length(ghost.reference)){
				missing.var = names(ghost.reference[!(names(ghost.reference) %in% variables)])
				msg = paste0("Sorry. One of your variables (", missing.var, ") is not in your formula.")
				stop(msg)
			}


			#### if they don't specify a reference group, choose one
			if (is.null(ghost.reference)){
				l = data[1,given]
			} 
				# if (length(ghost.reference)!=length(given)){
					# stop("When referencing a 'ghost line,' you must specify the value for each 'given' variable.")
				# }
				
	
			ghost.names = names(ghost.reference)
			##### select those columns in d specified
			k = data
			for (s in 1:length(ghost.reference)){
				k = k[k[,ghost.names[s]]==ghost.reference[s],]
			}				

			### identify which variables are in the given category
			ghost.given = which(ghost.names %in% given)
			g0 = ggplot(data=k, aes_string(x=axis[1], y=outcome))+gm
			d_smooth = ggplot_build(g0)$data[[1]]; 
			### rename columns
			names(d_smooth)[names(d_smooth)=="x"] = axis[1]; names(d_smooth)[names(d_smooth)=="y"] = outcome; 


			## add line to existing plot   
			p = p + geom_line(data=d_smooth, aes_string(x=axis[1], y= outcome), color=ghost.line)
			
		}		

		
	

	}	
			p
	if (!is.null(prediction)){	
		
		#### check if first variable is a continuous predictor
		if (is.numeric(data[,predictors])){
			
			print("got here")
			p = p + geom_line(data= prediction, aes(linetype=model, y=prediction, color=model), size=2)			
		} else {
			p = p + geom_point(data=prediction, aes(y=prediction, color=model), position=position_dodge(width=.2))
		}
		return(p)
	} else {
			return(p)
	}
}
