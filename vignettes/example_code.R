#rm(list=ls())
data(fakeMedicalData)
#### perform univariate tests on all but the ID variable
names(fakeMedicalData)
p.values = univariate.tests(dataframe=fakeMedicalData, exclude.cols=1, group="disease")
p.adjusted = p.adjust(p.values, method="fdr")
p.adjusted[p.adjusted<.05]

best.five = names(sort(p.adjusted)[1:5])
### prepare the layout
auto.layout(5)
for (i in 1:length(best.five)){
	
	### do my favority default plotting parameters
	par1() 
	
	### make a formula
	formula = make.formula(best.five[i], "disease")
	
	### plot them
	prism.plots(formula, data=fakeMedicalData)
	
	### show significance bars
	plotSigBars(formula, data=fakeMedicalData, type="tukey")
}


for (i in 1:length(best.five)){
	
	### do my favority default plotting parameters
	par1() 
	
	### make a formula
	formula = make.formula(best.five[i], "disease")
	
	### plot them
	densityPlotR(formula, data=fakeMedicalData)
}


	### change default parameters
par2() 
	#### color code according to disease status
colors = string.to.color(fakeMedicalData$disease, colors=c("blue", "red"))
	#### change symbol according to disease status
pch = as.numeric(string.to.color(fakeMedicalData$disease, colors=c(15, 16)))
	#### plot it
plot(fakeMedicalData[,best.five[1]], fakeMedicalData[,best.five[2]], col=colors, pch=pch,
		xlab = best.five[1], ylab=best.five[2])
		
		
	#### simulate skewed data
x = rnorm(100)^2
y = rnorm(100)^2

	### induce a correlation of .6 (approx) with choselski decomp
cor = matrix(c(1, .6, .6, 1), nrow=2)	
skewed.data = cbind(x,y)%*%chol(cor)
names(skewed.data) = c("x", "y")


	#### show original plot
par2()	
plot(skewed.data, xlab="x", ylab="y")
par2()
spearman.plot(skewed.data, xlab="rank(x)", ylab="rank(y)", pch=16)
	