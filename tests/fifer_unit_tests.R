clear()
devtools::document("research/RPackages/fifer")
devtools::install("research/RPackages/fifer")

require(fifer)
		#### test impute.me with flexplot
data(exercise_data)
mod = lm(muscle.gain.missing~motivation+rewards, data=exercise_data)
mod.imp = impute.me(mod, data=exercise_data, return.mod=T, predictors="muscle.gain", keep=F)
flexplot::compare.fits(muscle.gain.missing~motivation|rewards, data=exercise_data, mod, mod.imp)




		### rf with numeric outcome and gini
data(exercise_data)		
a = rfThresh(weight.loss~., data=exercise_data, nruns=10, importance="gini")
a; plot(a);
b = fifer::rfInterp(a, nruns=5, importance="gini")
b; plot(b)
c = fifer::rfPred(b, nfor.pred=5, importance="gini")
c; plot(c)


		### rf with numeric outcome and permutation
data(exercise_data)		
a = rfThresh(weight.loss~., data=exercise_data, nruns=2, importance="permutation")
a; plot(a);
b = fifer::rfInterp(a, nruns=5)
b; plot(b)
c = rfPred(b, nfor.pred=5)
c; plot(c)


data(authors)
d = authors