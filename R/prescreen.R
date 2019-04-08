# clear()
# d = read.csv("school/Fall 2017/Multivariate/datasets/weightLoss.csv")
# data = d
# head(d)
# vars = c("therapy.type", "health", "motivation", "weight.change", "gender", "satisfaction", "muscle.gain", "muscle.gain.missing", "rewards", "income")


# prescreen = function(..., data, input=T, directory=getwd(), file.name="prescreen.pdf", save=T){
	
	# args = unlist(list(...))
	
	# if (length(args)>16){
		# warning("Whoa there, tiger! That's a lot of variables. I'll try, but you might be better off doing the variables in sections. \n")
	# }
	# d = data #laziness on my part
	
	
	
	# #### determine which variables are NOT in dataset
	# if (length(which(!(args %in% names(data))))!=0){
		# stop("One of the variable names given is not in the dataset")
	# }
	
	# #### go through each variable and determine number of levels. If < 4, we'll assume it's nominal. Otherwise, we treat as quantitative

# i=1

	# ### preallocate vector to store data types for each
	# types = data.frame(variable=args, type=1:length(args))
	# for (i in 1:length(args)){
		# levs = unique(d[,args[i]])
		# if (length(levs)<5 | is.factor(levs)){
			# types[i,2] = "factor"
		# } else {
			# types[i,2] = "numeric"
		# }
	# }
	# cat("\n\n Here's how I'm categorizing your variables:\n\n")	
	# print(types)
	
	# ##### ask for user input
	# if (input){
		# my.name = readline(prompt="\nI'm going to assume you want your variables coded as above ^? Is that correct? Y for Yes, N for No: ")
		# s = 0
		# while (my.name != "Y"){
			# s = s+1
			# change = ifelse(types[1,2]=="numeric", "factor", "numeric")
			# row = readline(prompt=paste0("Enter the row number you want to edit. (e.g., 1 if you want to convert ", types[1,1], " from ", types[i,2], " to ", change, "):"))
			# types[row,2] = ifelse(types[row,2]=="numeric", "factor", "numeric")
			# if (s==1) {
				# rows.to.change = row
			# } else {
				# rows.to.change = c(rows.to.change, row)
			# }
			# cat("New dataset: \n")
			# print(types)
			# my.name = readline(prompt="Done! Anything else you want to change? Y for Yes, N for No: ")
			# my.name = ifelse(my.name=="Y", "N","Y")
		# }

	# cat("\n\n Done figuring out data types! Now, if you converted a factor to numeric, you may have some missing data as a result. Here's the new dataset:\n\n")
	# print(types)
	# }

	
	# #### now do the conversions
	# factors = data.frame(apply(d[,which(types$type=="factor")], 2, as.factor))
	# numerics = data.frame(apply(d[,which(types$type=="numeric")], 2, function(x){as.numeric(as.character(x))}))

	
	# names(factors) = types$variable[types$type=="factor"]
	# names(numerics) = types$variable[types$type=="numeric"]	
	
	# if (save){
		# pdf(paste0(directory, "/prescreen.pdf"))
	# }
	# #### show barcharts
	# auto.layout(ncol(factors) + ncol(numerics))
	# par2()
	# par(mar=c(3,3,3,1))
	# for (i in 1:ncol(factors)){
		# barplot(table(factors[,i]), main=paste0("Barchart of ", names(factors)[i]), xlab="")
	# }


	# #### plot histograms
	# list.before = ls()	
	# par2()
	# for (i in 1:ncol(numerics)){
		# hist(numerics[,i], main=paste0("Histogram of ", names(numerics)[i]), xlab="")
		# outlier_values = boxplot.stats(numerics[,i])$out  # outlier values.
		# if (length(outlier_values)>0){
			# assign(paste0(names(numerics)[i]), outlier_values)
		# }
	# }
	# list.aft = ls()
	# outliers = list.aft[which(!(list.aft %in% c(list.before, "list.before")))]
	
	# if (save){
		# dev.off()		
		# msg = paste0("Your plots have been saved at ", directory, "/prescreen.pdf")
	# } else {
		# msg = ""
	# }

	
	
	# if (length(outliers)>0){
		# cat("\n\nYou have the following outliers:")
		# for (i in 1:length(outliers)){
			# cat("\n", outliers[i], ": ", paste0(get0(outliers[i]), collapse=", "))
		# }
	
		# cat("\n\nYou might want to check those values. It has one or more pretty significant outliers. Unfortunately, I can't handle that for you :)")
	# }
	
	
	# cat(paste0("\n\nLast but not least, be sure to check the plots to make sure things look right.", msg, ". See you at the next step!"))
	
	# #### do some outlier detection
	# # you might want to check the variable [input]. 
	
	# ##### return the original data object as a list
	# d = data.frame(factors, numerics)
	# list(data=d, vars=vars, types=types$type)
	
# }


# ps = prescreen(vars, data=d, input=T)
