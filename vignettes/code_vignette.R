##################################################
##################################################
##################################################
##################################################
		# INSTALLATION
##################################################
##################################################
##################################################
##################################################

### first install devtools so I can use their install_github function
install.packages("devtools")
require(devtools)
### install fifer with the install_github function
devtools::install_github("fifer", username="dustinfife")
require(fifer)

##################################################
##################################################
##################################################
##################################################
		# BROWSE THE PACKAGE
##################################################
##################################################
##################################################
##################################################

### look at functions in the fifer package
contents("fifer")
### pull up vignette
vignette("fifer_package")

##################################################
##################################################
##################################################
##################################################
		# DATA MANIPULATION
##################################################
##################################################
##################################################
##################################################

### load the fake medical data
data(fakeMedicalData)
### look at first several rows of fakeMedicalData
head(fakeMedicalData)


### DEMONSTRATE THE r (range) FUNCTION
#################
#################
#################

## see documentation
?r 
### extract column indices between B_regs_10A and B_regs_9E
bregs = r("B_regs_10A", "B_regs_9E", data.names=names(fakeMedicalData))
bregs

### extract column names instead
bregs = r("B_regs_10A", "B_regs_9E", data.names=names(fakeMedicalData), names=T)
bregs


### DEMONSTRATE THE make.null FUNCTION
#################
#################
#################

### see documentation
?make.null
### extract only demographics columns and bregs columns
newData = make.null("ID", "gender", "ethnicity", "age", "disease",
					bregs, 
					data=fakeMedicalData,
					keep=TRUE)
### we could instead drop everything after bregs
newData2 = make.null(
			r("BCI_10A", "TNF_9E", data.names=names(fakeMedicalData)),
			data=fakeMedicalData, keep=FALSE)					
dim(newData)
dim(newData2)

### DEMONSTRATE THE excelMatch FUNCTION
#################
#################
#################

### see documentation
?excelMatch

#### extact the variable names corresponding to Excel Columns AA, CD, and FF
excel.names = excelMatch("AA", "CD", "FF", names=names(fakeMedicalData))excel.names

### or, we can extract the column indices instead### (note it doesn't require names in original dataset)excel.names = excelMatch("AA", "CD", "FF", n=length(names(fakeMedicalData))) 
excel.names

### now subset the matrix to just those using make.nullnew.dat = make.null(excel.names, data=fakeMedicalData, keep=T)head(new.dat)