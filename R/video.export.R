##' Export to video format a ggplot image
##'
##' Export to video format a ggplot image
##'	
##' Export to video format a ggplot image
##' @param a a ggplot object to be exported
##' @param name the name of the file
##' @param folder the directory in images where the file is to be placed
##' @param shape.plot the shape of the plot for scaling. Can be "vertical", "horizontal", or "square"
##' @export
##' @import extrafont flexplot ggplot2 egg
##' @examples
##' d = data.frame(Y=rnorm(10000, 50, 10))
##' a = flexplot::flexplot(Y~1, data=d)
##' video.export(a, "histogram", "distributions", num.plots=1)
video.export = function(a, name, folder="images", shape.plot = "horizontal"){

	### load fonts
	extrafont::loadfonts()
	
	### create folder if it doesn't exist
	path = "../Google Drive/images"
	dir.create(file.path(path, folder), showWarnings = FALSE)


	
	### save to file with appropriate size
	loc = paste0(c(path, folder, paste0(name, ".jpg")), collapse="/")
	if (shape.plot=="horizontal"){

		### append text to ggplot object
		a = a + theme(text=element_text(size=16, family="Moon Flower Bold"))	

		ggsave(loc, egg::set_panel_size(a, width=unit(4, "in"), height=unit(3, "in")),
			width=6.4, height=3.6)
	} else if (shape.plot=="square"){
		a = a + theme(plot.margin =unit(c(0,1.4,0, 1.4),"in"), text=element_text(size=0, family="Moon Flower Bold"))
		return(a)
		#ggsave(loc,a,width=6.4, height=3.6, dpi=300)
	} else if (shape.plot=="panel"){
		ggsave(loc, a)
	}
	
}

