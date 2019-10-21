##' Export to video format a ggplot image
##'
##' This will take a ggplot2 object and save it with the appropriate settings for a 1080p video. 
##' It also attempts to maintain correct aspect ratios in the process (ideally). 
##'	
##' Export to video format a ggplot image
##' @param a a ggplot object to be exported
##' @param name the name of the file
##' @param folder the directory in images where the file is to be placed. This defaults to "images" in the root folder. 
##' @param shape.plot the shape of the plot for scaling. Can be "vertical", "horizontal", or "square"
##' @export
##' @import extrafont flexplot ggplot2 egg
##' @references There's some diffulty with setting a fixed size: 
##' \url{https://stackoverflow.com/questions/54892086/setting-canvas-size-in-ggsave-to-a-fixed-width-while-maintaining-aspect-ratio-of}
##' @examples
##' d = data.frame(Y=rnorm(10000, 50, 10))
##' a = flexplot::flexplot(Y~1, data=d)
##' video.export(a, "histogram", "distributions")
video.export = function(a, name, folder="images", shape.plot = "horizontal"){

	### load fonts
	extrafont::loadfonts()
	
	### create folder if it doesn't exist
	dir.create(file.path(folder), showWarnings = FALSE)


	
	### save to file with appropriate size
	loc = paste0(c(folder, paste0(name, ".jpg")), collapse="/")
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
