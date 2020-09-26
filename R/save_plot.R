#' Save a plot for 1080p
#'
#' @param plot a ggplot2 object
#' @param file_location the location where the file will be saved
#' @param file_name the name of the file
#' @param text.size the size of the font. Defaults to 18
#'
#' @return a plot
#' @export
save_plot = function(plot, file_location="", file_name="plot", text.size=18){
  plot = plot + theme(text=element_text(size=text.size, family="Moon Flower Bold"))	
  ggplot2::ggsave(plot=plot, path=file_location, filename = paste0(file_name, ".jpg"), width=1920/150, height=1080/150, units="in", dpi=150)
}



