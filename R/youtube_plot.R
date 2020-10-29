#' Create a 1080p plot for YouTube
#'
#' @param p the ggplot object
#' @param name the name of the file (excluding the ".jpg")
#' @param path the path to the folder
#'
#' @export
youtube_plot = function(p, name="plot", path=""){
  require(extrafont)
  loadfonts(quiet=TRUE)
  p = p + theme(text=element_text(size=18, family="Moon Flower Bold"))
  file_path = ifelse(path=="", paste0(name, ".jpg"), paste0(path, "/", name, ".jpg"))
  ggsave(file_path, plot=p, height = 1080/150, width=1920/150, dpi=150, units = "in")
}

