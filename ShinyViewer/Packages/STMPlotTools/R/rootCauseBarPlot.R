#' Performs a bar plot
#' @title Bar plot
#' @param title the title of the plot 
#' @param x values to be plotted on the x axes
#' @param y values to be plotted on the y axes
#' @param xlab labels of the x axes
#' @param ylab labels of the y axes
#'
#' @return nothing, just perform the plot
#' @import ggplot2 ggthemes
#' @export

rootCauseBarPlot = function(x, y, title, xlab = "x", ylab = "y"){
  plotData = data.frame(X = x, Y = y)
  ggplot(data = plotData, mapping = aes(x = X, y = Y)) +
    geom_bar(mapping = aes(fill = Y), stat = "identity", position = position_dodge(), show.legend = FALSE) +
    coord_flip(ylim = c(min(plotData$Y)*0.9, max(plotData$Y)*1.1)) +
    ggtitle(title) +
    labs(x = xlab, y = ylab) +
    theme_classic(base_family = "serif") + 
    theme(panel.grid.major.x = element_line(colour = "black", size = 0.5, linetype = "dotted"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.ontop = TRUE) +
    scale_fill_gradient(low = "#9999ff", high = "#000066", guide = FALSE)
}