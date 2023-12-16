theme_field <- theme(panel.background = element_rect(fill = "white",
                                                     colour = "white",
                                                     linewidth = 0.5, linetype = "solid"),
                     panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                                     colour = "black"),
                     panel.grid.major.y = element_blank())

grab <- function(x){
  x[,,1] 
}

plot_matrix <- function(mat) {
  par(mar = rep(0, 4))
  standardize <- function(x) (x - min(x)) / (max(x) - min(x)) 
  mat |>
    standardize() |>
    as.raster() |>
    plot(interpolate = FALSE) # ?graphics::plot.raster
}

