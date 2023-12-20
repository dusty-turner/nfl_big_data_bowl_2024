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

simplifier <- function(data){
  if(Sys.info()["user"] == "dusty_turner1") {
    data
  } else {
    data %>% 
      filter(week_id == "week_1")
  }
}

x_list_maker <- function(data) {
  if (Sys.info()["user"] == "dusty_turner1") {
    map2(
      .x = example_play$game_id,
      .y = example_play$play_id,
      .f = ~ get_play_data(.x, .y, newdata, num_features_per_player_arg = num_features_per_player),
      .progress = TRUE
    )
  } else {
    library(furrr)
    plan(multisession)
    future_map2(
      .x = example_play$game_id,
      .y = example_play$play_id,
      .f = ~ get_play_data(.x, .y, newdata, num_features_per_player_arg = num_features_per_player),
      .progress = TRUE
    )
  }
}

y_list_maker <- function(data) {
  if (Sys.info()["user"] == "dusty_turner1") {
    map2(
      .x = example_play$game_id,
      .y = example_play$play_id,
      .f = ~ create_target_matrix(.x, .y, newdata, padded_length = max_length),
      .progress = TRUE
    )
  } else {
    library(furrr)
    plan(multisession)
    future_map2(
      .x = example_play$game_id,
      .y = example_play$play_id,
      .f = ~ create_target_matrix(.x, .y, newdata, padded_length = max_length),
      .progress = TRUE
    )
  }
}