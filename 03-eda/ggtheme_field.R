library(keras)
library(tensorflow)

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

custom_accuracy <- function(y_true, y_pred) {
  # Apply threshold to get binary class predictions
  threshold <- 0.5
  y_pred_binary <- k_cast(k_greater_equal(y_pred, threshold), 'float32')
  
  # Calculate accuracy
  correct_predictions <- k_equal(y_pred_binary, y_true)
  return(k_mean(correct_predictions))
}

# Custom F1 Score with a unique name
f1_score <- function(y_true, y_pred) {
  precision <- k_cast(k_sum(k_round(k_clip(y_true * y_pred, 0, 1))) / k_sum(k_round(k_clip(y_pred, 0, 1)) + k_epsilon()), 'float32')
  recall <- k_cast(k_sum(k_round(k_clip(y_true * y_pred, 0, 1))) / k_sum(k_round(k_clip(y_true, 0, 1)) + k_epsilon()), 'float32')
  f1 <- 2 * ((precision * recall) / (precision + recall + k_epsilon()))
  k_mean(f1, axis = -1L) %>% k_identity(name = 'f1_score')  # Assign a unique name
}

# # Custom Log Loss with a unique name
# log_loss <- function(y_true, y_pred) {
#   loss <- keras::binary_crossentropy(target = y_true, output = y_pred)
#   k_mean(loss, axis = -1L) %>% k_identity(name = 'log_loss')  # Assign a unique name
# }


# Define Model Checkpoint Callback
model_checkpoint_callback <- callback_model_checkpoint(
  filepath = "best_model.h5",  # Save the best model to this file path
  save_best_only = TRUE,
  monitor = "val_loss",
  verbose = 1
)

# Define Reduce Learning Rate on Plateau Callback
reduce_lr_callback <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",
  factor = 0.1,
  patience = 50,
  verbose = 1
)

# Define TensorBoard Callback
# tensorboard_callback <- callback_tensorboard(
#   log_dir = "./logs",
#   histogram_freq = 1
# )

# Define CSV Logger Callback
# csv_logger_callback <- callback_csv_logger("training_log.csv")

early_stop_callback <- callback_early_stopping(
  monitor = "val_loss",  # Monitor the validation loss
  patience = 100,         # Number of epochs with no improvement after which training will be stopped
  restore_best_weights = TRUE  # Restores model weights from the epoch with the best value of the monitored quantity
)

# Define the LSTM model architecture with more regularization
# model <- keras_model_sequential() %>%
#   layer_masking(mask_value = 0, input_shape = c(max_length, num_features)) %>%
#   bidirectional(layer_lstm(units = units_param, return_sequences = TRUE,
#                            # recurrent_activation = 'relu' ## this made it give NANs for the loss
#                            kernel_regularizer = regularizer_l2(l2_value_param),
#                            recurrent_regularizer = regularizer_l2(l2_value_param)
#                            )) %>%
#   layer_dropout(rate = rate_param) %>%
#   bidirectional(layer_lstm(units = units_param, return_sequences = TRUE,
#   #                          recurrent_activation = 'relu'
#                            kernel_regularizer = regularizer_l2(l2_value_param),
#                            recurrent_regularizer = regularizer_l2(l2_value_param)
#                            )) %>%
#   layer_dropout(rate = rate_param) %>%
#   bidirectional(layer_lstm(units = units_param, return_sequences = TRUE,
#   #                          recurrent_activation = 'relu'
#                            kernel_regularizer = regularizer_l2(l2_value_param),
#                            recurrent_regularizer = regularizer_l2(l2_value_param)
#                            )) %>%
#   time_distributed(layer_dense(units = 11, activation = 'sigmoid'))  # Adjust the unit count based on output shape
