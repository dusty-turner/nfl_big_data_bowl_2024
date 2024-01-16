library(tidyverse)
library(tidymodels)
source(here::here("03-eda", "1-data-clearning.R"))

running_data <- 
  dplyr::filter(defensive_model_building_data_model, is.na(pass_result)) |> select(-pass_result) |> 
  mutate(ball_in_fan = as.factor(ball_in_fan))  |> 
  mutate(position = if_else(is.na(position), "unknown", position)) |> 
  mutate(across(c(x_ball_next,y_ball_next), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |> 
  mutate(across(c(x_ball_next,y_ball_next), ~ifelse(is.nan(.), 25, .))) |> 
  mutate(across(c(x_ball_next,y_ball_next, distance_to_ball_next), ~ifelse(is.infinite(.), 25, .))) 


set.seed(49)
splits <- group_initial_validation_split(running_data, prop = c(.05,.01), group = game_idplay_id)
# splits <- group_initial_validation_split(running_data, prop = c(.02,.005), group = game_idplay_id)

defense_training <- training(splits) 
defense_validation <- validation_set(splits)
defense_test  <- testing(splits)

nrow_train <- splits$train_id |> length()
nrow_val <- splits$val_id |> length()
nrow_test<- splits$data |> nrow() - nrow_train - nrow_val
baseline_accuracy <- defense_test |> reframe(accuracy = (1-mean(as.numeric(as.character(tackle)), na.rm = TRUE))*100) |> round(2) |> str_c("%")

# assessment(defense_validation$splits[[1]]) 



mod <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") |>
  set_mode("classification")

# mod <-
#   rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
#   set_engine("ranger", importance = "impurity") |>
#   set_mode("classification")

# mod <-
#   boost_tree(
#              tree_depth = tune(),
#              trees = tune(),
#              learn_rate = tune(),
#              min_n = tune(),
#              loss_reduction = tune(),
#              sample_size = tune()
#              # stop_iter = tune()
#              ) %>%
#   set_engine('xgboost') %>%
#   set_mode('classification')


recipe <-
  recipe(tackle ~ ., data = defense_training) |> 
  step_rm(game_idplay_id, game_id, play_id, nfl_id, frame_id, club, x, y, x_ball, y_ball, ball_carrier, ball_carrier_id, ball_carrier_display_name, absolute_yardline_number,
          time, defensive_team, display_name, play_description, is_football, rank, v_approach) |>
  # step_rm(game_idplay_id, game_id, play_id, nfl_id, frame_id, club, x, y, x_going, y_going, ball_carrier, ball_carrier_id, ball_carrier_display_name, absolute_yardline_number,
  #         time, defensive_team, display_name, play_description, is_football, rank, v_approach) |>
  step_impute_mode(position) |>
  step_dummy(all_nominal_predictors()) |> 
  # step_impute_mean(v_approach)  
  step_interact(terms = ~starts_with("position"):starts_with("alignment_cluster"))

workflow <-
  workflow(spec = mod) |> 
  add_recipe(recipe = recipe)
  
grid <- expand_grid(penalty = 10^seq(-4, 1, length.out = 30)/10, mixture = 10^seq(-4, 1, length.out = 30)/10)

# grid <- grid_latin_hypercube(trees(), min_n(), mtry(range = c(4,17)), size = 20)

# grid <- grid_latin_hypercube(
#                      tree_depth(range = c(1, 10)),
#                              trees(range = c(50, 1000)),
#                              learn_rate(range = c(0.001, 0.1)),
#                      min_n(range = c(1, 10)),
#                      loss_reduction(range = c(-1, 2), trans = log10_trans()),
#                      sample_prop(range = c(1/10,1)),  # Assuming 'sample_size' is an integer and you're tuning it over a reasonable range
#                      # stop_iter(range = c(2, 10)),
#                      size = 20
# )

# Register a parallel backend to use multicore processing
doParallel::registerDoParallel(cores = parallel::detectCores())

res <- 
  workflow %>% 
  tune_grid(resamples = defense_validation,
            grid = grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc, accuracy))

# best_parameters <- select_best(lr_res, "roc_auc")

best_parameters <-
  show_best(res, "accuracy", n = 900) |> filter(mean == max(mean)) |> 
    filter(mixture != 1) |> 
  arrange(desc(penalty)) |> 
    arrange(desc(mixture)) |> 
  slice(1)
# best_parameters <- show_best(res, "accuracy", n = 1) #|> slice(5)

update_wflow <- finalize_workflow(workflow, best_parameters)

defense_fit <- last_fit(update_wflow, split = splits, add_validation_set = TRUE)

collect_metrics(defense_fit)

# interpret <- defense_fit |> extract_fit_parsnip() |> vip::vip()
interpret <- defense_fit |> extract_fit_parsnip() |> tidy() |> print(n = Inf)

# extract_fit_engine(defense_fit) |> tidy() |> print(n = Inf) filter(Lambda == best_parameters$penalty)
# extract_fit_engine(defense_fit) |> tidy() |> filter(step ==1) |> pull(Lambda)

prob_of_tackle <- defense_fit |> select(.predictions) |> unnest(.predictions) |> pull(.pred_1) 

# plot_this <- defense_test |>   group_by(game_idplay_id) |> filter(cur_group_id()==5) |> 
#   # select(play_description) 
#   ungroup()  |> distinct(game_id, play_id)


data_joined_with_pred <- 
week_1 |> left_join(
  defense_test |> select("game_id","play_id", "nfl_id", "display_name",  "frame_id") |> 
  mutate(prob_of_tackle = prob_of_tackle) 
  ) 



# data_joined_with_pred |> 
#   select(tackle, prob_of_tackle) |> 
#   filter(!is.na(prob_of_tackle)) |> arrange(-prob_of_tackle)
#   mutate(tackle = as.factor(tackle)) |> 
#   mutate(declared = as.factor(ifelse(prob_of_tackle > .5 , 1 , 0))) |> 
#   yardstick::accuracy(tackle, declared)


# data_joined_with_pred |> 
#   filter(game_id == plot_this$game_id, play_id == plot_this$play_id) |> 
#   mutate(color = case_when(ball_carrier == TRUE ~ "ballcarrier",
#                          tackle == "1" ~ "tackler",
#                          club == defensive_team ~ "defense",
#                          club != defensive_team ~ "offense")) |> 
#   select(distance_to_ball, x, y, color, absolute_yardline_number, ball_carrier, play_id, time, prob_of_tackle, play_description, is_football) %>%
#   {
#     ggplot(data = ., aes(x = x, y = y, alpha = prob_of_tackle)) +
#       geom_vline(aes(xintercept = absolute_yardline_number), color = "yellow") +
#       geom_point(aes(shape = is_football), show.legend = FALSE) +
#       # scale_color_gradient2(na.value = "dodgerblue", low = "black", high = "grey") +
#       # facet_wrap(~play_id) +
#       transition_time(time) + ease_aes("linear") +
#       labs(y = "", x = "Yards To Endzone", caption = .$play_description[1]) +
#       theme_field
#   }


data_joined_with_pred |> 
  select(game_id, play_id, nfl_id, display_name, tackle, prob_of_tackle) |> 
  filter(!is.na(prob_of_tackle)) |> 
  mutate(tackle  = as.integer(as.character(tackle))) |> 
  group_by(game_id, play_id, nfl_id, display_name) |> 
  reframe(expected_prob_of_tackle = mean(prob_of_tackle), tackle = mean(tackle)) |> 
  mutate(tackles_over_expected_play = ifelse(tackle == 1, 1-expected_prob_of_tackle, -expected_prob_of_tackle)) |> 
  group_by(nfl_id, display_name) |> 
  reframe(tackles_over_expected = sum(tackles_over_expected_play)) |> 
  arrange(-tackles_over_expected)
  
data_joined_with_pred |> 
  group_by(game_id, play_id, frame_id) |> 
  filter(cur_group_id() == 500) |> 
    reframe(total_prob = sum(prob_of_tackle, na.rm = TRUE)) 
  
#### getting items ready for penalized regression slides
pen_briar <-
data_joined_with_pred |> 
  select(game_id, play_id, nfl_id, display_name, tackle, prob_of_tackle) |> 
  filter(!is.na(prob_of_tackle)) |> 
  mutate(tackle  = as.integer(as.character(tackle))) |> 
  group_by(game_id, play_id, nfl_id, display_name) |> 
  reframe(expected_prob_of_tackle = mean(prob_of_tackle), tackle = mean(tackle)) |> 
  mutate(tackles_over_expected_play = ifelse(tackle == 1, 1-expected_prob_of_tackle, -expected_prob_of_tackle)) |> 
  group_by(nfl_id, display_name) |> 
  reframe(tackles_over_expected = sum(tackles_over_expected_play)) |> 
  arrange(-tackles_over_expected)

pen_grid <-
grid |> 
  ggplot(aes(x = penalty, y = mixture)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Penalty(Lambda)", y = "Mixture(Alpha)") +
  # scale_x_log10() +
  # scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Penalized Regression Tuning Parameters")

# pen_grid_results <-
res$.metrics[[1]] |> 
  select(penalty, mixture, .estimate) |>
  ggplot(aes(x = penalty, y = mixture, color = .estimate)) +
  # geom_tile() +  # Use geom_tile() for the heatmap
  geom_point() +  # Use geom_tile() for the heatmap
  scale_color_gradient2(low = "blue", high = "red", midpoint = .6) +  # Use a gradient from low to high .estimate values
  # scale_fill_gradient2(low = "blue", high = "red", midpoint = .6) +  # Use a gradient from low to high .estimate values
  theme_minimal() +
  labs(x = "Penalty(Lambda)", y = "Mixture(Alpha)", fill = "Accuracy") +
  # scale_x_log10() +
  # scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Optimal Tuning Parameters")

list(pen_grid = pen_grid, pen_grid_results = pen_grid_results, best_parameters = best_parameters, pen_briar = pen_briar, interpret = interpret, pen_accuracy = collect_metrics(defense_fit),
     nrow_train = nrow_train, nrow_val = nrow_val, nrow_test = nrow_test, baseline_accuracy = baseline_accuracy) |>  
  write_rds(file = "99-addm/penalty.RDS")
  
# defense_test |> 
#   select(tackle) |> 
#   mutate(tackle = as.numeric(as.character(tackle))) |> 
#   reframe(perc = (1-sum(tackle)/nrow(defense_test))*100) 
  


#### getting items ready for rf slides
rf_briar <-
  data_joined_with_pred |> 
  select(game_id, play_id, nfl_id, display_name, tackle, prob_of_tackle) |> 
  filter(!is.na(prob_of_tackle)) |> 
  mutate(tackle  = as.integer(as.character(tackle))) |> 
  group_by(game_id, play_id, nfl_id, display_name) |> 
  reframe(expected_prob_of_tackle = mean(prob_of_tackle), tackle = mean(tackle)) |> 
  mutate(tackles_over_expected_play = ifelse(tackle == 1, 1-expected_prob_of_tackle, -expected_prob_of_tackle)) |> 
  group_by(nfl_id, display_name) |> 
  reframe(tackles_over_expected = sum(tackles_over_expected_play)) |> 
  arrange(-tackles_over_expected)


rf_grid <-
  grid |> 
  ggplot(aes(x = min_n, y = mtry, size = trees)) +
  geom_point(alpha = 0.7) +  # Adjust transparency with alpha
  scale_size_continuous(name = "Number of Trees") +
  theme_minimal() +
  labs(x = "Min_n", y = "Mtry", title = "Latin Hyper-Cube Of Parameter Space")

rf_grid_results <-
  res$.metrics[[1]] |> 
  filter(.metric == "accuracy") |> 
  select(mtry, trees, min_n, .estimate) |>
    ggplot(aes(x = min_n, y = mtry, size = trees, color = .estimate)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(name = "Number of Trees") +
    scale_color_gradient(low = "blue", high = "red", name = "Accuracy") +
    theme_minimal() +
    labs(x = "Min_n", y = "Mtry", title = "Optimal Tuning Parameters")


list(rf_grid = rf_grid, rf_grid_results = rf_grid_results, best_parameters = best_parameters, rf_briar = rf_briar, rf_accuracy = collect_metrics(defense_fit), interpret = interpret,
     nrow_train = nrow_train, nrow_val = nrow_val, nrow_test = nrow_test, baseline_accuracy = baseline_accuracy) |>  
  write_rds(file = "99-addm/rf.RDS")

#### getting items ready for xgboost slides
xg_briar <-
  data_joined_with_pred |> 
  select(game_id, play_id, nfl_id, display_name, tackle, prob_of_tackle) |> 
  filter(!is.na(prob_of_tackle)) |> 
  mutate(tackle  = as.integer(as.character(tackle))) |> 
  group_by(game_id, play_id, nfl_id, display_name) |> 
  reframe(expected_prob_of_tackle = mean(prob_of_tackle), tackle = mean(tackle)) |> 
  mutate(tackles_over_expected_play = ifelse(tackle == 1, 1-expected_prob_of_tackle, -expected_prob_of_tackle)) |> 
  group_by(nfl_id, display_name) |> 
  reframe(tackles_over_expected = sum(tackles_over_expected_play)) |> 
  arrange(-tackles_over_expected)


xg_grid <-
  grid |> 
  ggplot(aes(x = tree_depth, y = min_n, size = trees)) +
  geom_point(alpha = 0.7) +  # Adjust transparency with alpha
  scale_size_continuous(name = "Number of Trees") +
  theme_minimal() +
  labs(x = "Tree Depth", y = "Min_n", title = "Latin Hyper-Cube Of Parameter Space")

xg_grid_results <-
  res$.metrics[[1]] |>
  filter(.metric == "accuracy") |> 
  # select(mtry, trees, min_n, .estimate) |>
  ggplot(aes(x = tree_depth, y = min_n, size = trees, color = .estimate)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(name = "Number of Trees") +
    scale_color_gradient(low = "blue", high = "red", name = "Accuracy") +
    theme_minimal() +
    labs(x = "Tree Depth", y = "Min_n", title = "Optimal Tuning Parameters")

list(xg_grid = xg_grid, xg_grid_results = xg_grid_results, best_parameters = best_parameters, xg_briar = xg_briar, xg_accuracy = collect_metrics(defense_fit), interpret = interpret, 
     nrow_train = nrow_train, nrow_val = nrow_val, nrow_test = nrow_test, baseline_accuracy = baseline_accuracy) |>  
  write_rds(file = "99-addm/xg.RDS")
