library(tidyverse)
library(tidymodels)
source("02_scripts/data_cleaning.R")

running_data <- dplyr::filter(defensive_model_building_data_model, is.na(passResult)) |> select(-passResult)

set.seed(49)
splits <- group_initial_validation_split(running_data, prop = c(.01,.0025), group = gameIdPlayId)
# splits <- group_initial_validation_split(running_data, prop = c(.6,.2), group = gameIdPlayId)

defense_training <- training(splits) 
defense_validation <- validation_set(splits)
defense_test  <- testing(splits)


mod <- 
  logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") |> 
  set_mode("classification") 

# mod <- 
#   rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
#   set_engine("ranger", importance = "impurity") |> 
#   set_mode("classification") 


# tackle, position, rank, defendersInTheBox, alignment, s, a, rank, v_approach

recipe <-
  recipe(tackle ~ ., data = defense_training) |> 
  step_rm(gameIdPlayId, gameId, playId, nflId, frameId, club, x, y, x_next, y_next, ball_carrier, ballCarrierId, ballCarrierDisplayName, absoluteYardlineNumber,
          time, defensiveTeam, displayName, playDescription, is_football, rank) |> 
  # step_rm(gameIdPlayId, gameId, playId, nflId, frameId, club, x, y, x_next, y_next, ball_carrier, ballCarrierId, ballCarrierDisplayName, absoluteYardlineNumber,
  #         time, defensiveTeam, displayName, playDescription, is_football, tackle, position, rank, defendersInTheBox, alignment, alignment_cluster) 
  step_impute_mode(position) |>
  step_dummy(all_nominal_predictors()) |> 
  step_impute_mean(v_approach)  
  # step_interact(terms = ~starts_with("position"):starts_with("alignment_cluster"))



workflow <-
  workflow(spec = mod) |> 
  add_recipe(recipe = recipe)
  
grid <- expand_grid(penalty = 10^seq(-4, -1, length.out = 30), mixture = 10^seq(-4, -1, length.out = 30))

# grid <- grid_latin_hypercube(trees(), min_n(), mtry(range = c(4,17)), size = 10)

res <- 
  workflow %>% 
  tune_grid(resamples = defense_validation,
            grid = grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc, accuracy))

# best_parameters <- select_best(lr_res, "roc_auc")

best_parameters <- show_best(res, "accuracy", n = 5) |> slice(5)

update_wflow <- finalize_workflow(workflow, best_parameters)

defense_fit <- last_fit(update_wflow, split = splits, add_validation_set = TRUE)

collect_metrics(defense_fit)

defense_fit |> extract_fit_parsnip() |> vip::vip()
defense_fit |> extract_fit_parsnip() |> tidy() |> print(n = Inf)

# extract_fit_engine(defense_fit) |> tidy() |> print(n = Inf) filter(lambda == best_parameters$penalty)
# extract_fit_engine(defense_fit) |> tidy() |> filter(step ==1) |> pull(lambda)

prob_of_tackle <- defense_fit |> select(.predictions) |> unnest(.predictions) |> pull(.pred_1) 

plot_this <- defense_test |>   group_by(gameIdPlayId) |> filter(cur_group_id()==5) |> 
  # select(playDescription) 
  ungroup()  |> distinct(gameId, playId)


data_joined_with_pred <- 
week_1 |> left_join(
  defense_test |> 
  mutate(prob_of_tackle = prob_of_tackle)
  ) 


# data_joined_with_pred |> 
#   select(tackle, prob_of_tackle) |> 
#   filter(!is.na(prob_of_tackle)) |> arrange(-prob_of_tackle)
#   mutate(tackle = as.factor(tackle)) |> 
#   mutate(declared = as.factor(ifelse(prob_of_tackle > .5 , 1 , 0))) |> 
#   yardstick::accuracy(tackle, declared)


data_joined_with_pred |> 
  filter(gameId == plot_this$gameId, playId == plot_this$playId) |> 
  mutate(color = case_when(ball_carrier == TRUE ~ "ballcarrier",
                         tackle == "1" ~ "tackler",
                         club == defensiveTeam ~ "defense",
                         club != defensiveTeam ~ "offense")) |> 
  select(distance_to_ball, x, y, color, absoluteYardlineNumber, ball_carrier, playId, time, prob_of_tackle, playDescription, is_football) |> 
  {
    ggplot(data = ., aes(x = x, y = y, alpha = prob_of_tackle)) +
      geom_vline(aes(xintercept = absoluteYardlineNumber), color = "yellow") +
      geom_point(aes(shape = is_football), show.legend = FALSE) +
      # scale_color_gradient2(na.value = "dodgerblue", low = "black", high = "grey") +
      # facet_wrap(~playId) +
      transition_time(time) + ease_aes("linear") +
      labs(y = "", x = "Yards To Endzone", caption = .$playDescription[1]) +
      theme_field
  }


data_joined_with_pred |> 
  select(gameId, playId, nflId, displayName, tackle, prob_of_tackle) |> 
  filter(!is.na(prob_of_tackle)) |> 
  mutate(tackle  = as.integer(as.character(tackle))) |> 
  group_by(gameId, playId, nflId, displayName) |> 
  reframe(expected_prob_of_tackle = mean(prob_of_tackle), tackle = mean(tackle)) |> 
  mutate(tackles_over_expected_play = ifelse(tackle == 1, 1-expected_prob_of_tackle, -expected_prob_of_tackle)) |> 
  group_by(nflId, displayName) |> 
  reframe(tackles_over_expected = sum(tackles_over_expected_play)) |> 
  arrange(-tackles_over_expected)
  
week_1  

data_joined_with_pred |> 
  group_by(gameId, playId, frameId) |> 
  filter(cur_group_id() == 500) |> 
    reframe(total_prob = sum(prob_of_tackle, na.rm = TRUE)) 
  
glimpse(week_1)
