


# mod_1 <- glm(tackle ~ distance_to_ball, family = binomial(link = "logit"), data = defensive_model_building_data)
mod_1 <- glm(tackle ~ distance_to_ball * defendersInTheBox + s + a + rank, family = binomial(link = "logit"), data = defensive_model_building_data)

summary(mod_1)

defensive_model_building_data |> 
  select(tackle) |> 
  mutate(prob_of_tackle = predict(object = mod_1, type = "response")) |> 
  mutate(prob_of_tackle_class = fct_rev(as.factor(ifelse(prob_of_tackle > .5, 1, 0)))) |> 
  mutate(tackle_num = as.numeric(tackle)-1)  |> 
  yardstick::accuracy(truth = tackle, estimate = prob_of_tackle_class)
# yardstick::mae(truth = tackle_num, estimate = prob_of_tackle)
# mutate(tackle = as.factor(ifelse(tackle == "no_tackle", 0 , 1))) |>
# yardstick::roc_auc(truth = tackle, estimate = prob_of_tackle, estimator = "binary", event_level = "second")

defensive_model_building_data_prediction <-
  defensive_model_building_data |> 
  select(gameId, playId, nflId, frameId, x, y) |> 
  mutate(prob_of_tackle = predict(object = mod_1, type = "response"))


week_1_pred <-
week_1 |> 
  select(gameId, playId, nflId, frameId, club, tackle, x, y, club, ball_carrier, ballCarrierId, ballCarrierDisplayName, absoluteYardlineNumber, 
         time, defensiveTeam, displayName, distance_to_ball, playDescription, is_football) |> 
  left_join(defensive_model_building_data_prediction)
  # mutate(prob_of_tackel = ifelse(is.na(prob_of_tackle), 0, prob_of_tackle))

play_game_ids <- week_1_pred |> distinct(gameId, playId) 

dak <- week_1_pred |> filter(str_detect(displayName, "Dak")) 



week_1_pred |>
  filter(gameId == dak$gameId[2], playId %in% unique(dak$playId)[7])  |> 
  mutate(color = case_when(ball_carrier == TRUE ~ "ballcarrier",
                           tackle == "1" ~ "tackler",
                           club == defensiveTeam ~ "defense",
                           club != defensiveTeam ~ "offense")) |> 
  select(distance_to_ball, x, y, color, absoluteYardlineNumber, ball_carrier, playId, time, prob_of_tackle, playDescription, is_football) %>%
  {
  ggplot(data = ., aes(x = x, y = y, color = prob_of_tackle)) +
  geom_vline(aes(xintercept = absoluteYardlineNumber), color = "yellow") +
  geom_point(aes(shape = is_football), show.legend = FALSE) +
  scale_color_gradient2(na.value = "dodgerblue", low = "grey", high = "black") +
  # facet_wrap(~playId) +
  transition_time(time) + ease_aes("linear") +
  labs(y = "", x = "Yards To Endzone", caption = .$playDescription[1]) +
  theme_field
}

library(sportyR)

nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667)

mod_dat <-
week_1_pred |>
  filter(gameId == dak$gameId[2], playId %in% unique(dak$playId)[8])  |> 
  mutate(color = case_when(ball_carrier == TRUE ~ "ballcarrier",
                           tackle == "1" ~ "tackler",
                           club == defensiveTeam ~ "defense",
                           club != defensiveTeam ~ "offense")) |> 
  select(distance_to_ball, x, y, color, absoluteYardlineNumber, ball_carrier, playId, time, prob_of_tackle, playDescription, is_football) 


  nfl_field +
  geom_point(data = mod_dat, aes(x = x, y = y, fill = prob_of_tackle, color = color), show.legend = F, size = 3, shape = 21) +
  # geom_point(data = mod_dat, aes(x = x, y = y, color = prob_of_tackle, fill = color), show.legend = F, size = 5, shape = 21) +
  geom_vline(data = mod_dat, aes(xintercept = absoluteYardlineNumber), color = "yellow") +
  scale_fill_gradient2(na.value = "dodgerblue", low = "grey", high = "black") +
  # facet_wrap(~playId) +
  labs(y = "", x = "Yards To Endzone", caption = str_wrap(mod_dat$playDescription[1], width = 110))  +
  transition_time(time) + ease_aes("linear") 

  
  
  
####
  
library(rstanarm)
library(parallel)


    
options(mc.cores = parallel::detectCores())

predictors <- c("distance_to_ball", "s", "a")

my_prior <- normal(location = c(1, 0, 1), scale = c(4, 2, 3), autoscale = FALSE) 

# Building the Bayesian logistic regression model
bayesian_model <- stan_glm(tackle ~ ., 
                           data = defensive_model_building_data[, c(predictors, "tackle")],
                           family = binomial(link = "logit"), 
                           prior = my_prior, 
                           prior_intercept = normal(0, 2.5),
                           algorithm = "sampling")

summary(bayesian_model)

bayesian_model

predict(bayesian_model, newdata = defensive_model_building_data)

write_rds(bayesian_model, "bayesian_model.rds")

plot(bayesian_model)
  

