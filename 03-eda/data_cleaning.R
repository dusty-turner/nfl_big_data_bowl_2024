library(tidyverse)
library(gganimate)
library(arrow)
source(here::here("03-eda","ggtheme_field.R")) 

week_1 <-
  read_parquet(here::here("02-clean-data","tracking.parquet")) |>
  # simplifier() |>
  left_join(y = read_parquet(here::here("02-clean-data","tackles.parquet"))) |>
  left_join(read_parquet(here::here("02-clean-data","plays.parquet"))) |>
  left_join(read_parquet(here::here("02-clean-data","players.parquet"))) |>
  select(-week_id) |>

  ## rearrange tackle
  mutate(position = as.factor(position)) |>
  mutate(tackle = as.factor(tackle)) |>
  mutate(tackle = fct_rev(fct_na_value_to_level(tackle, level = "0"))) |>

  ## indicator for ball carrier
  mutate(ball_carrier = display_name == ball_carrier_display_name) |>

  ## indicator for football
  mutate(is_football = fct_rev(ifelse(display_name == "football", "football", "not_football"))) |>

  ######
  ## Begin: where is the ball and what are all the ball attributes on the play
  ######
  mutate(x_ball = ifelse(display_name == "football", x, NA)) |>
  mutate(y_ball = ifelse(display_name == "football", y, NA)) |>
  mutate(s_ball = ifelse(display_name == "football", s, NA)) |>
  mutate(dir_ball = ifelse(ball_carrier, dir, NA))  |>
  mutate(o_ball = ifelse(ball_carrier, o, NA))  |>
  group_by(game_id, play_id, frame_id) |>
  mutate(x_ball = mean(x_ball,na.rm = T)) |>
  mutate(y_ball = mean(y_ball,na.rm = T)) |>
  mutate(s_ball = mean(s_ball,na.rm = T)) |>
  mutate(dir_ball = mean(dir_ball,na.rm = T)) |>
  mutate(o_ball = mean(o_ball,na.rm = T)) |>
  ungroup() |>
  ######
  ## End: where is the ball and what are all the ball attributes on the play
  ######

  ## very few defenders in the box were NA so replace with median
  mutate(defenders_in_the_box = ifelse(is.na(defenders_in_the_box), median(defenders_in_the_box, na.rm = TRUE), defenders_in_the_box)) |>

  ## find distance from each player to the ball
  group_by(game_id, play_id, frame_id) |>
  mutate(x_ball = mean(x_ball, na.rm = TRUE))  |>
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |>
  mutate(distance_to_ball = sqrt((x-x_ball)^2 +  (y - y_ball)^2)) |>
  ungroup()  |>

  ######
  # Begin: This creates projected location of all players and ball
  ######

  ## put in projected location at next step
  group_by(display_name, game_id, play_id) |>
  mutate(
    delta_t = .5,
    # Adjust the angle
    adjusted_dir = 90 - dir,
    adjusted_o = 90 - o,
    # Convert the angles from degrees to radians.
    dir_rad = pi / 180 * adjusted_dir,
    o_rad = pi / 180 * adjusted_o,
    # Decompose speed and acceleration into x and y components.
    v_x = s * cos(dir_rad),
    v_y = s * sin(dir_rad),
    a_x = a * cos(o_rad),
    a_y = a * sin(o_rad),
    # Project forward using the timestep (delta_t).
    x_going = x + v_x * delta_t + 0.5 * a_x * delta_t^2,
    y_going = y + v_y * delta_t + 0.5 * a_y * delta_t^2
  ) %>%
  mutate(across(.cols = c(x_going,y_going), .fns = ~lag(.))) |>
  ungroup() |>
  select(-delta_t, -adjusted_dir, -adjusted_o, -dir_rad, -o_rad, -v_x, -v_y, -a_x, -a_y)  |>

  ## do the same for the ball
  mutate(delta_t = 1) |>
  group_by(display_name, game_id, play_id) |>
  mutate(
    vx = ifelse(frame_id == 1, 0, (x - lag(x)) / delta_t),
    vy = ifelse(frame_id == 1, 0, (y - lag(y)) / delta_t)
  ) |>
  mutate(
    x_pred_ball = x + vx * delta_t + 0.5 * (a * delta_t^2 * (vx / s)),
    y_pred_ball = y + vy * delta_t + 0.5 * (a * delta_t^2 * (vy / s))
  ) |>
  ungroup() |>

  ## clean up last edits about where the ball is going
  mutate(x_going = ifelse(display_name == "football", x_pred_ball, x_going)) |>
  mutate(y_going = ifelse(display_name == "football", y_pred_ball, y_going))  |>

  select(-c(x_pred_ball, y_pred_ball, vx, vy)) |>

  mutate(x_going = ifelse(is.nan(x_going), x, x_going)) |>
  mutate(y_going = ifelse(is.nan(y_going), y, y_going)) |>
  mutate(x_going = ifelse(is.na(x_going), x, x_going)) |>
  mutate(y_going = ifelse(is.na(y_going), y, y_going)) |>
  ######
  # End: This creates projected location of all players and ball
  ######
  ## Start: where is the ball next
  ######

  mutate(x_ball_next = ifelse(display_name == "football", x_going, NA)) |>
  mutate(y_ball_next = ifelse(display_name == "football", y_going, NA)) |>
  group_by(game_id, play_id, frame_id) |>
  mutate(x_ball_next = mean(x_ball_next, na.rm = TRUE)) |>
  mutate(y_ball_next = mean(y_ball_next, na.rm = TRUE)) |>
  ungroup() |>
  mutate(x_ball_next = ifelse(is.nan(x_ball_next), x_ball, x_ball_next)) |>
  mutate(y_ball_next = ifelse(is.nan(y_ball_next), y_ball, y_ball_next)) |>
  ######
  ## End: where is the ball next
  ######

  ## find distance from each projected player to the projected ball
  mutate(distance_to_ball_next = sqrt((x_going-x_ball_next)^2 +  (y_going - y_ball_next)^2)) |>

  ######
  # Begin: This creates the v_approach vector
  ######

## # `v_approach` Interpretation:
# Positive: Objects moving towards each other.
# Negative: Objects moving away from each other.
# Near 0: Movement mostly perpendicular to line connecting their positions.
  mutate(
    # Compute relative position vector
    dx = x_ball - x,
    dy = y_ball - y,
    # Normalize the relative position vector
    r_magnitude = sqrt(dx^2 + dy^2),
    dx_unit = dx / r_magnitude,
    dy_unit = dy / r_magnitude,
    # Compute velocities in x/y plane for player and ball using the correct orientation
    v_x = s * sin(pi/180 * dir),
    v_y = s * cos(pi/180 * dir),
    v_x_ball = s_ball * sin(pi/180 * dir_ball),
    v_y_ball = s_ball * cos(pi/180 * dir_ball),
    # Compute relative velocity vector
    v_rel_x = v_x_ball - v_x,
    v_rel_y = v_y_ball - v_y,
    # Compute the relative speed along the line joining their positions
    v_approach = v_rel_x * dx_unit + v_rel_y * dy_unit
  )  |>
  select(-c(dx, dy, r_magnitude, dx_unit, dy_unit, v_x, v_y, v_x_ball, v_y_ball, v_rel_x, v_rel_y)) |>
  ######
  # End: This creates the v_approach vector
  ######
  ######
  # Begin: This creates the orientation and the range fan
  ######
  mutate(
    x_facing = x + 1 * cos((90 - o) * (pi / 180)),
    y_facing = y + 1 * sin((90 - o) * (pi / 180))
  ) |>
  mutate(
    x_left = x_facing + 1 * cos((90 - o - 30) * (pi / 180)),
    y_left = y_facing + 1 * sin((90 - o - 30) * (pi / 180)),
    x_right = x_facing + 1 * cos((90 - o + 30) * (pi / 180)),
    y_right = y_facing + 1 * sin((90 - o + 30) * (pi / 180))
  )  |>
  ######
  # Begin: This identifies if they are 3 yards away and inside that range fan
  ######
  mutate(
    # Convert orientation to radians for calculations
    o_rad = (90-o) * pi / 180,
    # Calculate angle to the ball
    angle_to_ball = atan2(y_ball - y, x_ball - x) - o_rad,
    # Normalize angle between -pi and pi
    angle_to_ball = (angle_to_ball + pi) %% (2 * pi) - pi,
    # Check if ball is within 'the fan' (30 degrees on either side)
    ball_in_fan3 = angle_to_ball >= -pi/6 & angle_to_ball <= pi/6 & distance_to_ball <= 3, # Assuming 3 unit is the radius of the fan,
    ball_in_fan3 = if_else(ball_in_fan3, "yes", "no"),
    ball_in_fan2 = angle_to_ball >= -pi/6 & angle_to_ball <= pi/6 & distance_to_ball <= 2, # Assuming 3 unit is the radius of the fan,
    ball_in_fan2 = if_else(ball_in_fan2, "yes", "no"),
    ball_in_fan1 = angle_to_ball >= -pi/6 & angle_to_ball <= pi/6 & distance_to_ball <= 1, # Assuming 3 unit is the radius of the fan,
    ball_in_fan1 = if_else(ball_in_fan1, "yes", "no")
    )  |>
  ######
  # End: This identifies if they are 3 yards away and inside that range fan
  ######
  # Add in single game/play grouping variable
  mutate(game_idplay_id = str_c(game_id,play_id)) |>
  # Remove several NA values
  mutate(offense_formation = if_else(is.na(offense_formation), "unk", offense_formation)) |>
  ######
  # Begin: Create Frames from Tackle Column
  ######
  group_by(game_id,play_id, nfl_id) |>
  mutate(row_with_tackle = mean(ifelse(event == "tackle", cur_group_rows(), NA ), na.rm = TRUE)) |>
  mutate(frames_from_tackle = cur_group_rows() - row_with_tackle) |>
  ungroup() |>
  select(-row_with_tackle)
  ######
  # End: Create Frames from Tackle Column
  ######

####
# Start: These don't have tackles in their plays
####
bad <-
  week_1 |> 
  group_by(game_id, play_id, display_name) |> 
  reframe(test = mean(as.numeric(as.character(tackle)))) |> 
  group_by(game_id, play_id) |> 
  reframe(sum = sum(test))  |> 
  filter(sum != 1) |> 
  mutate(temp = str_c(game_id, "_", play_id)) 
####
# End: These don't have tackles in their plays
####
####
# Start: Fix the ones with tackles not in their plays
####
fixed_these <-
  week_1 |> 
  mutate(temp = str_c(game_id, "_", play_id)) |> 
  filter(temp %in% bad$temp) |> 
  filter(!str_detect(string = play_description, pattern = "TOUCHDOWN")) |> 
  select(game_id, play_id, frame_id, tackle, display_name, play_description) |> 
  distinct(display_name, play_description, frame_id, .keep_all = TRUE) |>
  rowwise() |> 
  mutate(tackle_made = as.factor(is_name_in_description(display_name, play_description))) |> 
  ungroup()

fixed_these
####
# End: Fix the ones with tackles not in their plays
####
####
# Start: Add back in the fixed ones
####
week_1 <-
  week_1 |> 
  left_join(fixed_these) |> 
  mutate(tackle = ifelse(!is.na(tackle_made), as.character(tackle_made), as.character(tackle))) |> 
  select(-tackle_made)
####
# Start: Add back in the fixed ones
####


#### add in clusters for defense

clust_dat <-
week_1 |>
  filter(str_detect(event, "snap")) |>
  filter(club != possession_team) |>
  select(game_id, play_id, nfl_id, position, x, y, yardline_number, absolute_yardline_number, event) |>
  mutate(x_from_los = abs(absolute_yardline_number - x)) |>
  select(game_id, play_id, nfl_id, x_from_los, y)

library(mclust)
set.seed(1)
mclust_mod <- Mclust(data = clust_dat[, c("x_from_los","y")], G = 6)

clust_dat_with_cluster <-
  clust_dat |>
  mutate(alignment_cluster = as.factor(mclust_mod$classification)) |>
  select(-y)



defensive_model_building_data <-
  week_1 |>
  filter(defensive_team == club) |>
  group_by(game_id, play_id, frame_id) |>
  mutate(rank = as.factor(round(rank(distance_to_ball)))) |>
  ungroup()

run_play_alignments <-
  week_1 |>
  filter(is.na(pass_result)) |> filter(frame_id == 1) |>

  mutate(x_ball = ifelse(display_name == "football", x, NA)) |>
  mutate(y_ball = ifelse(display_name == "football", y, NA)) |>

  ## find distance from each player to the ball
  group_by(game_id, play_id) |>
  mutate(x_ball = mean(x_ball, na.rm = TRUE)) |>
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |>
  ungroup() |>

  filter(club == defensive_team | display_name == "football") |>

  select(display_name, nfl_id, defensive_team, frame_id, game_id, play_id, club, x, y , x_ball, y_ball, yardline_number, position, play_description) |>
  mutate(run_alignment = case_when(abs(x_ball-x) < 2 &  abs(y_ball-y) < 6 ~ "on_line",
                               abs(x_ball-x) < 10 & abs(y_ball-y) < 5 ~ "backer",
                                                    abs(y_ball-y) > 5 ~ "outside_pass",
                                                                 TRUE ~ "inside_pass")) |>
  select(nfl_id, game_id, play_id, run_alignment)


pass_play_alignment <-
week_1 |>
  filter(!is.na(pass_result)) |> filter(frame_id == 1) |>

  mutate(x_ball = ifelse(display_name == "football", x, NA)) |>
  mutate(y_ball = ifelse(display_name == "football", y, NA)) |>

  ## find distance from each player to the ball
  group_by(game_id, play_id) |>
  mutate(x_ball = mean(x_ball, na.rm = TRUE)) |>
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |>
  ungroup() |>

  filter(club == defensive_team | display_name == "football") |>

  mutate(pass_alignment = position) |>
  # group_by(game_id, play_id) |> filter(cur_group_id() == 1) |> ungroup() |>
  select(nfl_id, game_id, play_id, pass_alignment)


defensive_model_building_data <-
defensive_model_building_data |>
  left_join(run_play_alignments) |>
  left_join(pass_play_alignment) |>
  left_join(clust_dat_with_cluster, by = c("game_id", "play_id", "nfl_id"), relationship = "many-to-many") |>
  mutate(alignment = ifelse(is.na(run_alignment), pass_alignment, run_alignment)) |>
  # Puts values in for NAS with alignment_cluster
  mutate(alignment_cluster = if_else(is.na(alignment_cluster), "unk", alignment_cluster)) |>
  ######
  select(-run_alignment, -pass_alignment)
  # select(game_id:frame_id, y.x, y.y, contains("alignment"), contains("cluster"), event)
# |> filter(!is.na(alignment_cluster))


# temp <-
#   defensive_model_building_data |>
#   group_by(game_id, play_id)  |>
#   mutate(temp = cur_group_id()) |>
#   relocate(temp) |>
#   ungroup()
#
# set.seed(49)
# temp_sample <- sample(unique(temp$temp),1473, replace = F)
# # temp_sample <- sample(unique(temp$temp),100, replace = F)
#
# defensive_model_building_data <-
#   temp |> filter(temp %in% temp_sample) |>
#   select(-temp) |>


defensive_model_building_data_model <-
  defensive_model_building_data |>
  select(game_idplay_id, game_id, play_id, nfl_id, frame_id, club, tackle, x, y, x_going, y_going, s, a, position,
         rank, club, defenders_in_the_box, ball_carrier, ball_carrier_id, ball_carrier_display_name, absolute_yardline_number,
         time, defensive_team, display_name, distance_to_ball, distance_to_ball_next, play_description, is_football, alignment,
         alignment_cluster, pass_result, v_approach, ball_in_fan3, ball_in_fan2, ball_in_fan1, x_ball, y_ball, o_ball, x_ball_next, y_ball_next, s_ball) |>
  filter(frame_id > 5)

write_parquet(defensive_model_building_data, here::here("02-clean-data", "defensive_model_building_data.parquet"))

## week 1 is too big to save it all

week_1 |> as_tibble() |> 
  # group_by(game_id) |> filter(cur_group_id() %in% 1:16) |> ungroup() |>
  write_parquet(here::here("02-clean-data", "week_1.parquet"))

# I don't have clusters built in for passing plays
# ## some plays don't end in tackles.  Some plays have a forced fumble
# week_1 |>
#   # filter(forcedFumble == 1, tackle == 0) |>
#   filter(play_id == 896, game_id == 2022090800)  |>
#   filter(defensive_team == club) |>
#   select(forcedFumble, tackle, event, display_name, frame_id) |>
#   print(n = Inf)
#
# ## i do not know the conclusion of this play
# week_1 |> distinct(play_id, game_id)
# week_1 |> filter(tackle == 1) |> distinct(play_id, game_id)
# week_1 |> filter(play_id == 146) |> filter(game_id == 2022090800) |> count(prePenaltyPlayResult)

# week_1 |>
#   group_by(game_id) |> filter(cur_group_id() %in% 1) |>   ungroup() |>
#   write_csv("week_1.csv")

dak <- week_1 |> filter(display_name == "Dak Prescott") |> distinct(game_id, play_id)

digest::sha1(read_lines(here::here("03-eda", "data_cleaning.R"))) |> 
  write_lines("02-clean-data/cleaninghash.txt")
 
# 
# library(rlang)
# 
# 
# test <-
# week_1 |> 
#   # defensive_model_building_data |> 
#   group_by(game_id, play_id, frame_id) |>
#   filter(cur_group_id() %in% 5:8) |> 
#   ungroup() |> 
#   # filter(frame_id == 5) |>  
#   rowwise() 
# 
# # Define the threshold distance
# threshold_distance <- .001  # Adjust as needed
# 
# ax <- test$x[1]
# ay <- test$y[1]
# bx <- test$x_ball[1]
# by <- test$y_ball[1]
# px <- test$x[2]
# py <- test$y[2]
# game_id <- test$game_id[1]
# play_id <- test$play_id[1]
# frame_id <- test$frame_id[1]
# data <- test
# 
# player_x <-test$x[1] 
# player_y <- test$
# ball_x <- 
# ball_y <- 
# current_game_id <- 
# current_play_id <- 
# current_frame_i <- 
# 
# # Function to calculate the perpendicular distance from a point to a line
# perpendicular_distance <- function(ax, ay, bx, by, px, py) {
#   abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / sqrt((by - ay)^2 + (bx - ax)^2)
# }
# 
# # Function to check if any other player within the same play and frame is within a threshold distance from the line to the ball
# # check_other_players <- function(ax, ay, bx, by, game_id, play_id, frame_id, data, threshold_distance) {
# #   # Filter data to include only players from the same game, play, and frame
# #   relevant_players <-
# #     data %>%
# #     filter(game_id == "2022090800", play_id == "101", frame_id == "5", !(x == ax & y == ay))
# #     # filter(game_id == game_id, play_id == play_id, frame_id == frame_id, !(x == ax & y == ay))
# #   
# #   distances <- pmap_dbl(relevant_players, function(x, y, x_ball, y_ball, ...) {
# #     perpendicular_distance(ax, ay, bx, by, x, y)
# #   })
# #   
# #   any(distances <= threshold_distance)
# # }
# 
# check_other_players <- function(player_x, player_y, ball_x, ball_y, current_game_id, current_play_id, current_frame_id, data, threshold_distance) {
#   # Filter data to include only players from the same game, play, and frame, excluding the current player
#   relevant_players <- data %>%
#     filter(game_id == current_game_id, play_id == current_play_id, frame_id == current_frame_id, !(x == player_x & y == player_y))
#   
#   distances <- pmap_dbl(relevant_players, function(x, y, x_ball, y_ball, ...) {
#     perpendicular_distance(player_x, player_y, ball_x, ball_y, x, y)
#   })
#   
#   any(distances <= threshold_distance)
# }
# 
# 
# 
# # Apply the function to each row
# # data <- 
# test %>%
#   select(x, y, x_ball,y_ball, game_id, play_id, frame_id) |> 
#   rowwise() %>%
#   mutate(player_between = check_other_players(x, y, x_ball, y_ball, game_id, play_id, frame_id, data, threshold_distance))
#   ungroup() |> 
#   mutate(color = case_when(ball_carrier == TRUE ~ "ballcarrier",
#                            tackle == "1" ~ "tackler",
#                            is_football == "football" ~ "football",
#                            club == defensive_team ~ "defense",
#                            club != defensive_team ~ "offense")) |> 
#   # select(x,y,x_ball,y_ball,player_between, frame_id, game_id, play_id, frame_id, display_name, defensive_team)  |> 
#   select(player_between, distance_to_ball, x, y, color, absolute_yardline_number, ball_carrier, play_id, time, play_description, 
#          is_football, club, frame_id, game_id) 
# 
# 
# ggplot(aes(x = x, y = y, color = color, shape = player_between)) +
#   geom_point()
