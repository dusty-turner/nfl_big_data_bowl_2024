library(tidyverse)
library(gganimate)
library(arrow)
source(here::here("02_scripts","ggtheme_field.R"))

week_1 <-
# temp2 <-
  read_parquet(here::here("01_data","tracking_week_1.parquet")) |> 
  left_join(y = read_parquet(here::here("01_data","tackles.parquet"))) |> 
  left_join(read_parquet(here::here("01_data","plays.parquet"))) |> 
  left_join(read_parquet(here::here("01_data","players.parquet"))) |> 
  
  ## rearrange tackle
  mutate(position = as.factor(position)) |> 
  mutate(tackle = as.factor(tackle)) |> 
  mutate(tackle = fct_rev(fct_na_value_to_level(tackle, level = "0"))) |> 
  
  ## indicator for ball carrier
  mutate(ball_carrier = displayName == ballCarrierDisplayName) |> 
  
  ## where is the ball
  mutate(x_ball = ifelse(displayName == "football", x, NA)) |> 
  mutate(y_ball = ifelse(displayName == "football", y, NA)) |> 
  
  ## put in projected location at next step
  group_by(displayName, gameId, playId) |> 
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
    x_facing = x + v_x * delta_t + 0.5 * a_x * delta_t^2,
    y_facing = y + v_y * delta_t + 0.5 * a_y * delta_t^2
  ) %>%
  mutate(across(.cols = c(x_facing,y_facing), .fns = ~lag(.))) |> 
  ungroup() |> 
  mutate(x_facing = ifelse(is.nan(x_facing), x, x_facing)) |> 
  mutate(y_facing = ifelse(is.nan(y_facing), y, y_facing)) |>
  mutate(x_facing = ifelse(is.na(x_facing), x, x_facing)) |> 
  mutate(y_facing = ifelse(is.na(y_facing), y, y_facing)) |>

  # fill(x_facing, y_facing, .direction = "up") |> 
  # select(displayName, x, x_facing, y, y_facing, s, v_x, v_y) |> 
  
  ## find distance from each player to the ball
  group_by(gameId, playId) |> 
  mutate(x_ball = mean(x_ball, na.rm = TRUE)) |> 
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |> 
  mutate(distance_to_ball = sqrt((x-x_ball)^2 +  (y - y_ball)^2)) |> 
  ungroup() |> 

  mutate(delta_t = 1) |> 
  # select(gameId, playId, displayName, x, y, s, a, dis, frameId, delta_t) |> 
  group_by(displayName, gameId, playId) |> 
  # filter(displayName == "football") |>
  # filter(cur_group_id() == 1) |> 

  mutate(
    vx = ifelse(frameId == 1, 0, (x - lag(x)) / delta_t),
    vy = ifelse(frameId == 1, 0, (y - lag(y)) / delta_t)
  ) |> 
  mutate(
    x_pred_ball = x + vx * delta_t + 0.5 * (a * delta_t^2 * (vx / s)),
    y_pred_ball = y + vy * delta_t + 0.5 * (a * delta_t^2 * (vy / s))
  ) |> 
  ungroup() |> 
    
  mutate(x_facing = ifelse(displayName == "football", x_pred_ball, x_facing)) |> 
  mutate(y_facing = ifelse(displayName == "football", y_pred_ball, y_facing))  |> 
  select(-c(x_pred_ball, y_pred_ball, vx, vy)) |> 

  ## very few defenders in the box were NA so replace with median
  mutate(defendersInTheBox = ifelse(is.na(defendersInTheBox), median(defendersInTheBox, na.rm = TRUE), defendersInTheBox)) |> 
  mutate(is_football = fct_rev(ifelse(displayName == "football", "football", "not_football"))) |> 


## # `v_approach` Interpretation:
# Positive: Objects moving towards each other.
# Negative: Objects moving away from each other.
# Near 0: Movement mostly perpendicular to line connecting their positions.
# week_1 |> 
  # distinct(gameId, playId, nflId, displayName)  |> count(gameId, playId) |> filter(n!=23) 
  # filter(gameId == 2022091101, playId == 109) |> 
  # select(x,y,s,dir, ball_carrier, displayName, gameId, playId, frameId) |> 
  # filter(ball_carrier)
  mutate(x_ball = ifelse(ball_carrier, x, NA)) |> 
  mutate(y_ball = ifelse(ball_carrier, y, NA)) |> 
  mutate(s_ball = ifelse(ball_carrier, s, NA)) |> 
  mutate(dir_ball = ifelse(ball_carrier, dir, NA))  |> 
  mutate(o_ball = ifelse(ball_carrier, o, NA))  |> 
  group_by(gameId, playId, frameId) |> 
  mutate(x_ball = mean(x_ball,na.rm = T)) |>
  mutate(y_ball = mean(y_ball,na.rm = T)) |>
  mutate(s_ball = mean(s_ball,na.rm = T)) |>
  mutate(o_ball = mean(o_ball,na.rm = T)) |>
  mutate(dir_ball = mean(dir_ball,na.rm = T)) |> 
  ungroup()  |> 
    

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
  ) |> 


## where is the ball next
  mutate(x_ball_next = ifelse(displayName == "football", x_facing, NA)) |> 
  mutate(y_ball_next = ifelse(displayName == "football", y_facing, NA)) |>
  
  group_by(gameId, playId, frameId) |> 
  mutate(x_ball_next = mean(x_ball_next, na.rm = TRUE)) |> 
  mutate(y_ball_next = mean(y_ball_next, na.rm = TRUE)) |>  
  ungroup() |> 
  mutate(x_ball_next = ifelse(is.nan(x_ball_next), x_ball, x_ball_next)) |> 
  mutate(y_ball_next = ifelse(is.nan(y_ball_next), y_ball, y_ball_next)) |> 

  # filter(gameId == temp$gameId[1], playId == temp$playId[1], displayName == "football") |>  
  # select(displayName, frameId, x, y, x_facing, x_ball_next, y_facing, y_ball_next, gameId, playId, a, s) #distance_to_ball_next) 
  
  ## find distance from each projected player to the projected ball
  mutate(distance_to_ball_next = sqrt((x_facing-x_ball_next)^2 +  (y_facing - y_ball_next)^2)) |> 
  
  ## which are the players facing?
  ## what is the angle difference between ball and player
  ## are they within a certain tolerance?
  mutate(
    z_units = 1, 
    # Calculate the angle difference correctly, considering the coordinate system
    angle_to_ball = (atan2(y_ball - y, x_ball - x) * 180 / pi + 360) %% 360,
    o_corrected = (o + 360) %% 360,
    oriented_towards_ball = abs(o_corrected - angle_to_ball) <= 30 | abs(o_corrected - angle_to_ball) >= 330,
    
    # Adjust the difference in the direction of the player and the direction towards the ball
    dir_corrected = (dir + 360) %% 360,
    diff_dir = dir_corrected - angle_to_ball,
    diff_dir_adjusted = ifelse(diff_dir > 180, diff_dir - 360, ifelse(diff_dir < -180, diff_dir + 360, diff_dir)),
    turning_towards = sign(diff_dir_adjusted) < 0,
    
    # Calculate new x/y coordinates at a specified distance in the direction the player is facing
    unit_vector_x = cos(o * pi / 180),
    unit_vector_y = sin(o * pi / 180),
    # Normalize the unit vector to have a length of 'z_units'
    norm_factor = sqrt(unit_vector_x^2 + unit_vector_y^2),
    new_x = x + (unit_vector_x / norm_factor) * z_units,
    new_y = y + (unit_vector_y / norm_factor) * z_units,
    
    # Calculate two additional points forming a 30-degree fan oriented towards the ball
    fan_angle = 30,  # Half of 30 degrees for the fan spread
    # Point to the left of the fan
    fan_left_x = x + cos((o - fan_angle) * pi / 180) * z_units,
    fan_left_y = y + sin((o - fan_angle) * pi / 180) * z_units,
    # Point to the right of the fan
    fan_right_x = x + cos((o + fan_angle) * pi / 180) * z_units,
    fan_right_y = y + sin((o + fan_angle) * pi / 180) * z_units
  ) 


#### add in clusters for defense

clust_dat <-
week_1 |> 
  filter(str_detect(event, "snap")) |> 
  filter(club != possessionTeam) |> 
  select(gameId, playId, nflId, position, x, y, yardlineNumber, absoluteYardlineNumber, event) |> 
  mutate(x_from_los = abs(absoluteYardlineNumber - x)) |> 
  select(gameId, playId, nflId, x_from_los, y)

library(mclust)
set.seed(1)
mclust_mod <- Mclust(data = clust_dat[, c("x_from_los","y")], G = 6)

# mclust_mod

# summary(mclust_mod)
# plot(mclust_mod, what = "classification")

clust_dat_with_cluster <- clust_dat |> mutate(alignment_cluster = as.factor(mclust_mod$classification)) |> select(-y)


defensive_model_building_data <- 
  week_1 |> 
  filter(defensiveTeam == club) |> 
  group_by(gameId, playId, frameId) |> 
  mutate(rank = as.factor(rank(distance_to_ball))) |> 
  ungroup()

run_play_alignments <-
  week_1 |> 
  filter(is.na(passResult)) |> filter(frameId == 1) |>
  
  mutate(x_ball = ifelse(displayName == "football", x, NA)) |> 
  mutate(y_ball = ifelse(displayName == "football", y, NA)) |> 
  
  ## find distance from each player to the ball
  group_by(gameId, playId) |>
  mutate(x_ball = mean(x_ball, na.rm = TRUE)) |> 
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |> 
  ungroup() |> 
  
  filter(club == defensiveTeam | displayName == "football") |> 

  select(displayName, nflId, defensiveTeam, frameId, gameId, playId, club, x, y , x_ball, y_ball, yardlineNumber, position, playDescription) |> 
  mutate(run_alignment = case_when(abs(x_ball-x) < 2 &  abs(y_ball-y) < 6 ~ "on_line",
                               abs(x_ball-x) < 10 & abs(y_ball-y) < 5 ~ "backer",
                                                    abs(y_ball-y) > 5 ~ "outside_pass",
                                                                 TRUE ~ "inside_pass")) |> 
  select(nflId, gameId, playId, run_alignment)
    

pass_play_alignment <-
week_1 |> 
  filter(!is.na(passResult)) |> filter(frameId == 1) |>
  
  mutate(x_ball = ifelse(displayName == "football", x, NA)) |> 
  mutate(y_ball = ifelse(displayName == "football", y, NA)) |> 
  
  ## find distance from each player to the ball
  group_by(gameId, playId) |>
  mutate(x_ball = mean(x_ball, na.rm = TRUE)) |> 
  mutate(y_ball = mean(y_ball, na.rm = TRUE)) |> 
  ungroup() |> 
  
  filter(club == defensiveTeam | displayName == "football") |> 
  
  mutate(pass_alignment = position) |> 
  # group_by(gameId, playId) |> filter(cur_group_id() == 1) |> ungroup() |> 
  select(nflId, gameId, playId, pass_alignment)


defensive_model_building_data <-
defensive_model_building_data |> 
  left_join(run_play_alignments) |>
  left_join(pass_play_alignment) |>
  left_join(clust_dat_with_cluster, by = c("gameId", "playId", "nflId")) |> 
  mutate(alignment = ifelse(is.na(run_alignment), pass_alignment, run_alignment)) |> 
  select(-run_alignment, -pass_alignment) 
  # select(gameId:frameId, y.x, y.y, contains("alignment"), contains("cluster"), event) 
# |> filter(!is.na(alignment_cluster))


temp <-
  defensive_model_building_data |> 
  group_by(gameId, playId)  |> 
  mutate(temp = cur_group_id()) |> 
  relocate(temp) |> 
  ungroup()

set.seed(49)
temp_sample <- sample(unique(temp$temp),1473, replace = F)
# temp_sample <- sample(unique(temp$temp),100, replace = F)

defensive_model_building_data <- 
  temp |> filter(temp %in% temp_sample) |> 
  select(-temp) |> 
  mutate(gameIdPlayId = str_c(gameId,playId))

defensive_model_building_data_model <- 
  defensive_model_building_data |> 
  select(gameIdPlayId, gameId, playId, nflId, frameId, club, tackle, x, y, x_facing, y_facing, s, a, position,
         rank, club, defendersInTheBox, ball_carrier, ballCarrierId, ballCarrierDisplayName, absoluteYardlineNumber, 
         time, defensiveTeam, displayName, distance_to_ball, distance_to_ball_next, playDescription, is_football, alignment,
         alignment_cluster, passResult, v_approach) |> 
  filter(frameId > 5)

# I don't have clusters built in for passing plays

# ## some plays don't end in tackles.  Some plays have a forced fumble
# week_1 |> 
#   # filter(forcedFumble == 1, tackle == 0) |> 
#   filter(playId == 896, gameId == 2022090800)  |> 
#   filter(defensiveTeam == club) |> 
#   select(forcedFumble, tackle, event, displayName, frameId) |> 
#   print(n = Inf)
# 
# ## i do not know the conlcusion of this play
# week_1 |> distinct(playId, gameId)
# week_1 |> filter(tackle == 1) |> distinct(playId, gameId) 
# week_1 |> filter(playId == 146) |> filter(gameId == 2022090800) |> count(prePenaltyPlayResult)

week_1 |> 
  group_by(gameId) |> filter(cur_group_id() %in% 1) |>   ungroup() |> 
  write_csv("week_1.csv")

dak <- week_1 |> filter(displayName == "Dak Prescott") |> distinct(gameId, playId)
