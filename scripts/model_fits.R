### training GAM based empirical bayes estimates for simulation purposes

{
  options(scipen = 999) # no scientific notation
  library(tidyverse) # tidy data science
  library(scales) # better plotting
  library(broom) # tidy model objects
  library(tidymodels) # tidy machine learning models
  library(future) # for parallel processing
  library(nflseedR) # get nfl data
  library(modelbased) # easy plotting and analysis of models
  library(see) # easy stats plotting library
}

# set up 'future' plan
plan(multisession)


### get my theme ---------------------------
source('~/Documents/R Projects/jake_theme/jake_theme/theme_jake.R')


### load and prep data ------------------------------

# get historical data
historical_games_wide <- load_sharpe_games()

# convert to long format
historical_games_long <- historical_games_wide %>%
  pivot_longer(cols = away_team:home_score,
               names_to = c("set", ".value"),
               names_pattern = "(.+)_(.+)") %>%
  mutate(team = case_when(
    team == "STL" ~ "LA",
    team == "SD" ~ "LAC",
    team == "OAK" ~ "LV",
    TRUE ~ team))

# create variables of interest at the team level
history_games <- historical_games_long %>%
  select(game_id, season, game_type, week, div_game, set, team, score, result) %>%
  filter(season < 2022) %>%
  mutate(team = as_factor(team)) %>%
  group_by(season, team) %>%
  arrange(season, week) %>%
  mutate(
    result = as.numeric(result),
    win = case_when(
      result > 0 & set == "home" ~ 1,
      result < 0 & set == "away" ~ 1,
      result < 0  & set == "home" ~ 0,
      result > 0 & set == "away" ~ 0,
      TRUE ~ .5
    ),
    delta = case_when(
      win == 1 & set == "home" ~ result,
      win == 1 & set == "away" ~ -1 * result,
      win == 0  & set == "home" ~ result,
      win == 0 & set == "away" ~ -1 *result,
      TRUE ~ 0
    ),
    ### cumulative team statistics
    wins = cumsum(win),
    cumulative_point_mean = cummean(score),
    cumulative_point_sd = sqrt(cumsum(score - cumulative_point_mean)^2 / week),
    cumulative_pd = cumsum(delta),
    cumulative_pd_sd = sqrt(cumsum(delta - cummean(delta))^2 / week)
  ) %>%
  ungroup() %>%
  group_by(season, team) %>%
  mutate(reg_wins = sum(ifelse(game_type == "REG", win, 0)),
         tot_wins = sum(win),
         game_played = 1,
         game_num = cumsum(game_played),
         tot_games = max(game_num),
         reg_wp = reg_wins / 16,
         final_wp = tot_wins / tot_games
  ) %>%
  ungroup()

# create variables of interest at the week level
history_season_week <- historical_games_long %>%
  select(season, week, score, game_type) %>%
  filter(season < 2022) %>%
  group_by(season, week) %>%
  summarize(season_week_points_mean = mean(score),
            season_week_points_sd = sd(score),
            .groups = "drop") %>%
  group_by(season) %>%
  arrange(week) %>%
  mutate(cumulative_season_points_mean = cummean(season_week_points_mean)) %>%
  ungroup() %>%
  mutate(cumulative_season_points_sd = sqrt(cumsum(season_week_points_mean - cumulative_season_points_mean) ^ 2 / week))

# join team and week data
history_data <- history_games %>%
  left_join(history_season_week) %>%
  mutate(id = str_c(season, "_", team)) %>%
  relocate(id, .before = everything())

## keep factor levels consistent
set_team_levels <- levels(history_data$team)

#get the final win percentage for all teams across all years
season_final_wp <- history_data %>%
  mutate(team = factor(team, levels = set_team_levels)) %>%
  distinct(season, team, final_wp)

#isolate previous season win percentage
prev_season_wp <- season_final_wp %>%
  mutate(season = season + 1) %>%
  rename("prev_season_wp" = "final_wp")

# save team levels for later
write_rds(season_final_wp, here::here("data", "season_final_wp"))

### Estimate week one strength from 538 Elo history ------------------------------

#pull 538 data
initial_ratings <- read_csv(url("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv")) %>%
  filter(season >= 1998) %>%
  select(date, season, team_1 = team1, team_2 = team2, elo_1 = elo1_pre, elo_2 = elo2_pre) %>%
  pivot_longer(team_1:elo_2,
               names_to = c(".value", "set"),
               names_pattern = "(.*)_(.)") %>%
  select(-set) %>% 
  mutate(team = case_when(
    team == "LAR" ~ "LA",
    team == "OAK" ~ "LV",
    team == "WSH" ~ "WAS",
    TRUE ~ team
  ),
  team = factor(team, levels = set_team_levels)) %>%
  group_by(season, team) %>%
  slice_min(order_by = date, n = 1) %>%
  ungroup() %>%
  select(-date)

#join to historical data with final winning percentage for initial estimation
initial_ratings_join <- prev_season_wp %>%
  filter(season < 2022) %>%
  left_join(initial_ratings, by = c("season", "team")) %>%
  left_join(season_final_wp, by = c("season", "team")) %>%
  group_by(team) %>%  
  arrange(season, team) %>% 
  drop_na() %>%
  ungroup()

# how correlated are wp in seasons n and n+1?
initial_ratings_join %>%
  ggplot(aes(final_wp, prev_season_wp)) + 
  geom_point() + 
  geom_smooth()

# how about elo?
initial_ratings_join %>% 
  ggplot(aes(elo, final_wp)) +
  geom_point(color = jake_cols("sea green")) + 
  geom_smooth()

## formal statistics
initial_ratings_join %>%
  select(-season, -team) %>%
  correlation::correlation() # only slightly


#> split data into test and train

set.seed(538)
ratings_split <- initial_split(initial_ratings_join, strata = season)
ratings_train <- training(ratings_split)
ratings_test <- testing(ratings_split)
ratings_rs <- vfold_cv(ratings_train)

###> train the model --------------

# setting up a GAM spec
init_spec <- gen_additive_mod("regression", 
                              select_features = T, adjust_deg_free = tune()) %>%
  set_engine("mgcv", family = binomial)

# adding to a workflow with the right model formulas
init_wf <- workflow() %>%
  add_model(init_spec, formula = final_wp ~ s(elo, k = -1) + 
              s(prev_season_wp, k = -1) + 
              s(team, bs = "re") + 
              s(team, season, bs = "re")) %>%
  add_variables(outcomes = final_wp, predictors = c(elo, team, prev_season_wp, season))


## setting up a tuning grid for hyperparameters
set.seed(538)
init_grid <- tune_grid(
  init_wf,
  resamples = ratings_rs,
  grid = 20
)

# show me best params
show_best(init_grid)

#select best hyperparameters
best_init_params <- init_grid %>% 
  select_best()

#add them to the workflow
init_wf_final <- finalize_workflow(init_wf, best_init_params)


# train the model
init_mod <- init_wf_final %>%
  fit(ratings_train)


##> evaluate model on training set

#add predictions to training set
init_model_train <- ratings_train %>%
  mutate(predict(init_mod, new_data = .),
         predict(init_mod, new_data = ., type = "conf_int", std_error = T)) %>%
  rename("expected_wp" = .pred)

# sample for how to convert predictions to beta distributio
init_model_train %>%
  select(-starts_with(".pred")) %>%
  mutate(alpha = expected_wp^2 * (1 - expected_wp) / .std_error^2 - expected_wp, 
         beta = alpha * (1 / expected_wp - 1))

# show me predictions vs actual
init_model_train %>%
  ggplot(aes(final_wp, expected_wp)) +
  geom_point(color = jake_cols("blue bolt")) 

#get metrics
init_model_train %>%
  metrics(truth = final_wp, estimate = expected_wp)


##> evaluate model on testing set

#predict on unseen training data
init_model_test <- ratings_test %>%
  mutate(predict(init_mod, new_data = .)) %>%
  rename("expected_wp" = .pred) %>%
  mutate(tot_wins = final_wp * ifelse(season == 2021, 17,16),
         exp_wins = expected_wp  * ifelse(season == 2021, 17,16))


init_model_test %>%
  ggplot(aes(final_wp, expected_wp)) +
  geom_point(color = jake_cols("cornflower blue")) + 
  geom_smooth()

#isolate rmse for later
init_rmse <- init_model_test %>%
  metrics(truth = final_wp, estimate = expected_wp) %>%
  mutate(week = 0) %>%
  filter(.metric == "rmse")



###> Save the 'initial strength' estimation model ------------------

write_rds(init_mod, here::here("models", "inital_ratings_estimiator.rds"))

### 'true strength' model build -----------------

### > split data into test and train --------------

# remove playoff games
history_data_filt <- history_data %>%
  mutate(round_wins = as.integer(floor(tot_wins)),
         tot_games = as.integer(tot_games)) %>%
  filter(game_type == "REG") %>%
  left_join(initial_ratings_join %>% 
              mutate(predict(init_mod, new_data = .)) %>%
              rename("initial_rating" = .pred))


# shout out to the Burgh
set.seed(412)

#remove seasons prior to 2000 and Houstons first season from training and testing sets
strength_split <- group_initial_split(history_data_filt %>%
                                        filter(season > 1999,
                                               !(season <= 2002 & team == "HOU")), group = id)
strength_train <- training(strength_split)
strength_test <- testing(strength_split)
strength_rs <- vfold_cv(strength_train)

###> train a model ----------------------------------

set.seed(142)

#set a GAM spec
strength_spec <- gen_additive_mod("regression", 
                                  select_features = T) %>%
  set_engine("mgcv", family = quasibinomial, method = 'REML')

#add the right model formulas to workflow
strength_wf <- workflow() %>%
  add_model(strength_spec,
            formula = final_wp ~
              s(cumulative_point_mean) +
              s(cumulative_season_points_mean) +
              s(cumulative_point_sd) +
              s(cumulative_pd) +
              s(game_num) +
              s(initial_rating) +
              s(cumulative_season_points_mean) +
              ti(cumulative_point_mean, cumulative_season_points_mean) +
              ti(cumulative_point_sd, cumulative_season_points_mean) +
              ti(cumulative_pd, game_num) +
              ti(initial_rating, game_num) +
              as_factor(week) + 
              as_factor(game_num)) %>%
  add_variables(outcomes = final_wp, 
                predictors = c(cumulative_point_mean, cumulative_point_sd, cumulative_pd, initial_rating,cumulative_season_points_mean, cumulative_season_points_sd,
                               game_num, week))

##### no hyperparameter tuning since my machine wasn't handling it and it won't be that different
#### the MGCV implementation with REML does its own tuning to an extend

# train model
strength_mod <- strength_wf %>%
  fit(strength_train)


###> evaluate the base model without bayesian updating ------------------

#get model predictions for training data
strength_train_pred <- strength_train %>%
  mutate(predict(strength_mod, new_data = .),
         exp_wins = .pred * ifelse(season < 2022, 16, 17)) %>% 
  select(id, week, team, final_wp, "expected_wp" = .pred, reg_wins, exp_wins)

# test strenght estimates for a single week, just to confirm my alpha and beta calcs are accurate
strength_train %>%
  filter(season == 2021, week == 1) %>% 
  mutate(predict(strength_mod, new_data = .),
    predict(strength_mod, new_data = ., type = "conf_int", std_error = T)) %>% 
  select(team, week, season, starts_with(".pred"), .std_error) %>%
  arrange(-.pred) %>%
  mutate(alpha = .pred ^ 2 * (1 - .pred) / .std_error^2 - .pred,
         beta = alpha * (1 / .pred - 1),
         dists = dist_beta(alpha, beta)
    ) %>%
  ggplot(aes(y = fct_reorder(team, .pred))) + 
  ggdist::stat_dist_halfeye(aes(dist = dists)) +
  scale_x_continuous(labels = label_percent(), limits = c(0, 1))

# check predictions versus final by week
strength_train_pred %>%
  ggplot(aes(final_wp, expected_wp, color = as_factor(week))) + 
  geom_point() +
  geom_smooth()

# visualize expected season wins versus actual season wins, by week
strength_train_pred %>%
  ggplot(aes(reg_wins, exp_wins)) +
  geom_point(color = jake_cols("vivid blue")) +
  geom_smooth(color = jake_cols("vivid blue"), fill = jake_cols("vivid blue")) +
  facet_wrap(~week)

# get statistics for model eval
strength_train_pred %>%
  metrics(truth = reg_wins, estimate = exp_wins)

# visualize by week
strength_train_pred %>%
  group_by(week) %>%
  metrics(truth = final_wp, estimate = expected_wp) %>%
  arrange(-week) %>%
  filter(.metric == "rmse") %>% 
  ggplot(aes(week, .estimate)) +
  geom_col()


###> evaluate model fit on testing data ------------------

#get predictions for test set
strength_test_pred <- strength_test %>%
  mutate(predict(strength_mod, new_data = .),
         exp_wins = .pred * ifelse(season < 2022, 16, 17)) %>%
  select(id, week, team, final_wp, "expected_wp" = ".pred", reg_wins, exp_wins)

#check predictions vs actuals by week
strength_test_pred %>%
  ggplot(aes(final_wp, expected_wp)) +
  geom_point(color = jake_cols("vivid blue")) +
  geom_smooth(method = lm) + 
  facet_wrap(~week)

# get metrics for eval
strength_test_pred %>%
  metrics(truth = reg_wins, estimate = exp_wins)

## plot for twitter thread on RMSE by week
strength_test_pred %>%
  group_by(week) %>%
  metrics(truth = final_wp, estimate = expected_wp) %>%
  filter(.metric == "rmse") %>%
  bind_rows(init_rmse) %>%
  arrange(-week) %>%
  ggplot(aes(week, .estimate)) + 
  geom_col(fill = jake_cols("cornflower blue")) +
  scale_x_continuous(breaks = breaks_pretty(8)) + 
  labs(x = element_blank(),
       y = element_blank(),
       title = "The model improves quickly over time",
       subtitle = "RMSE of final winning percentage for predictions\nfollowing each week | training set")

plot_save(here::here("plots", "model_fit", "rmse_wp.png"), height = 10, width = 10)


###> Save the 'true strength' estimation model ------------------

write_rds(strength_mod, file = here::here("models", "true_strength_estimator.rds"))

### Build a predicted score model ------------------------------

# get weekly estimated true strengths for each team from prev model
true_strength_history_init <- history_data %>%
  left_join(initial_ratings %>% select(season, team, elo)) %>%
  mutate(predict(init_mod, new_data = .)) %>%
  rename("initial_rating" = .pred) %>%
  mutate(team = factor(team, levels = set_team_levels),
         predict(strength_mod, new_data = .)) %>%
  rename("estimated_strength" = .pred) %>%
  select(id:div_game, set:result, cumulative_point_mean, initial_rating, estimated_strength) %>%
  group_by(season, team) %>%
  arrange(week) %>%
  mutate(pre_game_strength = lag(estimated_strength),
         pre_game_strength = ifelse(is.na(pre_game_strength), initial_rating, pre_game_strength),
         pre_point_mean = lag(cumulative_point_mean),
         pre_point_mean = ifelse(is.na(pre_point_mean), 0, pre_point_mean)) %>%
  ungroup() 

# generate the strengths for each team in each game
game_comps <- true_strength_history_init %>%
  select(-id, -estimated_strength, -cumulative_point_mean) %>%
  pivot_wider(id_cols = c(game_id, game_type, week, div_game, result),
              names_from = set,
              values_from = c(team, pre_game_strength, score, pre_point_mean)) %>%
  rename("home_team" = "team_home", "away_team" = "team_away") %>%
  mutate(strength_diff = pre_game_strength_home - pre_game_strength_away,
         game_type_adj = as_factor(ifelse(game_type == "REG", "REG", "PLAYOFF")),
         across(c(away_team, home_team), ~ factor(., levels = set_team_levels))) %>%
  select(game_type, week, away_team, home_team, pre_game_strength_home, score_away, score_home,
         pre_point_mean_home, pre_point_mean_away,
         pre_game_strength_away, result, strength_diff, game_type_adj) %>%
  mutate(is_after_week_1 = ifelse(week == 1, 0, 1))



### > split data into test and train --------------

# shout out to my hometown
set.seed(724)

game_split <- initial_split(game_comps)
game_train <- training(game_split)
game_test <- testing(game_split)
game_rs <- vfold_cv(game_train)

# set up a linear model
score_spec <- linear_reg()

# do some minor preprocessing for a better model
mov_rec <- recipe(result ~ pre_game_strength_home +
                           strength_diff + is_after_week_1, data = game_train) %>%
  step_interact(~pre_game_strength_home:strength_diff) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

#create a workflow
mov_wf <- workflow() %>%
  add_recipe(mov_rec) %>%
  add_model(score_spec)

## fit across all samples to get an idea of stability
mov_tune <- fit_resamples(
  mov_wf,
  #grid = home_score_grid,
  resamples = game_rs
)

collect_metrics(mov_tune)

# fit model
mov_mod <- mov_wf %>%
  fit(game_train) 

## predict on training data
game_train_pred <- game_train %>%
  mutate(predict(mov_mod, new_data = .))


#check metrics
game_train_pred %>%
  metrics(truth = result, estimate = .pred) # in line with resamples

#predictions for test data
game_test_pred <- game_test %>%
  mutate(predict(mov_mod, new_data = .))


# check metrics
game_test_home_pred %>%
  metrics(truth = result, estimate = .pred)

# isolate standard error of the predictions for later
mov_se <- summary(mov_mod$fit$fit$fit)$sigma



###> Save the margin of victory model model ------------------

write_rds(mov_mod, here::here("models", "mov_predictor.rds"))
write_rds(mov_se, here::here("models", "mov_se.rds"))




