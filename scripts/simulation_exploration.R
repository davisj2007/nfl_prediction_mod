### simuolation and plotting script

#### > set options and parameters
{
  options(scipen = 999) # no scientific notation
  library(tidyverse) # tidy data science tools
  library(tidymodels) # tidy machine learning
  library(scales) # better plotting
  library(ggridges) # joy plots!
  library(nflseedR) # run nfl simulations
  library(future) # parallel processing
  library(gt) # make nice tables
  library(patchwork) # smash plots togther
  library(espnscrapeR)
  library(ggdist) # plot distributional objects
  library(distributional) # vectorized distributions
  library(tidytext)
}

# set up 'future' plan
plan(multisession)


### get my theme ---------------------------
source('~/Documents/R Projects/jake_theme/jake_theme/theme_jake.R')

### load simulation functions -----------------------------
source(here::here("scripts", "functions_simulation.R"))

## load preferred colors  -----------------
preferred_colors <- read_rds(here::here("data", "preferred_colors.rds"))

# set simulation parameters
total_sims <- 10000 # number of simulations
season_sim <- 2022 # which season
current_week <- 0 # when is this being run

## get the current strenght estimates from model
current_str <- get_current_strength(season_sim, current_week = current_week) %>%
  ungroup() %>%
  arrange(-estimated_strength)


####> Simulate season and explore

### simulate the season
completed_sim <- simulate_nfl(
  nfl_season = season_sim,
  process_games = weekly_predict_mod,
  current_strength = current_str,
  simulations = total_sims
  )

# save completed sims
write_rds(completed_sim, here::here("data", glue::glue("initial_sims_{current_week}.rds")))


## plot the summary of the simulations with my own adornments
plot_summary <- summary(completed_sim) %>%
  gt::opt_align_table_header("left") %>%
  gt::tab_source_note("Model: @davisj2007") %>%
  # gt::tab_header(title = md("**2021 NFL Projections**"), 
  #                subtitle = md(glue::glue("As of week {current_week} | Based on 10,000 simulated seasons")))
  gt::tab_header(title = md("**2021 NFL Projections**"), 
                 subtitle = md(glue::glue("Preseason projections | Based on 10,000 simulated seasons")))
  
# save summary
gt::gtsave(plot_summary,
           here::here("plots", "projections", glue::glue("summary_tbl_{current_week}.png")))



#extract the overall simulation rankings
sim_agg_summary <- completed_sim$overall %>%
  left_join(preferred_colors, by = c("team" = "team_abbr"))

#extract all games in simulation in wide format
sim_games_wide <- completed_sim$games %>%
  mutate(game_id = str_c(away_team, "_", home_team,"_", "wk", "_", week)) %>%
  left_join(preferred_colors, by = c("away_team" = "team_abbr")) %>%
  left_join(preferred_colors, by = c("home_team" = "team_abbr"),
            suffix = c("_away", "_home")) %>%
  mutate(winner = ifelse(result > 0, home_team, away_team))



#extract all games in long format
sim_games_long <- completed_sim$games %>%
  mutate(game_id = str_c(away_team, "_", home_team,"_", "wk", "_", week)) %>%
  select(game_id, sim, game_type, week, result, away_team, home_team) %>%
  pivot_longer(cols = c(away_team, home_team),
               names_to = "setting",
               values_to = "team") %>%
  left_join(nflfastR::teams_colors_logos %>% select(-team_nick, -team_name, -team_id), by = c("team" = "team_abbr")) %>%
  mutate(win = case_when(
    result > 0 & setting == "home_team" ~ 1,
    result < 0 & setting == "away_team" ~ 1,
    result < 0 & setting == "home_team" ~ 0,
    result > 0 & setting == "away_team" ~ 0)) %>%
  relocate(win, .after = "team")


# simulation level team summarys
sim_teams <- completed_sim$teams %>%
  left_join(preferred_colors, by = c("team" = "team_abbr"))

# cumulative distribution probabilites for wins
sim_team_probs <- completed_sim$team_wins %>%
  left_join(preferred_colors, by = c("team" = "team_abbr"))

# chart of probability of winning less than n games
sim_team_probs %>%
  filter(team == "PIT") %>%
  ggplot(aes(wins, under_prob)) + 
  ggshadow::geom_glowline(aes(color = team_color), size = 1.3) + 
  scale_color_identity() + 
  scale_x_continuous(breaks = breaks_pretty(8)) + 
  scale_y_continuous(labels = label_percent(1), breaks = breaks_pretty()) 

### use my function to plot the weekly predictions and probabilitues
sim_games_wide %>%
  plot_weekly_projections(week = 1)

plot_save(here::here("plots", "projections", glue::glue("week_{current_week + 1}_projections.png")), height = 12, width = 16)


## plot the full season for a single team
sim_games_wide %>%
  #filter(week > 1) %>%
  plot_season(team = "PIT")


## plot the current expected strenght distributions
current_str %>%
  left_join(divisions) %>%
  left_join(preferred_colors, by = c("team" = "team_abbr")) %>%
  ggplot(aes(y = fct_reorder(team, estimated_strength), dist = strength_dist, fill = preferred_color)) +
  stat_dist_gradientinterval(fill_type = "gradient") +
  scale_x_continuous(labels = label_percent(1),
                     breaks = pretty_breaks()) +
  scale_fill_identity() +
  facet_wrap(~conf, scales = "free_y") +
  theme_jake_light +
  labs(x = "\n'True' expected winning percentage over a large sample",
       y = element_blank(),
       title = "Estimation of the laten strength of each team\n(and our uncertainty)",
       subtitle = "Estimate of 'true' team strength | Preseason",
       caption = "Model: @davisj2007")

plot_save(here::here("plots", "projections", glue::glue("teamstrength_dist_{current_week}.png")), height = 12, width = 16)


## alternative to summary plot, since i didn't know that initially existed
full_season_tbl <- sim_agg_summary %>%
  #filter(won_conf > 0) %>%
  gt_nflseedR_overall(week = 20)

gt::gtsave(full_season_tbl,
           here::here("plots", "projections", glue::glue("full_proj_tbl_{current_week}.png")))




