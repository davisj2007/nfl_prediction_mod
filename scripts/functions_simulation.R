### building simulation

{
  options(scipen = 999) # no scientific notation
  library(tidyverse) # tidy data science
  library(broom) # tidy model objects
  library(nflseedR) # for game data and simulation
  library(tidymodels) # tidy machine learning models
  library(workflows)
  library(future) #parallel processiong
  library(distributional) # for easy distributions
  library(mgcv)

# set up 'future' plan
plan(multisession)
}


### function to get the current estimated strength for each team

get_current_strength <- function(current_season, current_week = NULL) {

  x <- {{current_week}}

  set_team_levels <- read_rds(here::here("models", "team_levels.rds"))
  init_mod <- read_rds(here::here("models", "inital_ratings_estimiator.rds"))
  strength_mod <- read_rds(here::here("models", "true_strength_estimator.rds"))
  season_final_wp <- read_rds(here::here("data", "season_final_wp.rds" ))

  prev_season_wp <- season_final_wp %>%
    filter(season == {{ current_season }} - 1) %>%
    mutate(season = season + 1) %>%
    rename("prev_season_wp" = "final_wp")


  initial_ratings <- read_csv(url("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv")) %>%
    filter(season == {{ current_season }} - 1 ) %>%
    dplyr::select(date, season, team_1 = team1, team_2 = team2, elo_1 = elo1_pre, elo_2 = elo2_pre) %>%
    pivot_longer(team_1:elo_2,
                 names_to = c(".value", "set"),
                 names_pattern = "(.*)_(.)") %>%
    select(-set) %>%
    group_by(season, team) %>%
    slice_max(order_by = date, n = 1) %>%
    mutate(elo = elo - (1/3 * (elo - 1505)),
           season = {{ current_season }},
           team = case_when(
             team == "LAR" ~ "LA",
             team == "OAK" ~ "LV",
             team == "WSH" ~ "WAS",
             TRUE ~ team
           ),
           team = factor(team, levels = set_team_levels)) %>%
    ungroup() %>%
    dplyr::select(-date)


  initial_ratings_pred <- initial_ratings %>%
    left_join(prev_season_wp) %>%
    mutate(predict(init_mod, new_data = .),
           predict(init_mod, new_data = ., type = "conf_int", std_error = T),
           alpha = .pred ^ 2 * (1 - .pred) / .std_error^2 - .pred,
           beta = alpha * (1 / .pred - 1),
           strength_dist = dist_beta(alpha, beta)) %>%
    rename("estimated_strength" = .pred) %>%
    select(season, team, elo, estimated_strength, alpha, beta, strength_dist)

  if (x == 0) {

return(initial_ratings_pred) }

  else {
    season_games <- nflseedR::load_sharpe_games() %>%
      filter(season == {{current_season}}, week <= {{current_week}}) %>%
      pivot_longer(cols = away_team:home_score,
                   names_to = c("set", ".value"),
                   names_pattern = "(.+)_(.+)") %>%
      dplyr::select(game_id, season, game_type, week, div_game, set, team, score, result) %>%
      dplyr::mutate(team = factor(team, levels = set_team_levels)) %>%
      group_by(season, team) %>%
      arrange(season, week) %>%
      mutate(
        result = as.numeric(result),
        win = case_when(
          result > 0 & set == "home" ~ 1,
          result < 0 & set == "away" ~ 1,
          result < 0  & set == "home" ~ 0,
          result > 0 & set == "away" ~ 0,
          is.na(result) ~ 0,
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
      mutate(game_played = 1,
             game_num = cumsum(game_played)) %>%
      ungroup()

    season_weeks <- nflseedR::load_sharpe_games() %>%
      filter(season == {{current_season}}, week <= {{current_week}}) %>%
      pivot_longer(cols = away_team:home_score,
                   names_to = c("set", ".value"),
                   names_pattern = "(.+)_(.+)") %>%
      dplyr::select(season, week, score, game_type) %>%
      group_by(season, week) %>%
      summarize(season_week_points_mean = mean(score),
                season_week_points_sd = sd(score),
                .groups = "drop") %>%
      mutate(cumulative_season_points_mean = cummean(season_week_points_mean)) %>%
      ungroup() %>%
      mutate(cumulative_season_points_sd = sqrt(cumsum(season_week_points_mean - cumulative_season_points_mean) ^ 2 / week))

    season_join <- season_games %>%
      left_join(season_weeks) %>%
      mutate(id = str_c(season, "_", team)) %>%
      relocate(id, .before = everything())


    current_strength <- season_join %>%
      left_join(initial_ratings_pred %>%
                  select(season, team, "initial_rating" = estimated_strength)) %>%
      mutate(predict(strength_mod, new_data = .),
             predict(strength_mod, new_data = ., type = "conf_int", std_error = T),
             alpha = .pred ^ 2 * (1 - .pred) / .std_error^2 - .pred,
             beta = alpha * (1 / .pred - 1),
             strength_dist = dist_beta(alpha, beta)) %>%
      rename("estimated_strength" = .pred) %>%
      group_by(team) %>%
      filter(week == 1 | !is.na(result)) %>%
      slice_max(order_by = week, n = 1) %>%
      select(team, estimated_strength, alpha, beta, strength_dist)

return(current_strength)
}
}



weekly_predict_mod <- function(teams, games, week_num, ...) {

  require(tidymodels)

  set_team_levels <- read_rds(here::here("models", "team_levels.rds"))
  mov_mod <- read_rds(here::here("models", "mov_predictor.rds"))
  mov_se <- read_rds(here::here("models", "mov_se.rds"))

  # round out (away from zero)
  # this way the simulator never simulates a tie
  # the simulator will still allow ties to be simulated if you want
  # ... but not on playoff games
  round_out <- function(x) {
    x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
    x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
    return(x)
  }

  if (!("estimated_strength" %in% colnames(teams))) {
    args <- list(...)
    if ("current_strength" %in% names(args)) {
      # pull the current_strength info from custom arguments
      team_join <- teams %>%
        dplyr::inner_join(args$current_strength %>%
                            select(team, alpha, beta),
                          by = c("team")) %>%
        mutate(pre_game_strength = rbeta(n(), alpha, beta))
      } else {
      # error with a friendly error message if no  data is passed in
      stop("Pass in a tibble `current_strength` as an argument to `simulate_nfl()`")
    }
  }

  ratings <- team_join %>%
    dplyr::select(sim, team, pre_game_strength)



  ## simulate game outcomes
  games_prep <- games %>%
    # add in the away team's base estimate to the game data
    # note we join on both `sim` and the team
    # always join on `sim` to make sure each sim cares about only its data
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename("pre_game_strength_away" = "pre_game_strength") %>%
    # repeat for the home team as well
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
    dplyr::rename("pre_game_strength_home" = "pre_game_strength") %>%
    dplyr::mutate(strength_diff = pre_game_strength_home - pre_game_strength_away,
           game_type_adj = as_factor(ifelse(game_type == "REG", "REG", "PLAYOFF")),
           is_after_week_1 = ifelse(week == 1, 0, 1),
           across(c(away_team, home_team), ~factor(., levels = set_team_levels)))


  games <- games_prep %>%
    mutate(predict(mov_mod, new_data = .),
           outcome_gen = rnorm(n(), .pred, mov_se),
           result = dplyr::case_when(
             !is.na(result) | week != week_num ~ result,
             week == week_num & is.na(result)  ~ as.integer(round_out(outcome_gen)),
             TRUE ~ as.integer(result)),
           across(c(away_team, home_team), ~as.character(.))) %>%
    dplyr::select(sim:result)

  return(list(teams = teams, games = games))

}



gt_nflseedR_overall <- function(data, week = NULL, ...){
  data %>%
    mutate(division = str_remove(division, "AFC |NFC ")) %>%
    dplyr::select(conf, division, team, wins, playoff, div1:won_sb) %>%
    group_by(conf, division) %>%
    arrange(conf, division, desc(wins)) %>%
    gt() %>%
    fmt_number(vars(wins), decimals = 1) %>%
    fmt_percent(vars(playoff, div1, seed1, won_conf, won_sb), decimals = 0) %>%
    cols_label(
      playoff = html("Make<br>Playoffs"),
      div1 = html("Win<br>DIV"),
      seed1 = html("1st-RD<br>Bye"),
      won_conf = html("Win<br>Conf"),
      won_sb = html("Win<br>SB")
    ) %>%
    data_color(
      columns = c(playoff, div1, seed1, won_conf, won_sb),
      colors = scales::col_numeric(palette = c("white", "#00DFFF", "#00AAFF"), domain = c(0, 1)),
    ) %>%
    gt::tab_header(title = md("**2021 NFL Projections**"), subtitle = md(glue::glue("As of week {week} | Based on 10,000 simulated seasons"))) %>%
    tab_source_note(source_note = "Model: @davis2007 | Simulation: @nflseedR | Table Design: @thomas_mock") %>%
    opt_align_table_header("left") %>%
    tab_style(style = cell_borders(sides = "bottom",  color = "white", weight = px(2)),
              locations = cells_body(columns = c(everything()),
                                     rows = nrow(data$`_data`))) %>%
    tab_options(column_labels.background.color = "white",
                table.border.top.width = px(3),
                table.border.top.color = "white",
                table.border.bottom.color = "black",
                table.border.bottom.width = px(3),
                column_labels.border.top.width = px(3),
                column_labels.border.top.color = "white",
                column_labels.border.bottom.width = px(3),
                column_labels.border.bottom.color = "black",
                data_row.padding = px(3),
                source_notes.font.size = 12,
                table.font.size = 16,
                heading.align = "left",
                table.background.color = "white",
                table.font.color = "black",
                table.font.names = "Bungee",
                heading.background.color = "white")
    #espnscrapeR::gt_theme_538()
}


plot_weekly_projections <- function(data, week) {

  title_lab <- glue::glue("Week { week } predictions")

  data %>%
    filter(week == {{ week }}) %>%
    mutate(fill_col = ifelse(result > 0, preferred_color_home, preferred_color_away),
           clean_title = str_c(away_team, " @ ", home_team)) %>%
    group_by(clean_title) %>%
    mutate(exp_result = number(abs(mean(result)), .1),
           win_prob_num = mean(ifelse(result > 0, 1, 0)),
           winner = ifelse(win_prob_num > .5, home_team, away_team),
           win_prob = ifelse(win_prob_num > .5, win_prob_num, 1 - win_prob_num),
           win_prob = percent(win_prob, .1),
           win_prob_num = ifelse(win_prob_num > .5, win_prob_num, 1 - win_prob_num)) %>%
    ungroup() %>%
    mutate(clean_title = fct_reorder(clean_title, win_prob_num, mean)) %>%
    ggplot(aes(result)) +
    geom_histogram(aes(fill = fill_col, y = stat(count) / 10000),
                   binwidth = 1,  show.legend = F, color = "grey85") +
    geom_hline(yintercept = 0, color = "#00AAFF") +
    geom_text(aes(x = 0, y = .065,
                  label = glue::glue("{winner} wins {win_prob}\nwith an expected MOV of {exp_result} points")),
              data = . %>% distinct(clean_title, exp_result, win_prob, winner),
              family = "Bungee") +
    geom_text(aes(x = 25, y = -.005,
                  label = glue::glue("{home_team} wins ->")),
              data = . %>% distinct(clean_title, home_team),
              family = "Bungee") +
    geom_text(aes(x = -25, y = -.005,
                  label = glue::glue("<- {away_team} wins")),
              data = . %>% distinct(clean_title, away_team),
              family = "Bungee") +
    scale_x_continuous(breaks = extended_breaks(n = 10)) +
    scale_y_continuous(labels = label_percent(1),
                       breaks = pretty_breaks(),
                       expand = expansion(mult = c(.05, .25))) +
    scale_fill_identity() +
    facet_wrap(~ clean_title) +
    theme_jake_light +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "#00AAFF", color = NA),
          strip.text = element_text(color = "black")) +
    labs(x = "Home Team margin of victory",
         y = element_blank(),
         title = title_lab,
         subtitle = "Projected distribution of home team margin of victory | Based on 10,000 simulations",
         caption = "Model: @davisj2007 | Simulation: @nflseedR")
}

plot_season <- function(data, team) {

  title_lab <- nflfastR::teams_colors_logos %>%
    filter(team_abbr == {{ team }}) %>%
    dplyr::select(team_abbr, team_name) %>%
    mutate(lab = glue::glue("2021 Schedule: {team_name}")) %>%
    pull(lab)

    winner_lab <- data %>%
      filter(home_team == {{ team }} | away_team == {{ team }} , week <= 18) %>%
      mutate(team_setting = ifelse(home_team == {{ team }} , glue::glue(" vs {away_team}"),
                                   glue::glue(" @ {home_team}")),
             clean_title = str_c("Week ", week, team_setting),
             clean_title = fct_reorder(clean_title, week),
             fill_col = ifelse(result > 0, preferred_color_home, preferred_color_away)) %>%
    group_by(clean_title) %>%
      mutate(exp_result = number(abs(mean(result)), .1),
             win_prob = mean(ifelse(result > 0, 1, 0)),
             winner = ifelse(win_prob > .5, home_team, away_team),
             win_prob = ifelse(win_prob > .5, win_prob, 1-win_prob),
             win_prob = percent(win_prob, .1)) %>%
      ungroup() %>%
      distinct(clean_title, exp_result, win_prob, winner)


    home_base <- data %>%
      filter(home_team == {{ team }} | away_team == {{ team }} , week <= 18) %>%
      mutate(team_setting = ifelse(home_team == {{ team }} , glue::glue(" vs {away_team}"),
                                   glue::glue(" @ {home_team}")),
             clean_title = str_c("Week ", week, team_setting),
             clean_title = fct_reorder(clean_title, week),
             fill_col = ifelse(result > 0, preferred_color_home, preferred_color_away)) %>%
      distinct(clean_title, home_team)

    away_base <- data %>%
      filter(home_team == {{ team }} | away_team == {{ team }} , week <= 18) %>%
      mutate(team_setting = ifelse(home_team == {{ team }} , glue::glue(" vs {away_team}"),
                                   glue::glue(" @ {home_team}")),
             clean_title = str_c("Week ", week, team_setting),
             clean_title = fct_reorder(clean_title, week),
             fill_col = ifelse(result > 0, preferred_color_home, preferred_color_away)) %>%
      distinct(clean_title, away_team)


  data %>%
    filter(home_team == {{ team }} | away_team == {{ team }} , week <= 18) %>%
    mutate(team_setting = ifelse(home_team == {{ team }} , glue::glue(" vs {away_team}"),
                                 glue::glue(" @ {home_team}")),
           clean_title = str_c("Week ", week, team_setting),
           clean_title = fct_reorder(clean_title, week),
           fill_col = ifelse(result > 0, preferred_color_home, preferred_color_away)) %>%
    ggplot(aes(result)) +
    geom_histogram(aes(fill = fill_col, y = stat(count) / 10000),
                   binwidth = 1,  show.legend = F, color = "grey85") +
    geom_hline(yintercept = 0, color = "#00AAFF") +
    geom_text(aes(x = 0, y = .065,
                  label = glue::glue("{winner} wins {win_prob}\nExpected MOV: {exp_result} points")),
              data = winner_lab,
              family = "Bungee") +
    geom_text(aes(x = 25, y = -.005,
                  label = glue::glue("{home_team} wins ->")),
              data = home_base,
              family = "Bungee") +
    geom_text(aes(x = -25, y = -.005,
                  label = glue::glue("<- {away_team} wins")),
              data = away_base,
              family = "Bungee") +
    scale_x_continuous(breaks = extended_breaks(n = 7)) +
    scale_y_continuous(labels = label_percent(1),
                       breaks = pretty_breaks(),
                       expand = expansion(mult = c(.05, .25))) +
    scale_fill_identity() +
    facet_wrap(~ clean_title) +
    theme_jake_light +
    coord_cartesian(ylim = c(-.01, .075)) +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "#00AAFF", color = NA),
          strip.text = element_text(color = "black")) +
    labs(x = "Home Team margin of victory",
         y = element_blank(),
         title = title_lab,
         subtitle = "Projected distribution of home team margin of victory | Based on 10,000 simulations",
         caption = "Model: @davisj2007 | Simulation: @nflseedR")
}


