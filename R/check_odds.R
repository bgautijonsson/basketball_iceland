library(tidyverse)
library(googlesheets4)
library(metill)
library(purrr)
library(nloptr)
library(here)
theme_set(theme_metill())
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

odds_sheet <- "https://docs.google.com/spreadsheets/d/1ZFa79A3fEXcAkxDz6cxbCMby4Q2WUT0CI9dp6N4UQlk/edit?gid=1774709455#gid=1774709455"

# What percent of optimal Kelly am I willing to bet?
kelly_frac <- 0.05
bet_digits <- 1

# What's my current pool size?
cur_pool <- 150

odds <- c("Niðurstaða") |> 
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |> 
  list_rbind()  |> 
  fill(date_obs) |> 
  mutate_at(
    vars(contains("date")),
    as_date
  ) |> 
  filter(
    date_game >= today()
  )

post <- bind_rows(
  read_csv(here("Basketball", "results", "next_game_preds_kk.csv")) |> mutate(kyn = "kk"),
  read_csv(here("Basketball", "results", "next_game_preds_kvk.csv")) |> mutate(kyn = "kvk")
)

preds <- bind_rows(
  read_csv(here("Basketball", "results", "next_game_preds_kk.csv")) |> mutate(kyn = "kk"),
  read_csv(here("Basketball", "results", "next_game_preds_kvk.csv")) |> mutate(kyn = "kvk")
) |> 
  summarise(
    p_home = mean(value > 0),
    p_away = mean(value < 0),
    .by = c(leikur, heima, gestir, kyn)
  )


d <- odds |> 
  filter(
    date_obs == max(date_obs),
    .by = c(home, away, booker)
  ) |> 
  select(
    booker, kyn, heima = home, gestir = away, o_home:o_away
  ) |> 
  pivot_longer(
    c(o_home:o_away),
    values_to = "o"
  ) |> 
  mutate(
    name = str_replace(name, "o_", "")
  ) |> 
  inner_join(
    preds |> 
      pivot_longer(
        c(p_home:p_away),
        values_to = "p"
      ) |> 
      mutate(
        name = str_replace(name, "p_", "")
      ) |> 
      select(-leikur)
  ) |> 
  rename(outcome = name) |> 
  mutate(
    outcome = factor(outcome, levels = c("home", "tie", "away"))
  ) |> 
  mutate(
    ev = p * (o - 1) - (1 - p),
    f = ev / (o - 1),
    kelly_fraction = pmax(f, 0)
  ) 

get_kelly <- function(p, o, p0 = NULL) {
  if (is.null(p0)) {
    p0 = 1 - p
  }
  
  R <- o - 1
  
  kelly_objective <- function(f) {
    sum(p * log(1 + f * R) + p0 * log(1 - f))
  }
  
  f_init <- rep(1 / length(p), length(p))
  
  constraint <- function(f) {
    sum(f) - 1
  }
  
  lb <- rep(0, length(p))
  ub <- rep(1, length(p))
  
  
  # Run optimization
  result <- nloptr(
    x0 = f_init,
    eval_f = function(f) -kelly_objective(f),  # Negative to maximize
    eval_g_ineq = function(f) constraint(f),   # Inequality constraint
    lb = lb, ub = ub,                          # Bound constraints
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-8)
  )
  
  result$solution
  
  
}

nidurstada_results <- d |> 
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima, booker, kyn)
  ) |> 
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome, kyn)
  ) |> 
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima, kyn)
  ) |> 
  mutate(
    ev = round(p * (o - 1) - (1 - p), 2),
    kelly = kelly * kelly_frac,
    bet_amount = round(kelly * cur_pool, bet_digits),
    kelly = round(kelly, 2),
    pred = round(p, 3),
    p_o = round(1/o, 3),
    text = glue::glue("€={bet_amount} (f={kelly})[ev={ev},p={p},o={p_o}=1/{o}]"),
    text = if_else(kelly == 0, "", text)
  ) |> 
  select(booker, kyn, heima, gestir, outcome, text) |> 
  pivot_wider(
    names_from = outcome, 
    values_from = text,
    values_fill = ""
  ) |> 
  select(booker, kyn, heima, gestir, home, away) 



change <- c("Forgjöf") |> 
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |> 
  list_rbind()  |> 
  fill(date_obs, booker) |> 
  mutate_at(
    vars(contains("date")),
    as_date
  ) |> 
  filter(
    date_game >= today()
  ) |> 
  rename(
    heima = home,
    gestir = away
  )

forgjof_results <- change |> 
  inner_join(
    bind_rows(
      read_csv(here("Basketball", "results", "next_game_preds_kk.csv")) |> mutate(kyn = "kk"),
      read_csv(here("Basketball", "results", "next_game_preds_kvk.csv")) |> mutate(kyn = "kvk")
    ),
    relationship = "many-to-many"
  ) |> 
  summarise(
    p_home = mean(value + change > 0),
    p_away = mean(value + change < 0),
    .by = c(booker, change, leikur, heima, gestir, kyn, o_home, o_away)
  ) |> 
  pivot_longer(
    c(o_home:p_away),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |> 
  pivot_wider(names_from = type) |> 
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, gestir, heima, kyn)
  ) |> 
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome, kyn)
  ) |> 
  mutate(
    kelly = get_kelly(p, o),
    .by = c(gestir, heima, kyn)
  ) |> 
  mutate(
    ev = round(p * (o - 1) - (1 - p), 2),
    kelly = kelly * kelly_frac,
    bet_amount = round(kelly * cur_pool, bet_digits),
    kelly = round(kelly, 2),
    pred = round(p, 3),
    p_o = round(1/o, 3),
    text = glue::glue("€={bet_amount} (f={kelly})[ev={ev},p={p},o={p_o}=1/{o}]"),
    text = if_else(kelly == 0, "", text)
  ) |> 
  select(booker, kyn, change, heima, gestir, outcome, text) |> 
  pivot_wider(
    names_from = outcome, 
    values_from = text,
    values_fill = ""
  ) |> 
  select(booker, kyn, change, heima, gestir, home, away) 


points <- c("Mörk") |> 
  map(
    \(x) read_sheet(odds_sheet, sheet = x)
  ) |> 
  list_rbind()  |> 
  fill(date_obs, booker) |> 
  mutate_at(
    vars(contains("date")),
    as_date
  ) |> 
  filter(
    date_game >= today()
  ) |> 
  rename(
    heima = home,
    gestir = away
  )

stigafjoldi_results <- bind_rows(
  read_csv(here("Basketball", "results", "next_games_post_kk.csv")) |> mutate(kyn = "kk"),
  read_csv(here("Basketball", "results", "next_games_post_kvk.csv")) |> mutate(kyn = "kvk")
) |> 
  inner_join(
    points,
    by = join_by(heima, gestir, kyn),
    relationship = "many-to-many"
  ) |> 
  mutate(
    limit2 = limit
  ) |> 
  summarise(
    p_over = mean(total_goals > limit2),
    p_under = 1 - p_over,
    .by = c(heima, limit, kyn, gestir, booker, o_over, o_under, limit)
  ) |> 
  pivot_longer(
    c(o_over:p_under, -limit),
    names_to = c("type", "outcome"),
    names_sep = "_"
  ) |>
  pivot_wider(names_from = type) |> 
  mutate(
    kelly = get_kelly(p, o),
    .by = c(booker, kyn, gestir, heima)
  ) |> 
  filter(
    kelly == max(kelly),
    .by = c(heima, gestir, outcome, kyn)
  ) |> 
  mutate(
    ev = round(p * (o - 1) - (1 - p), 2),
    kelly = kelly * kelly_frac,
    bet_amount = round(kelly * cur_pool, bet_digits),
    kelly = round(kelly, 2),
    pred = round(p, 3),
    p_o = round(1/o, 3),
    text = glue::glue("€={bet_amount} (f={kelly})[ev={ev},p={p},o={p_o}=1/{o}]"),
    text = if_else(kelly == 0, "", text)
  ) |> 
  select(booker, kyn, limit, heima, gestir, limit, outcome, text) |> 
  pivot_wider(
    names_from = outcome, 
    values_from = text,
    values_fill = ""
  ) |> 
  select(booker, kyn, limit, heima, gestir,limit, over, under) 


nidurstada_results
forgjof_results
stigafjoldi_results
