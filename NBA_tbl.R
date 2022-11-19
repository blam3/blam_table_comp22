library('tidyverse')
library('gt')
library('gtExtras')
library('BasketballAnalyzeR')

bball_df <- read_csv("modern_RAPTOR_by_team.csv")

## Rankings according to the Ringer.com for the 21-22 season that made the playoffs: 
# https://www.theringer.com/nba/2022/3/9/22967447/nba-top-25-players-2021-22-season
# 10. Jimmy Butler
# 9 Chris Paul
# 8. Demar DeRozan
# 7. Ja Morant
# 6. Luka Doncic
# 5. Stephen Curry
# 4. Kevin Durant
# 3. Joel Embiid
# 2. Giannis Antetokuompo
# 1. Nikola Jokic

bball_tbl <- bball_df %>% 
  filter(season == 2022) %>% 
  filter(player_name %in% c("Jimmy Butler", "Chris Paul", 
                            "DeMar DeRozan", "Ja Morant", "Luka Doncic",
                            "Stephen Curry", "Kevin Durant", 
                            "Joel Embiid", "Giannis Antetokounmpo",
                            "Nikola Jokic"))

bball_pre_df <- bball_tbl %>% 
  filter(season_type == "RS") %>% # Replace with your own file path
  mutate(logo = c("/Users/brendanlam/R Projects/Table_Contest_22/milwaukee.jpeg",
                  "/Users/brendanlam/R Projects/Table_Contest_22/miami.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/golden_state.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/chicago.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/dallas.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/brooklyn.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/philadelphia.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/denver.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/memphis.png",
                  "/Users/brendanlam/R Projects/Table_Contest_22/phoenix.png")) %>% 
  select(player_name, logo, season_type, poss, mp, war_reg_season, raptor_total, predator_total) %>% 
  dplyr::rename(reg_season = season_type)

bball_post_df <- bball_tbl %>% 
  filter(season_type == "PO") %>% 
  select(c(player_name, season_type, poss, mp, war_playoffs, raptor_total, predator_total)) %>% 
  dplyr::rename(post_season = season_type)

bball_cmb_tbl <- bball_pre_df %>% 
  left_join(bball_post_df, by = "player_name") %>% 
  select(!c(reg_season, post_season)) %>% 
  mutate(radial_plots = rep(NA, nrow(bball_pre_df)))

# Creating radial plots for each player

# Creating data frame for data the we will use for radial plots
radar_data <- bball_cmb_tbl %>% 
  select(war_reg_season, raptor_total.x, predator_total.x, war_playoffs, raptor_total.y,
         predator_total.y) %>% 
  dplyr::rename(WRS = war_reg_season, 
                RRS = raptor_total.x, 
                PRS  = predator_total.x, 
                WP = war_playoffs, 
                RP = raptor_total.y,
                PP = predator_total.y)

radial_plots <- list(rep(NA, 10))
for (i in 1:10) {
  radial_plots[i] <- radialprofile(data=radar_data[i,], std=FALSE)
}

# Create table
bball_cmb_tbl %>% 
  gt() %>% 
  tab_header(
    title = md("**NBA Player Stats**"),
    subtitle = md("Comparing performance during the regular and post=season for 2021-2022")
  ) %>% 
  # Renaming columns
  cols_label(
    player_name = "Name",
    logo = "Team",
    poss.x = "Possessions Played",
    mp.x = "MP",
    war_reg_season = "WAR",
    raptor_total.x = "RAPTOR",
    predator_total.x = "PREDATOR",
    poss.y = "Possessions Played",
    mp.y =  "MP",
    war_playoffs = "WAR",    
    raptor_total.y = "RAPTOR",
    predator_total.y = "PREDATOR",
    radial_plots = "Radial Plots"
  ) %>% 
  # Adding tab headers
  tab_spanner(
    label = "Regular Season",
    columns = c(poss.x, mp.x, war_reg_season, raptor_total.x, predator_total.x) ) %>% 
  tab_spanner(
    label = "Playoffs",
    columns = c(poss.y, mp.y, war_playoffs, raptor_total.y, predator_total.y) ) %>%
  # Centering text
  cols_align(
    align = "center",
    columns =  c(poss.x, war_reg_season, raptor_total.x, predator_total.x, war_playoffs, 
                 poss.y, raptor_total.y, predator_total.y)) %>% 
  # Rounding to one decimal place
  fmt_number(columns = c(war_reg_season, raptor_total.x, predator_total.x, poss.y, mp.y,
                         war_playoffs, raptor_total.y, predator_total.y), decimals = 1) %>% 
  # Regular Season WAR
  tab_style(
    style = list(
      cell_fill(color = "#00BE67"),
      cell_text(weight = "bold") ),
    locations = cells_body(
      columns = war_reg_season,
      rows = war_reg_season > war_playoffs) ) %>%
  # Regular Season RAPTOR
  tab_style(
    style = list(
      cell_fill(color = "#00BE67"),
      cell_text(weight = "bold") ),
    locations = cells_body(
      columns = raptor_total.x,
      rows = raptor_total.x > raptor_total.y) ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F8766D") ),
    locations = cells_body(
      columns = raptor_total.x,
      rows = raptor_total.x < raptor_total.y) ) %>%
  # Regular Season PREDATOR
  tab_style(
    style = list(
      cell_fill(color = "#00BE67"),
      cell_text(weight = "bold") ),
    locations = cells_body(
      columns = predator_total.x,
      rows = predator_total.x > predator_total.y) ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F8766D") ),
    locations = cells_body(
      columns = predator_total.x,
      rows = predator_total.x < predator_total.y) ) %>% 
  # Playoffs WAR
  tab_style(
    style = list(
      cell_fill(color = "#F8766D") ),
    locations = cells_body(
      columns = war_playoffs,
      rows = war_reg_season > war_playoffs) ) %>%
  # Playoffs RAPTOR
  tab_style(
    style = list(
      cell_fill(color = "#00BE67"),
      cell_text(weight = "bold") ),
    locations = cells_body(
      columns = raptor_total.y,
      rows = raptor_total.x < raptor_total.y) ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F8766D") ),
    locations = cells_body(
      columns = raptor_total.y,
      rows = raptor_total.x > raptor_total.y) ) %>%
  # Playoffs PREDATOR
  tab_style(
    style = list(
      cell_fill(color = "#00BE67"),
      cell_text(weight = "bold") ),
    locations = cells_body(
      columns = predator_total.y,
      rows = predator_total.x < predator_total.y) ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F8766D") ),
    locations = cells_body(
      columns = predator_total.y,
      rows = predator_total.x > predator_total.y) ) %>% 
  # Adding images
  gt_img_rows(columns = logo, img_source = "local", height = 30) %>%
  tab_options(data_row.padding = px(1)) %>% 
  # Adding radial/spider plots
  text_transform(
    locations = cells_body(columns = c(radial_plots)),
    fn = function(x) {
      map(radial_plots, ggplot_image, height = px(300))
    } ) %>% 
  # Adding footnotes
  tab_footnote(
    footnote = "Minutes Played",
    locations = cells_column_labels(columns = c(mp.x, mp.y)) ) %>% 
  tab_footnote(
    footnote = "Wins Above Replacement",
    locations = cells_column_labels(columns = c(war_reg_season, war_playoffs) )  ) %>%
  tab_footnote(
    footnote = "Points above average per 100 possessions added by player on both offense and defense, using both box and on-off components",
    locations = cells_column_labels(columns = c(raptor_total.x, raptor_total.y)) ) %>%
  tab_footnote(
    footnote = "Predictive points above average per 100 possessions added by player on both offense and defense",
    locations = cells_column_labels(columns = c(predator_total.x, predator_total.y)) ) %>%
  tab_footnote(
    footnote = "Radial plots where regular season and playoff metrics are on the opposite side of each other. Points closer to the edge of the circle indicate higher scores.",
    locations = cells_column_labels(columns = radial_plots) ) %>% 
  # Adding divider between Regular season and playoffs
  gt_add_divider(
    predator_total.x,
    sides = "right",
    color = "lightgrey",
    style = "solid",
    weight = px(2),
    include_labels = TRUE
  ) %>% 
  tab_source_note(source_note = "From https://github.com/fivethirtyeight/data/blob/master/nba-raptor/README.md")
