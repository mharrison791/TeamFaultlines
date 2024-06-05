library(worldfootballR)
library(tidyverse)
library(asw.cluster)

team_urls <- tm_league_team_urls(country_name = "England", start_year = 2023)

teams <- tm_squad_stats(team_url = team_urls)

#### Create Team ID
teams <- teams %>%
  group_by(team_name) %>% 
  mutate(teamid = cur_group_id())%>%
  ungroup()

playing_time <- teams %>%
  group_by(team_name)%>%
  slice(which.max(minutes_played))%>%
  select(team_name,minutes_played)%>%
  rename(MaxMins = minutes_played)%>%
  ungroup()

teams <- left_join(teams,playing_time)
teams <- teams %>%
  mutate(PctMins = round(minutes_played/MaxMins, digits = 2))

player_urls <- teams$player_url

joined_data_result <- data.frame()

for (i in player_urls){
  player_page <- tryCatch(xml2::read_html(i), error = function(e) NA)
  player_name <- player_page %>% rvest::html_nodes("div h1") %>% rvest::html_text()
  player_name <- gsub("#[[:digit:]]+ ", "", player_name) %>% stringr::str_squish()
  X1 <- player_page %>% rvest::html_nodes(".info-table__content--regular") %>% rvest::html_text() %>% stringr::str_squish()
  X2 <- player_page %>% rvest::html_nodes(".info-table__content--bold") %>% rvest::html_text() %>% stringr::str_squish()
  a <- cbind(X1, X2) %>% data.frame()
  a <- a %>% filter(X1=="Joined:" | X1=="Current club:")
  a$player_url <- i
  a <- a%>%
    pivot_wider(names_from = X1, values_from = X2)%>%
    mutate(years_with_team = str_sub(`Joined:`,-4,-1))%>%
    mutate(Current_club = gsub("U21", "", .data[["Current club:"]]))
  a$years_with_team <- as.numeric(a$years_with_team)
  a$years_with_team <- 2023-a$years_with_team
  a <- a%>%
    select(player_url,years_with_team,Current_club)
  joined_data_result <- rbind(joined_data_result,a)
  print(i)
}



joined_df <- left_join(teams,joined_data_result, join_by("player_url"=="player_url"))

joined_df <- joined_df %>%
  filter(team_name==Current_club)%>%
  distinct(player_url,.keep_all = TRUE)

#### Create Groupings

joined_df <- joined_df %>%
  mutate(agegroup = case_when(player_age >= 16 & player_age <= 19 ~ '16-19',
                              player_age >= 20 & player_age <= 23 ~ '20-23',
                              player_age >= 24 & player_age <= 27 ~ '24-27',
                              player_age >= 28 & player_age <=30 ~ '28-30',
                              player_age >= 31 & player_age <=34 ~ '31-34',
                              player_age > 34 ~ '34+'))%>%
  mutate(tenuregroup = case_when(years_with_team < 1 ~ '0',
                                 years_with_team >= 1 & years_with_team <= 2 ~ '1-2',
                                 years_with_team >= 3 & years_with_team <= 5 ~ '3-5',
                                 years_with_team >= 5 & years_with_team <=8 ~ '5-8',
                                 years_with_team >8 ~ '9+'))%>%
  mutate(playinggroup = case_when(appearances < 5 ~ '0-5',
                                  appearances >= 6 & appearances <= 10 ~ '6-10',
                                  appearances >= 11 & appearances <= 20 ~ '11-20',
                                  appearances >= 21 & appearances <=30 ~ '21-30',
                                  appearances >30 ~ '31+'))%>%
  mutate(playingtimegroup = case_when(PctMins < 0.05 ~ '0-5',
                                  PctMins >= 0.06 & PctMins <= 0.2 ~ '6-20',
                                  PctMins >= 0.21 & PctMins <= 0.40 ~ '21-40',
                                  PctMins >= 0.41 & PctMins <= 0.60 ~ '41-60',
                                  PctMins >= 0.61 & PctMins <= 0.80 ~ '61-80',
                                  PctMins > 0.80 ~ '81+'))

teams_sub <- joined_df %>%
  select(teamid,agegroup,playingtimegroup,tenuregroup)

my_attr <- c("nominal", "nominal", "nominal")

teams_sub <- as.data.frame(teams_sub)

my_ASW <- faultlines(data = teams_sub,
                     group.par = "teamid",
                     attr.type = my_attr, 
                     rescale = "sd",
                     method = "asw")

summary <- summary(my_ASW)$long

joined_df <- joined_df%>%
  arrange(teamid)
teamdata <- data.frame(joined_df, summary)

write.csv(teamdata,"subgroups_teams.csv")
