library(tidyverse)
library(lubridate)
library(cjmr)
library(patchwork)
library(tidytext)

source("R/tennis_helper_functions.R")


# ******************************************************************************
# import the data
# ******************************************************************************

atp_path <- "../../datasets/tennis_atp/"
wta_path <- "../../datasets/tennis_wta/"

# 2021 rankings table
atp_2021_rankings <- read_csv(str_c(atp_path, "atp_rankings_current.csv")) %>%
  janitor::clean_names() %>%
  rename(player_id = player) %>%
  mutate(ranking_date = lubridate::ymd(ranking_date))

wta_2021_rankings <- read_csv(str_c(wta_path, "wta_rankings_current.csv")) %>%
  janitor::clean_names() %>%
  rename(player_id = player) %>%
  mutate(ranking_date = lubridate::ymd(ranking_date))

# all players table
atp_players <- read_csv(str_c(atp_path, "atp_players.csv")) %>%
  janitor::clean_names() %>%
  mutate(dob = ymd(dob))

wta_players <- read_csv(str_c(wta_path, "wta_players.csv")) %>%
  janitor::clean_names() %>%
  mutate(dob = ymd(dob))


# ******************************************************************************
# process the data
# ******************************************************************************

start_end_2021_rankings <- atp_2021_rankings %>%

  # focus on just the start and end of year rankings
  mutate(year = year(ranking_date)) %>%
  filter(year == 2021) %>%
  filter(ranking_date == min(ranking_date) |
           ranking_date == max(ranking_date)) %>%

  # add in player details
  left_join(atp_players) %>%
  mutate(name = str_c(name_first, name_last, sep = " ")) %>%
  select(ranking_date, rank, player_id, name) %>%
  distinct() %>%

  # calculate change in ranking
  pivot_wider(names_from = ranking_date, values_from = rank) %>%
  unnest(cols = c(`2021-01-04`, `2021-11-22`)) %>%
  mutate(change_in_ranking = `2021-01-04` - `2021-11-22`)

end_of_year_top_100 <- start_end_2021_rankings %>%
  filter(`2021-11-22`<= 100)

start_of_year_top_100 <- start_end_2021_rankings %>%
  filter(`2021-01-04`<= 100)

# ******************************************************************************
# plot ranking stripes
# ******************************************************************************

s1 <- end_of_year_top_100 %>%
  ggplot(aes(`2021-11-22`, 1, fill = change_in_ranking)) +
  geom_col() +
  scale_fill_gradient2(low = "#999999", mid = "#ffffff", high = "#ef8a62",
                       name = "Change in ranking over 2021") +
  scale_x_reverse(expand = c(0,0), breaks = c(1,10,20,30,40,50,60,70,80,90,100)) +
  labs(x = "End of year ranking") +
  theme_cjmr_explanatory() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

s1


# ******************************************************************************
# plot age stripes
# ******************************************************************************

dobs <- atp_players %>%
  select(player_id, dob)

s2 <- end_of_year_top_100 %>%
  left_join(dobs) %>%

  # calculate age
  mutate(age = dob %--% ymd("20211125") / years(1),
         age = floor(age)) %>%

  # plot
  ggplot(aes(`2021-11-22`, 1, fill = age)) +
  geom_col() +

  scale_fill_gradient(low = "#ffffff", high = "blue") +

  scale_x_reverse(expand = c(0,0), breaks = c(1,10,20,30,40,50,60,70,80,90,100)) +

  labs(x = "End of year ranking") +
  theme_cjmr_explanatory() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

s2

s1 / s2


# ******************************************************************************
# plot breaking into top 100
# ******************************************************************************
top_100_breakers <- end_of_year_top_100 %>%
  filter(`2021-01-04` >= 100) %>%
  mutate(emphasis_group = case_when(
    `2021-01-04`>= 300 ~ "start out top 300",
    `2021-11-22`<= 50 ~ "break straight into top 50",
    TRUE ~ "de-emphasise"
  )) %>%
  mutate(lab = str_c(name, " ", "(", `2021-11-22`, ")"))


breakers_highlight <- top_100_breakers %>%
  filter(`2021-11-22` <= 50 |
           `2021-01-04` >= 300)


top_100_breakers_plot <- top_100_breakers %>%
  filter(emphasis_group == "de-emphasise") %>%
  pivot_longer(cols = c(`2021-01-04`, `2021-11-22`),
               names_to = "date",
               values_to = "ranking")


breakers_highlight_plot <-  breakers_highlight %>%

  pivot_longer(cols = c(`2021-01-04`, `2021-11-22`),
               names_to = "date",
               values_to = "ranking")


p1 <- ggplot(mapping = aes(date, ranking)) +

  geom_line(data = top_100_breakers_plot,
            aes(group = player_id), colour = "grey80") +

  geom_point(data = breakers_highlight_plot,
             mapping = aes(colour = emphasis_group)) +

  geom_line(data = breakers_highlight_plot,
            mapping = aes(colour = emphasis_group, group = player_id)) +

  ggrepel::geom_text_repel(data = filter(breakers_highlight_plot,
                                         date == "2021-11-22"),
                           mapping = aes(label = lab,
                                         colour = emphasis_group)) +

  scale_y_reverse(breaks = c(1,20, 50,100,150,200,300), limits = c(350, 1)) +

  scale_x_discrete(expand = c(0.1,0),
                   labels = c("start of year", "end of year")) +
  #scale_colour_gradient(low = "#ffffff", high = "#ef8a62") +

  labs(x = NULL) +

  theme_cjmr_explanatory() +
  theme(legend.position = "none")

p1

ggsave("images/top_100_breakers.svg", units = "mm", width = 250, height = 110)

p1_a <- ggplot(top_100_breakers, aes(change_in_ranking,
                             fill = emphasis_group,
                             colour = emphasis_group)) +
  geom_dotplot(alpha = 0.5, stackdir = "center", binwidth = 10) +
  scale_y_continuous(NULL, breaks = NULL) +
  theme_cjmr_explanatory() +
  theme(legend.position = "none")

p1 / p1_a


# ******************************************************************************
# plot fallers from top 100
# ******************************************************************************
top_100_fall_out <- start_of_year_top_100 %>%
  filter(`2021-11-22` > 100) %>%
  mutate(emphasis_group = case_when(
    `2021-11-22` > 150 ~ "fell outside top 150",
    `2021-01-04` < 50 ~ "fell from top 50",
    TRUE ~ "de-emphasise"
  )) %>%

  mutate(lab = str_c(name, " ", "(", `2021-11-22`, ")")) %>%
  pivot_longer(cols = c(`2021-01-04`, `2021-11-22`),
               names_to = "date",
               values_to = "ranking")

top_100_fall_out_highlight <- top_100_fall_out %>%
  filter(emphasis_group!= "de-emphasise")

top_100_fall_out_background <- top_100_fall_out %>%
  filter(emphasis_group == "de-emphasise")




p2 <- ggplot(mapping = aes(date, ranking)) +

  # geom_point(data = breakers_highlight_plot,
  #            mapping = aes(colour = emphasis_group)) +
  #
  # ggrepel::geom_text_repel(data = filter(breakers_highlight_plot,
  #                                        date == "2021-11-22"),
  #                          mapping = aes(label = name,
  #                                        colour = emphasis_group)) +

  # background line
  geom_line(data = top_100_fall_out_background,
            aes(group = player_id), colour = "grey80") +

  # highlighted lines
  geom_point(data = top_100_fall_out_highlight,
             mapping = aes(colour = emphasis_group)) +

  geom_line(data = top_100_fall_out_highlight,
            mapping = aes(colour = emphasis_group, group = player_id)) +

  ggrepel::geom_text_repel(data = filter(top_100_fall_out_highlight,
                                         date == "2021-11-22"),
                           mapping = aes(label = lab,
                                         colour = emphasis_group)) +

  scale_y_reverse(breaks = c(1,20, 50,100,150,200,300), limits = c(350, 1)) +

  scale_x_discrete(expand = c(0.1,0),
                   labels = c("start of year", "end of year")) +
  #scale_colour_gradient(low = "#ffffff", high = "#ef8a62") +

  labs(x = NULL) +

  theme_cjmr_explanatory() +
  theme(legend.position = "none")

p2

p1 + p2




# ******************************************************************************
# plot risers within the top 100
# ******************************************************************************

# identify players who start in the top 100 and then rise
start_t100_and_rise <- start_of_year_top_100 %>%
  filter(`2021-01-04` <= 100 &
           change_in_ranking > 0)

rise_more_than_20 <- start_t100_and_rise %>%
  filter(change_in_ranking >= 20)

start_t100_and_rise_ids <- start_t100_and_rise$player_id

plus_20_risers <- atp_2021_rankings %>%

  filter(player_id %in% rise_more_than_20$player_id) %>%
  left_join(select(atp_players, player_id, name_first, name_last)) %>%
  #mutate(name = str_c(name_first, name_last, sep = " ")) %>%
  left_join(end_of_year_top_100) %>%
  mutate(label_text = str_c("rose ", change_in_ranking, " places \n over 2021"))


start_t100_and_rise_rank_hist <-  atp_2021_rankings %>%

  filter(player_id %in% start_t100_and_rise_ids) %>%
  rename(player_id_dummy = player_id)


# work out order of facets
ordered_facet_levels <- plus_20_risers %>%
  select(name, change_in_ranking, `2021-11-22`) %>%
  distinct() %>%
  arrange(`2021-11-22`) %>%
  .$name

plus_20_risers$name <- factor(plus_20_risers$name,
                              levels = ordered_facet_levels)

# indentify key tournament successes for plotting
#read in and tidy results csvs for 2021
key_res <- get_res(2021, atp_path, "atp") %>%

  tidy_tennis_df("atp") %>%
  filter(player_id %in% rise_more_than_20$player_id) %>%

  # identify tournament wins, finals and semi-finals
  filter(result == "winner") %>%
  select(player_id, name, tourney_date, round, tourney_name) %>%
  arrange(name, tourney_date) %>%
  mutate(result = case_when(
    round == "F" ~ 1,
    round == "SF" ~ 2,
    round == "QF" ~ 3.5,
    TRUE ~ -1
  )) %>%
  filter(result > 0) %>%
  group_by(player_id, name, tourney_date, tourney_name) %>%
  summarise(result = min(result)) %>%
  ungroup() %>%
  mutate(result_text = case_when(
    result == 1 ~ "Winner",
    result == 2 ~ "Finalist",
    result == 3.5 ~ "Semi-finalist",
    TRUE ~ "Other"
  )) %>%

  # add in ranking at time of result
  left_join(start_t100_and_rise_rank_hist,
            by = c("player_id" = "player_id_dummy",
                   "tourney_date" = "ranking_date"))

key_res$name <- factor(key_res$name,
                              levels = ordered_facet_levels)

p3 <- ggplot(mapping = aes(ranking_date, rank)) +

  # background lines
  # geom_line(data = start_t100_and_rise_rank_hist, colour = "grey80", alpha = 0.2,
  #           mapping = aes(group = player_id_dummy)) +

  # player of interest line
  geom_line(data = plus_20_risers, mapping = aes(group = player_id),
            size = 1, colour = "green") +

  # add start and end of year rankings
  geom_text(data = plus_20_risers,
            mapping = aes(x = ymd("2021-06-01"), y = 100, label = change_in_ranking)) +
  # geom_text(data = plus_20_risers,
  #           mapping = aes(x = ymd("2021-01-01"), y = 100, label = `2021-01-04`)) +
  # geom_text(data = plus_20_risers,
  #           mapping = aes(x = ymd("2021-11-22"), y = 100, label = `2021-11-22`)) +

  # add key tournament successes
  geom_point(data = key_res,
             mapping = aes(tourney_date, rank,
                           colour = result_text),
             size = 3) +

  scale_y_reverse(breaks = c(1,10, 20, 50,100), limits = c(120, 1)) +
  scale_color_manual(values = c("#2F7ACA", "#61CDC9","#C33D50")) +
  #scale_y_reverse(breaks = c(), limits = c(120, 1)) +
  theme_cjmr_explanatory() +
  theme(strip.background = element_blank(),
        legend.position = "none") +
  facet_wrap(~name, ncol = 5) +
  labs(x = NULL)

p3

ggsave("images/top_100_climbers.svg", units = "mm", width = 250, height = 110)

# write out key results for inpsection and to help annotated the plot

write_csv(key_res, "data_out/big_wins.csv")

# end_of_year_top_100 %>%
#   mutate(climbers = change_in_ranking > 0,
#          started_in_top_100 = `2021-01-04` <= 100) %>%


# ggplot(end_of_year_top_100, aes(change_in_ranking)) +
#   geom_histogram()
#
# ggplot(start_of_year_top_100, aes(change_in_ranking)) +
#   geom_histogram()









