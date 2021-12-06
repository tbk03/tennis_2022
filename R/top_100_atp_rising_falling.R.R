library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(cjmr)

# ******************************************************************************
# import the data
# ******************************************************************************

atp_path <- "../../datasets/tennis_atp/"

# 2021 rankings table
atp_2021_rankings <- read_csv(str_c(atp_path, "atp_rankings_current.csv")) %>%
  janitor::clean_names() %>%
  rename(player_id = player) %>%
  mutate(ranking_date = lubridate::ymd(ranking_date))

# all players table
atp_players <- read_csv(str_c(atp_path, "atp_players.csv")) %>%
  janitor::clean_names() %>%
  mutate(dob = ymd(dob))




# ******************************************************************************
# Process data
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
  filter(`2021-11-22`<= 100) %>%
  mutate(change_binned = cut(change_in_ranking, c(-500,-10,-1,0,10, 500))) %>%
  mutate(ranking_binned = cut(`2021-11-22`,
                              c(1, 11, 21, 31, 51, 100)))


# ******************************************************************************
# Produce plot
# ******************************************************************************

s1 <- end_of_year_top_100 %>%
  ggplot(aes(`2021-11-22`, 1,
             fill = change_binned)) +
  geom_col() +
  # scale_fill_gradient2(low = "#999999", mid = "#ffffff", high = "#ef8a62",
  #                      name = "Change in ranking over 2021") +
  scale_x_reverse(expand = c(0,0), breaks = c(1,10,20,30,40,50,60,70,80,90,100)) +
  scale_fill_manual(values = c("grey70", "grey90", "white","#577298", "#0C336C"))+
  labs(x = "End of year ranking") +
  theme_cjmr_explanatory() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank())

s1

# custom_pallete <- c("#E41315", "#2B96CC", "#5BDCDB", "#E5E5E5")
# ranking_labels <- c("Top 10", "11-20", "21-50", "51-100")
#
# ggplot() +
#   geom_beeswarm(data = end_of_year_top_100,
#                 mapping = aes(x = `2021-11-22`, y = 1,
#                               colour= change_binned),
#                 size = 5, cex = 5, groupOnX = TRUE) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_cjmr_explanatory() +
#   theme(axis.text.y = element_blank(),
#         panel.grid.major.y = element_blank())
#
# ggsave("images/top_100_by_rank_change.svg", units = "mm", width = 250, height = 110)
#
#
# ggplot() +
#   geom_beeswarm(data = both_top_100,
#                mapping = aes(tour, age,
#                              colour = rank_group),
#                size = 3, cex = 3.5) +
#
#   scale_colour_manual(values = custom_pallete,
#                       labels = ranking_labels) +
#   scale_x_discrete(labels = c("Men", "Women")) +
#   scale_y_continuous(breaks = seq(16,44,2)) +
#
#   theme_cjmr_explanatory() +
#   theme(legend.position = "right",
#         plot.background = element_rect(fill = "#FDFCFC"),
#         panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(),
#         legend.title = element_text()) +
#   #guides(color=guide_legend(title="title")) +
#   labs(colour = "Ranking") +
#   coord_flip()
#
# ggsave("images/top_100_by_age.svg", units = "mm", width = 250, height = 110)


# ******************************************************************************
# Export data for review in Excel
# ******************************************************************************
both_top_100_out <- both_top_100 %>%
  mutate(full_name = str_c(name_first, name_last, sep = " ")) %>%
  select(-tour)

write_csv(both_top_100_out, "data_out/top_100_player_end_2021.csv")

