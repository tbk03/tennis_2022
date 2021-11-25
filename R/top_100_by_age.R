library(tidyverse)
library(lubridate)
library(ggbeeswarm)
library(cjmr)

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
# Check for NAs
# ******************************************************************************
list(atp_2021_rankings, atp_players, wta_2021_rankings, wta_players) %>%
  map(visdat::vis_miss)

# ******************************************************************************
# Process data
# ******************************************************************************
atp_top_100 <- atp_2021_rankings %>%

  # focus on top 100 for most recent date (assume that is year end)
  filter(ranking_date == max(ranking_date) &
           rank <= 100) %>%
  left_join(atp_players) %>% # add player details

  # calculate age
  mutate(age = dob %--% ymd("20211125") / years(1),
         age = floor(age),
         tour = "atp")

wta_top_100 <- wta_2021_rankings %>%

  # focus on top 100 for most recent date (assume that is year end)
  filter(ranking_date == max(ranking_date) &
           rank <= 100) %>%
  left_join(wta_players) %>% # add player details

  # calculate age
  mutate(age = dob %--% ymd("20211125") / years(1),
         age = floor(age),
         tour = "wta")

both_top_100 <- atp_top_100 %>%
  bind_rows(wta_top_100) %>%
  mutate(rank_group = cut(rank, breaks = c(0,10,20,50,101)))

# ******************************************************************************
# Produce plot
# ******************************************************************************

custom_pallete <- c("#E41315", "#2B96CC", "#5BDCDB", "#E5E5E5")
ranking_labels <- c("Top 10", "11-20", "21-50", "51-100")

ggplot() +
  geom_beeswarm(data = both_top_100,
               mapping = aes(tour, age,
                             colour = rank_group),
               size = 3, cex = 3.5) +

  scale_colour_manual(values = custom_pallete,
                      labels = ranking_labels) +
  scale_x_discrete(labels = c("Men", "Women")) +
  scale_y_continuous(breaks = seq(16,44,2)) +

  theme_cjmr_explanatory() +
  theme(legend.position = "right",
        plot.background = element_rect(fill = "#FDFCFC"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        legend.title = element_text()) +
  #guides(color=guide_legend(title="title")) +
  labs(colour = "Ranking") +
  coord_flip()

ggsave("images/top_100_by_age.svg", units = "mm", width = 250, height = 110)


# ******************************************************************************
# Export data for review in Excel
# ******************************************************************************
both_top_100_out <- both_top_100 %>%
  mutate(full_name = str_c(name_first, name_last, sep = " ")) %>%
  select(-tour)

write_csv(both_top_100_out, "data_out/top_100_player_end_2021.csv")

