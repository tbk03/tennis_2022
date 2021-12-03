get_res <- function(year, file_folder, tour){

  # form file location string based on year
  file_name <- glue::glue("{tour}_matches_{year}.csv")
  file_location <- str_c(file_folder, file_name)

  # read in data from csv
  atp_res_year <- read_csv(file_location,
                           col_types = cols()) %>%  # this line suppresses output
    # when guessing column types
    janitor::clean_names() %>%

    # add tour identifier
    mutate(tour = tour)

  return(atp_res_year)
}

tidy_tennis_df <- function(tennis_res_df, tour){

  # tidy up data for match losers
  losers <- tennis_res_df %>%

    # drop columns containing match stats of the winner
    select(-c(starts_with("winner"), starts_with("w_"))) %>%

    # simplify column naming to enable row binding below
    rename_with(.fn = ~str_replace( .x, "loser_", ""),
                .cols = starts_with("loser")) %>%
    rename_with(.fn = ~str_replace( .x, "l_", ""),
                .cols = starts_with("l_")) %>%

    # rename for simplicity
    # rename(player_id = id) %>%

    # add result column and match_id
    mutate(result = "loser",
           match_id = 1:n(),
           match_id_tour = str_c(match_id, tour, sep = "_"),
           .after = name) %>%
    select(-match_id)


  # tidy up data for match winners
  winners <- tennis_res_df %>%

    # drop columns containing  match stats of the loser
    select(-c(starts_with("loser"), starts_with("l_"))) %>%

    # simplify column naming to enable row binding below
    rename_with(.fn = ~str_replace( .x, "winner_", ""),
                .cols = starts_with("winner")) %>%
    rename_with(.fn = ~str_replace( .x, "w_", ""),
                .cols = starts_with("w_")) %>%

    # rename for simplicity
    rename(player_id = id) %>%

    # add result column and match id
    mutate(result = "winner",
           match_id = 1:n(),
           match_id_tour = str_c(match_id, tour, sep = "_"),
           .after = name) %>%
    select(-match_id)

  # put winner and loser data back together to give a tidy format
  atp_res_tidy <- winners %>%
    bind_rows(losers) %>%
    arrange(tourney_id, match_num)

  # convert variable types where necessary
  atp_res_tidy <- atp_res_tidy %>%
    mutate(tourney_date = lubridate::ymd(tourney_date))

  # return the tidy dataframe
  return(atp_res_tidy)
}
