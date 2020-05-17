library(tidyverse)
library(nflfastR)

retrieve_nfl_pbp_data <- function(start_season, end_season) {
    # Source: https://github.com/guga31bb/nflfastR-data
    seasons <- start_season:end_season
    pbp <- purrr::map_df(seasons, function(x) {
        readRDS(
            url(
                glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
            )
        )
    })
    return(pbp)
}


convert_time_of_day_to_seconds <- function(time_of_day) {
    seconds <- strtoi(as.difftime(time_of_day, format = "%H:%M:%S", units = "secs"))
    return(seconds)
}

get_average_play_duration <- function(play_type) {
    if (is.na(play_type)) {
        return(0)
    } else if (play_type == "run") {
        return(4)
    } else if (play_type == "pass") {
        return(8)
    } else {
        return(0)
    }
}


eval_bool <- function(x) {
    if (is.na(x)) {
        return(0)
    } else {
        return(x)
    }
}

add_play_time_diff <- function(df) {
    df %>%
        group_by(game_id) %>%
        arrange(play_id, .by_group = TRUE) %>%
        mutate(time_in_seconds = convert_time_of_day_to_seconds(time_of_day)) %>%
        mutate(play_time_diff = time_in_seconds - lag(time_in_seconds)) %>%
        ungroup()
}

add_stoppage_column <- function(df) {
    df %>%
        add_play_time_diff() %>%
        mutate(after_stoppage = if_else(is.na(play_time_diff) | play_time_diff > 75, TRUE, FALSE)) %>%
        mutate(after_stoppage = as.factor(after_stoppage))
}

filter_high_leverage_plays <- function(df) {
    df %>%
        mutate(
            important_down = (down == 3 | down == 4),
            under_2 = (half_seconds_remaining <= 120),
            garbage_time = (wp <= 0.1 | wp >= 0.9),
            redzone = (yardline_100 <= 20)
        ) %>%
        filter(!garbage_time) %>%
        filter(important_down | under_2 | redzone)
}


plot_epa_by_team <- function(offense_df, defense_df, off_epa_col, def_epa_col, xlabel, ylabel, title) {
    library(ggimage)
    logos <- teams_colors_logos %>% select(team_abbr, team_logo_espn)

    offense_df %>%
        inner_join(defense_df, by = c("posteam" = "defteam")) %>%
        inner_join(logos, by = c("posteam" = "team_abbr")) %>%
        ggplot(aes(x = off_epa_col, y = def_epa_col)) +
        #geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3), alpha = .2) +
        geom_hline(aes(yintercept = mean(off_epa_col)), color = "red", linetype = "dashed") +
        geom_vline(aes(xintercept = mean(def_epa_col)), color = "red", linetype = "dashed") +
        geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
        labs(
            x = xlabel,
            y = ylabel,
            caption = "Data: @nflfastR | EPA model: @nflscrapR",
            title = title
        ) +
        theme_bw() +
        theme(
            aspect.ratio = 9 / 16,
            plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
        ) +
        scale_y_reverse()
}

plot_epa_by_coach <- function(offense_df, defense_df, off_epa_col, def_epa_col, xlabel, ylabel, title) {
    library(ggrepel)

    offense_df %>%
        inner_join(defense_df, by = c("off_coach" = "def_coach")) %>%
        ggplot(aes(x = off_epa_col, y = def_epa_col)) +
        geom_hline(aes(yintercept = mean(off_epa_col)), color = "red", linetype = "dashed") +
        geom_vline(aes(xintercept = mean(def_epa_col)), color = "red", linetype = "dashed") +
        geom_point(color = "black", alpha=1/4) +
        geom_text_repel(aes(label=off_coach),
                        force=1, point.padding=0,
                        segment.size=0.1) +
        labs(
            x = xlabel,
            y = ylabel,
            caption = "Data: @nflfastR | EPA model: @nflscrapR",
            title = title
        ) +
        theme_bw() +
        theme(
            aspect.ratio = 9 / 16,
            plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
        ) +
        scale_y_reverse()
}
