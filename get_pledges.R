# get $ pledges for each show
library(tidyverse)
library(duckplyr)
# load show urls
show_urls <- read_parquet_duckdb("data/playlistURLs.parquet")

base_url = "https://www.wfmu.org/playlists"

latest_shows <- show_urls |>
  dplyr::group_by(DJ) |>
  dplyr::slice_max(AirDate, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(url = paste0(base_url, "/", show_id)) |>
  dplyr::select(DJ, AirDate, show_id, url) |>
  dplyr::arrange(DJ)

get_pledge_info <- function(dj_id) {
  url <- paste0(
    "https://pledge.wfmu.org/static/progress/microgoal-",
    dj_id,
    ".json"
  )
  resp <- httr2::request(url) |>
    httr2::req_perform()
  httr2::resp_body_json(resp) |>
    tibble::as_tibble() |>
    dplyr::mutate(DJ = dj_id, .before = 1) |>
    dplyr::relocate(program_name, .after = DJ)
}

# get pledge info for all DJs
all_pledges <- latest_shows$DJ |>
  purrr::map(purrr::possibly(get_pledge_info, otherwise = NULL)) |>
  purrr::compact() |>
  dplyr::bind_rows()
# 1 = highest progress; ties get same rank, no gaps
library(dplyr)

all_pledges <- all_pledges %>%
  # set goal == progress for shows with no goal
  mutate(goal_amount = ifelse(goal_amount == 0, progress_amount, goal_amount)) %>%
  mutate(progress_rank = dense_rank(desc(progress_amount)))


# save all pledges
saveRDS(all_pledges, "data/pledge_info.rds")
all_pledges <- readRDS("data/pledge_info.rds")

all_pledges |>
  filter(goal_amount > 0) |>
  mutate(
    pct = progress_amount / goal_amount,
    program_name = fct_reorder(program_name, pct)
  ) |>
  filter(pct <= quantile(pct, 0.99)) |>
  ggplot(aes(x = pct, y = program_name, fill = progress_rank)) +
  geom_col() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_viridis_c(direction = -1) +
  annotate(
    "rect",
    xmin = 0, xmax = 1.82, ymin = 60.5, ymax = 61.5,
    fill = NA, color = "black", linewidth = 0.8
  ) +
  annotate(
    "text",
    x = .1, y = 61,
    label = "Where the Action Is!",
    hjust = 0, size = 5, color = "black"
  ) +
  labs(
    title = "WFMU Marathon 2026: Amount Raised as % of Goal",
    subtitle = "Dashed line = 100% goal | Suspect data (top 1%) excluded",
    x = "% of Goal Raised",
    y = NULL,
    fill = "Dollar Rank"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
