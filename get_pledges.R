# load show urls
library(tidyverse)
library(duckplyr)
library(rvest)
# load show urls
# show_urls <- read_parquet_duckdb("data/playlistURLs.parquet")
dj_key <- read_parquet_duckdb("data/djKey.parquet")

source("func_get_time_slots.R")
time_slots <- get_time_slots()
saveRDS(time_slots,"data/time_slots.rds")

time_slots <- readRDS("data/time_slots.rds")
base_url = "https://www.wfmu.org/playlists"

latest_shows <- dj_key |> 
  filter(onSched == TRUE) |> 
  select(DJ,ShowName,Channel)

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
  purrr::map(purrr::possibly(get_pledge_info, otherwise = NULL), .progress = "Fetching pledge info") |>
  purrr::compact() |>
  dplyr::bind_rows()


all_pledges <- all_pledges %>%
  # set goal == progress for shows with no goal
  # mutate(goal_amount = ifelse(goal_amount == 0, progress_amount, goal_amount)) %>%
  mutate(pledge_rank = dense_rank(desc(progress_amount))) |> 
  left_join(select(dj_key,DJ,Channel),by = "DJ") |> 
  left_join(time_slots,by = "DJ")

# save all pledges
saveRDS(all_pledges, "data/pledge_info.rds")

# =================================================================================
all_pledges <- readRDS("data/pledge_info.rds")

all_pledges |>
  filter(goal_amount > 0) |>
  mutate(
    pct = progress_amount / goal_amount,
    program_name = fct_reorder(program_name, pct)
  ) |>
  filter(pct <= quantile(pct, 0.99)) |>
  ggplot(aes(x = pct, y = program_name, fill = pledge_rank)) +
  geom_col() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_viridis_c(direction = -1) +
  annotate(
    "rect",
    xmin = 0, xmax = 1.81, ymin = 59.5, ymax = 60.5,
    fill = NA, color = "black", linewidth = 0.8
  ) +
  annotate(
    "text",
    x = .1, y = 60,
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
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

all_pledges |>
  collect() |>
  filter(!is.na(Channel), Channel != "Archive",program_name != "Wake") |>
  mutate(program_name = fct_reorder(program_name, progress_amount)) |>
  ggplot(aes(x = progress_amount, y = program_name, fill = Channel)) +
  geom_col() +
  scale_x_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # add a vertical line at $1000
  geom_vline(xintercept = 1000, linetype = "dashed", color = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ Channel, scales = "free", ncol = 2) +
  labs(
    title = "WFMU Marathon 2026: Amount Raised by Program by Channel",
    subtitle = "Dashed line = $1000 | Excludes Archives and Wake Show",
    x = "Amount Raised ($)",
    y = NULL,
    fill = "Channel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# same chart but progress amount / pledge count
all_pledges |>
  collect() |>
  filter(!is.na(Channel), Channel != "Archive") |>
  mutate(program_name = fct_reorder(program_name, progress_amount / pledge_count)) |>
  ggplot(aes(x = progress_amount / pledge_count, y = program_name, fill = Channel)) +
  geom_col() +
  scale_x_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # add a vertical line at $25
  geom_vline(xintercept = 25, linetype = "dashed", color = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ Channel, scales = "free", ncol = 2) +
  labs(
    title = "WFMU Marathon 2026: Average Pledge Amount by Program by Channel",
    subtitle = "Dashed line = $25 | Excludes Archives",
    x = "Average Pledge Amount ($)",
    y = NULL,
    fill = "Channel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# show a histogram of pledge counts
all_pledges |>
  collect() |>
  filter(!is.na(Channel), Channel != "Archive") |>
  ggplot(aes(x = pledge_count, fill = Channel)) +
  geom_histogram(binwidth = 10, color = "black") +
  scale_x_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~ Channel, scales = "free", ncol = 2) +
  labs(
    title = "WFMU Marathon 2026: Distribution of Pledge Counts by Channel",
    subtitle = "Excludes Archives",
    x = "Number of Pledges",
    y = "Count of Programs",
    fill = "Channel"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
