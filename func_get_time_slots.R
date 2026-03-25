# Function Get Time Slots
# Scrapes the time slot for each on-schedule DJ from their WFMU playlist page
# Returns a data frame with columns: DJ, day, start_time, end_time, duration


pause = 0.25
get_time_slots <- function() {
  djKey <- arrow::read_parquet("data/djKey.parquet") |>
    filter(onSched == TRUE)

  base_url <- "https://wfmu.org/playlists"

  # regex to capture day, start, end, timezone from the bold time slot text
  time_pattern <- paste0(
    "(Monday-Friday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)",
    "\\s+",
    "(\\d{1,2}(?::\\d{2})?(?:am|pm)?|Noon|Midnight)",
    "\\s*-\\s*",
    "(\\d{1,2}(?::\\d{2})?(?:am|pm)?|Noon|Midnight)",
    "\\s*\\([A-Z]+\\)"
  )

  # helper to convert time strings like "3pm", "Noon", "Midnight", "9am", "3:01" to hours
  parse_hour <- function(time_str, am_pm_hint = "am") {
    time_str <- str_trim(time_str)
    if (time_str == "Noon") return(12)
    if (time_str == "Midnight") return(0)

    has_am <- str_detect(time_str, "am$")
    has_pm <- str_detect(time_str, "pm$")
    clean <- str_remove(time_str, "(am|pm)$")

    parts <- str_split(clean, ":")[[1]]
    hour <- as.numeric(parts[1])
    minutes <- if (length(parts) > 1) as.numeric(parts[2]) / 60 else 0

    if (has_pm && hour != 12) {
      hour <- hour + 12
    } else if (has_am && hour == 12) {
      hour <- 0
    } else if (!has_am && !has_pm) {
      # inherit am/pm from the end time hint
      if (am_pm_hint == "pm" && hour != 12) hour <- hour + 12
      if (am_pm_hint == "am" && hour == 12) hour <- 0
    }
    return(hour + minutes)
  }

  scrape_one <- function(dj_code) {
    url <- paste0(base_url, "/", dj_code)
    doc <- safe_get_html(url)
    if (is.null(doc)) return(NULL)

    everything_div <- doc |> html_element("div.everything")
    if (is.na(everything_div)) return(NULL)

    text <- html_text2(everything_div)
    match <- str_match(text, time_pattern)
    if (is.na(match[1])) return(NULL)
    # show progress
    print(djcode,match[1])

    day_str <- match[2]
    start_str <- match[3]
    end_str <- match[4]

    # determine am/pm context from the end time (which always has am/pm or is Noon/Midnight)
    end_ampm <- case_when(
      end_str == "Noon" ~ "pm",
      end_str == "Midnight" ~ "am",
      str_detect(end_str, "am$") ~ "am",
      str_detect(end_str, "pm$") ~ "pm",
      TRUE ~ "pm"
    )

    start_hour <- parse_hour(start_str, am_pm_hint = end_ampm)
    end_hour <- parse_hour(end_str)

    duration <- end_hour - start_hour
    if (duration <= 0) duration <- duration + 24

    tibble(
      DJ = dj_code,
      day = day_str,
      start_time = start_str,
      end_time = end_str,
      duration = duration
    )
  }

  results <- map(djKey$DJ, \(dj) {
    Sys.sleep(pause)
    scrape_one(dj)
  }) |>
    bind_rows()

  return(results)
}
