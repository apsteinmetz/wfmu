# get show links
duckplyr::methods_restore()
date_pattern <- "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+\\d{1,2},\\s+\\d{4}\\b"
back_playlist <- NULL

# global variables
dj_profiles <- tibble(
  DJ = character(0),
  profileURL = character(0),
  other_shownames = character(0)
)

# get_other_shownames for the given DJ
get_other_shownames <- function(url) {
  # read_html with error checking
  html <- try(xml2::read_html(url), silent = TRUE)
  if (inherits(html, "try-error")) {
    html <- NULL
  }
  if (is.null(html)) {
    return(NULL)
  }
  shownames <- html %>%
    html_nodes(".KDBprogram + a") |>
    html_text() |>
    # remove the current show name from the list
    paste0(collapse = '\n')
  return(shownames)
}

# ====================================================
# Function to get show links for a given DJ ID
# A side effect is to populate dj_profiles with profile URLs and other shownames
# as a global variable
get_show_links <- function(dj_id, back_playlist = NULL) {
  empty <- tibble(
    dj = character(0),
    show_id = character(0),
    show_id = character(0),
    date = as.Date(character(0))
  )

  # remember whether this is the top-level call (so we don't clobber the parameter)
  top_level <- is.null(back_playlist)

  if (top_level) {
    dj_url <- paste0(base_url, "/", dj_id)
  } else {
    dj_url <- paste0(base_url, "/", back_playlist)
  }
  print(dj_url)

  doc <- safe_get_html(dj_url)

  if (is.null(doc)) {
    return(empty)
  }

  anchors <- html_elements(
    doc,
    xpath = ".//a[(contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlist/')) or (contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlists/'))]"
  )
  if (length(anchors) == 0) {
    return(tibble(date = as.Date(character()), show_id = character()))
  }

  hrefs <- html_attr(anchors, "href")
  anchor_text <- html_text2(anchors)
  parent_text <- map_chr(xml_parent(anchors), html_text2)

  dates_from_anchor <- str_extract(anchor_text, date_pattern)
  dates_from_parent <- str_extract(parent_text, date_pattern)

  dates_chr <- ifelse(
    !is.na(dates_from_anchor),
    dates_from_anchor,
    dates_from_parent
  )

  keep <- !is.na(dates_chr)
  if (!any(keep)) {
    return(empty)
  }

  all_items <- tibble(
    dj = dj_id,
    date = dates_chr[keep] |>
      parse_date_time(orders = "BdY", quiet = TRUE) |>
      as.Date(),
    show_id = hrefs[keep]
  )

  # recursively handle links to prior years
  prior_years <- all_items$show_id[str_detect(
    all_items$show_id,
    paste0(dj_id, "\\d{4}")
  )]

  playlist_rows <- all_items |>
    filter(!show_id %in% prior_years) |>
    filter(!show_id == paste0("/playlists/", dj_id))

  if (is.null(back_playlist)) {
    prior_years <- prior_years |>
      str_remove("/playlists/")

    if (length(prior_years) > 0) {
      back_playlist <- map(prior_years, \(back_year) {
        get_show_links(dj_id, back_year)
      }) |>
        bind_rows()
      playlist_rows <- bind_rows(playlist_rows, back_playlist)
    }
  }

  # as long as we're at it, get profile URL and other shownames for this DJ
  if (top_level) {
    profileURL <- doc |>
      html_nodes(xpath = "//a[contains(@href,'profile')]") %>%
      html_attr("href") |>
      pluck(1)
    # if profile URL is not found, use the DJ URL
    if (length(profileURL) == 0) {
      profileURL <- dj_url
      other_shownames <- "none"
    } else {
      other_shownames <- get_other_shownames(profileURL)
      if (length(other_shownames) == 0) other_shownames <- "none"
    }

    dj_profiles <<- dj_profiles |>
      bind_rows(
        tibble(
          DJ = dj_id,
          profileURL = profileURL,
          other_shownames = other_shownames
        )
      )
  }

  # final cleanup
  playlist_rows <- playlist_rows |>
    # links to fill in djs which will be found elsewhere
    filter(!str_detect(href, "wfmu.org/playlists")) |>
    mutate(
      show_id = str_remove(href, "^/playlists/")
    ) |>
    select(dj, date, show_id)

  return(playlist_rows)
}
