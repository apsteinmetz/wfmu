# get show links
get_show_links <- function(dj_id, back_playlist = NULL) {
  date_pattern <- "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+\\d{1,2},\\s+\\d{4}\\b"
  empty <- tibble(
    dj = character(0),
    href = character(0),
    date = as.Date(character(0))
  )
  if (is.null(back_playlist)) {
    dj_url <- paste0(base_url, "/", dj_id)
  } else {
    dj_url <- paste0(base_url, "/", back_playlist)
  }
  print(dj_url)

  doc <- safe_get_html(dj_url)

  if (is.null(doc_html)) {
    return(empty)
  }

  anchors <- html_elements(
    doc,
    xpath = ".//a[(contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlist/')) or (contains(translate(@href,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'/playlists/'))]"
  )
  if (length(anchors) == 0) {
    return(tibble(date = as.Date(character()), href = character()))
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
    href = hrefs[keep]
  )

  # recursively handle links to prior years
  prior_years <- all_items$href[str_detect(
    all_items$href,
    paste0(dj_id, "\\d{4}")
  )]

  playlist_rows <- all_items |>
    filter(!href %in% prior_years) |>
    filter(!href == paste0("/playlists/", dj_id))

  if (is.null(back_playlist)) {
    prior_years <- prior_years |>
      str_remove("/playlists/")

    if (length(prior_years) > 0) {
      back_playlist <- map(prior_years, \(back_year) {
        get_show_links_2(dj_id, back_year)
      }) |>
        bind_rows()
      playlist_rows <- bind_rows(playlist_rows, back_playlist)
    }
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
