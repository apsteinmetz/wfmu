# Function Get Show Names
pause = 0.5
ua <- httr::user_agent(
  "wfmu-comment-counter/1.0 (contact: aspteinmetz@yahoo.com)"
)
safe_get_html <- function(url) {
  res <- tryCatch(httr::GET(url, ua, httr::timeout(30)), error = function(e) {
    NULL
  })
  if (is.null(res) || httr::http_error(res)) {
    return(NULL)
  }
  tryCatch(read_html(res), error = function(e) NULL)
}

get_show_names <- function() {
  all_shows <- safe_get_html("https://www.wfmu.org/playlists")

  bench_node <- xml2::xml_find_first(all_shows, '//*[@id="bench"]')

  on_rows <- xml2::xml_find_all(
    bench_node,
    "preceding::tr[.//span[@class='KDBFavIcon KDBprogram']]"
  )
  off_rows <- xml2::xml_find_all(
    bench_node,
    "following::tr[.//span[@class='KDBFavIcon KDBprogram']]"
  )

  make_dj_table <- function(show_rows) {
    # extract DJ IDs from the show rows
    dj_ids <- show_rows |>
      # extract the href attribute from the anchor tag
      html_element("a") |>
      html_attr("href") |>
      # extract the DJ ID from the URL
      # extract the second group from this regex: "(&id=)([A-Z0-9]{2})(&)"
      str_extract("(?<=&id=)[A-Z0-9]{2}(?=&)")
    # create a regex that extracts text between a tab and the first instance of " - playlists" or " [ RSS"
    text_between_regex <- "(?<=\\t)(.*?)(?= - playlists| \\[ RSS)"

    show_names <- show_rows |>
      html_text2() |>
      str_extract(text_between_regex)
    return(tibble(
      DJ = dj_ids,
      ShowName = show_names,
      Channel = "WFMU"
    ))
  }
  show_names <- make_dj_table(on_rows) |>
    mutate(onSched = TRUE) |>
    bind_rows(
      make_dj_table(off_rows) |>
        mutate(onSched = FALSE, Channel = "Archive")
    ) |>
    distinct()

  # now identify which of the djs on the alternate stream
  alt_streams <-
    tibble(
      url = c(
        "https://www.wfmu.org/rocknsoulradio",
        "https://www.wfmu.org/drummer",
        "https://www.wfmu.org/sheena"
      ),
      Channel = c("Rock & Soul", "Give the Drummer", "Sheena's Jungle Room")
    )

  get_alt_stream_djs <- function(stream_url) {
    doc <- safe_get_html(stream_url)
    # get all tables from the page
    stream_djs <- html_elements(doc, "table") |>
      # get all the hrefs from the table
      html_elements("a") |>
      html_attr("href") |>
      # extract 2-character DJ IDs from the URLs
      str_extract("(?<=playlists\\/)([A-Z0-9]{2})") |>
      na.omit() |>
      unique()
    return(tibble(DJ = stream_djs))
  }

  alt_show_names <-
    alt_streams |>
    rowwise() |>
    mutate(djs = list(get_alt_stream_djs(url))) |>
    unnest(djs) |>
    mutate(onSched = TRUE) |>
    select(-url) |>
    distinct() |>
    left_join(show_names |> select(DJ, ShowName), by = "DJ")

  show_names <- show_names |>
    rows_update(alt_show_names, by = "DJ")

  return(show_names)
}
# example usage:
show_names <- get_show_names()
