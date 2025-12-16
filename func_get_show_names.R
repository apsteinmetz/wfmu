# Function Get Show Names

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
        mutate(onSched = FALSE)
    )
  return(show_names)
}
