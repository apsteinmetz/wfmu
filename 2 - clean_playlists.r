library(xts)
library(duckplyr)

# try the collapse package
# library(collapse)
# set_collapse(mask = NULL)

# clean only the most recently fetched playlists
UPDATE_ONLY <- FALSE
STRIP_SIG <- TRUE
CONDENSE_ARTISTS <- TRUE
condense_artist_tokens <- function(playlists) {
  #use artisttoken to select the most common version of the artist name and make that the token.
  token_artist_counts <- playlists |>
    select(ArtistToken, Artist) |>
    summarise(.by = c(ArtistToken, Artist), tokens = n())

  # compute max count per ArtistToken (duck-friendly: summarise, not windowed mutate)
  token_max <- token_artist_counts |>
    summarise(.by = ArtistToken, max_tokens = max(tokens))

  # keep only artists that reach the per-token max, then break ties deterministically
  top_artist_map <- token_artist_counts |>
    inner_join(token_max, by = "ArtistToken") |>
    filter(tokens == max_tokens) |>
    summarise(.by = ArtistToken, base_artist = min(Artist))

  top_artist_map <- top_artist_map |>
    mutate(
      base_artist = gsub("^The ", "", base_artist),
      base_artist = gsub("^the ", "", base_artist),
      base_artist = gsub(", The$", "", base_artist),
      base_artist = gsub(", the$", "", base_artist)
    )

  # join the one-row-per-token mapping and replace tokens without expanding rows
  playlists <- playlists |>
    left_join(top_artist_map, by = "ArtistToken") |>
    mutate(ArtistToken = coalesce(base_artist, ArtistToken)) |>
    select(-base_artist)

  return(playlists)
}

strip_signature_songs <- function(playlists, strip_threshold = 0.5) {
  stopifnot(strip_threshold > 0, strip_threshold < 1)

  # count distinct shows per DJ
  dj_shows <- playlists |>
    summarise(.by = DJ, show_count = n_distinct(AirDate))

  # count each title per DJ
  dj_title_counts <- playlists |>
    summarise(.by = c(DJ, Title), title_count = n())

  # find titles that exceed the threshold for ANY DJ
  titles_to_strip <- dj_title_counts |>
    left_join(dj_shows, join_by(DJ)) |>
    mutate(title_ratio = title_count / show_count) |>
    filter(title_ratio > strip_threshold) |>
    distinct(DJ, Title)
  # DEBUG
  print(titles_to_strip)
  # anti-join to remove those DJ/Title combos
  playlists |>
    anti_join(titles_to_strip, join_by(DJ, Title))
}

clean_playlists <- function(playlists_raw) {
  #  methods_restore()
  # remove shows with only one row
  bad_shows <- playlists_raw |>
    summarise(.by = c(DJ, AirDate), n = n()) |>
    filter(n == 1)

  playlists <- playlists_raw |>
    anti_join(bad_shows) |>
    distinct()

  # so many fancy string manupulation functions from dplyr that
  # don't translate to duckplyr. Too bad but speed is not of the essence here.
  # playlists <- playlists |>
  #   as_tibble()

  #filter out squirrelly dates
  #only Diane "Kamikaze" has archived playlists stretching back to the '80s.  Yay, Diane!
  # Charlie Lewis has playlists going back to 1997 but for some reason the dates I scraped
  # go way too far back for about 10 shows.  I chose to lose them since Charlie has mucho
  # episodes
  cutoff_date <- as.Date("1982-01-01")
  playlists <- playlists |> filter(AirDate > cutoff_date)

  cutoff_date <- as.Date("1997-01-01")
  playlists <- playlists |>
    filter(!(AirDate < cutoff_date & DJ == "CL"))

  playlists <- filter(playlists, !grepl("Music (B|b)ehind", Artist))
  playlists <- filter(playlists, !grepl("Music (B|b)ehind", Title))
  playlists <- filter(playlists, !grepl("Wake N Bake", Artist))
  playlists <- filter(playlists, !grepl("Wfmu", Title))
  playlists <- filter(playlists, !grepl("Primavera", Title))
  playlists <- filter(playlists, !grepl("^[tT]aking [Cc]alls", Title))
  playlists <- filter(playlists, !grepl("^Bob Barth", Artist))
  # remove call-in (kept for inspection)
  playlists <- playlists |> filter(!grepl("Call 201", Artist))
  playlists <- playlists |> filter(!grepl("Call 201", Title))

  playlists <- playlists |>
    mutate(Title = gsub("^$", "Unknown", Title))

  #get rid of wake n bake non-music plays
  claylists <- playlists |> filter(DJ == "WA")
  playlists <- playlists |> filter(DJ != "WA")

  claylists <- filter(claylists, !grepl("^Wake ", Artist))
  claylists <- filter(claylists, !grepl("^Pidge ", Artist)) # note space after Pidge so "Pidgeon" not affected
  claylists <- filter(claylists, !grepl("^Clay ", Artist))
  playlists <- union(playlists, claylists)

  # Create ArtistToken column to hold cleaned artist names
  playlists <- playlists |> mutate(ArtistToken = Artist)

  # get rid of breaks
  playlists <- playlists |> filter(!grepl("Speaks$", ArtistToken))
  playlists <- playlists |> filter(!grepl("D[Jj] Speaks", ArtistToken))

  # one artist is all punctuation so give !!! special treatment
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^!!!$", "chkchkchk", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^\\.\\.\\.$", "Unknown", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Uknown", "Unknown", ArtistToken))

  # now change some common punctuation to space
  cat("Stripping Punctuation\n")
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^\\? \\&", "Question Mark And ", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^\\? And", "Question Mark And ", ArtistToken))
  playlists <- playlists |> mutate(ArtistToken = gsub("\\&", " ", ArtistToken))

  cat("Stripping filler words\n")
  # get rid of anything between parenthesis
  #tricky regex to handle cases of multiple parentheticals in one artist
  playlists <- playlists |>
    mutate(ArtistToken = gsub("\\([^()]+\\)", "", ArtistToken))

  # COMPUTE
  # I choose to strip out the stuff below though dealing with it might get better analysis
  #remove any text in parentheses
  cat("drop out of duckplyr")
  playlists <- playlists |>
    compute(prudence = "lavish") |>
    mutate(ArtistToken = tolower(ArtistToken))

  # remove 'featuring' or 'with' artists
  # I chose not to remove "Versus" because that is a band name
  playlists <- playlists |>
    mutate(
      ArtistToken = gsub(
        "(feat |featuring |and the |with |vs |vs\\.).+",
        "",
        ArtistToken
      )
    )

  # get rid of 'live' identifier
  playlists <- playlists |>
    mutate(
      ArtistToken = gsub("(live @ |live on|@).+", "", ArtistToken)
    )

  #now get rid of remaining non-word characters except space
  playlists <- playlists |>
    mutate(ArtistToken = gsub("[^A-Za-z0-9 ]", "", ArtistToken))

  #while we are at it, strip punctuantion from songs, as well
  playlists <- playlists |> mutate(Title = gsub("[^A-Za-z0-9 ]", "", Title))

  # get rid of 'interview'
  playlists <- playlists |>
    mutate(ArtistToken = gsub("(interview w|interview)", "", ArtistToken))

  # get rid of unspecified artists
  playlists <- playlists |>
    mutate(
      ArtistToken = gsub("unknown artist(s| )|unknown", "Unknown", ArtistToken)
    )
  playlists <- playlists |>
    mutate(
      ArtistToken = gsub("various artists|various", "Unknown", ArtistToken)
    )

  #get rid of the marathon finale
  playlists <- playlists |>
    filter(!grepl("hoof[a-zA-Z ]+sinfonia", Artist))

  #make "new york" one word.  Lots of bands start with the term
  playlists <- playlists |>
    mutate(ArtistToken = gsub("new york", "newyork", ArtistToken))

  #make "x ray" one word. hopefully we've stripped out the dash already.Lots of bands start with the term
  playlists <- playlists |>
    mutate(ArtistToken = gsub("x ray", "xray", ArtistToken))

  #now some connecting words that might be spelled/used variantly
  playlists <- playlists |>
    mutate(ArtistToken = gsub("and | of | the ", " ", ArtistToken))

  #and leading "the"
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^the ", " ", ArtistToken))

  # strip leading/trailing whitespace
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^\\s+", "", ArtistToken)) |>
    mutate(ArtistToken = gsub("\\s+$", "", ArtistToken))

  #did we create any null entries
  playlists <- filter(playlists, Artist != "")
  playlists <- filter(playlists, Artist != "Artist")

  cat("Using only first two words as artist token\n")
  numWords = 2 #is two enough for uniqueness?
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  #combine first two words

  playlists <- playlists |>
    mutate(
      ArtistToken = str_squish(ArtistToken), # collapse multiple spaces
      ArtistToken = str_to_title(ArtistToken), # title case
      # keep up to `numWords` words (works when there is only one word)
      ArtistToken = sub(
        paste0("^\\s*(\\S+(?:\\s+\\S+){0,", numWords - 1, "}).*$"),
        "\\1",
        ArtistToken,
        perl = TRUE
      )
    )

  # get rid of super long artist names that are probably garbage
  playlists <- playlists |>
    as_tibble() |>
    mutate(len = nchar(ArtistToken)) |>
    filter(len < 100) |>
    select(-len)

  # move back into duckplyr
  methods_overwrite()
  playlists <- playlists |> compute(prudence = "stingy")

  # strip cases where Show name got into artist field

  cat(
    "Combining iconic 2-name artists into one name to save space in wordcloud\n"
  )
  #  playlists <- playlists |>
  #    mutate(ArtistToken = gsub("Rolling Stones", "Stones", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Ennio Morricone", "Morricone", ArtistToken)) #only on WFMU!
  playlists <- playlists |>
    mutate(ArtistToken = gsub("David Bowie", "Bowie", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Bob Dylan", "Dylan", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Elvis Presley", "Elvis", ArtistToken))
  # expand common artists where 3 words are needed
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Yo La", "Yo La Tengo", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Guided By", "Guided By Voices", ArtistToken))

  #make some empty cases uniform
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Unkown", "Unknown", ArtistToken))

  # There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
  # playlists <- playlists |>
  # mutate(
  #   ArtistToken = gsub('BruceSp.+', "Springsteen", ArtistToken)
  # )

  # any empties left?
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^$", "Unknown", ArtistToken))

  playlists <- playlists |>
    filter(!grepl("Your DJ",Title)) |>
    filter(ArtistToken != "Your DJ") |>
    filter(Title != "Your DJ") |>
    filter(ArtistToken != "Hoof Mouth") |>
    filter(ArtistToken != "Tom Wilson") |> #not songs
    filter(ArtistToken != "Hank Levine") |> #not songs
    filter(ArtistToken != "Commercial") |> #not songs
    distinct() #why would there be dupes?  Don't know, but there are

  # squish: trim + collapse multiple spaces to single space for all character columns
  playlists <- playlists |>
    mutate(ArtistToken = gsub("\\s+", " ", ArtistToken)) |>
    mutate(Artist = gsub("\\s+", " ", Artist)) |>
    mutate(Title = gsub("\\s+", " ", Title)) |>
    mutate(ArtistToken = gsub("\\s+$", "", ArtistToken))

  # now filter out any entries where the artist token matches the show token
  playlists <- playlists |>
    anti_join(
      djKey |> select(DJ, ShowToken) |> distinct(),
      by = c("DJ", "ArtistToken" = "ShowToken")
    )

  #filter(ArtistToken == ShowToken) #  |>
  #select(-ShowToken)

  if (CONDENSE_ARTISTS) {
    playlists <- condense_artist_tokens(playlists)
  }

  # ------------------------------------------------------------
  #OPTIONAL
  #using judgement to pare legitimate entries that distort analysis
  if (STRIP_SIG) {
    print("stripping signature songs that are played in most episodes by a given DJ, which distort analysis of most common artists and songs")
    methods_restore()
    playlists <- strip_signature_songs(as_tibble(playlists))
    methods_overwrite()
  }

  return(playlists)
}

djKey <- read_parquet_duckdb("data/djKey.parquet")
playlists <- read_parquet_duckdb("data/playlists.parquet")

if (UPDATE_ONLY) {
  # load only recently fetched raw playlists
  playlists_raw <- read_parquet_duckdb("data/playlists_temp.parquet")
  playlists_update <- clean_playlists(playlists_raw)
} else {
  # load full raw scraped playlists
  playlists_raw <- read_parquet_duckdb("data/playlists_raw.parquet")
  # careful we don't overwrite existing playlists when updating
  playlists <- clean_playlists(playlists_raw)
}

if (UPDATE_ONLY) {
  # load existing playlists
  existing_playlists <- read_parquet_duckdb("data/playlists.parquet")
  # playlists <- read_parquet_duckdb("data/playlists.parquet")
  # combine with new playlists
  playlists <- union(playlists_update, existing_playlists) |>
    distinct()
}

# save unique artisttokens as parquet
cat("Saving unique artist tokens as rdata\n")
all_artisttokens <- playlists |>
  select(ArtistToken) |>
  distinct() |>
  arrange(ArtistToken) |>
  pull(ArtistToken)
# save as rdata
save(all_artisttokens, file = "data/all_artisttokens.rdata")

# save as parquet
# cat("Saving playlists as parquet\n")
compute_parquet(playlists, "data/playlists.parquet")

# compute showCount, FirstShow and LastShow from playlists for each DJ and update djKey
show_stats <- playlists |>
  summarise(
    .by = DJ,
    showCount = n_distinct(AirDate),
    FirstShow = min(AirDate, na.rm = TRUE),
    LastShow = max(AirDate, na.rm = TRUE)
  )
djKey <- djKey |>
  rows_update(show_stats, by = "DJ", unmatched = "ignore") |>
  arrange(DJ)
compute_parquet(djKey, "data/djKey.parquet")


#summary stats
summarize_playlists_duck <- function(df) {
  library(dplyr)
  library(rlang)
  library(tidyr)

  summary_df <- df |>
    summarise(
      total_rows = n(),
      earliest = min(AirDate, na.rm = TRUE),
      latest = max(AirDate, na.rm = TRUE)
    )

  shows_df <- df |> select(DJ, AirDate) |> distinct() |> summarise(Shows = n())

  # collect small results
  summary_combined <- bind_cols(collect(summary_df), collect(shows_df))

  cols <- names(df)
  uniq_exprs <- setNames(
    lapply(cols, function(c) rlang::expr(n_distinct(!!rlang::sym(c)))),
    cols
  )
  uniques_row <- df |> summarise(!!!uniq_exprs) |> collect()

  uniques <- uniques_row |>
    tidyr::pivot_longer(
      everything(),
      names_to = "column",
      values_to = "n_unique"
    ) |>
    arrange(desc(n_unique))

  list(summary = summary_combined, uniques = uniques)
}
summarize_playlists_duck(playlists)
