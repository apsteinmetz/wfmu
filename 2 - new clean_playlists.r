# clean playlists within duckplyr
library(tidyverse)
library(ineq) #inequality measures
library(xts)
library(duckplyr)

# try the collapse package
library(collapse)
set_collapse(mask = NULL)

# clean only the most recently fetched playlists
UPDATE_ONLY <- TRUE
STRIP_SIG <- TRUE
#TEST
playlists <- read_parquet_duckdb("data/playlists.parquet")


strip_signature_songs <- function(playlists) {
  cat("Stripping signature opening and closing songs\n")
  #strip out signature opening songs where one opens a show more than 20 times
  #this will strip the song entirely from the database.
  #should strip the artist/title pair, not the title
  STRIP_THRESHOLD <- 20
  playlists <- playlists |>
    mutate(artist_song = paste(ArtistToken, Title))

  strip_songs <- function(playlist) {
    playlist <- playlist |>
      summarize(.by = c("DJ", "AirDate"), FirstSong = first(artist_song)) |>
      summarise(.by = "FirstSong", FirstPlayCount = n()) |>
      arrange(desc(FirstPlayCount)) |>
      filter(FirstPlayCount > STRIP_THRESHOLD) |>
      pull(FirstSong)
    return(playlist)
  }

  songs_to_strip <- strip_songs(playlists)
  print(songs_to_strip)
  playlists <- playlists |>
    filter(!(artist_song %in% songs_to_strip))
  # a few DJs play TWO signature songs to open the show.  Get rid of the second one by doing it again
  songs_to_strip <- strip_songs(playlists)
  print(songs_to_strip)
  playlists <- playlists |>
    filter(!(artist_song %in% songs_to_strip))

  #now strip closing songs
  songs_to_strip <- playlists |>
    summarize(.by = c("DJ", "AirDate"), FirstSong = last(artist_song)) |>
    summarise(.by = "FirstSong", FirstPlayCount = n()) |>
    arrange(desc(FirstPlayCount)) |>
    filter(FirstPlayCount > STRIP_THRESHOLD) |>
    pull(FirstSong)
  print(songs_to_strip)
  playlists <- playlists |>
    filter(!(artist_song %in% songs_to_strip))

  #Songs where only one DJ plays it - over and over even though it might not be a signature song
  #distort the analysis.  I use the Gini coefficent (used for measuring income inequality) to
  # test for song/DJ concentration.  If the Gini is over 0.990, just one DJ has overwhelmingly played it.  If it
  #is also in the top 200 ranking of songs over all, I strip it out.

  #how aggressive should we be in scrubbing artists with lopsided appeal?
  #Setting TOLERANCE to 1.000 would only filter songs with exactly one DJ accounting for all plays.
  # I have set this to 0.997 which essentially deprecates the function because the show, Greasy
  # Kid stuff played a few songs an awful lot but I didn't want to lose the greatest hits.
  TOLERANCE <- 0.997
  NUM_DJS <- length(unique(playlists$DJ))

  song_conc <- function(song) {
    g <- playlists |>
      filter(artist_song == song) |>
      select(DJ, artist_song) |>
      summarise(.by = "DJ", n = n()) |>
      arrange(desc(n)) |>
      pull(n) |>
      c(rep(0, NUM_DJS))

    g <- g[1:NUM_DJS] |> #pad to include no-play DJs in Gini calc
      ineq::Gini()
    return(g)
  }

  count_by_song <- playlists |>
    ungroup() |>
    summarise(.by = "artist_song", Song_Count = n()) |>
    arrange(desc(Song_Count))

  cat('Computing DJ concentration of most-played songs\n')
  songs_to_strip <- NULL
  for (n in 1:200) {
    cat(n, " ")
    song <- count_by_song$artist_song[n]
    gini <- song_conc(song)
    if (gini > TOLERANCE) {
      songs_to_strip <- c(songs_to_strip, song)
    }
  }
  cat("\n")
  cat("Stripping\n")
  print(songs_to_strip)

  playlists <- playlists |>
    filter(!(artist_song %in% songs_to_strip))

  # save the results
  playlists <- playlists |>
    select(-artist_song) # remove before saving. much smaller file
  return(playlists)
}

clean_playlists <- function(playlists) {
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

  # COMPUTE
  playlists <- playlists |>
    compute(prudence = "lavish") |>
    mutate(ArtistToken = tolower(ArtistToken)) |>
    compute(prudence = "stingy")
  # I choose to strip out the stuff below though dealing with it might get better analysis
  #remove any text in parentheses
  cat("Stripping filler words\n")
  # get rid of anything between parenthesis
  #tricky regex to handle cases of multiple parentheticals in one artist
  playlists <- playlists |>
    mutate(ArtistToken = gsub("\\([^()]+\\)", "", ArtistToken))

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

  numWords = 2 #is two enough for uniqueness?
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  #combine first two words
  cat("Trying to make sense of artist names\n")
  #does this break if numWords> number of words?
  playlists$ArtistToken <- playlists$ArtistToken |>
    str_to_title()
  t <- str_split_fixed(
    playlists$ArtistToken,
    pattern = "[ ]+",
    n = numWords + 1
  )[, 1:numWords]
  playlists$ArtistToken <- apply(t, MARGIN = 1, FUN = paste, collapse = " ")

  # move back into duckplyr
  methods_overwrite()
  playlists <- playlists |> compute(prudence = "stingy")

  playlists <- playlists |>
    mutate(ArtistToken = gsub("Rolling Stones", "Stones", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Ennio Morricone", "Morricone", ArtistToken)) #only on WFMU!
  playlists <- playlists |>
    mutate(ArtistToken = gsub("David Bowie", "Bowie", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Bob Dylan", "Dylan", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Yo La", "Yo La Tengo", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Elvis Presley", "Elvis", ArtistToken))
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Guided By", "Guided By Voices", ArtistToken))

  #make some empty cases uniform
  playlists <- playlists |>
    mutate(ArtistToken = gsub("Unkown", "Unknown", ArtistToken))

  stop("break here")

  # There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
  playlists <- playlists |>
    mutate(
      ArtistToken = gsub('BruceSp.+', "Springsteen", ArtistToken)
    )
  cat(
    "Combining iconic 2-name artists into one name to save space in wordcloud\n"
  )

  # any empties left?
  playlists <- playlists |>
    mutate(ArtistToken = gsub("^$", "Unknown", ArtistToken))

  playlists <- playlists |>
    filter(ArtistToken != "Your Dj") |>
    filter(Title != "Your Dj") |>
    filter(ArtistToken != "Hoof Mouth") |>
    filter(ArtistToken != "Tom Wilson") |> #not songs
    filter(ArtistToken != "Hank Levine") |> #not songs
    filter(ArtistToken != "Commercial") |> #not songs
    distinct() #why would there be dupes?  Don't know, but there are

  # squish: trim + collapse multiple spaces to single space for all character columns
  playlists <- playlists |>
    mutate(ArtistToken = gsub("\\s+", " ", ArtistToken)) |>
    mutate(Artist = gsub("\\s+", " ", Artist)) |>
    mutate(Title = gsub("\\s+", " ", Title))

  #use artisttoken to select the most common version of the artist name and make that the token.
  top_artist_version <- playlists |>
    select(ArtistToken, Artist) |>
    summarise(.by = c(ArtistToken, Artist), tokens = n()) |>
    collect() |>
    summarise(
      .by = ArtistToken,
      tokens = sum(tokens),
      Artist = first(Artist)
    ) |>
    slice_max(order_by = tokens, n = 1, by = "ArtistToken") |>
    rename(base_artist = Artist)

  playlists <- top_artist_version |>
    right_join(playlists, by = 'ArtistToken', relationship = "many-to-many") |>
    select(-ArtistToken, -tokens) |>
    rename(ArtistToken = base_artist)

  # ------------------------------------------------------------
  #OPTIONAL
  #using judgement to pare legitimate entries that distort analysis
  if (STRIP_SIG) {
    methods_restore()
    playlists <- strip_signature_songs(as_tibble(playlists))
    methods_overwrite()
  }

  return(playlists)
}

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
cat("Saving playlists as parquet\n")
compute_parquet(playlists, "data/playlists.parquet")
