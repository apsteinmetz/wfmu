# create func_get_time_slots.r

## write a function to get the time slots for each DJ's show.

### required setup info
1. the information can be found on the web page of the form https://wfmu.org/playlists/<DJ>

2. <DJ> is the two-letter dj code and "onSched" are found in the local file djKey.parquet.  Select only DJ's where onSched == TRUE

### scraping info
The time slot will be wrapped in <div class="everything">
The time slot will be in bold type of the form <day of week> <start> - <end><am/pm> <timezone> e.g. "Wednesday 4 - 7pm (EDT)"

Scrape each of the pages.

### data processing

create a data frame with these 5 columns, DJ, the time slot as day, start time, end time and duration.

The function returns this data frame.