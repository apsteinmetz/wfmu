Free form radio, WFMU.ORG, maintains a huge trove of past playlists from many DJ's radio shows.  More recently, web-only programming has been added to this.  This dataset offers lots of opportunities for analysis.  I scraped all the playlists I could from the web site and started asking questions.  The raw data set is here for your own explorations. It represents over a million plays spanning decades.

The scraping and data-cleaning process was the most time consuming part of the exercise. Playlist tables are not in a consistent format and my HTML skills are rudimentary.  The raw scraped data set is 'playlists_raw.csv.'

I cleaned up the raw data to reduce errors and create consistency.  DJ's are inconsistent in how they enter artist names.  There are 12 different ways Andy Breckman can misspell "Bruce Springsteen"  I take a stab at fixing some of the glaring errors. I'm sure I missed many. Additionally, many artist names are variant due to collaborators with "featuring," "and the," "with," etc. in the name.  I condense the first two words of every artist name into a token and drop the rest. In a very few cases the air date is clearly wrong. I strip those shows out.  This data set is 'playlists_full.csv.'

There is a final step which is really a judgement call.  Many DJs have signature songs to open and/or close their shows.  Including these skews the play count for songs.  I have chosen to strip those out, or try to.  Songs where one DJ accounts for just about all the plays are stripped out as well.  Should they count anyway?  Dunno. It's your call.  The filtered data set is 'playlists.csv.'

The end result is an reasonably accurate but incomplete record of all the playlists available at WFMU.ORG as of July 2017.  The code used for scraping and cleaning is not included here but is available at https://github.com/apsteinmetz/wfmu.

Thanks to station manager, Ken Freedman, for giving me permission to scrape the site.

-- Art Steinmetz (apsteinmetz@yahoo.com)
