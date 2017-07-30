---
title: "My Science Project: A Statistical Examination of 33 Years of Playlists WFMU.org."
output: 
  html_notebook: 
    code_folding: hide
author: Arthur Steinmetz
---
Free form radio, WFMU.ORG maintains a huge trove of past playlists from many DJ's radio shows.  More recently, web-only programming has been added to this.  This dataset offers lots of opportunities for analysis.  I scraped all the playlists I could from the web site and started asking questions.

The scraping and data-cleaning process was the most time consumer part of the exercise. Playlist tables are not in a consistent format and my HTML skills are rudimentary.  Further, the DJ's are inconsistent in how they enter artist names.  There are 12 different ways Andy Breckman can misspell "Bruce Springsteen"  I take a stab at fixing some of the glaring errors. Additionally, many artist names are variant due to collaborators with "featuring," "and the," "with," etc in the name.  I condense the first two words of every artist name into a token and drop the rest. Also, many DJs have signature songs to open and/or close their shows.  Including these skews the play count for songs.  I have chosen to strip those out, or try to.  In a few cases the air date is clearly wrong. I strip those shows out as well. The result is an reasonably accurate but incomplete record of all the playlists available at WFMU.ORG as of July 2017.  The code used for scraping and cleaning is in scrape_playlists.r and clean_playlists.r.  The notebook with the analysis is analyze_playlists.rmd. playlists.RData has the cleaned playlist data used by analyze_playlists.rmd. playlists.zip (contains playlists.csv) is available for your convenience.

In the future, I plan to make an interactive tool to query the data set.

Thanks to station manager, Ken Freedman, for giving me permission to scrape the site.
