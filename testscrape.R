library(RCurl)
# #test
# 
# # This page claims to be in iso-8859-1:
# url <- 'http://www.elections.ca/content.aspx?section=res&dir=cir/list&document=index&lang=e#list'
# elections <- read_html(url)
# x <- elections %>% html_nodes("table") %>% .[[2]] %>% html_table() %>% .$TO
# # But something looks wrong:
# x
# # It's acutally UTF-8!
# guess_encoding(x)
# # We can repair this vector:
# repair_encoding(x)
# # But it's better to start from scratch with correctly encoded file
# elections <- read_html(url, encoding = "UTF-8")
# elections %>% html_nodes("table") %>% .[[2]] %>% html_table() %>% .$TO
# 
# html("http://www.sec.gov/litigation/suspensions.shtml") %>%
#   html_nodes("p+ table a") %>% html_attr(name="href")
# 
# 
# url <- "http://espn.go.com/golf/player/_/id/11/stuart-appleby"
# url %>% 
#   html %>% 
#   html_nodes(xpath='//li[contains(.,"Age")]') %>% 
#   html_text() %>% 
#   str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")
# 

omegahatExists = url.exists("http://www.omegahat.org")
# Regular HTTP
if(omegahatExists) {
  txt = getURL("http://www.omegahat.org/RCurl/")
  # Then we could parse the result.
  if(require(XML))
    htmlTreeParse(txt, asText = TRUE)
}
# You may need to set proxy details, etc.,  in the call to getURL
theurl <- "https://en.wikipedia.org/wiki/Brazil_national_football_team"
webpage <- getURL(theurl)
# Process escape characters
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

# Parse the html tree, ignoring errors on the page
pagetree <- htmlTreeParse(webpage, error=function(...){})

# Navigate your way through the tree. It may be possible to do this more efficiently using getNodeSet
body <- pagetree$children$html$children$body 
divbodyContent <- body$children$div$children[[1]]$children$div$children[[4]]
tables <- divbodyContent$children[names(divbodyContent)=="table"]

#In this case, the required table is the only one with class "wikitable sortable"  
tableclasses <- sapply(tables, function(x) x$attributes["class"])
thetable  <- tables[which(tableclasses=="wikitable sortable")]$table

#Get columns headers
headers <- thetable$children[[1]]$children
columnnames <- unname(sapply(headers, function(x) x$children$text$value))

# Get rows from table
content <- c()
for(i in 2:length(thetable$children))
{
  tablerow <- thetable$children[[i]]$children
  opponent <- tablerow[[1]]$children[[2]]$children$text$value
  others <- unname(sapply(tablerow[-1], function(x) x$children$text$value)) 
  content <- rbind(content, c(opponent, others))
}

# Convert to data frame
colnames(content) <- columnnames
as.data.frame(content)


#-------------------------------------------------------
# 
# 
# #xp<-"//body//center[2]//table[1]//ul[2]"
# 
# #t4<-html_node(playListRaw,xpath=xp)
# t4<-html_nodes(playListRaw,xpath="//table")[4:10]
# t5<-html_nodes(t4,xpath='//a[contains(.,"Playlists")]') %>% html_attr(name="href") 
# html_table(t4[1])
# #-------------------------------------------------------
# 
# t <- html_nodes(playListRaw,"table")[8]
# t2<-html_nodes(t,"table")
# html_table(t2[[2]],fill=T,trim=T)
# t3<-html_nodes(t2[[2]],"tr")
# 
# playListRaw%>%
#   html_nodes(xpath='//tr[contains(.,"Presidential play in")]')%>%
#   html_text()%>%  
#   str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")
# 
# playListRaw%>%
#   html_nodes(xpath='//tr[contains(.,"docdate")]')%>%
#   html_text()%>%  
#   str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")
# 
# playListRaw%>%
#   html_nodes(xpath='//tr[contains(.,"Republican Candidates")]')%>%
#   html_text()%>%  
#   str_extract("[A-Z][a-z]{2,} [0-9]{1,2}, [0-9]{4}")
# 
# html_nodes(t2,"a")[1:16] %>%html_attr("href")
# 
# xpathSApply(playListRaw,"//td[@class='docdate']")
# xpathSApply(playListRaw,"//td[@class='doctext']")
# xpathSApply(playListRaw,"//span[@class='doctext']")
# 
# # fill missing columns, and match by col names
# DT1 = data.table(A=1:3,B=letters[1:3])
# DT2 = data.table(B=letters[4:5],C=factor(1:2))
# l = list(DT1,DT2)
# rbindlist(l, use.names=TRUE, fill=TRUE)
# 
# 
# #now we're serious
# #-------------------------------------
# makeplayList <- function(trow)
#   # parse dates and description plus transcript URL, if any
#   c(text=trow%>%html_nodes("td")%>%html_text, url=trow%>%html_nodes("a")%>%html_attr("href"))
# # -----------------------------------
# tocURL <-"http://www.presidency.ucsb.edu/plays.php"
# playListRaw<- html(tocURL)
# 
# #narrow down page to relevant table
# t <- html_nodes(playListRaw,"table")[8]
# t2<-html_nodes(t,"tr")
# t3<-t2[4:length(t3)]
# 
# # extract data elements
# playList<-(lapply(t3,makeplayList))
# 
# options(stringsAsFactors=F)
# t4<-  do.call("rbind.fill",lapply(lapply(playList,t),as.data.frame))
# t5<-filter(t4,is.na(url)==FALSE)
# 
# startDate <- strptime("June 01,2015",format="%B %d, %Y")
# strptime(playList[[166]][1],format="%B %d, %Y")
# 
# #not working
# rbindlist(playList,fill=TRUE)
# playTable<-data.frame(date=NULL,description=NULL,url=NULL)
# 
basic <- html("<p class='a'><b>Bold text</b></p>")
p <- html_node(basic, "p")
p
# Can subset with numbers to extract children
p[[1]]
# Use html_attr to get attributes
html_attr(p, "class")

