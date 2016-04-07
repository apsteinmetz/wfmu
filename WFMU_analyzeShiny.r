#shiny version of analyze DJ data
library(shiny)
library(tm)
library(wordcloud)
library(memoise)

#--------------------------------------------------------------
makeWordCloud<-function(djtdm=djtdm,maxWords=100) {
  #for wordcloud of most widely played artists
  #removing sparse terms at 0.99 means that artists played by fewer than 50 DJs will be dropped
  #and will return about 400 artists
  
  #just onMic?
  #idx <- meta(djCorpus, "cluster") == 5
  #djtdm<-TermDocumentMatrix(djCorpus[idx])%>%removeSparseTerms(0.80)
  
  
  m <- as.matrix(djtdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  t<-head(d, 200)
  rownames(t)<-NULL
  print(t)
  #save(t,file='artistfreq.txt',ascii = TRUE)
  
  print("Create Word Cloud")
  #scalefactor magnifies differences for wordcloud
  scaleFactor=3
  #maxWords = 200
  wordcloud(words = t$word, freq = t$freq^scaleFactor,max.words=maxWords, random.order=FALSE,rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),scale = c(3,.3))
  
}

#---------------------------------------------------
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    makewordcloud(names(v),maxWords=input$max)
                  
  })
}
#-------------------------------------------------------
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("mic", "DJ OnAir Status:",
                  choices = micStatus),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

shinyApp(ui = ui, server = server)
