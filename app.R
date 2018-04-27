library(tidyverse)
library(shinysense)
library(shiny)
library(jsonlite)
library(DT)

tt <- jsonlite::fromJSON(file("clinical.json"))$clinical_trials %>%
  as_data_frame() %>%
  select(1:7)

ui <- fixedPage(
  h2("Clinical Trial Finder Arterial Hypertension"),
  shinyswiprUI( "quote_swiper",
                h4(
                  textOutput("quote")),
                # h4("Trail Phase:"),
                tags$em(textOutput("quote_author")),
                # h4("Link:"),
                htmlOutput(("abstract"))
  ),
  hr(),
  h4("Swipe History"),
  dataTableOutput("resultsTable"),
  hr(),
  p("This is a simple demo of the Disease Dashboard provided by SpringerNature Medizin.
    Search for a particular disease (e.g., hypertension), and then click, drag, and
    release the Searh Results box to either select a given paper as relevant (drag right) 
    or to discard a paper as irrelevant (drag left). Please drag the box slowly to make 
    sure the app can correctly identify the direction (right or left)."),
  p("NOTE: If you drag to the right, the paper will be considered relevant
    and saved in your Swipe History. Drag to the left, and the paper is considered
    irrelevant and removed from the Search Results."),
  hr(),
  h6("Credits:"),
     p(""),
     p("Papr app - https://github.com/jtleek/papr/blob/master/ui.R"),
     p("SpringerNature Hack Day Apr 2018"),
     p("Dimensions API")
  )

server <- function(input, output, session) {
  card_swipe <- callModule(shinyswipr, "quote_swiper")
  
  appVals <- reactiveValues(
    df = sample_n(tt, 1),
    swipes = data.frame(quote = character(), author = character(), swipe = character())
  )
  
  our_quote <- isolate(appVals$df)
  output$quote <- renderText({ our_quote$title })
  output$abstract <- renderText({ paste0('<a href="', appVals$df$linkout, '">',  appVals$df$linkout, '</a>') })
  output$quote_author <- renderText({ our_quote$phase })
  output$resultsTable <- renderDataTable({appVals$swipes})
  
  observeEvent(card_swipe(),{
    #Record our last swipe results.
    appVals$swipes <- rbind(
      data.frame(
        id = paste0('<a href="', appVals$df$linkout, '">',  appVals$df$id, '</a>'),
        title = appVals$df$title,
        phase = appVals$df$phase,
        swipe = card_swipe()
      ), appVals$swipes
    )
    #send only results swiped to the right to the output.
    output$resultsTable <-  renderDataTable({appVals$swipes %>% filter(swipe == "right") %>% select(-swipe)}, escape = FALSE)
    
    #update the quote
    appVals$df <- sample_n(tt, 1)
    
    #send update to the ui.
    output$quote <- renderText({ appVals$df$title })
    
    output$quote_author <- renderText({ paste0(appVals$df$phase, " (Date: ", appVals$df$date, ". Registry: ", appVals$df$registry, ")")  })
    
    output$abstract <- renderText({ paste0('<a href="', appVals$df$linkout, '">',  appVals$df$linkout, '</a>') })
  }) #close event observe.
}

shinyApp(ui, server)
