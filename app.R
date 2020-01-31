#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(jsonlite)
library(curl)
library(DT)
# options(DT.options = list(pageLength = 15, language = list(search = 'Filter:')))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Get NBA Scores"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput(inputId = "date",
                   label = "Select Date"),
         actionButton("go", "Go!")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("boxscores"),
        DT::dataTableOutput("boxscores_west"),
        DT::dataTableOutput("boxscores_east")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$go, {
    date <- reactiveValues()
    url <- reactiveValues()
    nba <- reactiveValues()
    
    observe(date$day <- ifelse(nchar(day(as.character(input$date))) == 1, paste0("0", day(as.character(input$date))), day(as.character(input$date))))
    observe(date$month <- ifelse(nchar(month(as.character(input$date))) == 1, paste0("0", month(as.character(input$date))), month(as.character(input$date))))
    observe(date$year <- year(as.character(input$date)))
    observe(url$url1 <- glue("http://stats.nba.com/stats/scoreboard/?GameDate={date$month}/{date$day}/{date$year}&LeagueID=00&DayOffset=0"))
    observe(nba$con <- curl(url$url1))
    open(nba$con)
    observe(nba$nba <- read_lines(nba$con))
    observe(nba$nba1 <- jsonlite::prettify(rawToChar(nba$nba$content)))
    observe(nba$nba2 <- fromJSON(nba$nba1))
    observe(nba$json <- fromJSON(txt = nba$nba))
    observe(nba$gameid <- nba$json$resultSets$rowSet[[4]][,1]) #extract GameIDs)
    
    observe(nba$west <- nba$json$resultSets$rowSet[[6]][,c(6,8,9,10)]) #%>%
    observe(nba$east <- nba$json$resultSets$rowSet[[5]][,c(6,8,9,10)])
    
    boxscores <- reactive({
      results <- list()
      for (i in 1:length(nba$gameid)) {
        url2 <- glue("http://stats.nba.com/stats/boxscoresummaryv2/?GameId={nba$gameid[i]}&StartPeriod=0&EndPeriod=0&RangeType=0&StartRange=0&EndRange=0")
        nba2 <- readLines(url2)
        json <- fromJSON(txt = nba2)
        results[[i]] <- json$resultSets$rowSet[[6]]
      }
      for_print <- tibble(NA, nrow = length(nba$gameid), ncol = 6)
      for (i in 1:length(nba$gameid)) {
        for_print[i,1] <- results[[i]][2,6] # away team city
        for_print[i,2] <- results[[i]][2,7] # away team name
        for_print[i,3] <- results[[i]][2,23] # away team final score
        for_print[i,4] <- results[[i]][1,23] # home team final score
        for_print[i,5] <- results[[i]][1,6] # home team city
        for_print[i,6] <- results[[i]][1,7] # home team name
      }
      names(for_print) <- c("away city", "away team", "away points", "home points", "home city", "home team")
      for_print
    })
    
    output$boxscores <- renderText({
      as.character(nba$nba2$resultSets$rowSet[7])
      
      boxscores()
    })     
    output$boxscores_west <- DT::renderDataTable({
      nba$west <- nba$west %>% as_tibble()
      names(nba$west) <- c("Team", "Wins", "Losses", "%")
      nba$west
    })
    output$boxscores_east <- DT::renderDataTable({
      nba$east <- nba$east %>% as_tibble()
      names(nba$east) <- c("Team", "Wins", "Losses", "%")
      nba$east
    })
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

