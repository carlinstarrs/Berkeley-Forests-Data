##Blodgett Data App
library("dplyr")
library("RODBC")
library("ggplot2")
library("reshape2")
library("shiny")
options(scipen = 999)

###READ IN BLODGETT DATA, GET ALL TABLES, CONVERT TO CSVS.####
#This only needs to be run when data in the access database is updated.

# directory <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(getwd(), "BFRS Inv.accdb"))
# conn <- odbcDriverConnect(directory) #Change the text after "DBH=" to the correct directory for your project
# tables <- data.frame(sqlTables(conn))
# table.names <- tables$TABLE_NAME[tables$TABLE_TYPE == "TABLE"]
# table.names <- table.names[-which(grepl("~", table.names))]
# for (i in 1:length(table.names)) {
#   write.csv(sqlFetch(conn, paste0(table.names[i]), as.is = TRUE), paste0(table.names[i], ".csv"), row.names = FALSE)
# }
# write.csv(table.names, "table.names.csv", row.names = FALSE)
# odbcCloseAll()

table.names <- read.csv("table.names.csv")

for (i in 1:nrow(table.names)) {
  assign(paste0(table.names$x[i]), read.csv(paste0(table.names$x[i], ".csv")), envir = .GlobalEnv)
}

# Define UI for data download app ----
ui <- fluidPage(
  
  fluidRow(
    column(6,
           #Sidebar panel for inputs
           wellPanel (selectInput("dataset", "Choose a dataset:",
                                  choices = table.names$x),
                      
                      # Button
                      downloadButton("downloadData", "Download Table")
           )
    ), 
    column(3, 
           wellPanel(
             downloadButton('downloadAllData', 'Download All Data')
           )
    ),
    column(3,"")
  ),
  fluidRow(
    column(12,
           uiOutput("summary"),
           tableOutput("table")

    )
  )
)


server <- function(input, output) {
  x <- nrow(table.names)
  all.data <- vector(mode="list", length=nrow(table.names))
  
  for (i in 1:nrow(table.names)) {
    all.data[[i]] <- get(paste0(table.names$x))
  }
  # Reactive value for selected dataset ----
  datasetInput <- reactive({ switch(input$dataset, get(paste0(table.names$x[which(table.names$x == input$dataset)])))
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    if(nrow(datasetInput()) > 50) {
      n <- 30
    } else {
      n = nrow(datasetInput())
    }
    head(datasetInput(), n)
  }, spacing = 'xs')
  
  output$summary <- renderUI({
    if(nrow(datasetInput()) > 50) {
      wellPanel("Note: Due to size limitations, only the first 30 rows of data are shown below. To view the complete dataset, use the Download button above")
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  output$downloadAllData <- downloadHandler(
    
    filename = function(){
      paste0("Blodgett.Data",".zip")
      
    },
    
    content = function(file) {
      # write.csv(dataInput()[[2]], file, row.names = FALSE)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      #loop through the sheets
      for (i in 1:length(all.data)){
        #write each sheet to a csv file, save the name
        name.list <- table.names$x
        fileName <- paste(name.list[i],".csv",sep = "")
        write.csv(all.data[[i]],fileName,row.names = F)
        files <- c(fileName,files)
      }
      #create the zip file
      zip(file,files)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)

#rsconnect::deployApp('G:/Dropbox/Carlin/GitHub/Berkeley-Forests-Data')
