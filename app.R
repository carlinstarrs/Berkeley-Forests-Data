##Berkeley Forests Data App
#Load in necessary packages
library("shiny")
options(scipen = 999)

#Make sure you set working directory
# setwd("G:/Dropbox/Carlin/GitHub/Berkeley-Forests-Data")

###READ IN BLODGETT DATA, GET ALL TABLES, CONVERT TO CSVS.####
#This only needs to be run when data in the access database is updated.

# library("dplyr")
# library("RODBC")

#Make sure you have the Microsoft Access Databse Engine Driver https://www.microsoft.com/en-us/download/confirmation.aspx?id=23734
#and you are using 32-bit R (set in RStudio by going to Tools -> Global Options)

# convert.all.access.to.csv <- function() {
#   access.file.name <- c(list.files(path = ".", pattern = glob2rx("*.accdb"))) #get names of files ending in .accdb
#   pattern <- c("Inv 7_2017.accdb", " Inv.accdb")
#   forest.names <- gsub(paste0(pattern, collapse = "|"), "", c(list.files(path = ".", pattern = glob2rx("*.accdb")))) #get names of files ending in .accdb
#   forest.names <- trimws(forest.names)
#   forest.names[forest.names == "BFRS"] <- "Blodgett"
#   for (j in 1:length(access.file.name)) {
#     directory <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", file.path(getwd(), paste0(access.file.name[j])))
#     conn <- odbcDriverConnect(directory) #Change the text after "DBH=" to the correct directory for your project
#     tables <- data.frame(sqlTables(conn))
#     # table.names <- tables$TABLE_NAME[tables$TABLE_TYPE == "TABLE"]
#     # tables.not.to.include <- c("FuelSpeciesNames", "Paste Errors", "Cactos Tree Codes", "CalcConstants", "Switchboard Items",
#     #                            "Trees - Regen Tally Regen", "Trees - Regen", "Trees - Seedlings")
#     # table.names <- table.names[!table.names %in% tables.not.to.include]
#     # if (length(which(grepl("~", table.names))) > 0 ){
#     # table.names <- table.names[-which(grepl("~", table.names))]
#     # }
#     # write.csv(table.names, "table names.csv", row.names = FALSE)
#     table.names <- data.frame(read.csv("table names.csv", header = FALSE))
#     tables.in.database <- table.names[table.names$V1 %in% tables$TABLE_NAME[tables$TABLE_TYPE == "TABLE"],]
#     dir.create(file.path(getwd(), forest.names[j]), showWarnings = FALSE)
#     old_wd <- getwd()
#     new_wd <- file.path(getwd(), forest.names[j])
#     setwd(new_wd)
#     for (i in 1:length(tables.in.database)) {
#       write.csv(sqlFetch(conn, paste0(tables.in.database[i]), as.is = TRUE), paste0(tables.in.database[i], ".csv"), row.names = FALSE)
#     }
#     setwd(old_wd)
#     odbcCloseAll()
#   }
# }
# 
# 
# convert.all.access.to.csv()

pattern <- c("Inv 7_2017.accdb", " Inv.accdb")
forest.names <- gsub(paste0(pattern, collapse = "|"), "", c(list.files(path = ".", pattern = glob2rx("*.accdb")))) #get names of files ending in .accdb
forest.names <- trimws(forest.names)
forest.names[forest.names == "BFRS"] <- "Blodgett"

table.names <- read.csv("table names.csv", header = FALSE)

master.list <- vector(mode="list", length=length(forest.names))
for (i in 1:length(forest.names)) {
  forest <- forest.names[i]
  files <- list.files(path = paste0("./", forest))
  num <- length(files)
  forest.list <- vector(mode="list", length=num)
  for (j in 1:num) {
    data <- read.csv(paste0(forest, "/", files[j]))
    forest.list[[j]] <- data
    names(forest.list)[[j]] <- gsub(".csv", "", files[j])
  }
  master.list[[i]] <- forest.list
  names(master.list)[[i]] <- forest
}

get.table <- function(forest.name, table.name) {
    forest <- master.list[which(grepl(forest.name, names(master.list)))]
    forest <- forest[[1]]
    if (length(which(grepl(table.name,names(forest)))) > 0){
    data <- data.frame(forest[[which(grepl(table.name,names(forest)))]])
    } else {
      data <- "Table not available"
    }
    return(data)
  }
  
# get.table("Baker", "Compartment Activity")

get.metadata <- function(table.name) {
  all.txt.files <- list.files(path = ".", pattern = glob2rx("*.txt"))
  all.txt.files <- gsub(".txt", "", all.txt.files)
  if(table.name %in% all.txt.files) {
    metadata <- readLines(paste0(table.name, ".txt"))
  } else {
    metadata <- NA
  }
  return(metadata)
}

# get.metadata("Trees - Live")


# Define UI for data download app ----
ui <- fluidPage(
  
  fluidRow(
    column(6,
           #Sidebar panel for inputs
           wellPanel (selectInput("forest.names", "Choose a Berkeley Forest:",
                                  choices = forest.names),
                      selectInput("dataset", "Choose a dataset:",
                                  choices = table.names$V1)
           )
    ), 
    
    column(6,
           wellPanel(p(strong("Download all data for selected forest:")),
                     downloadButton('downloadAllData', 'Download All'),
                     br(),
                     br(),
                     p(strong("Download currently selected dataset:")),
                     downloadButton("downloadData", "Download Table")
                     
           ))
  ),
  fluidRow(
    column(12,
           tabsetPanel(
             type = "tabs", 
             tabPanel("Data", 
                      br(),
                      br(),
                      uiOutput("summary"),
                      tableOutput("table")),
             tabPanel("Metadata", 
                      br(),
                      htmlOutput("metadata"))
           )

    )
  )
)



server <- function(input, output) {

  #Reactive value for selected dataset ----
  datasetInput <- reactive({get.table(input$forest.names, input$dataset)
  })
  
  datasetMetadata <- reactive({get.metadata(input$dataset)
  })
  
  output$table <- renderTable({
    if (is.data.frame(datasetInput())) {
      if(nrow(datasetInput()) > 50) {
        n <- 30
      } else {
        n <- nrow(datasetInput())
      }
      head(datasetInput(), n)
    } else {
      datasetInput()
    }
  }, spacing = 'xs')
  
  output$metadata <- renderUI({
    if(!is.na(datasetMetadata()[1])) {
      HTML(paste(datasetMetadata(), collapse = "<br/>"))
    } else {
      "No metadata available for this table"
    }

  })

  output$summary <- renderUI({
    if (is.data.frame(datasetInput())) {
      if(nrow(datasetInput()) > 50) {
        wellPanel("Note: Due to size limitations, only the first 30 rows of data are shown below. To view the complete dataset, use the Download button above")
      }
    }
  })

  #Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$forest.names, input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

  output$downloadAllData <- downloadHandler(

    filename = function(){
      paste0(input$forest.names,".zip")

    },

    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      forest <- master.list[which(grepl(input$forest.names, names(master.list)))]
      forest <- forest[[1]]

      #loop through the sheets
      for (i in 1:length(names(forest))){
        #write each sheet to a csv file, save the name
        name.list <- names(forest)
        fileName <- paste(name.list[i],".csv",sep = "")
        write.csv(forest[[i]],fileName,row.names = F)
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
