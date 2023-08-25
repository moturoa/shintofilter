



library(shiny)
library(dplyr)
library(glue)

source("input_wrappers.R")
source("datafilter.R")
source("utils.R")

mtcars$cyl <- as.character(mtcars$cyl)

cfg <- list(
  cyl = list(
    column_name = "cyl",
    label = "Cylinder",
    tooltip = "Number of cylinders<br>in your <b>car</b>",
    ui = "select",
    choices = list(
      four = "4",
      six = "6",
      eight = "8"
    )
  ),
  qsec = list(
    column_name = "qsec", 
    label = "Accel",
    ui = "numeric_range"
  )
)



filterUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(class= "shintofilter-ui-box",
    selectInput(ns("sel_fil"), "Maak selectie:", choices = c("cyl","qsec")),
    actionButton(ns("btn1"), "Voeg toe", class = "btn-info"),
    
    tags$hr(),
    tags$div(style = "width: 500px;",
             tags$div(id = ns("placeholder"))
    ),
    
    actionButton(ns("btn2"), "Maak overzicht", class = "btn-success")   
  )
  
  
}


filterModule <- function(input, output, session, cfg){
  
  ns <- session$ns
  
  filters <- reactiveVal()
  filtered_data <- reactiveVal()
  
  unfiltered_data <- reactiveVal(mtcars)  
  
  observeEvent(input$btn1, {
    
    cfil <- cfg[[input$sel_fil]]
    
    if(!is.null(cfil$ui)){
      cfil$filter_ui <- cfil$ui
      cfil$ui <- NULL
    }
    
    
    fil <- do.call(datafilter$new, 
                   c(list(data = filtered_data()), cfil))
    
    filters(c(filters(), setNames(list(fil), fil$id)))
    
    
    fui <- fil$ui(ns = ns)
    
    boxui <- tags$div(
      id = paste0(fil$id, "-box"),
      style = glue("display: inline-block; 
                 width: 300px; 
                 margin: 16px; 
                 border: 1px solid grey; 
                 vertical-align: top; 
                 padding: 8px;"),
      tags$div(
        style = "width: 100%; height: 16px;",
        tags$div(
          style = "float:right;",
          actionButton(paste0(fil$id, "-dismiss"), HTML("&times;"), style = "background:none;border:none;")
        )
      ),
      
      fui
    )
    
    insertUI(selector = paste0("#", session$ns("placeholder")), 
             where = "beforeEnd", ui = boxui)
    
    observeEvent(input[[paste0(fil$id, "-dismiss")]], {
      
      removeUI(selector = paste0("#", paste0(fil$id, "-box")), immediate = TRUE)
      
      delid <- fil$id
      i <- match(delid, names(filters()))
      newf <- filters()[-i]
      filters(newf)
      
    })
    
  })
  
  observeEvent(input$btn2, {
    
    fils <- filters()
    
    out <- unfiltered_data()
    
    browser()
    for(filter in fils){
      out <- filter$apply_filter(out, input)
    }
    
    filtered_data(out)
    
  })
  

return(filtered_data)
  
}




ui <- fluidPage(
  
  fluidRow(
    
    column(6, 
           tableOutput("tab1")         
    ),
    
    column(6,
           
           filterUI("test")     
           
    )
    
  )
  
  
)

server <- function(input, output, session) {
  
  filtered_data <- callModule(filterModule, "test", cfg = cfg)
  
  output$tab1 <- renderTable({
    filtered_data()
  })
  
  
}


shinyApp(ui, server)






