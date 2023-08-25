


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



ui <- fluidPage(
  
  fluidRow(
    
    column(6, 
           tableOutput("tab1")         
           ),
    
    column(6,
           
           selectInput("sel_fil", "Maak selectie:", choices = c("cyl","qsec")),
           actionButton("btn1", "Voeg toe", class = "btn-info"),
           
           tags$hr(),
           tags$div(style = "width: 500px;",
                    tags$div(id = "placeholder")
           ),
           
           actionButton("btn2", "Maak overzicht", class = "btn-success")         
           
           )
    
  )
  
  
)

server <- function(input, output, session) {
  
  
  filters <- reactiveVal()
  filtered_data <- reactiveVal()

  unfiltered_data <- reactiveVal(mtcars)  
  
  #--> alleen zodat er een default complete dataset is 
  observeEvent(unfiltered_data(), {
    filtered_data(unfiltered_data())
  })
  
  observeEvent(input$btn1, {
    
    cfil <- cfg[[input$sel_fil]]
    fil <- do.call(datafilter$new, 
                   c(list(data = filtered_data()), cfil))
                         
    filters(c(filters(), setNames(list(fil), fil$id)))
    
    
    fui <- fil$ui()
    
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
    
    insertUI(selector = "#placeholder", where = "beforeEnd", ui = boxui)
      
    observeEvent(input[[paste0(fil$id, "-dismiss")]], {
      
      removeUI(selector = paste0("#", paste0(fil$id, "-box")), immediate = TRUE)
      
      delid <- fil$id
      i <- match(delid, names(filters()))
      newf <- filters()[-i]
      filters(newf)
      
    })
    
  })

  display <- reactiveVal(FALSE)
  
  observeEvent(input$btn2, {
    
    fils <- filters()
    
    out <- unfiltered_data()
    
    for(filter in fils){
      out <- filter$apply_filter(out, input)
    }
    
    filtered_data(out)
    display(TRUE)
  })
  
  output$tab1 <- renderTable({
    req(display())
    filtered_data()
  })
  
  
}


shinyApp(ui, server)





