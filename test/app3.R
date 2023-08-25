



library(softui)
library(shiny)
library(dplyr)
library(glue)

source("input_wrappers.R")
source("datafilter.R")
source("utils.R")

mtcars$cyl <- as.character(mtcars$cyl)
mtcars$yes_or_no <- as.logical(sample(0:1, nrow(mtcars), replace = TRUE))

cfg <- list(
  cyl = list(
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
    label = "Accel",
    ui = "numeric_range",
    value = c(10,20)
  ),
  yes_or_no = list(
    label = "Indicator ja/nee",
    tooltip = "Probeer deze switch",
    ui = "switch",
    choices = list(
      Ja = TRUE,
      Nee = FALSE
    )
  )
)


read_filter_config <- function(cfg_filters){
  
  nms <- names(cfg_filters)
  for(i in seq_along(cfg_filters)){
    cfg_filters[[i]]$column_name <- nms[i]
  }
  
  cfg_filters
  
}

filter_new <- function(cfg, data = NULL){
  
  if(!is.null(cfg$ui)){
    cfg$filter_ui <- cfg$ui
    cfg$ui <- NULL
  }
  
  do.call(datafilter$new, 
          c(list(data = data), cfg))
}




filterUI <- function(id){
  
  ns <- NS(id)
  
  tags$div(class= "shintofilter-ui-box",
           selectInput(ns("sel_fil"), "Maak selectie:", choices = names(cfg)),
           softui::action_button(ns("btn1"), "Voeg toe", class = "btn-info"),
           
           tags$hr(),
           tags$div(style = "width: 500px;",
                    tags$div(id = ns("placeholder"))
           ),
           
           softui::action_button(ns("btn2"), "Maak overzicht", class = "btn-success"),
           softui::action_button(ns("btn_reset"), "Reset"),
           
           tags$hr(),
           verbatimTextOutput(ns("txtout")),
           tags$hr(),
           verbatimTextOutput(ns("txtout2"))
  )
  
  
}


filterModule <- function(input, output, session, cfg){
  
  ns <- session$ns
  
  cfg_filters <- read_filter_config(cfg)
  
  
  unfiltered_data <- reactiveVal(mtcars)  
  
  
  filters <- reactiveVal()
  filtered_data <- reactiveVal()
  
  observeEvent(input$btn_reset, {
    filters(NULL)
    filtered_data(NULL)
  })
  
  observeEvent(input$btn1, {
    
    cfil <- cfg_filters[[input$sel_fil]]
    
    dat <- filtered_data()
    if(is.null(dat))dat <- unfiltered_data()
    
    fil <- filter_new(cfil, dat)
    
    filters(c(filters(), setNames(list(fil), fil$id)))
    
    
    fui <- fil$ui(ns = ns)
     
    boxui <- softui::sub_box(grey_level = 0, dashed_border = TRUE,
      title = "Filter", icon = bsicon("filter-circle-fill"),
      closable = TRUE, id_close = ns(paste0(fil$id, "-dismiss")),
      
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

    out <- unfiltered_data()
    
    for(filter in filters()){
      out <- filter$apply_filter(out, input)
    }
    
    filtered_data(out)
    
  })
  
  used_filters <- reactive({
    
    fils <- filters()
    
    if(length(fils) == 0){
      return(NULL)
    }
    
    chk <- sapply(fils, function(x){
      !isTRUE(all.equal(as.character(input[[x$id]]), 
                        as.character(x$value_initial)))
    })
    
    vals <- lapply(fils[chk], function(x){
      list(column_name = x$column_name,
           value = input[[x$id]])
    })
    
    if(length(drop_nulls(vals)) == 0)return(NULL)
    
    vals
  })
  
  output$txtout <- renderPrint({
    used_filters()
  })
  
  output$txtout2 <- renderPrint({
    
  })
  
  return(filtered_data)
  
}




ui <- softui::simple_page(
  
  softui::fluid_row(
    
    column(6, 
           tableOutput("tab1")         
    ),
    
    column(6,
           box(
             filterUI("test")  
           )
           
           
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






