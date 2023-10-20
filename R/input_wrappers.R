
#' @importFrom shiny sliderInput selectizeInput checkboxGroupInput dateRangeInput
#' @importFrom shiny numericInput
#' @importFrom shinyWidgets numericRangeInput pickerInput virtualSelectInput materialSwitch

slider_input <- function(id, self, label = TRUE){
  
  options <- self$options
  
  if(is.null(options$value)){
    options$value <- c(floor_digits(self$range[1], self$round_digits),
                       ceiling_digits(self$range[2], self$round_digits))
  }
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  min_val <- ifelse(is.null(options$min), 
                    options$value[1],
                    options$min)
  
  max_val <- ifelse(is.null(options$max), 
                    options$value[2],
                    options$max)
  
  options$min <- NULL
  options$max <- NULL
  
  options <- c(list(inputId = id), 
               options, 
               list(min = min_val, 
                    max = max_val))
  
  list(ui = do.call(shiny::sliderInput, options),
       value = options$value)
  
}


numericrange_input <- function(id, self, label = TRUE){
  
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- c(floor_digits(self$range[1], self$round_digits), 
                       ceiling_digits(self$range[2], self$round_digits))

  } else {
    
    if("<<this_year>>" %in% options$value){
      options$value[options$value == "<<this_year>>"] <- 1900 + as.POSIXlt(Sys.Date())$year
    }
    if("<<value_max>>" %in% options$value){
      options$value[options$value == "<<value_max>>"] <- ceiling_digits(self$range[2], self$round_digits)
    }
    if("<<value_min>>" %in% options$value){
      options$value[options$value == "<<value_min>>"] <- floor_digits(self$range[1], self$round_digits)
    }
    options$value <- as.numeric(options$value)
    
  }
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), options)
  list(ui = do.call(shinyWidgets::numericRangeInput, options),
       value = options$value)
}


select_input <- function(id, self, type = c("select","picker","pickersearch","virtualsearch"),
                         label = TRUE){
  
  type <- match.arg(type)
  input_field <- switch(type,
                        select = shiny::selectizeInput,
                        picker = shinyWidgets::pickerInput,
                        pickersearch = shinyWidgets::pickerInput,
                        virtualsearch = shinyWidgets::virtualSelectInput
  )
  
  options <- self$options
  
  if(type == "select"){
    options$options <- list(plugins = 'remove_button')
  }
  
  # 'value' is synonym for 'selected' to give unified interface for all filters
  if("value" %in% names(options)){
    options$selected <- options$value
    options$value <- NULL
  }

  if(!("selected" %in% names(options))){
    
    options$selected <- NULL
    
    if(!is.null(self$all_choice)){
      options$selected <- self$all_choice
    }
  }
  if(!("multiple" %in% names(options))){
    options$multiple <- TRUE
  }
  if(!("choices" %in% names(options))){
    
    #if(is.null(self$n_label)){
      options$choices <- c(self$all_choice, self$unique)  
    # } else {
    #   options$choices <- self$n_label
    # }
    
  }

  if(type == "picker"){
    options <- c(options, 
                 list(options = list(`actions-box` = TRUE, 
                      `deselect-all-text` = "Alles uit", 
                      `dropdown-align-right` = TRUE, 
                      `selected-text-format` = "count > 3", 
                      `none-selected-text` = "Geen selectie", 
                      `select-all-text` = "Alles aan", 
                      `none-results-text` = "Geen selectie", 
                      `count-selected-text` = ">3 Geselecteerd", 
                      `dropup-auto` = FALSE)))
  }
  
  if(type == "pickersearch"){
    options <- c(options, 
                 list(options = list(`live-search` = TRUE,
                                     `live-search-normalize` = TRUE,
                                     `actions-box` = TRUE, 
                                     `deselect-all-text` = "Alles uit", 
                                     `dropdown-align-right` = TRUE, 
                                     `selected-text-format` = "count > 3", 
                                     `none-selected-text` = "Geen selectie", 
                                     `select-all-text` = "Alles aan", 
                                     `none-results-text` = "Geen selectie", 
                                     `count-selected-text` = ">3 Geselecteerd", 
                                     `dropup-auto` = FALSE)))
  }
  
  if(type == "virtualsearch"){
    options <- c(options,
                 list(search = TRUE,
                      zIndex = 10,
                      hideClearButton = FALSE,
                      placeholder = "Maak selectie ...",
                      noOptionsText = "Geen keuze mogelijk",
                      noSearchResultsText = "Geen resultaten",
                      selectAllText = "Selecteer alles",
                      searchPlaceholderText = "Zoeken ...",
                      optionsSelectedText = "Geselecteerd",
                      allOptionsSelectedText = "Alles geselecteerd"
                      ))
  }
  
  
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(input_field, options),
       value = options$selected)
  
}


checkboxes_input <- function(id, self, label = TRUE){
  
  options <- self$options

  if(self$filter_ui == "checkboxes"){
    if(!("selected" %in% names(options))){
      options$selected <- NULL
    }
    if(!("choices" %in% names(options))){
      options$choices <- self$unique
    }  
  }
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), options)
  
  list(ui = do.call(shiny::checkboxGroupInput, options),
       value = options$selected)
  
}


date_range_input <- function(id, self, label = TRUE){
  
  options <- self$options
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), options)
  
  if(!("start" %in% names(options))){
    
    if(!("value" %in% names(options))){
      options$start <- self$range[1]
      options$end <- self$range[2]  
    } else {
      
      if("<<this_date>>" %in% options$value){
        options$value[options$value == "<<this_date>>"] <- format(Sys.Date())
      }
      if("<<value_min>>" %in% options$value){
        options$value[options$value == "<<value_min>>"] <- format(self$range[1])
      }
      if("<<value_max>>" %in% options$value){
        options$value[options$value == "<<value_max>>"] <- format(self$range[2])
      }
      options$start <- options$value[1]
      options$end <- options$value[2]
      options$value <- NULL
    }
    
    
  }
  
  
  
  
  
  options$language <- "nl"
  options$format <- "dd-mm-yyyy"
  options$separator <- "tot"
  
  list(ui = do.call(shiny::dateRangeInput, options),
       value = c(options$start, options$end))
  
}


numeric_input <- function(id, self, type = c("min", "max"), label = TRUE){
  
  type <- match.arg(type)
  type_index <- switch(type,
                       min = 1,
                       max = 2)
  options <- self$options
  
  if(!("value" %in% names(options))){
    options$value <- self$range[type_index]
  }
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), 
               options, 
               list(min = self$range[1], max = self$range[2]))
  
  list(ui = do.call(shiny::numericInput, options),
       value = options$value)
}


binary_input <- function(id, self, type = "switch", label = TRUE){

  options <- self$options
  type <- match.arg(type)
  
  if(!("value" %in% names(options))){
    options$value <- FALSE
  }
  
  options$label <- self$label
  if(!label)options$label <- ""
  
  options <- c(list(inputId = id), 
               options)
  
  list(ui = do.call(shinyWidgets::materialSwitch, options),
       value = options$value)

}


