#' Make a data filter
#' @param data Dataframe 
#' @param column_name Name column in the dataframe
#' @param label Label (without tooltip!)
#' @param filter_ui Choice of filter UI
#' @param tooltip Tooltip text (optional)
#' @param array_field Logical. If TRUE, the column is coded as an array
#' @param array_separator If array_field, the separator (";" or "json")
#' @param array_comparison Name of function to do array comparison ("any", "all")
#' @param search_method For text-like columns, how to filter ('equal' or 'regex' for pattern search)
#' @param round_digits For numeric-like columns, number of digits to round the filter value to (for numeric range)
#' @param choices Optional, (named) vector of choices for select-like filters
#' @param options Options to the filter, these are settings sent to the actual filter input (e.g. 'width' to selectInput)
#' @param ... Further arguments ignored
#' @param static Deprecated
#' @param section Deprecated
#' @param updates Deprecated
#' @param server Deprecated
#' @param select_choices Deprecated
#' @export
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter between
#' @importFrom rlang sym
#' @importFrom tippy tippy
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tags
datafilter <- R6::R6Class(
  lock_objects = FALSE,
  public = list(
    
    #' @description Make a new filter object
    initialize = function(data, column_name, label, 
                          filter_ui = c("picker", 
                                        "pickersearch",
                                        "virtualsearch",
                                        "select",
                                        "checkboxes",
                                        "slider",    
                                        "numeric_range",
                                        "numeric_min",  
                                        "numeric_max",  
                                        "date_range",
                                        "switch"),
                          
                          tooltip = "",
                          array_field = FALSE,
                          array_separator = ";",
                          array_comparison = "any",
                          search_method = c("equal","regex"),
                          round_digits = 1,
                          choices = NULL, 
                          options = list(),
                          ...,
                          static = NULL, # deprecated
                          section = NULL, # deprecated
                          updates = NULL, # deprecated
                          server = NULL, # deprecated
                          select_choices = NULL  # deprecated
                          ){
    
      
        search_method <- match.arg(search_method)
      
        self$id <- uuid::UUIDgenerate()
        
        self$column_name <- column_name
        self$filter_ui <- filter_ui
        self$options <- options
        
        self$label_txt <- label
        self$label <- private$add_tooltip(label, tooltip)
        
        self$array_field <- array_field
        self$array_separator <- array_separator
        self$array_comparison <- array_comparison
        self$search_method <- search_method
        self$select_choices <- choices
        self$round_digits <- round_digits
        
        # no longer args. needed to use old shinyfilterset code
        self$sort <- TRUE
        self$pass_na <- TRUE
        self$n_label <- FALSE
        
        if(!column_name %in% names(data)){
          stop("Column not found in data")
        }
        
        # Vector of data to use to populate choices/min maxs, etc.
        self$column_data <- data[[column_name]]
        if(is.factor(self$column_data)){
          self$column_data <- as.character(self$column_data)
        }
        
        private$set_range_unique(self$column_data)
        

    },
    
    

      
    #' @description Make the UI for the filter
    #' @param ns Shiny namespace
    #' @param label Logical, if FALSE, does not render a label
    ui = function(ns = NS(NULL), label = TRUE){
      
      id <- ns(self$id)
      
      out <- switch(self$filter_ui, 
                    
                    slider = slider_input(id, self, label = label),
                    select = select_input(id, self, label = label),
                    checkboxes = checkboxes_input(id, self, label = label),
                    picker = select_input(id, self, type = "picker", label = label),
                    pickersearch = select_input(id, self, type = "pickersearch", label = label),
                    virtualsearch = select_input(id, self, type = "virtualsearch", label = label),
                    numeric_min = numeric_input(id, self, "min", label = label),
                    numeric_max = numeric_input(id, self, "max", label = label),
                    numeric_range = numericrange_input(id, self, label = label),
                    switch = binary_input(id, self, type = "switch", label = label),
                    date_range = date_range_input(id, self, label = label)
      )
      
      self$value_initial <- out$value
      
      #self$n_updates <- 0
      
      
      
      return(out$ui)
    },
    
    #' @description For select-input type filters, make the choices
    #' @param x A vector of data
    #' @param selected Optional vector of pre-selected choices
    make_choices = function(x, selected = NULL){
      
      if(is.factor(x)){
        x <- as.character(x)
      }
      
      if(all(is.na(x))){
        return(NA)
      }
      
      vals <- private$get_unique(x, self$sort, self$array_field, self$array_separator)
      
      if(!is.null(self$select_choices)){
        
        if(is.list(self$select_choices[[1]])){
          vals <- lapply(self$select_choices, function(lis){
            lis[lis %in% vals]
          })  
        } else {
          vals <- self$select_choices[self$select_choices %in% vals]
        }
        
      }

      if(self$array_field && !is.null(selected)){
        vals <- vals[which(vals %in% selected)]
      }
      
      vals
    },
    

    
    #' @description Apply the filter to data
    #' @param input Shiny input object
    apply_filter = function(data, input){
      
      value <- input[[self$id]]
      
      colname <- self$column_name
      
      # If we have no data, return data
      if(nrow(data) == 0)return(data)
      
      # If the filter UI has not been generated yet
      if(is.null(value)){
        return(data)
      }
      
      # Custom filter function
      if(!is.null(self$filter_function)){

        data <- self$filter_function(data, value)
        return(data)
      }
      
      if(self$filter_ui %in% c("slider","numeric_range","date_range")){

        if(!self$pass_na){
          data <- dplyr::filter(data,
                                dplyr::between(!!rlang::sym(colname), value[1], value[2]))
        } else {

          data <- dplyr::filter(data,
                                is.na(!!rlang::sym(colname)) |
                                  dplyr::between(!!rlang::sym(colname), value[1], value[2]))

        }

      }
      
      if(self$filter_ui %in% c("select","picker","pickersearch","virtualsearch","checkboxes")){
        
        # 'all_choice' = single choice that acts as all selector (e.g. "All options")
        if(!is.null(value)){
          
          # Filter with equality
          if(self$search_method == "equal"){
            
            if(!self$array_field){
              data <- dplyr::filter(data, !!rlang::sym(colname) %in% value)    
            } else {
              data <- dplyr::filter(data, search_array(!!rlang::sym(colname),
                                                       what = value,
                                                       array_separator = self$array_separator,
                                                       array_comparison = self$array_comparison))
            }
            
            
          }
          
          # Filter with regular expression
          else if(self$search_method == "regex"){
            regex <- paste(value, collapse = "|")
            data <- dplyr::filter(data, grepl(regex, !!sym(colname)))
          }
          
        }
      }
      
      if(self$filter_ui == "numeric_min"){
        data <- dplyr::filter(data, !!rlang::sym(colname) >= value)
      }

      if(self$filter_ui == "numeric_max"){
        data <- dplyr::filter(data, !!rlang::sym(colname) <= value)
      }

      if(self$filter_ui == "switch"){

        # If the switch is OFF (FALSE), don't filter. Only filter the TRUE values if the switch is ON.
        if(value){
          data <- dplyr::filter(data, !!sym(colname) == value)
        }

      }
      
      return(data)  
      
      
    }
    
  ),
  
  private = list(
    
    add_tooltip = function(txt, hlp){
      
      toolt <- if(!is.null(hlp) && hlp != ""){
        tippy::tippy(htmltools::tags$span(bsicons::bs_icon("info-circle-fill")), hlp)
      }
      
      htmltools::tags$span(txt, toolt)
      
    },
    
    set_range_unique = function(column_data){
      
      
      # Text-based categorical filter
      if(self$filter_ui %in% c("picker","pickersearch","select","checkboxes","virtualsearch")){
        
        if(is.factor(column_data)){
          column_data <- as.character(column_data)
        }
        
        self$unique <- self$make_choices(self$column_data)
        
        self$range <- NULL
        
      } else if(self$filter_ui %in% c("slider",
                                      "numeric_min",
                                      "numeric_max",
                                      "numeric_range",
                                      "date_range")){
        
        self$unique <- NULL
        self$range <- range(column_data, na.rm = TRUE) 
        
      } else if(self$filter_ui == "switch"){
        
        self$unique <- c(TRUE,FALSE)
        self$range <- NULL
        
      } 
      
      
    },
    
    from_json = function(x){
      
      lapply(x, 
             function(x){
               tryCatch(
                 jsonlite::fromJSON(x),
                 error = function(e)x
               )  
             }
      )
      
    },
    
    
    get_unique = function(x, sort = TRUE, array_field = FALSE, array_separator = ";"){
      
      if(array_field){
        
        if(array_separator == "json"){
          i_v <- vapply(x, function(x)!all(is.na(x) | x %in% c("{}","[]")), FUN.VALUE = logical(1))
          z <- x[i_v]
          els <- private$from_json(unique(unname(z)))
        } else {
          els <- strsplit(x, array_separator)
        }
        
        if(is.list(els)){
          els <- do.call(c, els)
        }
        
        out <- unique(els)
        
      } else {
        out <- unique(x)
      }
      
      if(sort){
        out <- sort(out)
      }
      
      out
    }
    
  )
  
)
