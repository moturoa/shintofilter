floor_digits <- function(x, digits){
  
  floor(x * 10^digits) / 10^digits
  
}


ceiling_digits <- function(x, digits){
  
  ceiling(x * 10^digits) / 10^digits
  
}

drop_nulls <- function (x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

numeric_breaks_categories <- function(x, breaks, round_digits = 1){
  
  x <- x[!is.na(x)]
  lower <- c(floor_digits(min(x), round_digits),
             breaks)
  upper <- c(breaks, 
             ceiling_digits(max(x), round_digits))
  
  paste(lower, upper, sep = " - ")
  
}

search_array <- function(x, what, array_separator = ";", array_comparison = c("all","any")){
  
  from_json <- function(x){
    
    lapply(x, 
           function(x){
             tryCatch(
               jsonlite::fromJSON(x),
               error = function(e)x
             )  
           }
    )
    
  }
  
  if(array_separator == "json"){
    lis <- from_json(x)
  } else {
    lis <- strsplit(x, array_separator)  
  }
  
  # all, any, ...
  how <- base::get(match.arg(array_comparison))
  
  sapply(lis, function(el){
    how(what %in% el)
  })
  
}




is_empty <- function(x){
  if(is.null(x))return(TRUE)
  
  if(length(x) == 1){
    out <- is.null(x) || as.character(x) == ""
    out || is.na(out)  
  } else {
    sapply(x, is_empty)
  }
  
}
