#' End of day US Stock Prices
#'
#' @details Retrieve data from the Quandl Dataset.
#' @param code string or list of strings, code a.k.a ticker symbol on Quandle.
#' @param start_date A Date object, format="YYYY-MM-DD". Retrieve data rows on and after the specified start date.
#' @param end_date A Date object, format="YYYY-MM-DD". Retrieve data rows up to and including the specified end date.
#' @param collapse Change the sampling frequency of the returned data. Default is none; i.e., data is returned in its original granularity.
#' @param api_key Authentication token for a Quandl user.
#' @param skipNA If FALSE, STOP is a code for a predefined date range is not found, otherwise WARNIGN. Default=FALSE
#' @return list(data=dataframe of returns per stock, missingCode=vector of codes that do not satisfy input params)
#' @examples \dontrun{
#' code <- c("AAPL", "FB", "GOOG")
#' start_date <- "2017-12-31"
#' end_date <- "2018-12-31"
#' collapse <- "daily"
#' api_key <- "Xsgzx1TsTQY6YFLRf8at"
#' time_series <- get.market.data (code=code, start_date=start_date, end_date=end_date, collapse=collapse, api_key=api_key)
#' }
#' @note Due to Quandle database specification, all ".", " " symbols are represented as "_".
#' @references This R package uses the Quandl API. For more information go to \url{https://www.quandl.com/docs/api}. For more help on the package itself go to \url{https://www.quandl.com/help/r}.
#' @export
get.market.data <- function(code, start_date, end_date,
                            collapse=c("daily", "weekly", "monthly", "quarterly", "annual"),
                            api_key, skipNullStocks = FALSE, skipNA = FALSE) {
  
  # check if code is correct
  if (!all(gsub("[^A-Z0-9_.]", "", code) == code)) {
    stop("Codes are comprised of capital letters, numbers and underscores only.")
  }
  
  # convert freq to interger. Used in skipNA=TRUE to validate if data.frame is of complete form.
  frequency2integer <- function(freq) {
    
    if (is.null(freq) || is.na(freq)) {
      return(252)
    } else {
      switch(freq,
             "daily"    = 252,
             "weekly"   = 52,
             "monthly"  = 12,
             "quarterly" = 4,
             "yearly"   = 1,
             1)
    }
  }
  
  # setup params for GET request
  params <- list()
  params$collapse <- collapse
  params$start_date <- as.Date(start_date, format = "%Y-%m-%d")
  params$end_date <- as.Date(end_date, format = "%Y-%m-%d")
  params$api_key <- api_key
  
  result <- list()
  
  for (c in code) {
    result[[c]] <- get.dataset(c, params, skipNullStocks)
  }
  
  return(result)
}

get.dataset <- function(code, params, skipNullStocks) {
  
  writeLines(sprintf("Calling %s data...", code))
  json <- api.call(url_base=sprintf("https://www.quandl.com/api/v3/datasets/WIKI/%s/data.json?", code),
                   params=params)$dataset
  
  if (length(json$data) == 0) {
    if (skipNullStocks == FALSE) {
      stop(sprintf("Data for %s between %s and %s (%s) does not exists.",
                   code, params$start_date, params$end_date, params$collapse))
    } else {
      writeLines(sprintf("Data for %s between %s and %s (%s) does not exists.",
                         code, params$start_date, params$end_date, params$collapse))
      return(NULL)
    }
  }
  
  fin_data <- data.frame(json$data, stringsAsFactors = FALSE)
  names(fin_data) <- paste(json$column_names, sep = "_", code)
  names(fin_data)[1] <- "Date"
  
  fin_data[, 1] <- as.Date(fin_data[, 1])
  
  for (i in 2:length(fin_data)) {
    fin_data[[i]] <- as.numeric(fin_data[[i]])
  }
  
  return(fin_data[order(fin_data$Date), ] )
  
}

api.call <- function(url_base, params) {
  
  response <- httr::GET(url_base, query=params)
  
  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as = "text"), call. = FALSE)
  }
  
  text_response <- httr::content(response, as = "text")
  
  json_response <- tryCatch(jsonlite::fromJSON(text_response, simplifyVector = TRUE), error = function(e) {
    stop(e, " Failed to parse response: ", text_response)
  })
  
  return(json_response)
}



