require(ggplot2)
require(grid)
require(dplyr)
require(reshape2)
require(tidyverse)
require(RColorBrewer)

rdiff <- function(x) {
  mean(diff(x)/x[-length(x)]) * 100
}

add_theme <- function() {
  theme(plot.background=element_rect(fill="black"),
        panel.background=element_rect(fill="#101040"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(linetype=2),
        axis.text=element_text(face="bold", color="white"),
        axis.title=element_blank(), 
        plot.title=element_text(color="#FAB521", hjust = 0.5, size=14),
        plot.margin=unit(c(1,1,1,1), "cm"), 
        panel.border=element_rect(colour = "white", fill=NA)
  )
}

add_text_annotate <- function(df, x_margin=0, y_margin=0) {
  
  annotate(geom="text", x=df$date + x_margin, y=df$price + y_margin, label=df$label,
           color=df$color, hjust = 0)
}    

build_start_df <- function(df) {
  
  headers <- names(df)[-1]
  
  res <- data.frame(date = rep(df$Date, length(headers)),
                    price = as.numeric(df[-1]), 
                    label = names(df)[-1], 
                    color = brewer.pal(n = 8, name = "Set2")[1:length(headers)])
 
  res <- res[order(res$price), ] 
}

build_end_df <- function(df) {
  
  headers <- names(df)[-1]
  
  res <- data.frame(date = rep(df$Date, length(headers)),
                    price = as.numeric(df[-1]), 
                    label = as.character(df[-1]), 
                    color = brewer.pal(n = 8, name = "Set2")[1:length(headers)])
  
  res <- res[order(res$price), ] 
}

#' Plot Quandl time-series
#'
#' Plot time-sereis from the Quandl Dataset.
#' @param time_series List of data.frame retrieved from Quandl Dataset.
#' @param column_names 	String or list of strings of titles for each column in this time-series. Map one or more columns from each time-sereis
#' @param codes String or list of strings, code a.k.a ticker symbol on Quandle to be plotted. If NULL, then all codes are being included into a plot.
#' @param collapse A string giving the distance between breaks like "2 weeks", or "10 years". Default is 2 weeks.
#' @param func If FALSE, STOP is a code for a predefined date range is not found, otherwise WARNIGN. Default=FALSE
#' @param ... Other arguments passed on to methods. Not currently used.
#' @return a ggplot object
#' @examples \dontrun{
#' # Plot only only High and Low prices for each code (symbol) 
#' plot.timeseries (time_series=time_series, column_names=c("High", "Low"), codes=c("FB", "AAPL"))
#' 
#' # Plot only only Open and Close prices for Facebook (FB) and Apple (AAPL)
#' plot.timeseries (time_series=time_series, column_names=c("Open", "Close"), codes=c("FB", "AAPL"))
#' 
#' # Plot only only Open and Close prices for Facebook (FB) and Apple (AAPL) and add square root function
#' plot.timeseries (time_series=time_series, column_names=c("Open", "Close"), codes=c("FB", "AAPL"), func=y~sqrt(x))
#' }
#' @export
plot.timeseries <- function(time_series, column_names, codes=NULL, collapse="2 weeks", func=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  params <- list(...)
  
  joined_series <- time_series %>% 
    reduce(full_join, by="Date") %>%
    select(Date, matches(paste("", sep="_", codes, collapse="|"))) %>%
    select(Date, matches(sprintf("^(%s)", paste(column_names, collapse="|"))))
  
  # colors <- brewer.pal(n = 3, name = "Set2")
  min_date <- min(joined_series$Date)
  max_date <- max(joined_series$Date)
 
  plot_title <- sprintf("Symbol: %s | Price: %s | Start: %s | End: %s | Function: %s", 
                        paste(codes, collapse="; "), 
                        paste(column_names, collapse="; "), 
                        min_date, 
                        max_date, 
                        ifelse(is.null(func), "None", deparse(func)))
  
  shaped_series <- melt(joined_series, id="Date")
  
  start_point <- build_start_df(joined_series %>% 
                                  filter(Date == min_date))
  
  end_point <- build_end_df(joined_series %>% 
                              filter(Date == max_date))
  
  p <- ggplot(shaped_series,aes(x=Date, y=value, colour=variable, group=variable)) + 
    scale_colour_brewer(palette="Set2") + 
    geom_line(size=1.2) +  
    scale_y_continuous(position="right") +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %Y") + 
    ggtitle(plot_title) + 
    theme(legend.position = "none") + 
    add_theme()
    
    for (row in 1:nrow(start_point)) {
      p <- p + add_text_annotate(start_point[row, ], x_margin=-4, row) + 
        add_text_annotate(end_point[row, ], x_margin=1, 0.3)
    }
  
  if (!is.null(func)) {
    
    method <- ifelse(is.null(params[["method"]]), "loess", params[["method"]])  
    
    p <- p +  stat_smooth(
     linetype = "twodash",
     method = method, formula = func
   )
  }
  
  p
}



plot.barcharts <- function(time_series, codes=NULL, column_names, collapse="2 weeks", func=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  joined_series <- x %>% 
    reduce(full_join, by="Date") %>%
    select(Date, matches(paste("", sep="_", codes, collapse="|"))) %>%
    select(Date, matches(sprintf("^(%s)", paste(column_names, collapse="|"))))
  
  func_res <- lapply(joined_series[, -1], FUN = rdiff)
  df <- data.frame(matrix(unlist(strsplit(names(func_res), "_")), ncol=2, byrow=TRUE), as.numeric(unlist(func_res)))
  names(df) <- c("Price", "Code", "Func")
  
  p <- ggplot(data=df, aes(x=Code, y=Func, fill=Price)) + 
    geom_bar(stat="identity", position=position_dodge(), width =0.8) + 
    geom_text(aes(label=round(Func, 2)), position=position_dodge(width=0.9), color="white") + 
    scale_fill_brewer(palette="Blues") +
    add_theme() + 
    theme(legend.position="bottom", 
          legend.background=element_rect(fill="#101040"), 
          legend.text = element_text(color="white")) + 
    guides(colour = guide_legend(title.position = "left"))
  p
}
