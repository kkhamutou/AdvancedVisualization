require(ggplot2)
require(grid)
require(dplyr)
require(reshape2)
require(tidyverse)
require(RColorBrewer)

rdiff <- function(x) {
  mean(diff(x)/x[-length(x)], na.rm = TRUE) * 100
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

add_text_annotate <- function(df, x_margin=0, y_margin=0, h_just=0) {
  
  annotate(geom="text", x=df$date + x_margin, y=df$price + y_margin, label=df$label,
           color=df$color, hjust = h_just)
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

require(grid)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Plot Quandl time-series
#'
#' Plot time-sereis from the Quandl Dataset.
#' @param time_series List of data.frame retrieved from Quandl Dataset.
#' @param column_names 	String or list of strings of titles for each column in this time-series. Map one or more columns from each time-sereis
#' @param codes String or list of strings, code a.k.a ticker symbol on Quandle to be plotted. If NULL, then all codes are being included into a plot.
#' @param gen_title String. Add automatically generated title.
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
plot.timeseries <- function(time_series, column_names, codes=NULL, gen_title=TRUE, collapse="2 weeks", func=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  params <- list(...)
  
  joined_series <- time_series %>% 
    reduce(full_join, by="Date") %>%
    select(Date, matches(paste("", sep="_", codes, collapse="|"))) %>%
    select(Date, matches(sprintf("^(%s)", paste(column_names, collapse="|"))))

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
    scale_x_date(date_breaks = collapse, date_labels = "%b %Y") + 
    theme(legend.position = "none") + 
    add_theme()
    
  if (gen_title == TRUE) {
    p <- p +  ggtitle(plot_title)
  }
    for (row in 1:nrow(start_point)) {
      p <- p + add_text_annotate(start_point[row, ], x_margin=-10, row, h_just=0.5) + 
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

#' Plot all Quandl time-series
#'
#' Plot all time-sereis from the Quandl Dataset on the same page.
#' @param time_series List of data.frame retrieved from Quandl Dataset.
#' @param column_names 	String or list of strings of titles for each column in this time-series. Map one or more columns from each time-sereis
#' @param codes String or list of strings, code a.k.a ticker symbol on Quandle to be plotted. If NULL, then all codes are being included into a plot.
#' @param by A string: code or columns. Split into multuple graps.
#' @param ncol Integer, number of column
#' @param gen_title String. Add automatically generated title.
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

plot.timeseries.all <- function(time_series, column_names, codes=NULL, by="codes", ncol = 2, collapse="2 weeks", func=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  plots <- list()
  
  for (i in get(by)) {
    plots[[i]] <- plot.timeseries(time_series, column_names, codes)
  }
  
  multiplot(plotlist = plots, cols = ncol)
}


#' Plot Quandl bar chart
#'
#' Plot bar chart from the Quandl Dataset.
#' @param time_series List of data.frame retrieved from Quandl Dataset.
#' @param column_names 	String or list of strings of titles for each column in this time-series. Map one or more columns from each time-sereis
#' @param codes String or list of strings, code a.k.a ticker symbol on Quandle to be plotted. If NULL, then all codes are being included into a plot.
#' @param func If FALSE, STOP is a code for a predefined date range is not found, otherwise WARNIGN. Default=FALSE
#' @param ... Other arguments passed on to methods. Not currently used.
#' @return a ggplot object
#' @examples \dontrun{
#' # Plot only only High and Low prices for each code (symbol) 
#' plot.barchart (time_series=time_series, column_names=c("High", "Low"), codes=c("FB", "AAPL"))
#' 
#' # Plot only only Open and Close prices for Facebook (FB) and Apple (AAPL)
#' plot.barchart (time_series=time_series, column_names=c("Open", "Close"), codes=c("FB", "AAPL"))
#' 
#' # Plot only only Open and Close prices for Facebook (FB) and Apple (AAPL) and add square root function
#' plot.barchart (time_series=time_series, column_names=c("Open", "Close"), codes=c("FB", "AAPL"), func=y~sqrt(x))
#' }
#' @export
plot.barchart <- function(time_series, column_names, codes=NULL, func=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  joined_series <- time_series %>% 
    reduce(full_join, by="Date") %>%
    select(Date, matches(paste("", sep="_", codes, collapse="|"))) %>%
    select(Date, matches(sprintf("^(%s)", paste(column_names, collapse="|"))))
  
  min_date <- min(joined_series$Date)
  max_date <- max(joined_series$Date)
  
  plot_title <- sprintf("Symbol: %s | Price: %s | Start: %s | End: %s | Function: %s (mean)", 
                        paste(codes, collapse="; "), 
                        paste(column_names, collapse="; "), 
                        min_date, 
                        max_date, 
                        ifelse(is.null(func), "Rate of Return", deparse(func)))
  
  func_res <- lapply(joined_series[, -1], FUN =  ifelse(is.null(func), rdiff, func))
  df <- data.frame(matrix(unlist(strsplit(names(func_res), "_")), ncol=2, byrow=TRUE), as.numeric(unlist(func_res)))
  names(df) <- c("Price", "Code", "Func")
  
  p <- ggplot(data=df, aes(x=Code, y=Func, fill=Price)) + 
    geom_bar(stat="identity", position=position_dodge(), width =0.8) + 
    geom_text(aes(label=round(Func, 2)), position=position_dodge(width=0.9), color="white") + 
    ggtitle(plot_title) +
    scale_fill_brewer(palette="YlOrRd") +
    add_theme() + 
    theme(legend.position="bottom", 
          legend.background=element_rect(fill="black"), 
          legend.text = element_text(color="white"))
  p
}

#' Plot Quandl pie chart
#'
#' Plot pie chart from the Quandl Dataset.
#' @param time_series List of data.frame retrieved from Quandl Dataset.
#' @param w 	List of numbers, weights of stocks. If less then 1, "Other" code will be created.
#' @param codes String or list of strings, code a.k.a ticker symbol on Quandle to be plotted. If NULL, then all codes are being included into a plot.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @return a ggplot object
#' @examples \dontrun{
#' # Plot only only Open and Close prices for Facebook (FB) and Apple (AAPL)
#' plot_piechart (time_series=time_series, w=c(0.3, 0.7), codes=c("FB", "AAPL"))
#' 
#' plot.barchart (time_series=time_series, w=c(0.3, 0.3, 0.3), codes=c("FB", "AAPL", "GOOG"))
#' }
#' @export
plot_piechart <- function(time_series, w, codes=NULL, ...) {
  
  if (is.null(codes)) {
    codes <- names(time_series)
  }
  
  if (length(codes) != length(w))  {
    stop("Length of codes and weights shoud be the same!")
  }

  if (sum(w) > 1) {
    stop("Sum of weights should not be greate then 1!")
  }
  
  if (sum(w) < 1) {
    w <- append(w, 1-sum(w))
    codes <- append(codes, 'Other')
  }
  
  data <- data.frame(codes=codes, weights=w)
  
  data <- data %>% 
    arrange(desc(codes)) %>%
    mutate(prop = w / sum(data$w) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  p <- ggplot(data, aes(x="", y=prop, fill=codes)) + 
    geom_bar(stat="identity", width=1) +
    scale_fill_brewer(palette="Set1") +
    geom_text(aes(y = ypos, label = codes), color = "white", size=6) + 
    coord_polar(theta = "y", start=0) + 
    add_theme()
  p <- p +
    theme(legend.position="none", 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank())
  p
  
}