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

plot.timeseries <- function(time_series, codes=NULL, column_names, collapse="2 weeks", func=NULL, ...) {
  
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


plot.barcharts(x, column_names=c("Open", "Close", "Low"))

p <- ggplot(x, aes(x=Date, y=Open)) +
  geom_line() + 
  xlab("FB")
p

my.plot <- ggplot(data=data_series$AAPL, aes(x=Date, y=Close, group = 1))+
  geom_line(color="#FAB521", size=1) + 
  geom_line(aes(x=Date, y=Open), color="#FAB521", size=0.5) + 
  scale_y_continuous(position="right") +
  scale_x_date(expand=c(0,0)) +
  theme(panel.background = element_rect(fill='#393939'),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 0),
        panel.grid.major.y = element_line(colour = 'white', size=0.1),
        panel.grid.minor = element_line(colour = 'white', size=0.1))
my.plot

dat <- x

dat$date <- dat$Date
dat$price <- dat$Open

p <- ggplot(dat, aes(x = date, y = price)) +
  geom_line(color="#FAB521", size=1) + 
  geom_line(aes(x=Date, y=Close), color="#FAB521", size=0.5) + 
  scale_y_continuous(position="right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %Y") + 
  ggtitle("AAPL | Open Price | 1D | 06/05/2016 | 06/08/2016") + 
  add_theme() + 
  coord_cartesian(ylim=c(min(dat$price) - 1, max(dat$price) + 1),
                  xlim=c(min(dat$date) + 4, max(dat$date)) )  + 
  add_last_price(max(dat$date), dat$price[dat$date==max(dat$date)])

gt <-  ggplot_gtable(ggplot_build(p))

# gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

  annotate("rect", xmin=max(dat$date) + 0.75, xmax=max(dat$date) + 7.25, 
           ymin=dat$price[dat$date==max(dat$date)] - 0.25, 
           ymax=dat$price[dat$date==max(dat$date)] + 0.25, fill="white", colour="black") +
  annotate("text", max(dat$date) + 1, dat$price[dat$date==max(dat$date)], 
           label=paste0("$", round(dat$price[dat$date==max(dat$date)],2)), 
           colour="black", hjust=0)


z <- x
x <- filter(z$GOOG, Date != "2018-04-01")
y <- z$FB

q <- merge(x, y, by = "Date")


multi_full <- Reduce(function(x, y, ...) merge(x, y, all = TRUE, by="Date", suffixes = c(".x",".y")),
                      z)

multi_full <- x %>% 
  reduce(full_join, by="Date") %>%
  select(Date, starts_with("Open"))


s <- multi_full %>% 
  filter(Date == min(Date))

shaped <- melt(multi_full, id="Date")


p <- ggplot(data=shaped, aes_string(x="Date", y="Open_GOOG")) + 
  geom_line(color="red", size=0.5) + 
  geom_line(aes_string(y=Open.y), color="black", size=0.5) + 
  geom_line(aes(x=Date, y=Open.x.x), color="green", size=0.5) + 
  geom_line(aes(x=Date, y=Open.y.y), color="red", size=0.5) 
p

ggplot(shaped,aes(x=Date, y=value, colour=variable, group=variable)) + geom_line() +  scale_y_continuous(position="right") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %Y") + 
  geom_text(aes(label=c("FB", "GOOG", "AAPL"))) +
  ggtitle("AAPL | Open Price | 1D | 06/05/2016 | 06/08/2016") + 
  add_theme() 

colnames(m2)

regex_str <- paste("", sep = "_", codes, collapse="|")