library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)

url <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/chiba_corona_data.xlsx"
d <- read.xlsx(url, sheet = 3)
test <- as.numeric(d[-c(1, 2), 5])
posi <- as.numeric(d[-c(1, 2), 3])

rate <- posi / test
rate[is.nan(rate) == T] <- 0

day <- seq(as.Date("2020-01-25"), by = "day", length.out = length(test))

dts <- stl(ts(rate, frequency = 7), s.window = 'per')
trd = dts$time.series[, 2]
trd[1:36] <- NA
trd <- trd * 8000

df <- data.frame(day, test, posi, trd)

mtitle <- paste0('Chiba, daily from ', df$day[1], ' to ', df$day[nrow(df)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 8),
                seq(as.Date("2020-02-15"), by = "month", length.out = 8))

g <- ggplot(data = df)
g <- g + geom_segment(aes(x = day, y = 0, xend = day, yend=test),
                      color = "lightblue", size = 1.5)
g <- g + geom_segment(aes(x = day, y = 0, xend = day, yend=posi),
                      color = "darkblue", size = 1.5, alpha=0.5)
g <- g + geom_line(aes(x = day, y = trd), color = "darkorange", size = 1.0, alpha= 0.8)
g <- g + theme_bw()
g <- g + scale_y_continuous(
        limits = c(0, 1200),
        sec.axis = sec_axis(trans=~.* 0.0125, name = "% positive"))
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + labs(title = mtitle,
              x = "Day", 
              y = "Daily positive & tests",
              caption = paste("Data Source: ", url)) 
g <- g + theme(plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)
