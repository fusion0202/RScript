library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)

url <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/chiba_corona_data.xlsx"
d <- read.xlsx(url)
rep <- as.numeric(d[-c(1, 2), 5])
day <- seq(as.Date("2020-01-30"), by = "day", length.out = length(rep))

dts <- stl(ts(rep, frequency = 7), s.window = 'per')
df <- data.frame(day, rep, trd = dts$time.series[, 2])

mtitle <- paste0('Chiba, daily from ', df$day[1], ' to ', df$day[nrow(df)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 10),
                seq(as.Date("2020-02-15"), by = "month", length.out = 9))

g <- ggplot(data = df)
g <- g + geom_segment(aes(x = day, y = 0, xend = day, yend=rep),
                      color = "lightblue", size = 1.5)
g <- g + geom_line(aes(x = day, y = trd), color = "darkorange", size = 1.0, alpha　= 0.8)
g <- g + theme_light()
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + labs(title = mtitle,
              x = "Day", 
              y = "Positive reported",
              caption = paste("Data Source: ", url)) 
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)
