library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)

URL <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/0125kansensya.xlsx"
d <- read.xlsx(URL, sheet = 1)
d2 <- d[-(1:4),]
od <- as.data.frame(table(convertToDate(d2$X7)))
colnames(od) <- c("Date", "Cases")
od$Date <- as.Date(od$Date)

mtitle <- paste0('Chiba, from ', od$Date[1], ' to ', od$Date[length(od$Date)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 12)) 

g <- ggplot(data = od)
g <- g + geom_segment(aes(x = Date, y = 0, xend = Date, yend = Cases),
                      color = "lightblue", size = 1.0)
g <- g + theme_light()
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + labs(title = mtitle,
              x = "Day of Onset", 
              y = "Cases",
              caption = paste("Data Source: ", URL)) 
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)
