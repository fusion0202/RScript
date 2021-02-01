library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)

URL <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/0201kansensya.xlsx"
d <- read.xlsx(URL, sheet = 1)
d2 <- d[-(1:4),]

od <- as.data.frame(table(convertToDate(d2$X7)))
colnames(od) <- c("Date", "Cases")
od$Date <- as.Date(od$Date)

d3 <- d2[d2$X6 == "県内発生", ]
ods <- as.data.frame(table(convertToDate(d3$X7)))
colnames(ods) <- c("Date", "Cases")
ods$Date <- as.Date(ods$Date)

mtitle <- paste0('Chiba COVID-19 EPI Curve, from ',
                 od$Date[1], ' to ', od$Date[length(od$Date)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 13)) 

g <- ggplot()
g <- g + geom_segment(data = od,
                      aes(x = Date, y = 0, xend = Date, yend = Cases),
                      color = "lightblue", size = 1.0)
g <- g + geom_segment(data = ods,
                      aes(x = Date, y = 0, xend = Date, yend = Cases),
                      color = "darkblue", size = 1.0, alpha = 0.5)
g <- g + theme_light()
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + scale_y_continuous(
  limits = c(0, 300), breaks = seq(0, 300, by = 50))
g <- g + labs(title = mtitle,
              x = "Day of Onset", 
              y = "Cases",
              caption = paste("Data Source: ", URL)) 
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)
