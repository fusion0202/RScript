library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(tidyverse)

URL <- "https://raw.githubusercontent.com/fusion0202/RScript/master/covid_chiba_deaths.csv"
#URL <- "/home/my/data/Chiba_pref_covid/covid_chiba_deaths.csv"
d <- read.csv(URL)


df <- data.frame(table(d$Date))
df <- df[-1,]
colnames(df) <- c("date", "Freq")
df %>% 
  mutate(date = str_replace(date, "日", "")) %>%
  mutate(date = str_replace(date, "月", "-")) %>%
  mutate(date = str_replace(date, "年", "-")) %>%
  mutate(date = as.Date(date)) -> df2

url <- "https://github.com/fusion0202/RScript/blob/master/covid_chiba_deaths.csv"
mtitle <- paste0('Daily New Deaths in Chiba, from ',
                 df2$date[1], ' to ', df2$date[length(df2$date)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 24))

g <- ggplot(data = df2)
g <- g + geom_segment(aes(x = date, y = 0, xend = date, yend = Freq),
                      color = "blue", size = 0.5, alpha=0.5)
g <- g + theme_light()
g <- g + ylim(0,15)
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + labs(title = mtitle,
              x = "Date", 
              y = "Deaths",
              caption = paste("Data Source: ", url)) 
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)

