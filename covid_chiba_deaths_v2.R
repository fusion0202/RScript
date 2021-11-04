library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)
library(tidyverse)


mdf <- data.frame(date = seq(as.Date("2020-02-01"), by = "day", length.out = 365 * 2))


#
# Deaths
#

URL1 <- "https://raw.githubusercontent.com/fusion0202/RScript/master/covid_chiba_deaths.csv"
d <- read.csv(URL1)

df <- data.frame(table(d$Date))
df <- df[-1,]
colnames(df) <- c("date", "deaths")
df %>% 
  mutate(date = str_replace(date, "日", "")) %>%
  mutate(date = str_replace(date, "月", "-")) %>%
  mutate(date = str_replace(date, "年", "-")) %>%
  mutate(date = as.Date(date)) -> df2

mdf <- left_join(mdf, df2, by = c("date" = "date"))


#
# Cases
#

URL2 <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/1103kansensya.xlsx"

s1 <- read.xlsx(URL2, sheet = 1)
s1 <- s1[-c(1:4), 8] 
s1 <- as.Date(as.numeric(s1), origin = "1899-12-30")
s1 <- s1[!is.na(s1)]
d1 <- as.data.frame(table(s1))
d1$s1 <- as.Date(as.vector(d1$s1))

s2 <- read.xlsx(URL2, sheet = 2)
s2 <- s2[-1, 7] 
s2 <- as.Date(as.numeric(s2), origin = "1899-12-30")
s2 <- s2[!is.na(s2)]
d2 <- as.data.frame(table(s2))
d2$s2 <- as.Date(as.vector(d2$s2))

dt <- left_join(d1, d2, by = c("s1" = "s2"))
dt$Freq.y[which(is.na(dt$Freq.y))] <- 0
dt$cases <- dt$Freq.x + dt$Freq.y
dt <- dt[,-c(2,3)]

mdf <- left_join(mdf, dt, by = c("date" = "s1"))
mdf$deaths[which(is.na(mdf$deaths))] <- 0
mdf$cases[which(is.na(mdf$cases))] <- 0


#
# trim dataframe:mdf
#

ddf <- mdf[mdf$date >= as.Date("2020-02-01"),]
ddf <- ddf[ddf$date <= as.Date("2021-12-01"),]

#
# plot deaths
#

datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 23))
mtitle1 <- paste0('Daily New Deaths in Chiba, from ',
                 df2$date[1], ' to ', df2$date[length(df2$date)])

g <- ggplot(data = ddf)
g <- g + geom_segment(aes(x = date, y = 0, xend = date, yend = deaths),
                      color = "blue", size = 0.5, alpha = 0.5)
g <- g + theme_light()
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + labs(title = mtitle1,
              x = "Day", 
              y = "Deaths",
              caption = paste("Data Source: ", URL1)) 
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)


#
# plot cases
#

mtitle2 <- paste0('Daily new confirmed cases in Chiba, from ', min(dt$s1), ' to ',
                 max(dt$s1))

g <- ggplot()
g <- g + geom_segment(data = ddf, aes(x = date, y = 0, xend = date, yend = cases),
                      color = "darkorange", size = 0.5, alpha = 0.8)
g <- g + theme_light()
g <- g + scale_x_date(breaks = datebreaks, labels = date_format("%m/%d")) 
g <- g + scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 500))
g <- g + labs(title = mtitle2,
              x = "Day", 
              y = "Positive Confirmed",
              caption = paste("Data Source: ", URL2) )
g <- g + theme(panel.grid.minor = element_blank(),
               plot.title = element_text(size = rel(1.4)),
               axis.title = element_text(size = rel(1.2)),
               axis.text = element_text(size = rel(1.0)))
print(g)

