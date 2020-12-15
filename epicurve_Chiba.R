library(tabulizer)
library(purrr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)

#URL <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/1214kansensya.pdf"
URL <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/1215kansensya.pdf"

tabulizer::extract_tables(URL) %>%
  purrr::map_dfr(as.data.frame) -> df
df[df$V1 == "",] <- df[df$V1 == "",][,-1]
df %>% 
  slice(-1) %>% 
  filter(!is.na(V7)) %>%
  filter(V7 != "") %>%
  filter(str_detect(V6, pattern = "日")) %>%
  mutate(V6 = str_replace(V6, "日", "")) %>%
  mutate(V6 = str_replace(V6, "月", "-")) %>%
  mutate(V6 = paste0("2020-", V6)) %>%
  mutate(V6 = as.Date(V6)) -> df2

od <- as.data.frame(table(df2$V6))
colnames(od) <- c("Date", "Cases")
od$Date <- as.Date(od$Date)

mtitle <- paste0('Chiba, from ', od$Date[1], ' to ', od$Date[length(od$Date)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 11),
                seq(as.Date("2020-01-15"), by = "month", length.out = 12))

g <- ggplot(data = od)
g <- g + geom_segment(aes(x = Date, y = 0, xend = Date, yend = Cases),
                      color = "lightblue", size = 1.5)
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
