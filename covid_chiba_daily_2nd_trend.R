library(ggplot2)
library(ggthemes)
library(scales)
library(openxlsx)
library(rstan)

url <- "https://www.pref.chiba.lg.jp/shippei/press/2019/documents/chiba_corona_data.xlsx"
d <- read.xlsx(url)
rep <- as.numeric(d[-c(1, 2), 5])
# rep[length(rep) + 1] <- 
day <- seq(as.Date("2020-01-30"), by = "day", length.out = length(rep))


N <- length(day)
y <- rep
dat <- list(N = N, y = y)
fit <- stan('https://raw.githubusercontent.com/fusion0202/RScript/master/covid19_chiba_2nd_trend.stan', 
            data = dat, iter = 1000, chains = 4)

fit.smp <- extract(fit)
trend <- rep(0, N)
for (i in 1:N) {
        tmp <- density(fit.smp$trend[, i])
        trend[i] <- tmp$x[tmp$y == max(tmp$y)]
}
trd <- cumsum(trend)


df <- data.frame(day, rep, trd)

mtitle <- paste0('Chiba, daily from ', df$day[1], ' to ', df$day[nrow(df)])
datebreaks <- c(seq(as.Date("2020-02-01"), by = "month", length.out = 11),
                seq(as.Date("2020-02-15"), by = "month", length.out = 10))

g <- ggplot(data = df)
g <- g + geom_segment(aes(x = day, y = 0, xend = day, yend = rep),
                      color = "lightblue", size = 1.5)
g <- g + geom_line(aes(x = day, y = trd), color = "darkorange", size = 1.0, alpha = 0.8)
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

