library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)

Days <- 7

chiba <- st_read("https://raw.githubusercontent.com/fusion0202/RScript/master/chiba_admin.geojson")
df <- read.csv("https://raw.githubusercontent.com/fusion0202/RScript/master/covid_chiba_rev_2.csv", check.names = FALSE)

cDate <- tail(colnames(df), 1)
pDate <- tail(colnames(df), 2)[1]
sDate <- as.character(as.Date(cDate) - Days)

df %>% select(sichoson, all_of(cDate), all_of(pDate), all_of(sDate)) %>% 
  rename(Today = all_of(cDate), Yesterday = all_of(pDate), Begin = all_of(sDate)) %>% 
  mutate(Today = replace(Today, is.na(Today), 0),
         Yesterday = replace(Yesterday, is.na(Yesterday), 0),
         Begin = replace(Begin, is.na(Begin), 0)) %>%
  mutate(total = Today - Begin, new = Today - Yesterday) %>%
  mutate(total = replace(total, total == 0, NA),
         new = replace(new, new == 0, NA)) %>% 
  select(sichoson, new, total) -> dt

pf <- read.csv("https://raw.githubusercontent.com/fusion0202/RScript/master/chiba_pop_20210701.csv", 
               check.names = FALSE)
pop <- pf[-(2:7), c(1,2)]
dt$total2 <- round(dt$total * 100000 / pop$T, digits = 0)

map <- left_join(chiba, dt, by = c("SIKUCHOSON" = "sichoson"))
cap <- "Data Source: https://github.com/fusion0202/RScript/blob/master/covid_chiba_rev.csv"

nx <- c( 0.00,  0.02,  0.00,  0.00, -0.01,  0.00, -0.02,  0.03,  0.00,  0.00,
        -0.01,  0.00,  0.00,  0.00,  0.03,  0.015, 0.00,  0.00,  0.00,  0.00,
         0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, -0.01,
         0.00, -0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
        -0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
        -0.01,  0.00,  0.00, -0.01)
ny <- c(-0.01, -0.02,  0.00, -0.015,-0.02,  0.00,  0.00, -0.05,  0.00,  0.00,
         0.00,  0.00,  0.00,  0.00, -0.03,  0.00,  0.00,  0.00,  0.00,  0.00,
         0.00,  0.00,  0.00, -0.03,  0.00,  0.00, -0.01,  0.00,  0.00,  0.01,
         0.00,  0.045, 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
         0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
         0.00,  0.00,  0.00,  0.00)


ggplot(data = map) +
  geom_sf(aes(fill = new),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#dd8585", 
                      na.value = "white", name = "No. of Cases")+#, breaks=c(2,4,6,8, 10)) +
  geom_sf_text(aes(label = new), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("on ", cDate),
       caption = cap)


ggplot(data = map) +
  geom_sf(aes(fill = total),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#cd0505", 
                      na.value = "white", name = "No. of Cases") +
  geom_sf_text(aes(label = total), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("from ", as.character(as.Date(sDate) + 1), " to ", cDate),
       caption = cap)


ggplot(data = map) +
  geom_sf(aes(fill = total2),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#cd0505", 
                      na.value = "white", name = "per 100,000") +
  geom_sf_text(aes(label = total2), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("from ", as.character(as.Date(sDate) + 1), " to ", cDate),
       caption = cap)

