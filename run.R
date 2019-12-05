tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
library(tidyverse)
library(maps)
library(OpenStreetMap)
library(rgdal)
library(ggrepel)
library(lubridate)
library(wesanderson)
library(gghighlight)
library(ggmap)

df_violation <- tickets %>% 
  mutate_if( is.character, as.factor) %>% 
  mutate(
    issuing_agency= fct_explicit_na(issuing_agency)
  ) %>% 
  group_by(violation_desc) %>% 
  summarize(sum=sum(fine, na.ignore=TRUE), count=n(), mean=mean(fine, na.ignore=TRUE)) %>% 
  ungroup %>% 
  mutate(
    mean = as.integer(mean)
  )

f1 <- df_violation %>% 
  arrange(desc(sum)) %>% 
  top_frac(.1, sum) %>% 
  mutate(
    sum= sum/1000
  ) %>% 
  ggplot(aes(reorder(violation_desc, sum), sum, fill=sum, label=paste("$", mean)))+
  geom_bar(stat = "identity")+
  theme_bw()+
  xlab("Violation Description")+
  ylab("Total City k$ income by traffic fines 2017")+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradientn(colours=wes_palette(10, name = "Zissou1", type = "continuous"))+
  geom_text_repel(nudge_y = -5)
f1

f2 <- tickets %>%
  filter(violation_desc=="METER EXPIRED CC") %>% 
  mutate(
    month = month(issue_datetime, label = TRUE)
  ) %>% 
  mutate_if(is.character, as.factor) %>% 
  print

map <- get_stamenmap(bbox = c(left = -75.25, bottom = 39.90, right = -75.1, top=40),  getmaptype = "terrain", zoom=15)
ggmap(map)+
  geom_point(data=f2, aes(lon,lat, color=month))

ggplot(aes(reorder(violation_desc, count), count, fill=violation_desc))+
  geom_bar(stat="identity")+
  theme_bw()+
  xlab("Violation Description")+
  ylab("No of fines issued in 2017")+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradientn(colours=pal)
f2

 
ggplot(tcts, aes(issuing_agency, fine)) +
  geom_point()
