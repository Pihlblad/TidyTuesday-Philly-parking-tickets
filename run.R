tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
library(tidyverse)
library(maps)
library(OpenStreetMap)
library(ggrepel)
library(lubridate)

tcts <- tickets %>% 
  mutate_if( is.character, as.factor) %>% 
  mutate(
    issuing_agency= fct_explicit_na(issuing_agency),
    date = ymd(issue_datetime)
  ) %>% 
  print
  group_by(date, issuing_agency) %>% 
  summarize(sum=sum(fine, na.ignore=TRUE), count=n(), mean=mean(fine, na.ignore=TRUE)) %>% 
  print

unique(tcts$issuing_agency)

df_violation <- tickets %>% 
  mutate_if( is.character, as.factor) %>% 
  mutate(
    issuing_agency= fct_explicit_na(issuing_agency)
  ) %>% 
group_by(violation_desc) %>% 
  summarize(sum=sum(fine, na.ignore=TRUE), count=n(), mean=mean(fine, na.ignore=TRUE)) %>% 
  ungroup

arrange(desc(sum)) %>% 
  top_frac(.1, sum)

Highes_issued <- tickets %>% 
  mutate_if( is.character, as.factor) %>% 
  mutate(
    issuing_agency= fct_explicit_na(issuing_agency)
  ) %>% 
  group_by(violation_desc) %>% 
  summarize(sum=sum(fine, na.ignore=TRUE), count=n(), mean=mean(fine, na.ignore=TRUE)) %>% 
  ungroup %>% 
  arrange(desc(count)) %>% 
  top_frac(.1, count)


f1 <- df_violation %>% 
  arrange(desc(sum)) %>% 
  top_frac(.1, sum) %>% 
ggplot(aes(reorder(violation_desc, sum), sum))+
  geom_bar(stat = "identity")
f1

 
ggplot(tcts, aes(issuing_agency, fine)) +
  geom_point()
