##TB research graphics

library (ggplot2)
library (dplyr)
library (data.table)
library (tidyverse)
library (ggpubr)
library (readxl)
library (ggrepel)
library (ggmap)
library(scales)
library(plotly)
library(networkD3)

# KOR - yearly trend of health sector ODA 
KOR_ODA_TREND <- read_excel("Desktop/SNU_TB_research/KOR_ODA_TREND.xlsx", 
                            sheet = "dev_total")
View(KOR_ODA_TREND)

# developing countries total

KOR_ODA_TREND %>%
  filter(!Category %in% c('health_tot')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = Category, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('Health general sector', 'infectious disease', 'TB')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# adding % column 
KOR_ODA_TREND %>%
  filter(!Category %in% c('health_tot')) %>%
  group_by(Year) %>% mutate (Percentage = paste0(round(value/sum(value)*100, 2), "%")) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = Category, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('Health general sector', 'infectious disease', 'TB')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5))

# TB trend: all developing countries
KOR_ODA_TREND <- read_excel("Desktop/SNU_TB_research/KOR_ODA_TREND.xlsx", 
                            sheet = "tb_trend")
View(KOR_ODA_TREND)

KOR_ODA_TREND %>%
  filter(country == "dev_total") %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes (x = Year, y = value, label = value)) + 
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

KOR_ODA_TREND %>%
  filter(!country %in% c('dev_total')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = country, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('Africa', 'Asia', 'Melanesia')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA by channel -- all developing countries
KOR_ODA_TREND <- read_excel("Desktop/SNU_TB_research/KOR_ODA_TREND.xlsx", 
                            sheet = "tb_channel")
View(KOR_ODA_TREND)

KOR_ODA_TREND %>%
  filter(country == "dev_total") %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = channel, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('NGO', 'PPP', 'Public sector', 'Research organisation')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# Health ODA trend in CAN by category
CAN_ODA_TREND <- read_excel("Desktop/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "dev_total")
View(CAN_ODA_TREND)

CAN_ODA_TREND %>%
  filter(!Category %in% c('health_tot')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = Category, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('Health general sector', 'infectious disease', 'TB')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA trend for all developing countries
CAN_ODA_TREND <- read_excel("OneDrive - London School of Hygiene and Tropical Medicine/CAN_ODA_TREND.xlsx", 
                            sheet = "tb_trend", col_types = c("numeric", 
                                                              "numeric", "text"))
View(CAN_ODA_TREND)

CAN_ODA_TREND %>%
  filter(country == "dev_tot") %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes (x = Year, y = value, label = value)) + 
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  geom_bar(stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA by continent 
CAN_ODA_TREND %>%
  filter(!country %in% c('dev_tot')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = country, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Africa', 'America', 'Asia', 'Europe', 'Middle East', 'Oceania')) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA by channel
CAN_ODA_TREND <- read_excel("OneDrive - London School of Hygiene and Tropical Medicine/CAN_ODA_TREND.xlsx", 
                            sheet = "tb_channel")
View(CAN_ODA_TREND)

CAN_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = channel, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Multilateral', 'NGO', 'Other', 'Research organisation')) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# Health ODA trend in AUS by category
AUS_ODA_TREND <- read_excel("~/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
                            sheet = "dev_total")
View(AUS_ODA_TREND)

AUS_ODA_TREND %>%
  filter(!Category %in% c('health_tot')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = Category, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(AUS_ODA_TREND$Year),max(AUS_ODA_TREND$Year),by=1)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_discrete(labels = c('Health general sector', 'infectious disease', 'TB')) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA trend in AUS from 2011-2020 at regional level
AUS_ODA_TREND <- read_excel("~/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
                            sheet = "tb_trend")
View(AUS_ODA_TREND)

AUS_ODA_TREND %>%
  filter(!country %in% c('dev_tot')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = country, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(AUS_ODA_TREND$Year),max(AUS_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Africa', 'Asia', 'Oceania', 'Unspecified')) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# TB ODA by channle in Australia
AUS_ODA_TREND <- read_excel("~/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
                            sheet = "tb_channel")
View(AUS_ODA_TREND)

AUS_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  ggplot (aes(fill = channel, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(AUS_ODA_TREND$Year),max(AUS_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Multilateral', 'NGO', 'Other', 'PPP', 'Public', 'Research organisation')) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  labs(x = "Year", y = "US Dollar, Millions")

# KOR % of each sector 
KOR_ODA_TREND <- read_excel("KOR_ODA_TREND.xlsx", 
                            sheet = "sector_prop")
View(KOR_ODA_TREND)
# stacked 100% 
KOR_ODA_TREND$sector <- factor(KOR_ODA_TREND$sector, levels = c('infectious_disease', 'malaria', 'hiv', 'tb', 'health_tot'))

KOR_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  filter(!sector %in% c('health_tot')) %>%
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Infectious disease', 'Malaria', 'HIV', 'TB')) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  coord_flip() 

# CAN % of each sector 
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "sector_prop")
View(CAN_ODA_TREND)

# stacked 100% 
CAN_ODA_TREND$sector <- factor(CAN_ODA_TREND$sector, levels = c('infectious_disease', 'malaria', 'hiv', 'tb'))

CAN_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('Infectious disease', 'Malaria', 'HIV', 'TB')) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  coord_flip() 

#  AUS stacked 100%
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all", col_types = c("numeric", 
                                                              "text", "numeric", "text"))
View(CAN_ODA_TREND)

CAN_ODA_TREND$sector <- factor(CAN_ODA_TREND$sector, levels = c('hiv', 'infectious_disease', 'malaria', 'tb'))

CAN_ODA_TREND %>%
  filter(country == "australia") %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  scale_fill_discrete(labels = c('HIV', 'Infectious disease', 'Malaria', 'TB')) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  coord_flip() 

# US stacked 100%
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all", col_types = c("numeric", 
                                                              "text", "numeric", "text"))
View(CAN_ODA_TREND)

CAN_ODA_TREND$sector <- factor(CAN_ODA_TREND$sector, levels = c('hiv', 'malaria', 'infectious_disease', 'tb'))

CAN_ODA_TREND %>%
  filter(country == "us") %>% 
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_fill_discrete(labels = c('HIV', 'Malaria', 'Infectious disease', 'TB')) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  coord_flip() 


# % 2020 (including COVID control)
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND) 

# KOREA
CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "korea") %>% 
  mutate(per = `value`/sum(`value`)) %>%
  arrange(desc(sector))

CAN_ODA_TREND$label <- scales::percent(CAN_ODA_TREND$per)
CAN_ODA_TREND <- 
  CAN_ODA_TREND %>%  dplyr::mutate(across(where(is.numeric), round, 2)) 


plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#CANADA
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND) 

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "canada") 

plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#AUS
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND) 

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "australia") 

plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#US
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all", col_types = c("numeric", 
                                                              "text", "numeric", "text"))
View(CAN_ODA_TREND)

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "us" & Year == "2020") 

plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# scatter plot (relationship with gdp and % to TB)
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all", col_types = c("numeric", 
                                                              "text", "numeric", "text", "numeric", 
                                                              "numeric", "numeric"))
View(CAN_ODA_TREND)

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  group_by(Year, country) %>% 
  mutate(health_cap = `health_oda_capita` * 1000000 / `pop_size` *1000) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  filter(!Year %in% c('2020')) %>%
  filter(sector == "tb")%>%
  arrange(desc(sector)) 

# log-log scale graph (tb-gdp log log scale)

plot_ly(data   = CAN_ODA_TREND, x = ~gdp, y = ~value, color = ~country, type = 'scatter',   
        colors = "Paired", size = ~health_cap) %>%
  layout(xaxis = list(title = 'log GDP per capita (USD), 2011-2019',
                      gridcolor = 'ffff', type = "log"),
         yaxis = list(title = 'ODA for TB (log)',
                      gridcolor = 'ffff', type = "log")) 


# sankey diagram
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "sankey_1")
View(CAN_ODA_TREND)

linelist <- CAN_ODA_TREND

# counts by hospital and age category

con_channel_links <- linelist %>% 
  drop_na(value) %>%
  select(country, recepient, value) %>%
  count(country, recepient, value) %>%
  rename(source = country,
         target = recepient,
         value  = value)
con_channel_links

channel_recep_links <- linelist %>%
  drop_na(country) %>%
  select(recepient, region, value) %>%
  count(recepient, region, value) %>%
  rename(source = recepient,
         target = region,
         value  = value)
channel_recep_links

# combine links
links <- bind_rows(con_channel_links, channel_recep_links)

# The unique node names
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

nodes  # print


# match to numbers, not names
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# plot
######
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 12,
  nodeWidth = 30,
  iterations = 0)        # ensure node order is as in data
p

# sankey diagram -- KOREA
library(readxl)
CAN_ODA_TREND <- read_excel("CAN_ODA_TREND.xlsx", 
                            sheet = "sankey_kor")
View(CAN_ODA_TREND)

linelist <- CAN_ODA_TREND

con_channel_links <- linelist %>% 
  drop_na(value) %>%
  select(country, recepient, value) %>%
  count(country, recepient, value) %>%
  rename(source = country,
         target = recepient,
         value  = value)
con_channel_links

channel_recep_links <- linelist %>%
  drop_na(country) %>%
  select(recepient, region, value) %>%
  count(recepient, region, value) %>%
  rename(source = recepient,
         target = region,
         value  = value)
channel_recep_links

# combine links
links <- bind_rows(con_channel_links, channel_recep_links)

# The unique node names
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

nodes  # print


# match to numbers, not names
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# plot
######
p_kor <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 12,
  nodeWidth = 30,
  iterations = 0)        # ensure node order is as in data
p_kor












