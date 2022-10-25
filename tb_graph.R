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
library(gridExtra)

# KOR - yearly trend of health sector ODA 
library(readxl)
KOR_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/KOR_ODA_TREND.xlsx", 
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
AUS_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
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
AUS_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
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
library(readxl)
AUS_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/AUS_ODA_TREND.xlsx", 
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
library(readxl)
KOR_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/KOR_ODA_TREND.xlsx", 
                            sheet = "sector_prop")
View(KOR_ODA_TREND)
# stacked 100% 
KOR_ODA_TREND$sector <- factor(KOR_ODA_TREND$sector, levels = c('infectious_disease', 'malaria', 'hiv', 'tb', 'health_tot'))

cols <- c("infectious_disease" = "#F8766D", "malaria" = "#7CAE00",
          "hiv" = "#00BFC4", "tb" = "#C77CFF")

bar_kor <- KOR_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  filter(!sector %in% c('Health total')) %>%
  filter(!Year %in% c('2020')) %>%
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  scale_color_manual(values = cols, aesthetics = c("colour")) +
  coord_flip() 

scale_fill_discrete(labels = c('Infectious disease', 'Malaria', 'HIV', 'TB')) +
  


# % 2020 (including COVID control)
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
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


plot_kor <- plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

## arranging 

# CAN % of each sector 
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "sector_prop")
View(CAN_ODA_TREND)
# stacked 100% 
CAN_ODA_TREND$sector <- factor(CAN_ODA_TREND$sector, levels = c('infectious_disease', 'malaria', 'hiv', 'tb'))

bar_can <- CAN_ODA_TREND %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  filter(!Year %in% c('2020')) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  scale_color_manual(values = cols, aesthetics = c("colour")) +
  coord_flip() 


#  AUS stacked 100%
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all")
View(CAN_ODA_TREND)


bar_aus <- CAN_ODA_TREND %>%
  filter(country == "australia") %>% 
  filter(!Year %in% c('2020')) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  group_by(Year) %>%
  mutate(percent = value / sum(value)) %>%
  ggplot (aes(fill = sector, x = Year, y = value, label = value)) +
  scale_x_continuous(breaks = seq(min(CAN_ODA_TREND$Year),max(CAN_ODA_TREND$Year),by=1)) +
  scale_color_manual(values = cols, aesthetics = c("colour")) +
  geom_bar(stat="identity", position = "fill") +
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")), size = 2, position = position_fill(vjust = 0.5)) +
  labs(x = "Year", y = "Percentage") +
  coord_flip() 

scale_fill_discrete(labels = c('HIV', 'Infectious disease', 'Malaria', 'TB')) +
  
# faceted graphs 
plot_grid(bar_can, bar_aus, bar_kor, ncol =1, labels = c('Canada', 'Australia', 'Korea'), 
          label_size = 8)



# US stacked 100%
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all")
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
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

# KOREA
CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "korea") %>% 
  mutate(per = `value`/sum(`value`)) %>%
  arrange(desc(sector))

CAN_ODA_TREND$label <- scales::percent(CAN_ODA_TREND$per)

df_kor <- CAN_ODA_TREND %>%  dplyr::mutate(across(where(is.numeric), round, 2)) 


pie_kor <- plot_ly(df_kor, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#CANADA
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

df_can <- CAN_ODA_TREND  %>% 
  filter(country == "canada") 

pie_can <- plot_ly(df_can, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#AUS
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_20")
View(CAN_ODA_TREND)

df_aus <- CAN_ODA_TREND %>% 
  filter(country == "australia") 

pie_aus <- plot_ly(df_aus, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

## multiple graphs

t <- list(size=7, color = "Black")
  
plot_ly(labels = ~sector, values = ~value, legendgroup = ~sector, 
        textinfo = 'label+percent',
        textfont = 5) %>%
  add_pie(data = df_can, name = "Canada", domain = list(row = 0, column = 0),
          name = "Canada", domain = list(x = c(0, 0.4), y = c(0.4, 1)),
          showarrow = F,
          textfont=list(size = 4),
          title = "Canada") %>%
  add_pie(data = df_aus, name = "Australia", domain = list(row = 0, column = 1),
          name = "Australia", domain = list(x = c(0.6, 1), y = c(0.4, 1)),
          textfont=list(size = 4),
          title = "Australia") %>% 
  add_pie(data = df_kor, name = "Korea", domain = list(row = 1, column = 0),
          textfont=list(size = 4),
          title = "Korea") %>% 
  layout(showlegend = T,
         grid=list(rows=2, columns=2),
         textfont = 8)



#US
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  filter(country == "us" & Year == "2020") 

plot_ly(CAN_ODA_TREND, labels = ~sector, values = ~value, text = ~value, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  add_pie(hole = 0.5) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# scatter plot (relationship with gdp and % to TB)
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "prop_all", col_types = c("numeric", 
                                                              "text", "numeric", "text", "numeric", 
                                                              "numeric", "numeric"))
View(CAN_ODA_TREND)

CAN_ODA_TREND <- CAN_ODA_TREND %>% 
  group_by(Year, country) %>% 
  mutate(health_cap = health_oda_capita * 1000000 / pop_size *1000) %>%
  dplyr::mutate(across(where(is.numeric), round, 1)) %>% 
  filter(!Year %in% c('2020')) %>%
  filter(sector == "TB")%>%
  arrange(desc(sector)) 

# log-log scale graph (tb-gdp log log scale)
fv <- CAN_ODA_TREND %>% lm(value ~ gdp,.) %>% fitted.values()

fit <- lm(value ~ gdp, data = CAN_ODA_TREND)

plot_ly(data   = CAN_ODA_TREND, x = ~gdp, y = ~value, color = ~country, type = 'scatter',   
        colors = "Paired", size = ~health_cap) %>%
  layout(xaxis = list(title = 'log GDP per capita (USD), 2011-2019',
                      gridcolor = 'ffff', type = "log"),
         yaxis = list(title = 'ODA for TB (log)',
                      gridcolor = 'ffff', type = "log")) %>%
  add_trace(x = ~gdp, y = fitted(fit), mode = "markers") 

ggplot(data= CAN_ODA_TREND, aes(x=gdp, y=value)) + 
   geom_point(aes(colour = factor(country))) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  theme_classic()

# sankey diagram
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
                            sheet = "sankey_1")
View(CAN_ODA_TREND)

CAN_ODA_TREND <- read_excel("Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
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
nodes$group <- as.factor(c("a","b","c","d","e","f","g","h",
                           "h", "i", "j", "k", "l", "m", "o",
                           "p", "q", "r"))
my_color <- 'd3.scaleOrdinal() .domain(["a","b","c","d","e","f","g","h",
                           "h", "i", "j", "k", "l", "m", "o",
                           "p", "q", "r"]) .range([ "#042333b2", "#0c2a50b2", "#13306db2", "#253582b2", "#403891b2", "#593d9cb2",
                                                    "#6b4596b2", "#7e4e90b2", "#90548bb2", "#a65c85b2", "#b8627db2", "#cc6a70b2",
                                                    "#de7065b2", "#eb8055b2", "#f68f46b2", "#f9a242b2", "#f9b641b2", "#f7cb44b2",
                                                    "#efe350b2", "#e8fa5bb2"])'

p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 8,
  nodeWidth = 15,
  iterations = 0,
  sinksRight = FALSE,
  nodePadding = 10,
  colourScale=my_color,
  NodeGroup="group")        # ensure node order is as in data
p

# sankey diagram -- KOREA
library(readxl)
CAN_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/CAN_ODA_TREND.xlsx", 
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
nodes$group <- as.factor(c("a","b","c","d","e","f","g","h",
                           "h", "i", "j", "k", "l", "m", "o",
                           "p", "q", "r"))
my_color <- 'd3.scaleOrdinal() .domain(["a","b","c","d","e","f","g","h",
                           "h", "i", "j", "k", "l", "m", "o",
                           "p", "q", "r"]) .range([ "#042333b2", "#0c2a50b2", "#13306db2", "#253582b2", "#403891b2", "#593d9cb2",
                                                    "#6b4596b2", "#7e4e90b2", "#90548bb2", "#a65c85b2", "#b8627db2", "#cc6a70b2",
                                                    "#de7065b2", "#eb8055b2", "#f68f46b2", "#f9a242b2", "#f9b641b2", "#f7cb44b2",
                                                    "#efe350b2", "#e8fa5bb2"])'


p_kor <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 12,
  nodeWidth = 15,
  sinksRight = FALSE,
  iterations = 0,
  colourScale=my_color,
  NodeGroup="group")        # ensure node order is as in data
p_kor


# agg 

library(readxl)
KOR_ODA_TREND <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/SNU_TB_research/KOR_ODA_TREND.xlsx", 
                            sheet = "agg")
View(KOR_ODA_TREND)


prop1 <- KOR_ODA_TREND %>% filter(Category == "Total health")

prop <- KOR_ODA_TREND %>% filter(!Category %in% c('Total health')) 

 ggplot (KOR_ODA_TREND, aes(x = Year, y = value, fill = Category)) +
  scale_x_continuous(breaks = seq(min(KOR_ODA_TREND$Year),max(KOR_ODA_TREND$Year),by=1)) +
  geom_bar(position=position_dodge(.8), stat="identity") +
  facet_grid(.~country) +
  scale_fill_discrete(labels = c('Infectious disease', 'TB', 'Health total')) +
  labs(x = "Year", y = "US Dollar, Millions") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
   

# r&d by product -- Korea 

 library(readxl)
 rnd_kor <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/rnd_kor.xlsx", 
                       sheet = "pd_all")
 View(rnd_kor)

ggplot (rnd_kor, aes(x = year, y = value, fill = product)) +
   scale_x_continuous(breaks = seq(min(rnd_kor$year),max(rnd_kor$year),by=1)) +
   geom_bar(position=position_stack(.8), stat="identity") +
   facet_grid(.~country) +
   labs(x = "Year", y = "US Dollar") +
   theme_classic() +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
 


# grant data sankey

library(readxl)
grant_data <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/grant_data.xlsx", 
                         sheet = "sankey_kor")
View(grant_data)

linelist <- grant_data

con_pd_links <- linelist %>% 
  drop_na(value) %>%
  select(funder, product, value) %>%
  count(funder, product, value) %>%
  rename(source = funder,
         target = product,
         value  = value)
con_pd_links

pd_recep_links <- linelist %>%
  drop_na(funder) %>%
  select(product, recepient, value) %>%
  count(product, recepient, value) %>%
  rename(source = product,
         target = recepient,
         value  = value)
pd_recep_links

# combine links
links <- bind_rows(con_pd_links, pd_recep_links)

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

p_grant <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 8,
  nodeWidth = 15,
  sinksRight = FALSE,
  iterations = 0)        # ensure node order is as in data

p_grant

## canada
library(readxl)
grant_data <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/grant_data.xlsx", 
                         sheet = "sankey_can")
View(grant_data)

linelist <- grant_data

con_pd_links <- linelist %>% 
  drop_na(value) %>%
  select(funder, product, value) %>%
  count(funder, product, value) %>%
  rename(source = funder,
         target = product,
         value  = value)
con_pd_links

pd_recep_links <- linelist %>%
  drop_na(funder) %>%
  select(product, recepient, value) %>%
  count(product, recepient, value) %>%
  rename(source = product,
         target = recepient,
         value  = value)
pd_recep_links

# combine links
links <- bind_rows(con_pd_links, pd_recep_links)

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

p_can <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 8,
  nodeWidth = 15,
  sinksRight = FALSE,
  iterations = 0)        # ensure node order is as in data

p_can

# australia

library(readxl)
grant_data <- read_excel("C:/Users/Hyolim/OneDrive - London School of Hygiene and Tropical Medicine/SNU_TB/grant_data.xlsx", 
                         sheet = "sankey_au")
View(grant_data)

linelist <- grant_data

con_pd_links <- linelist %>% 
  drop_na(value) %>%
  select(funder, product, value) %>%
  count(funder, product, value) %>%
  rename(source = funder,
         target = product,
         value  = value)
con_pd_links

pd_recep_links <- linelist %>%
  drop_na(funder) %>%
  select(product, recepient, value) %>%
  count(product, recepient, value) %>%
  rename(source = product,
         target = recepient,
         value  = value)
pd_recep_links

# combine links
links <- bind_rows(con_pd_links, pd_recep_links)


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

p_au <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  units = "TWh",
  fontSize = 8,
  nodeWidth = 13,
  sinksRight = FALSE,
  iterations = 0)        # ensure node order is as in data

p_au

