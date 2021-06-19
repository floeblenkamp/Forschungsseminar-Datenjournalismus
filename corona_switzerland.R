rm(list=ls())

library(devtools)
library(shiny)
library(plotly)
library(magick)
library(cowplot)
library(ggpubr)
library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(lemon)
library(RColorBrewer)
library(readr)
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(foreign)
library(scales)
library(ggforce)

library(extrafont)
font_import()



setwd("~/UZH/Forschungsseminar")

#load most recent datasets----
owid_fileurl = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

owid_covid <- as.data.frame(fread(owid_fileurl,
                                  sep = ",",
                                  encoding = "UTF-8",
                                  stringsAsFactors = FALSE))

owid_covid$FallInzidenz <- owid_covid$new_cases_smoothed/(owid_covid$population/100000)
owid_covid$TestInzidenz <- owid_covid$new_tests_smoothed/(owid_covid$population/100000)

#subset data for plots----
#most different systems----
md_all <- owid_covid %>% filter(location == c("Switzerland",
                                              "China",
                                              "Russia",
                                              "South Korea",
                                              "United States",
                                              "Taiwan"
)
#                              & date >= "2020-08-01" & date <= "2020-11-15"
) %>%
  select(location, population, positive_rate, date, FallInzidenz, TestInzidenz, new_tests_smoothed, new_cases_smoothed) %>% 
  #mutate(date = dmy(date), new_cases_smoothed_per_thousand = new_cases_per_million/100) %>% 
  na.exclude
write.csv(md_all, file = "md_all.csv")

#most similar systems----
ms_all <- owid_covid %>% filter(location == c("Switzerland",
                                              "Ireland",
                                              "Belgium",
                                              "New Zealand",
                                              "Norway"
)
#                              & date >= "2020-08-01" & date <= "2020-11-15"
) %>%
  select(location, population, positive_rate, date, FallInzidenz, TestInzidenz, new_tests_smoothed, new_cases_smoothed) %>% 
  #mutate(date = dmy(date), new_cases_smoothed_per_thousand = new_cases_per_million/100) %>% 
  na.exclude
#write.csv(ms_all, file = "ms_all.csv")


sui_bel_irl <- owid_covid %>% filter(location == c("Switzerland",
                                                   "Ireland",
                                                   "Belgium",
                                                   "New Zealand"
                                                  )
#                              & date >= "2020-08-01" & date <= "2020-11-15"
) %>%
  select(location, population, positive_rate, date, FallInzidenz, TestInzidenz, new_tests_smoothed, new_cases_smoothed) %>% 
  #mutate(date = dmy(date), new_cases_smoothed_per_thousand = new_cases_per_million/100) %>% 
  na.exclude

# plots on cases and tests----
theme_set(theme_classic())


pos_md <- ggplot(data = md_all %>% filter(date >= "2020-08-01"),  aes(x = date))+
  geom_line(aes(y = positive_rate), show.legend = FALSE, size = 1)+
  labs(title = "Unterschiedliche Systeme: Die höchste Positivrate ist in der Schweiz")+
  ylab("Positivrate")+
  theme(axis.title.y = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())+
  facet_grid(. ~ location)+
  theme(text=element_text(family="serif"))+
  theme(    strip.text = element_text( size = rel(1.2)),
            strip.background = element_rect(fill = "white", colour = "white", size = 0.8))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))
pos_md

pos_ms <- ggplot(data = ms_all %>% filter(date >= "2020-08-01"),  aes(x = date))+
  geom_line(aes(y = positive_rate), show.legend = FALSE, size = 1)+
  labs(title = "Ähnliche Systeme: Die höchste Positivrate ist in der Schweiz")+
  ylab("Positivrate")+
  theme(axis.title.y = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.text = element_text(size = 15), legend.title = element_blank())+
  facet_grid(. ~ location)+
  theme(text=element_text(family="serif"))+
  theme(    strip.text = element_text( size = rel(1.2)),
            strip.background = element_rect(fill = "white", colour = "white", size = 0.8))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))
pos_ms 

cases_sui_bel_irl <- ggplot(data = sui_bel_irl %>% filter(date >= "2020-08-01"),  aes(x = date))+
   geom_line(aes(y = FallInzidenz, linetype = "Cases"),
             show.legend = TRUE, size = 1)+
   geom_line(aes(y = TestInzidenz, linetype = "Tests"),
             show.legend = TRUE, size = 1)+
#  labs(title = "Die Schweiz testet zu wenig")+
  ylab("Inzidenz p. 100Tsd")+
  theme(axis.title.y = element_text(size = 11 ), axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, family = "serif"))+
  theme(legend.text = element_text(size = 11), legend.title = element_blank(),
        legend.position = "bottom")+
 # theme(plot.title = element_text(size = 20, hjust = 0.5))+
  facet_grid(. ~ location)+
  theme(text=element_text(family="serif"))+
  theme(    strip.text = element_text(size = rel(1.2)),
            strip.background = element_rect(fill = "white", colour = "white", size = 0.8))+
  scale_linetype_manual(name="Legend",values=c(Cases="solid", Tests="dotted")) +
  theme (panel.border = element_blank ())
cases_sui_bel_irl



#plotly----
 plotly_sui_bel_irl <- ggplotly(cases_sui_bel_irl, width = 600, height = 350,
                                dynamicTicks = TRUE, tooltip = c("x", "gear", "hp"))  %>%
   layout(hovermode = "x", list(orientation = 'h'), autosize = F ) %>% 
   style(cases_sui_bel_irl, hoverinfo = "y", traces = 1:8)
# api_create(plotly_sui_bel_irl, filename = "SUI_IRL_BEL")


plotly_md <- ggplotly(pos_md, width = 700, height = 350,
                                            dynamicTicks = TRUE, tooltip = c("x", "gear", "hp"))  %>%
  layout(hovermode = "x", list(orientation = 'h'), autosize = F ) %>% 
  style( hoverinfo = "y", traces = 1:5)
#api_create(plotly_md, filename = "pos_md")


plotly_ms <- ggplotly(pos_ms, width = 700, height = 350,
                      dynamicTicks = TRUE, tooltip = c("x", "gear", "hp"))  %>%
  layout(hovermode = "x", list(orientation = 'h'), autosize = F ) %>% 
  style( hoverinfo = "y", traces = 1:5)
#api_create(plotly_ms, filename = "pos_ms")

