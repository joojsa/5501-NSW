setwd("C:/Users/32631/Documents/Rstudio")

library(dplyr)
library(tidyverse)
library(plotly)
library(highcharter)
install.packages("DT")
library(DT)
install.packages("leaflet")
library(leaflet)
library(plotly)

View(state.x77)
dim(state.x77)
head(state.x77)
str(state.x77)
summary(state.x77)

state.division
state.region
state.x77

 
US_states77 <- data.frame(state.x77)%>%
  rownames_to_column( . , var = "State")%>%
  tibble(Region = state.region,
         Division = state.division,
         Longitude = state.center$x,
         Latitude = state.center$y)

str(US_states77)
View(US_states77)

is.na(US_states77)

col_pop<-US_states77 %>%
  group_by(Region)%>%
  summarize(Population=sum(Population))%>%
  ggplot(aes(x=reorder(Region,-Population),y=Population))+
  geom_col()+
  scale_y_continuous(breaks = seq(from = 00, to = 70000, by = 10000))+
  labs(x='Regions',y='Population size(in thousands)')+
  ggtitle("Population size per US region in 1975")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
col_pop

#using plot_ly
US_states77 %>%
  group_by(Region)%>%
  summarize(Population = sum(Population))%>%
  plot_ly()%>%
  add_trace( x = ~ reorder(Region, - Population),
             y = ~ Population,
             type = "bar")%>%
  layout(title,'<b> Population size per US region in 1975 </b>',
         xAxis = list(text = "Regions"),
         yAxis = list(text = 'Population size (in thousands)'))

#using highcharter
US_states77 %>%
  group_by(Region)%>%
  summarise(Population = sum(Population))%>%
  arrange(.,-Population )%>%
  hchart(type = "column",
         hcaes (x = Region,
                y = Population))%>%
    hc_xAxis (title = list(text = "Regions"))%>%
    hc_yAxis (title = list(text = "Population size (in thousands)"), max = 70000)%>%
    hc_title (text = "<b> Population size per US region in 1975 </b>",
              align = "center", style = list(color = "black"))

#plotly

US_states77 %>%
  arrange(., -Population)%>%
  slice(1:5)%>%
  plot_ly()%>%
  add_trace(y = ~reorder(State,Population),
            x = ~Population,
            type = "bar")%>%
  layout(title = '<b> The 5 nost populated US states in 1975 </b>',
        xAxis = list(title = "States"),
        yAxis = list(title = 'Population size (in thousands)', dtick=3000))

#highcharter
US_states77 %>%
  group_by(Region)%>%
  summarise(Population = sum(Population))%>%
  arrange(.,-Population )%>%
  hchart(type = "bar",
         hcaes (x = Region,
                y = Population))%>%
  hc_xAxis (title = list(text = "Regions"))%>%
  hc_yAxis (title = list(text = "Population size (in thousands)"), max = 70000)%>%
  hc_title (text = "<b> Population size per US region in 1975 </b>",
            align = "center", style = list(color = "black"))
