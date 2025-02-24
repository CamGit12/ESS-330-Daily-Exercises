#Cameron McLaughlin
#Feb 21 2025
#Daily exercises 08
###Make a faceted plot of the cumulative cases & deaths by USA region. Your x axis should be the date and the y axis value/count. To do this you will need to join and pivot the COVID-19 data.
library(tidyverse)
library(scales)

#REad in the COVID-19 data
covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#Create new data.frame using the available state.abb, state.name, state.region objects in base R. Be intentional about creating a primary key to match to the COVID data
state_info <- data.frame(state = state.name,
                         region = state.region,
                         abb = state.abb)

#Join new data.frame to the raw COVID data. Think about right, inner, left, or full join…
covid_joined <- covid_raw %>%
  left_join(state_info, by = "state")

#split-apply the joined data to determine the daily, cumulative, cases and deaths for each region
covid_cumulative <- covid_joined %>%
  group_by(region, date) %>%
  summarize(
    cases_cum = sum(cases, na.rm = TRUE),
    deaths_cum = sum(deaths, na.rm = TRUE),
    .groups = "drop") %>% arrange(region, date)

#Pivot data from wide format to long
covid_long <- covid_cumulative %>%
  pivot_longer(
    cols = c(cases_cum, deaths_cum),
    names_to = "Type Sum",
    values_to = "Value")

covid_long <- covid_long %>% drop_na(region)


#plot data in a compelling way (setup, layers, labels, facets, themes)
#faceted plot
ggplot(covid_long, aes(x = date, y = Value, color = `Type Sum`)) +
  geom_line() +
  facet_grid(`Type Sum` ~ region, scales = "free_y") +
  scale_x_date(limits = as.Date(c("2020-03-01", "2020-08-31")), date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Cumulative Cases and Deaths by US Region", subtitle = "COVID-19 Data - ESS 330", x = "Date", y = "Daily Cumulative Values", color = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 10))


