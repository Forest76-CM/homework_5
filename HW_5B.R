library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(broom)
library(ggplot2)
library(scales)
library(forcats)
library(lubridate)


homicides_url <- paste0("https://raw.githubusercontent.com/washingtonpost/",
                        "data-homicides/refs/heads/master/homicide-data.csv")

homicides <- read_csv(homicides_url) %>%
  unite(city_name, city, state, sep = ",", remove =FALSE)

homicides$reported_date <- as.Date(as.character(homicides$reported_date),
                                   format = "%Y%m%d")




# Debugging 
tulsa_records <- homicides %>%
  filter(city == "Tulsa",
         victim_last == "HARRIS")
print(tulsa_records, width = Inf)

# mutating the one record of Tulsa, AL
homicides <- read_csv(homicides_url) %>%
  mutate(state = if_else(city == "Tulsa", "OK", state)) %>%
  unite(city_name, city, state, sep =", ", remove = FALSE)

# Check to see that the single Tulsa, AL record is changed
tulsa_records <- homicides %>%
  filter(city == "Tulsa",
         victim_last == "HARRIS")
print(tulsa_records, width = Inf)

homicides_2 <- homicides %>%
  mutate(
    year = substr(reported_date, 1, 4),
    month = substr(reported_date, 5, 6),
    day = substr(reported_date, 7, 8)
  ) %>%
  select(-reported_date)




# filtering correct
Baltimore_homicides_2 <- homicides %>%
  filter(city_name == "Baltimore, MD") %>%
  mutate(
    year = year(reported_date),
    month = month(reported_date),
    season = case_when(
      month %in% c(05, 06, 07, 08, 09, 10) ~ "Summer",
      TRUE ~ "Winter"
    )
  )

#Counts

monthly_counts_3 <- Baltimore_homicides_2 %>%
  group_by(year, month, season) %>%
  nest() %>%
  mutate(
    count = map_int(data, nrow)
    ) %>%
  select(year, month, count, season)

monthly_counts_3 <- monthly_counts_3 %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


options(repr.plot.width = 20, repr.plot.height =0.02)

# Plot with ggplot2
ggplot(monthly_counts_3, aes(x = date, y = count, fill = season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Summer" = "gray", "Winter" = "lightblue")) +
  labs(x = "Date", y = "Homicide Count", title = "Monthly Homicide Counts in Baltimore") +
  theme_dark() 
  

theme(
  plot.margin = margin(2, 2, 2, 2, unit = "cm"),
  aspect.ratio = 0.5
  