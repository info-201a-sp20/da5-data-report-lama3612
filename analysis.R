library("dplyr")
library("ggplot2")
library("plotly")
library("leaflet")
library("httr")

####### Summary information
shootings <- read.csv("C:/Users/Alex/Desktop/Info 201/da5-data-report-lama3612/data/shootings-2018.csv",header = TRUE, stringsAsFactors = FALSE)
column_names <- colnames(shootings)
number_col <- ncol(shootings)

number_shootings <- nrow(shootings)

lives_lost <- sum(shootings$num_killed)

## most casualities
most_impacted <- filter(shootings, num_killed == max(shootings$num_killed))
name_most_impacted <- select(most_impacted, "city")

## most injured
most_injured <- filter(shootings, num_injured == max(shootings$num_injured))
name_most_injured <- select(most_injured, "city")

## state with most fatalities on average
grp <- group_by(shootings, state)
summary_state_deaths <- summarize(grp, sum_death = sum(num_killed), sum_inj = sum(num_injured), total_impacted = sum_death + sum_inj)
max_death <- filter(summary_state_deaths, sum_death == max(summary_state_deaths$sum_death))
name_max_death <- select(max_death, "state")

####### Summary Table
grp_summary <- grp %>%
  mutate(avg_killed = mean(num_killed), avg_injured = mean(num_injured)) %>%
  summarize(total_deaths = sum(num_killed), total_injuries = sum(num_injured), total_impacted = total_deaths + total_injuries)
total_impacted_sum <- sum(grp_summary$total_impacted)

####### Description of a particular incident
cali_case <- grp %>%
  mutate(avg_killed = mean(num_killed), avg_injured = mean(num_injured), total_impacted = num_killed + num_injured) %>%
  filter(state == "California") %>%
  filter(total_impacted == max(total_impacted))

####### An interactive map
interactive_map <- leaflet(data = shootings) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lat = ~lat, lng = ~long, popup = paste("Date:", shootings$date, "<br>", "City:", shootings$city, "<br>",
                                                    "Address:", shootings$address, "<br>", "Killed:", shootings$num_killed, "<br>",
                                                    "Injured:", shootings$num_injured), label = "Click me for more info",
                                                    radius = (shootings$num_injured + shootings$num_killed))

####### Plot of my choice
plot_df <- summarize(grp, sum_death = sum(num_killed), sum_inj = sum(num_injured), total_impacted = sum_death + sum_inj) %>%
  arrange(-total_impacted) %>% slice(1:5)
bar_chart <- ggplot(plot_df) + 
  geom_col(mapping = aes(x = state, y = total_impacted, fill = total_impacted), position = "dodge") +
  labs(title = "Top 5 States With the Most Impacted Individuals Overall") + 
  geom_text(aes(x = state, y = total_impacted, label = total_impacted), nudge_y = 5)

           