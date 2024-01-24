library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

select(surveys, plot_id, species_id, weight)

select(surveys, -record_id, -species_id)

filter(surveys, year == 1995)


surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)

surveys_sml2 <- select(filter(surveys, weight < 5), species_id, sex, weight)

surveys %>%
  filter(weight<5) %>%
  select(species_id, sex, weight)

surveys%>%
  filter(year < 1995)%>%
  select(year, sex, weight)

surveys_weight_conv <- surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight/1000, weight_lb*2.2)%>%
  select(-weight)




#
surveys %>%
  filter(!is.na(sex))%>%
  group_by(sex) %>%
  summarise(mean_weight = mean(weight, na.rm = T))

surveys %>%
  filter(!is.na(sex)%>%
  group_by(sex) %>%
  mean(weight, na.rm = T))

surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight = mean(weight, na.rm = T)) %>%
  tail()

surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight = mean(weight, na.rm = T)) %>%
  print(n = 15)

# Challenge
surveys$plot_type <- factor(surveys$plot_type)
nlevels(surveys$plot_type)

# Challenge solutions
#1
surveys%>%
  count(plot_type)
#2
surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  group_by(species_id) %>%
  summarise(
    mean_hindfoot_length = mean(hindfoot_length),
    min_hindfoot_length = min(hindfoot_length),
    max_hindfoot_length = max(hindfoot_length),
    n = n()
  ) %>%
  View()
#3
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species_id, weight) %>%
  arrange(year)




# long to wide
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarise(mean_weight = mean(weight))

str(surveys_gw)

surveys_wide <- surveys_gw%>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)


# wide to long
surveys_long <- surveys_wide %>%
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id)

str(surveys_long)

# challenge
surveys_long <- surveys %>%
  pivot_longer(names_to = "measurement", values_to = "values", cols = c(weight, hindfoot_length)) %>%
  view()

surveys_long %>%
  group_by(year, measurement, plot_type) %>%
  summarize(mean_value = mean(values,na.rm = T)) %>%
  pivot_wider(names_from = measurement, values_from = mean_value)

###
surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

write_csv(surveys_complete, file "data_raw/surveys_complete.csv")
          