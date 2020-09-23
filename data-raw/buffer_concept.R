library("tidyverse")
library("kableExtra")
load("./data/buffer_distances.rda")

# dynamic plot/table combo that shows values of any block groups 
# which intersects a given buffer around each park or trail segment

# user selected variables
chosen_distance = "1 mi"
chosen_variable = "ageunder18_percent"
chosen_agency = "St. Paul Parks and Recreation"

#dynamic plot
buffer_distances[[chosen_distance]] %>% 
  filter(agency == chosen_agency)  %>%
  select(NAME, agency, chosen_variable, status) %>%
  rename(RESPONSE = chosen_variable) %>%
  ggplot(aes(x = NAME, y = RESPONSE, col = status, pch = status)) + 
  geom_jitter(height = 0, width = .2, alpha = .5)+
  stat_summary(position = position_dodge(width = .4), fun = "mean", 
               aes(fill= status), col = "black", size = .8) +
  scale_fill_manual(values = c("park" = "#0875C3", 
                                "park_planned" = "#A16C4C", 
                                "trail" = "#A14D5D",
                                "trail_planned" = "#643967", 
                                "trail_search" = "#F6BD9C")) +
  scale_color_manual(values = c("park" = "#0875C3", 
                                "park_planned" = "#A16C4C", 
                                "trail" = "#A14D5D",
                                "trail_planned" = "#643967", 
                                "trail_search" = "#F6BD9C")) +
  scale_shape_manual(values = c("park" = 21, 
                                "park_planned" = 22, 
                                "trail" = 23,
                                "trail_planned" = 24, 
                                "trail_search" = 25)) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 40, hjust = 1)) +
  labs(x = "Park or trail name", 
       y = chosen_variable, 
       title = paste(chosen_agency, chosen_distance, sep = " - buffer "),
       fill = "Type", col = "Type", shape = "Type")

#dynamic table
kable(buffer_distances[[chosen_distance]] %>% 
  as_tibble() %>%
  filter(agency == chosen_agency)  %>%
  select(NAME, agency, chosen_variable, status) %>%
  rename(RESPONSE = chosen_variable) %>%
    filter(!is.na(RESPONSE)) %>%
  group_by(NAME, agency, status) %>%
  summarise(`Num intersecting block groups` = n(), Mean = mean(RESPONSE), `Std. Dev.` = sd(RESPONSE))) %>%
  column_spec(1:2, width = "8cm") %>%
  column_spec(4:6, width = "3cm") %>%
  kable_styling(fixed_thead = T) 
