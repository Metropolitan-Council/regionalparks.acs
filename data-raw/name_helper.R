# prepare name_helper
library(tidyverse)


name_helper <- as_tibble(names(as_tibble(block_group_map))) %>%
  rename(acscode = value) %>%
  filter(str_detect(acscode, "percent|meanhhinc")) %>%
  # mutate(acscode = gsub("meanhhinc", "meanhhinc_per", acscode)) %>%
  mutate(dropdownname = acscode %>%
    str_remove("_percent") %>%
    str_replace("_", "-") %>%
    str_replace("age", "Age, % ") %>%
    str_replace("under15", "under 15") %>%
    str_replace("-up", "+") %>%
    if_else(grepl("nh$", .), paste0("Race, % ", .), .) %>%
    str_replace("amindnh", "American Indian") %>%
    str_replace("asiannh", "Asian") %>%
    str_replace("blacknh", "Black") %>%
    str_replace("othermultinh", "Other + Multiracial") %>%
    str_replace("whitenh", "White") %>%
    if_else(str_detect(., "hisppop"), paste0("Ethnicity, % ", .), .) %>%
    str_replace("nothisppop", "not-Hispanic") %>%
    str_replace("hisppop", "Hispanic") %>%
    str_replace("lep-span", "Language, % Spanish speakers") %>%
    str_replace("lep", "Language, % limited English proficiency") %>%
    str_replace("hh-noveh", "Transportation, % Households without a vehicle") %>%
    str_replace("pov185", "Socioeconomic, % below 185% poverty line") %>%
    str_replace("meanhhinc-per", "Socioeconomic, Mean household income ($)")) %>%
  # values from census tract aren't automatically inserted yet
  rbind(
    c("adj_ambdis_per", "Disability, % ambulatory disability"),
    c("adj_anydis_per", "Disability, % any other disability"),
    c("adj_forborn_per", "Origin, % foreign-born"),
    c("adj_usborn_per", "Origin, % US-born")
  ) %>%
  mutate(order = if_else(grepl("under", dropdownname), 1, NA_real_)) %>%
  arrange(order, dropdownname) %>%
  mutate(category = case_when(
    str_detect(dropdownname, "Age") ~ "Age",
    str_detect(dropdownname, "Disability") ~ "Disability",
    str_detect(dropdownname, "Race|Ethnicity") ~ "EthRace",
    str_detect(dropdownname, "Language") ~ "Language",
    str_detect(dropdownname, "Origin") ~ "Origin",
    str_detect(dropdownname, "Socioeconomic") ~ "Socioeconomic",
    str_detect(dropdownname, "Transportation") ~ "Transportation",
    TRUE ~ "other"
  )) %>%
  # include html for leafletlegends
  mutate(
    leglab = case_when(
      category == "Age" ~ paste0("% population<br>age ", gsub("Age, % ", "", dropdownname)),
      category == "Disability" ~ paste0("% population with<br>", gsub("Disability, % ", "", dropdownname)),
      category == "EthRace" ~ paste0(gsub("Race, |Ethnicity, ", "", dropdownname), "<br>population"),
      category == "Origin" ~ paste0(gsub("Origin, ", "", dropdownname), "<br>population"),
      acscode == "lep_percent" ~ "% population with<br>limited English proficiency",
      acscode == "lep_span_percent" ~ "% population primarily<br>speaking Spanish",
      acscode == "hh_noveh_percent" ~ "% households<br>without a vehicle",
      acscode == "pov185_percent" ~ "% households below<br>185% poverty line",
      acscode == "meanhhinc_per" ~ "Mean household<br>income ($)"
    ),
    leglab = gsub("age under", "under age", leglab)
  ) %>%
  # include column for popup labels
  mutate(popuplab = gsub("<br>", " ", leglab)) %>%
  # include column for buffer geos --> ideally would like to streamline this, but it works for now
  mutate(
    buffercode = paste0("adj_", gsub("percent", "per", acscode)),
    buffercode = gsub("adj_adj_", "adj_", buffercode)
  )

usethis::use_data(name_helper, overwrite = TRUE)
