library(tidyverse)
bc_pop <- cansim::get_cansim("17-10-0005-01") %>%
  janitor::clean_names() %>%
  filter(
    geo == "British Columbia",
    !str_detect(age_group, "to"),
    !str_detect(age_group, "and over"),
    !str_detect(age_group, "Median"),
    !str_detect(age_group, "Average"),
    !str_detect(age_group, "All"),
    sex != "Both sexes"
  ) %>%
  mutate(
    age_group = as.numeric(gsub(".*?([0-9]+).*", "\\1", age_group)),
    ref_date = as.numeric(ref_date)
  ) %>%
  select(ref_date, value, age_group, sex)
#population projections need to be manually downloaded :( from https://bcstats.shinyapps.io/popApp/ ---------
projections <- read_csv(here::here("raw_data", "Population_Projections.csv")) %>%
  select(-Region, -`Regional District`, -Total) %>%
  pivot_longer(cols = c(-"Year", -"Gender"), names_to = "age_group", values_to = "value") %>%
  rename(
    sex = Gender,
    ref_date = Year
  ) %>%
  filter(
    !str_detect(age_group, "\\+"),
    sex != "T"
  ) %>%
  mutate(
    sex = if_else(sex == "M", "Males", "Females"),
    age_group = as.numeric(age_group)
  )

past_and_future <- bind_rows(bc_pop, projections)

write_rds(past_and_future, here::here("processed_data", "bc_pop.rds"))

retirements <- cansim::get_cansim("14-10-0126-01") %>%
  janitor::clean_names() %>%
  filter(
    geo == "British Columbia",
    reason == "Retired",
    characteristics == "Total, unemployed and not in the labour force",
    sex %in% c("Males", "Females"),
    age_group == "15 years and over"
  ) %>%
  select(ref_date, val_norm, reason, sex, age_group)

write_rds(retirements, here::here("processed_data", "retirements.rds"))

temp <- cansim::get_cansim("14-10-0017-01")

participation <- temp%>%
  janitor::clean_names()%>%
  filter(geo=="British Columbia",
        labour_force_characteristics=="Participation rate",
        sex!="Both sexes",
        age_group=="50 to 54 years")%>%
  select(Date=ref_date,`Participation Rate`=val_norm,Sex=sex)%>%
  mutate(Date=lubridate::ym(Date))

write_rds(participation, here::here("processed_data", "participation.rds"))

