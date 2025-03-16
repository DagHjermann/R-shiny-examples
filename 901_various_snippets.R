
library(dplyr)
library(readr)
library(ggplot2)

# create dataset
dat <- readRDS("../seksjon 212/Milkys2_pc/Files_from_Jupyterhub_2022/Raw_data/105_data_with_uncertainty_2024-09-22.rds")

milkys_example <- dat %>%
  group_by(STATION_CODE) %>%
  mutate(
    last_year = max(MYEAR),
    n_years = length(unique(MYEAR))) %>%
  ungroup() %>%
  filter(last_year == 2022,
         n_years >= 5,
         LATIN_NAME %in% c("Gadus morhua", "Mytilus edulis", "Nucella lapillus", "Somateria mollissima"),
         PARAM %in% c("CB118", "CD", "PFOA", "VDSI", "DDEPP")) %>%
  group_by(STATION_CODE, LATIN_NAME, PARAM, MYEAR) %>%
  summarize(
    concentration = signif(median(VALUE_WW), 4), 
    loq = ifelse(mean(FLAG1 %in% "<"), "Below LOQ", "Above LOQ"), .groups = "drop")

milkys_example_coord <- readxl::read_excel("../seksjon 212/Milkys2_pc/Files_to_Jupyterhub_2021/Kartbase_edit.xlsx")

write_csv(milkys_example, "input_data/milkys_example.csv")
write_csv(milkys_example_coord, "input_data/milkys_example_coord.csv")

if (FALSE){
  # test 
  read_csv("input_data/milkys_example.csv") %>%
    ggplot(aes(MYEAR, concentration)) +
    geom_point() +
    facet_wrap(vars(PARAM, STATION_CODE))
}

