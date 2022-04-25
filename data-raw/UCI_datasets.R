## code to prepare `UCI_datasets` dataset goes here

# #read in the data from the data-raw folder
# UCI_datasets <- readr::read_csv("data-raw/UCI_datasets.csv", na = "")
# 
# #clean up the names
# names(UCI_datasets) <- c("name", "data_types", "default_task", "attribute_types", "num_instances", "num_attributes", "year")
# 
# #remove the whitespace
# UCI_datasets <- purrr::map_df(UCI_datasets, stringr::str_trim)
# 
# #convert to numeric
# UCI_datasets$num_attributes <- as.numeric(UCI_datasets$num_attributes)
# UCI_datasets$num_instances <- as.numeric(UCI_datasets$num_instances)
# UCI_datasets$year <- as.numeric(UCI_datasets$year)

library(rvest)
library(tidyverse)

url <- read_html("https://archive.ics.uci.edu/ml/datasets.php")

# Get all the <a> nodes on the page
links <- url %>%
  html_nodes("tr") %>%
  html_nodes("a")

# match up the hrefs with the corresponding text
urls <- tibble(
  url = links %>%
    html_attr("href"),
  name = links %>%
    html_text()
) %>%
  filter(name != "") %>%
  distinct(url, .keep_all = TRUE)

large_table <- url %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE) 

UCI_datasets <- large_table[[6]]
UCI_datasets <- UCI_datasets[-1, -1:-2]


names(UCI_datasets) <- c("name", "data_types", "default_task", "attribute_types", "num_instances", "num_attributes", "year")

UCI_datasets <- UCI_datasets %>%
  filter(!is.na(name))

UCI_datasets <- UCI_datasets %>%
  mutate(
    data_types = ifelse(data_types == "", NA, data_types),
    default_task = ifelse(default_task == "", NA, default_task),
    attribute_types = ifelse(attribute_types == "", NA, attribute_types),
    num_instances = ifelse(num_instances == "", NA, num_instances),
    num_attributes = ifelse(num_attributes == "", NA, num_attributes),
    year = ifelse(year == "", NA, year)
  ) %>% 
  left_join(urls, by = "name")

# duplicates
UCI_datasets %>%
  group_by(name) %>%
  count(2) %>%
  filter(n > 1)


usethis::use_data(UCI_datasets, overwrite = TRUE)
