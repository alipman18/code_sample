library(tidyverse)
library(httr)

# Downloading Cook County tax data on commercial properties 2021 from Cook County's data catalog
# Creating the query
base_url <- "https://datacatalog.cookcountyil.gov/resource/uzyt-m557.json"
classes <- 499:600 # find all class 5 commercial properties
class_conditions <- paste0("class = '", classes, "'")
class_query <- paste(class_conditions, collapse = " OR ")
year_condition <- "year == 2021"
where_condition <- paste0("(", class_query, ") AND ", year_condition)

# Exeucting the query
all_assessments <- GET(
  base_url,
  query = list(
    `$select` = paste0(c("year", "pin", "class", "mailed_tot", "certified_tot", "board_tot"), collapse = ","),
    `$where` = where_condition,
    `$limit` = 5000000L
  )
)

# Converting to dataframe
all_assessments <- fromJSON(rawToChar(all_assessments$content))

# Creating a variable that shows whether reductions were granted by Assessor's office or by the Board of Review
all_assessments_final <- all_assessments %>%
  mutate(ccao_reduction = ifelse(mailed_tot != certified_tot, 1, 0),
         bor_reduction = ifelse(certified_tot != board_tot, 1, 0),
         year = as.numeric(year))

# Creating a dataframe that summarizes reductions granted vs. average property value
assessment_summary <- all_assessments_final %>%
  na.omit() %>%
  mutate(across(mailed_tot:board_tot, ~ as.numeric(.x)),
         ccao_red_amt = mailed_tot - certified_tot,
         bor_red_amt = certified_tot - board_tot) %>%
  group_by(ccao_reduction, bor_reduction) %>%
  summarize(avg_value = mean(mailed_tot)) %>%
  mutate(ccao_bor = paste0(ccao_reduction, bor_reduction)) %>% # create a unique identifier
  ungroup() %>%
  select(c(-ccao_reduction, -bor_reduction))

# Visualizing relationship in a bar chart format  
assessment_summary %>%
  ggplot(aes(x = ccao_bor, y = avg_value)) +
  geom_col(color = "#0c2c84", fill = "#0c2c84") +
  labs(title = "Average Property Value Varies with Reductions Granted",
       x = "",
       y = "Average Property Value") +
  geom_text(aes(label = scales::dollar(avg_value)), vjust = 2, color = "white") + 
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(labels = c("No Reduction", "BOR Only", "CCAO Only", "BOR and CCAO")) +
  theme_classic() +
  theme(plot.title = element_text(size = 11, hjust = 0.5))
  
