library(tabulapdf)
library(janitor)
library(tidyr)
library(dplyr)

register_2025 <- "https://www.lbbd.gov.uk/sites/default/files/2025-08/Public%20Register.pdf"

table_one <- tabulapdf::extract_tables(register_2025, pages = 1, output="tibble")

page_one <- base::as.data.frame(table_one[1]) 

# Tidy column names
names_one <- colnames(page_one)
names_two <- head(page_one, 2) %>% tidyr::fill(all_of(names_one), .direction="updown") %>% head(1) %>% as.character((.))
all_names <- paste0(names_one, " ", names_two)
page_one_data <- tail(page_one, -2)
colnames(page_one_data) <- all_names

# Handle multi-row groups
multi_row_groups <- function(data, grouping_columns){
  summarised_data <- data %>%
    # Fill license numbers etc. up and down
    dplyr::mutate_at(grouping_columns, vctrs::vec_fill_missing, direction="down", max_fill = 1) %>%
    dplyr::mutate_at(grouping_columns, vctrs::vec_fill_missing, direction="up", max_fill = 1) %>%
    replace(is.na(.), "") %>%
    # Concatenate hours of operations
    dplyr::group_by_at(grouping_columns) %>%
    dplyr::summarise_all(paste0, collapse = "") %>% 
    dplyr::ungroup()
  return(summarised_data)
}
tidied_page_one <- multi_row_groups(page_one_data, colnames(page_one_data)[1:7])
# Remove unnecessarily duplicated column
tidied_page_one <- tidied_page_one %>% 
  dplyr::select(-`...4 Premises Name`) %>% 
  dplyr::select(-`...13 NA`)


# Handle other data pages
page_areas <- list(c(625.6, 468.7, 4820, 6500))
table_two <- tabulapdf::extract_tables(register_2025, pages = 2, guess=FALSE, area=page_areas, col_names=FALSE, output="tibble")
table_two <- as.data.frame(table_two) %>% 
  # Empty columns at position 15, not picked up
  dplyr::mutate(X145 = "") %>% 
  relocate(X145, .before ='X15')
colnames(table_two) <- colnames(tidied_page_one)
tidied_page_two <- multi_row_groups(table_two, colnames(table_two)[1:6])

table_three <- tabulapdf::extract_tables(register_2025, pages = 3, guess=FALSE, area=page_areas, col_names=FALSE, output="tibble")
table_three <- as.data.frame(table_three) %>% 
  # Empty columns at position 15, not picked up
  dplyr::mutate(X145 = "") %>% 
  relocate(X145, .before ='X15')
colnames(table_three) <- colnames(tidied_page_one)
tidied_page_three <- multi_row_groups(table_three, colnames(table_three)[1:6])

table_four <- tabulapdf::extract_tables(register_2025, pages = 4, guess=FALSE, area=page_areas, col_names=FALSE, output="tibble")
table_four <- as.data.frame(table_four)
colnames(table_four) <- colnames(tidied_page_one)
tidied_page_four <- multi_row_groups(table_four, colnames(table_four)[1:6])

table_five <- tabulapdf::extract_tables(register_2025, pages = 5, guess=FALSE, area=page_areas, col_names=FALSE, output="tibble")
table_five <- as.data.frame(table_five)%>% 
  # Empty columns at positions 18, 19, 20, not picked up
  dplyr::mutate(X18 = "", X19 = "", X20 = "")
colnames(table_five) <- colnames(tidied_page_one)
tidied_page_five <- multi_row_groups(table_five, colnames(table_five)[1:6])


# add all tables together
barking_dagenham_register <- rbind(tidied_page_one, tidied_page_two, tidied_page_three, tidied_page_four, tidied_page_five)
clean_names = c(
  "Licence Number",
  "Date Licence Granted",
  "Premises Name", 
  "Premises Address", 
  "Licence Holder",
  "Designated Premises Supervisor",
  "Supply of Alcohol",
  "Late Night Refreshment",
  "Live Music",
  "Recorded Music",
  "Opening Hours",
  "Plays",
  "Films",
  "Indoor Sporting Events",
  "Boxing or Wrestling Entertainment",
  "Performance of Dance",
  "Other Entertainment Similar to Live or Recorded Music or Dance Performance",
  "Facilities for Making Music",
  "Facilities for Dancing",
  "Other Entertainment Facilities Similar to Making Music or Dancing"
)
colnames(barking_dagenham_register) <- clean_names
write.csv(barking_dagenham_register,"scraped_data/register_barking_and_dagenham.csv", row.names = FALSE)
