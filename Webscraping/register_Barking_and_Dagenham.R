library(tabulapdf)
library(janitor)
library(tidyr)
library(dplyr)

register_2025 <- "https://www.lbbd.gov.uk/sites/default/files/2025-08/Public%20Register.pdf"

table <- tabulapdf::extract_tables(register_2025)

page_one <- base::as.data.frame(table[1]) %>% 
  #Drop empty column
  dplyr::select(!("...13")) 

# Tidy column names
names_one <- colnames(page_one)
names_two <- head(page_one, 2) %>% tidyr::fill(all_of(names_one), .direction="updown") %>% head(1) %>% as.character((.))
all_names <- paste0(names_one, " ", names_two)
page_one_data <- tail(page_one, -2)
colnames(page_one_data) <- all_names

# Remove empty column
page_one_data <- subset(page_one_data, select = -c(`...4 Premises Name`))

# Handle multi-row groups
test <- page_one_data %>%
  dplyr::mutate_all(vctrs::vec_fill_missing, direction="down", max_fill = 1) %>%
  dplyr::mutate_all(vctrs::vec_fill_missing, direction="up", max_fill = 1)

