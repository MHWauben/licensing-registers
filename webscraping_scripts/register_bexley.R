library(dplyr)
library(tidyr)
library(rvest)


register <- read.csv("https://documents.sevenoaks.gov.uk/licensing/Trackers/Bexley/BexleyPREM.csv") %>%
  tidyr::separate("Reference.URL", sep="keyVal=", into=c("url", "key"))

get_table_info <- function(key="IIJ1YGBE3A000", activeTab="summary") {
  URL_string <- paste0("https://pa.sevenoaks.gov.uk/online-licensing/licencingDetails.do?activeTab=",activeTab, "&keyVal=",key)
  table_info <- rvest::read_html(URL_string) %>%
    rvest::html_element("table") %>%
    rvest::html_table()
  table_info <- 
  return(table_info)
}
