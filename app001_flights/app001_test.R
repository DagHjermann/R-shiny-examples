
library(dplyr)
library(nycflights13)

input <- list()

# ?airports
# ?flights

table(flights$origin)
table(flights$month)

origin_airports <- unique(flights$origin)

origin_table_data <- airports %>%
  filter(faa %in% origin_airports)

input$origin_table_rows_selected <- 1:2

origin_table_data_sel <- origin_table_data[input$origin_table_rows_selected,]

flights_by_month <- flights %>%
  filter(origin %in% origin_table_data_sel$faa) %>%
  count(month, name = "Number_of_flights")

input$flights_by_month_table_rows_selected <- 6:7

flights_month_by_dest <- flights %>%
  filter(origin %in% origin_table_data_sel$faa,
         month %in% input$flights_by_month_table_rows_selected) %>%
  count(dest, name = "Number_of_flights")

