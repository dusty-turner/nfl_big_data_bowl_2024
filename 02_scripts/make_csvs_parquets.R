library(tidyverse)
library(arrow)

data <- list.files("nfl-big-data-bowl-2024/")  
takles <-  read_parquet("01_data/tackles.parquet")

read_csv_change_to_parquet <- function(path = data[1]){
  read_csv(file = str_c("nfl-big-data-bowl-2024/", path)) %>%  
    write_parquet(x = ., sink = str_c("01_data/", str_replace(string = path, pattern = "csv", replace = "parquet")))
}

data |> walk(.f = ~read_csv_change_to_parquet(path = .x))