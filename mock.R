library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)
library(lubridate)


create_mock_data <- function(){
  df <- read.csv("MOCK_DATA.csv") %>% rename(day = id)
  df <- distinct(df)
  df1 <- df %>% mutate(day = sort(ymd(day)))
  df2 <- df %>% mutate(day = sort(ymd(day)))
  
  clin_visit <- sample(c(TRUE, NA,NA,NA,NA,NA,NA,NA), nrow(df1), replace = TRUE)
  df1$clin_visit <- clin_visit
  
  dialysis <- c(TRUE,NA)
  df2$dial <- sample(dialysis,nrow(df2),replace = TRUE)
  
  df <- merge(df1,df2, by = "day",all.x = TRUE)
}