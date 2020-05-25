library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)
library(reshape2)
library(lubridate)
source("mock.R")

options("lubridate.week.start" = 1)
monthdf <- function(k){

  #set ceiling as a month ahead
  end_date <- ceiling_date(k,unit = "months")-1
  #create a sequence of days from 7 days before the floor to 7 days after the ceiling by day
  seq_days=seq(k-7,end_date+7,by=1)

  #create a tibble for relevant infromation
  dates <-
    tibble(
      day       = seq_days,
      wday_name = wday(day, label = TRUE, abbr = TRUE),
      weekend   = wday(day) > 5,
      isoweek   = isoweek(day),
      month     = month(day, label = TRUE, abbr = FALSE),
      isoyear   = isoyear(day),
      week_year = fct_rev(sprintf("%s - %s", isoyear, isoweek)))
  #create mockdata 
  df <- create_mock_data()
  #right join the mock data with dataframe by day
  df <- merge(dates,df,by = "day",all.x = TRUE)
  #ensure ony unique rows fill up the dataframe
  df <- distinct(df)
  #new dataframe,select day,clin_visit,dial
  df_new <- df %>% 
    select(day,clin_visit,dial)
  #more mockdata
  df_new$dial_time= ifelse(df$dial == TRUE, 4, NA)

  clin_log <- which(df_new$clin_visit == TRUE)
  df_new$clin_time = NA
  df_new$clin_time[clin_log] = sample(c("08:00:00","09:30:00","12:00:00","14:00:00","17:00:00"),length(clin_log),replace = TRUE)
  
  df_new$dr_name = sample(c("Dr. Thicc","Dr. PePe", "Dr.Ligma"), nrow(df_new),replace= TRUE)
  
  
  ## -- Edited
  
  df_new$clin_visit_type = sample(c("Physical Therapy", "Blood Test"), nrow(df_new),replace= TRUE)
  
  
  ##
  
  
  #drop clinvisit and dial columns
  df_new = df_new %>% 
    select(-c(clin_visit,dial))
  #right join wtih original column
  df_all <-merge(df,df_new, by = "day",all.x=TRUE)
  #create new column identifying whether there is a scheduled clinical visit OR dialysis schedule
  df_all$event <- ifelse((!is.na(df$clin_visit)|!is.na(df$dial)),1,0)
  #create a matrix for heaplot
  mat_plot <- acast(df_all, week_year~wday_name, value.var='event')
  #set na values as 0 in matrix
  mat_plot[is.na(mat_plot)] <- 0
  
  
  return(list(forplot <- mat_plot, fordata <-df_all))
}
