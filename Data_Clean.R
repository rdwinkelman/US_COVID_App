#Load Libraries 
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(here)
library(janitor)

folder <- "Data"
date <- paste0(str_replace_all(Sys.Date(),"-","_"),"_")

# Create tibble of state full names and state abbreviations - will be helpful later when joining different datasets together
state_name_df <- tibble(state_abb = state.abb,
                        state_fullname = state.name) %>%
  bind_rows(tibble(state_abb = c("D.C.","Grand Princess","Diamond Princess"),
                   state_fullname = c("District of Columbia","Grand Princess","Diamond Princess")))

#Part 0: Read in state census data
# Since it may be of interest to take into account the relative size of each state
# We will also use tidycensus package to pull population numbers for each state
# and join it to the testing datasets.  I will pull the population 
# numbers once and then save the file off locally.

#get population data
# census_api_key("key",install=T,overwrite = T)
# 
# states_pop_df_raw  <- tidycensus::get_estimates(geography = "state",product="population")
# states_pop_df_raw %>%  write_rds("2020_03_18_tidycensus_states_pop_df_raw.rds")

states_pop_df_raw <- read_rds(here("Data","2020_03_18_tidycensus_states_pop_df_raw.rds"))

states_pop_df <- states_pop_df_raw %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  clean_names() %>%
  select(-geoid) %>%
  rename(state_fullname = name) %>%
  left_join(state_name_df %>%
              mutate(state_abb = str_remove_all(state_abb,"\\.")),
            by="state_fullname")



#Part 1: Prep JHU State-Level Data 
## BRIEF OVERVIEW
# Ultimately we want to end up with case/death counts each date for each state.
# Although JHU has graciously shared their data with the world, they made 
# several made several different changes to how they reported the data along 
# the way.
#
# To summarize it can be broken up into three different parts.
#   1) From 1/22 through 3/9: reported as county level data in a single .csv
#   2) From 3/10 through 3/22: reported as state level data in the same single .csv
#   3) From 3/23 onward: reported as county level data again in daily .csv files
#
# This different formatting means we will have to clean the data in three steps.
# In the first step we will preserve the county level data and in the subsequent
# steps we will keep the data summarized at the state level.


## Step 0: Read in Data file
# JHU CSSE team hosts their data on their github page. Below is the code I used to 
# grab this data on 3/24. Now the data has now been moved to an archive folder. 
# Since this may be moved again I will save the data off locally as an .rds file.
# Now that it is saved locally I will comment out the github query and just read 
# in the local file directly

# jhu_confirmed_cases_wide <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
#   clean_names() %>%
#   filter(country_region == "US") %>%
#   filter(province_state != "US")
# 
# jhu_confirmed_cases_wide %>%
#   write_rds("2020_03_24_JHU_wide_data.rds")

jhu_confirmed_cases_wide <- read_rds(here("Data","2020_03_24_JHU_wide_data.rds"))



## Step 1 - From 1/22 through 3/9 - Data Summarized at County Level

jhu_confirmed_cases_wide <- read_rds(here("Data","2020_03_24_JHU_Long_Data.rds"))
jhu_confirmed_cases_county_level <- jhu_confirmed_cases_wide %>%
  filter(str_detect(province_state,",|Princess")&str_detect(province_state,"County|Princess|D.C.")) %>% # need to add filter statement to select only county-level and cruise ship data
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  mutate(state_abb = substr(province_state,str_locate(province_state,", ")+2,nchar(province_state))) %>%
  mutate(state_abb = ifelse(is.na(state_abb),province_state,state_abb)) %>%
  left_join(state_name_df,
            by="state_abb") %>%
  filter(date_fmt<=as.Date("2020-03-09")) %>%# only collect data before 3/10/20 in this dataframe - will be using state-level summary data after this date
  select(-dates)

## Step 2 - Data Summarized at State Level (3/10 to 3/22)
filtered_out_province_state <- c("Wuhan Evacuee", "American Samoa", "Diamond Princess", "Grand Princess","Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands",
                                 "Hawaii","Alaska","United States Virgin Islands","Recovered")

jhu_confirmed_cases_state_level <- jhu_confirmed_cases_wide %>%
  filter(!str_detect(province_state,",")) %>% # need to add filter statement to select only state-level and cruise ship data - avaiable on and after 3/10/2020
  filter(! province_state %in% filtered_out_province_state) %>%
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y"),
         state_fullname = province_state) %>%
  left_join(state_name_df,
            by="state_fullname") %>%
  filter(date_fmt>=as.Date("2020-03-10"),
         date_fmt<=as.Date("2020-03-22")) %>% # only collect data from 3/10/20 and onward in this dataframe - will be using state-level summary data 
  select(-dates)


## Step 3 - Data summarized at county-level (3/23 onward) - Broken up into Daily Files 
base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

date_range <- format(seq(Sys.Date()-1,as.Date("2020-03-23"),-1),format="%m-%d-%Y")


jhu_daily_files_compiled_df <- NULL
for(date_i in date_range){
  temp <-read_csv(paste0(base_url,date_i,".csv")) %>%
    clean_names() %>%
    filter(country_region == "US") %>%
    filter(! province_state %in% filtered_out_province_state) %>%
    #mutate(date_fmt = as.Date(last_update)) %>%
    mutate(date_unfmt = str_extract(last_update,"[0-9-/]{1,}"),
           date_fmt = as.Date(parse_date_time(date_unfmt,orders=c("%m/%d/%y","%Y-%m-%d")))) %>%
    mutate(date_str=format(date_fmt,format="%m/%d/%y")) %>%
    group_by(province_state,country_region,date_str,date_fmt) %>%
    summarise(case_count = sum(confirmed)) %>%
    ungroup() %>%
    left_join(jhu_confirmed_cases_state_level %>%
                distinct(province_state,lat,long),
              by="province_state")
  if(is.null(jhu_daily_files_compiled_df)){jhu_daily_files_compiled_df <- temp}
  else{jhu_daily_files_compiled_df <- bind_rows(jhu_daily_files_compiled_df,temp)}
} 

jhu_state_daily_files_compiled_df <- jhu_daily_files_compiled_df %>%
  mutate(state_fullname = province_state) %>%
  left_join(state_name_df,
            by="state_fullname")



## Step 4: Create one composite dataframe
# Now that we have three data frames, we will bind the three separate data frames
# into one data frame


jhu_state_confirmed_cases <- jhu_confirmed_cases_county_level %>%
  bind_rows(jhu_confirmed_cases_state_level %>% 
              select(names(jhu_confirmed_cases_county_level))) %>% 
  bind_rows(jhu_state_daily_files_compiled_df %>% 
              select(names(jhu_confirmed_cases_county_level))) %>%
  left_join(state_name_df %>%
              distinct(state_abb) %>%
              mutate(color_pal = scales::hue_pal()(nrow(.))),
            by="state_abb")



# JHU Death Data
# OK now we will repeat the same steps above for the death counts data. 

## Step 0 - Read in deaths dataset
# jhu_confirmed_deaths_wide <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Deaths_archived_0325.csv") %>%
#   clean_names() %>%
#   filter(country_region=="US") %>%
#   filter(province_state != "US")
#  
# jhu_confirmed_deaths_wide %>% write_rds("2020_03_29_JHU_Wide_Death_data_archived.rds")

jhu_confirmed_deaths_wide <- read_rds(here("Data","2020_03_29_JHU_Wide_Death_data_archived.rds"))

## Step 1 - From 1/22 through 3/9 - Data Summarized at County Level
jhu_confirmed_deaths_county_level <- jhu_confirmed_deaths_wide %>%
  filter(str_detect(province_state,",|Princess")&str_detect(province_state,"County|Princess|D.C.")) %>% # need to add filter statement to select only county-level and cruise ship data
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "death_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  mutate(state_abb = substr(province_state,str_locate(province_state,", ")+2,nchar(province_state))) %>%
  mutate(state_abb = ifelse(is.na(state_abb),province_state,state_abb)) %>%
  left_join(state_name_df,
            by="state_abb") %>%
  filter(date_fmt<=as.Date("2020-03-09")) %>%# only collect data before 3/10/20 in this dataframe - will be using state-level summary data after this date
  select(-dates)



## Step 2 - Data Summarized at State Level (3/10 to 3/22)
filtered_out_province_state <- c("Wuhan Evacuee", "American Samoa", "Diamond Princess", "Grand Princess","Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands",
                                 "Hawaii","Alaska","United States Virgin Islands","Recovered")

jhu_confirmed_deaths_state_level <- jhu_confirmed_deaths_wide %>%
  filter(!str_detect(province_state,",")) %>% # need to add filter statement to select only state-level and cruise ship data - avaiable on and after 3/10/2020
  filter(! province_state %in% filtered_out_province_state) %>%
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "death_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y"),
         state_fullname = province_state) %>%
  left_join(state_name_df,
            by="state_fullname") %>%
  filter(date_fmt>=as.Date("2020-03-10"),
         date_fmt<=as.Date("2020-03-22")) %>% # only collect data from 3/10/20 and onward in this dataframe - will be using state-level summary data 
  select(-dates)


## Step 3 - Data summarized at county-level (3/23 onward) - Broken up into Daily Files 
base_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

date_range <- format(seq(Sys.Date()-1,as.Date("2020-03-23"),-1),format="%m-%d-%Y")


jhu_daily_files_deaths_compiled_df <- NULL
for(date_i in date_range){
  temp <-read_csv(paste0(base_url,date_i,".csv")) %>%
    clean_names() %>%
    filter(country_region == "US") %>%
    filter(! province_state %in% filtered_out_province_state) %>%
    #mutate(date_fmt = as.Date(last_update)) %>%
    mutate(date_unfmt = str_extract(last_update,"[0-9-/]{1,}"),
           date_fmt = as.Date(parse_date_time(date_unfmt,orders=c("%m/%d/%y","%Y-%m-%d")))) %>%
    mutate(date_str=format(date_fmt,format="%m/%d/%y")) %>%
    group_by(province_state,country_region,date_str,date_fmt) %>%
    summarise(death_count = sum(deaths)) %>%
    ungroup() %>%
    left_join(jhu_confirmed_cases_state_level %>%
                distinct(province_state,lat,long),
              by="province_state")
  if(is.null(jhu_daily_files_deaths_compiled_df)){jhu_daily_files_deaths_compiled_df <- temp}
  else{jhu_daily_files_deaths_compiled_df <- bind_rows(jhu_daily_files_deaths_compiled_df,temp)}
} 

jhu_state_daily_files_deaths_compiled_df <- jhu_daily_files_deaths_compiled_df %>%
  mutate(state_fullname = province_state) %>%
  left_join(state_name_df,
            by="state_fullname") %>%
  arrange(state_fullname,date_fmt)



## Step 4: Create one composite dataframe

jhu_state_confirmed_deaths <- jhu_confirmed_deaths_county_level %>%
  bind_rows(jhu_confirmed_deaths_state_level %>% 
              select(names(jhu_confirmed_deaths_county_level))) %>% 
  bind_rows(jhu_state_daily_files_deaths_compiled_df %>% 
              select(names(jhu_confirmed_deaths_county_level))) %>%
  left_join(state_name_df %>%
              distinct(state_abb) %>%
              mutate(color_pal = scales::hue_pal()(nrow(.))),
            by="state_abb")


# Combine JHU cases + death dataset together
jhu_state_combined <- jhu_state_confirmed_cases %>%
  left_join(jhu_state_confirmed_deaths %>%
              select(state_fullname,
                     province_state,
                     date_fmt,
                     death_count),
            by=c("state_fullname",
                 "province_state",
                 "date_fmt")) %>%
  arrange(state_fullname,province_state,date_fmt) %>%
  left_join(states_pop_df %>%
              select(-state_abb),
            by="state_fullname")
# Write off data locally
jhu_state_combined %>% write_rds(here("Data",paste0(date,"jhu_state_combined.rds")))


#Part 2: Prep JHU Overall U.S. Data 
# Besides state-level data JHU also provides data on overall numbers for each 
# country. We will use overall numbers to track progression for overall U.S.


# Step 1: JHU global case data for U.S.
jhu_global_confirmed_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
  clean_names() %>%
  filter(country_region=="US") %>%
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  select(-dates)

# Step 2: JHU global death data for U.S.
jhu_global_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
  clean_names() %>%
  filter(country_region=="US") %>%
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "death_count")%>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y")) %>%
  select(-dates)

# Step 3: Combinde JHU case and death datasets
jhu_global_combined <- jhu_global_confirmed_cases %>%
  left_join(jhu_global_deaths %>%
              distinct(country_region,date_fmt,death_count)) %>%
  mutate(date_fmt_num = as.numeric(date_fmt),
         case_total=case_count,
         death_total=death_count) %>%
  arrange(country_region,date_fmt) %>%
  group_by(country_region) %>%
  mutate(case_total_daily = case_total - lag(case_total),
         case_total_daily = ifelse(is.na(case_total_daily),0,case_total_daily),
         death_total_daily = death_total - lag(death_total),
         death_total_daily = ifelse(is.na(death_total_daily),0,death_total_daily)) %>%
  ungroup()

# Step 4: Write off data locally

jhu_global_combined  %>% write_rds(here("Data",paste0(date,"jhu_global_combined.rds")))




#Part 3: COVID Testing Data from COVID Tracking Project



# Step 1: Read in State Data
covid_tracking_states <- read_csv("https://covidtracking.com/api/states/daily.csv") %>%
  clean_names() %>%
  rename(state_abb = state) %>% 
  left_join(read_csv("https://covidtracking.com/api/states.csv") %>%
              clean_names() %>%
              rename(state_abb = state) %>%
              select(state_abb,grade),
            by="state_abb")


# Step 2: Join Census data to COVID tracking project data
covid_tracking_states_w_pop <- covid_tracking_states %>%
  left_join(states_pop_df,
            by="state_abb") %>%
  left_join(jhu_state_combined %>%
              distinct(state_abb,color_pal),
            by="state_abb") %>%
  mutate(date_fmt = as.Date(date_checked),
         total_per_pop = (total/pop*1e6),
         pct_positive=round_half_up(positive/total*100)) %>%
  select(-c(date,date_checked)) %>%
  select(date_fmt,everything()) %>%
  mutate(test_total = total,
         test_total_per_100k = round_half_up(test_total/pop*1e5,digits=1)) %>%
  arrange(state_fullname,date_fmt) %>%
  group_by(state_fullname,state_abb) %>%
  mutate(test_total_daily = test_total - lag(test_total),
         test_total_daily = ifelse(is.na(test_total_daily),0,test_total_daily),
         test_total_daily_per_100k =  round_half_up(test_total_daily/pop*1e5,digits=1)) %>%
  ungroup()

# Step 4: Write off data locally

covid_tracking_states_w_pop %>%
  write_rds(here("Data",paste0(date,"CTP_Data.rds")))

