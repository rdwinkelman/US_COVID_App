# Load libraries
library(sf)
library(maps)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(shinyMobile)
library(shinyWidgets)
library(shiny)
library(plotly)

# Load data

## Step 1: Read in data frame of state polygons and convert to sf
states <- st_as_sf(maps::map(database = "state",plot=F,fill=T))

## Step 2: Read in JHU Combined Dataset
jhu_state_confirmed_cases <- read_rds("2020_03_29_jhu_state_combined.rds") %>%
  mutate(case_count = ifelse(is.na(case_count),0,case_count))

## Step 3: Reaggregate JHU Combined dataset at state-level 
jhu_state_sum_confirmed_cases <- jhu_state_confirmed_cases %>%
  group_by(state_fullname,state_abb,date_fmt,color_pal) %>%
  summarise(case_total = sum(case_count,na.rm=T),
            death_total = sum(death_count,na.rm=T)) %>%
  ungroup() %>%
  group_by(state_fullname,state_abb,color_pal) %>%
  mutate(case_total_daily = case_total - lag(case_total),
         case_total_daily = ifelse(is.na(case_total_daily),0,case_total_daily),
         death_total_daily = death_total - lag(death_total),
         death_total_daily = ifelse(is.na(death_total_daily),0,death_total_daily)) %>%
  ungroup() %>%
  filter(!str_detect(state_fullname,"Princess")) %>%
  mutate(scaled_case_total = ((case_total-1)/(max(case_total)-1)),
         scaled_death_total = ((death_total-1)/(max(death_total)-1)),
         state_abb = case_when(str_detect(state_abb,"\\.")~str_replace_all(state_abb,"\\.",""),
                               T~state_abb))  

## Step 4: Read in Overall U.S. Data
jhu_global_combined <- read_rds("2020_03_29_jhu_global_combined.rds")

## Step 5: Read in COVID Tracking Project Data on Testing
covid_tracking_states_w_pop <- read_rds("2020_03_29_CTP_Data.rds") %>%
  mutate(test_total = total) %>%
  arrange(state_fullname,date_fmt) %>%
  group_by(state_fullname,state_abb) %>%
  mutate(test_total_daily = test_total - lag(test_total),
         test_total_daily = ifelse(is.na(test_total_daily),0,test_total_daily)) %>%
  ungroup()

