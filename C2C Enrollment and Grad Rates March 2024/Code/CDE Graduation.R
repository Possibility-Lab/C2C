# -------------------------------------------------------------------------
# Merging and Cleaning CDE Graduation Rate Data for C2C
# by Randy Clopton (Possibility Lab at UC Berkeley)
# Prepared March 2024
# -------------------------------------------------------------------------
#
# This file is designed to take the raw graduation rate data collected from the 
# California Dept of Ed and merge it into one clean file for all years. The 
# code is designed to be run for as many years of data as necessary using CDE's
# current data formatting. To add new datafiles, go to the links below and
# download the year desired, then name it "CDE Graduation YYYY-YYYY" for the 
# given schoolyear. This will ensure the code runs properly and creates the 
# proper datafile.
# 
# Links:
# https://www.cde.ca.gov/ds/ad/filesoygrads.asp
# https://www.cde.ca.gov/ds/ad/filesgrad.asp

# Package Load ------------------------------------------------------------

library(tidyverse)
library(readxl)


# Load and Merge Data -----------------------------------------------------
## CDE changed their reporting methods at some point during the timeframe of data this file works with. There are two separate import protocols as a result.
## For txt files (Historical Rates)

gradfiles_txt=list.files(path="Raw Data", pattern="^CDE Graduation.+txt$", full.names=TRUE) #create a list of data files with "CDE Graduation" at the start and txt at the end

get_gradfiles_txt=lapply(X=gradfiles_txt, FUN=read.table, sep="\t", header=TRUE, fill=TRUE, quote = "") #read in all the files in the list as dataframes (tab as separator). Fill and quote are set to fill in missing data with an empty string

names(get_gradfiles_txt)=as.list(gsub("Raw Data/CDE Graduation ","",gsub(".txt","",gradfiles_txt))) #name each dataframe in the list with the year of the data

full_grad_txt=bind_rows(get_gradfiles_txt, .id = "Year") #merge list into a single dataframe with an ID column for the year of the data

## For .xlsx files (One-Year Rates)
## Note that for this, we are loading in the third sheet as this is where the data is. Hopefully this data format does not change as there was no great solution to grabbing names.

gradfiles_xlsx=list.files(path="Raw Data", pattern="^CDE Graduation.+xlsx$", full.names=TRUE) #create a list of data files with "CDE Graduation" at the start

get_gradfiles_xlsx=lapply(X=gradfiles_xlsx, FUN=read_excel, sheet=3, col_names=TRUE, skip=1) #read in the third sheet of the xlsx file, ignore the first row (in these files, the first row is the title of the sheet) and make the second row column names

names(get_gradfiles_xlsx)=as.list(gsub("Raw Data/CDE Graduation ","",gsub(".xlsx","",gradfiles_xlsx))) #name each dataframe in the list with the year of the data

full_grad_xlsx=bind_rows(get_gradfiles_xlsx, .id = "Year") %>% #merge list into a single dataframe with an ID column for the year of the data
  select(-Year) #the Year ID variable is extraneous in this dataset, so we're dropping it


# Merging txt and xlsx files ----------------------------------------------
## For this, we will be retrofitting the historical data to match the modern dataset.
## This will give us one long file with a separate row for each school/race pairing (and gender for the new years)

## Prepare the txt file
colnames(full_grad_txt)=c("Academic Year","School Code","County Name","District Name", #change column names for matching purposes
                          "School Name","Hispanic/Latino","American Indian/Alaskan Native",
                          "Asian","Pacific Islander","Filipino","African American","White",
                          "Two or more races","Not reported","Total","Year")

full_grad_txt_long=full_grad_txt %>%
  select(-c(Year,"School Code")) %>% #we don't need these (there's an academic year variable and removing the school code will make matching easier)
  pivot_longer(cols=c("Hispanic/Latino","American Indian/Alaskan Native", 
                      "Asian","Pacific Islander","Filipino","African American","White",
                      "Two or more races","Not reported","Total"),
               names_to="Reporting Category",
               values_to="One-Year Graduate Count") %>% #make our table long rather than wide
  mutate("Aggregate Level"="School") #add in the aggregation level for later

### Create some summary tables to add to the main table for similarity
#### Full state data
full_grad_txt_state=full_grad_txt_long %>%
  group_by(`Academic Year`,`Reporting Category`) %>% #group at state level
  mutate("One-Year Graduate Count"=sum(`One-Year Graduate Count`,na.rm=TRUE)) %>% #create summation
  distinct(`Academic Year`,`One-Year Graduate Count`, .keep_all=TRUE) %>% #remove duplicates
  mutate("Aggregate Level"="State", #set proper values
         "County Name"="N/A",
         "District Name"="N/A",
         "School Name"="N/A")

#### Full county data
full_grad_txt_cnty=full_grad_txt_long %>%
  group_by(`Academic Year`,`County Name`,`Reporting Category`) %>%
  mutate("One-Year Graduate Count"=sum(`One-Year Graduate Count`,na.rm=TRUE)) %>%
  distinct(`Academic Year`,`County Name`,`One-Year Graduate Count`, .keep_all=TRUE) %>%
  mutate("Aggregate Level"="County",
         "District Name"="N/A",
         "School Name"="N/A")

#### Full District Data
full_grad_txt_dist=full_grad_txt_long %>%
  group_by(`Academic Year`,`County Name`,`District Name`,`Reporting Category`) %>%
  mutate("One-Year Graduate Count"=sum(`One-Year Graduate Count`,na.rm=TRUE)) %>%
  distinct(`Academic Year`,`County Name`,`District Name`,`One-Year Graduate Count`, .keep_all=TRUE) %>%
  mutate("Aggregate Level"="County",
         "School Name"="N/A")

### Merge all the subtables to the main table
full_grad_txt_long=bind_rows(full_grad_txt_long,full_grad_txt_state,full_grad_txt_cnty,full_grad_txt_dist)



## Prepare the xlsx file
colnames(full_grad_xlsx)=gsub("[\r\n]", "", colnames(full_grad_xlsx)) #for some reason, CDE put a hard line break in a column name, so this strips that

full_grad_xlsx=full_grad_xlsx %>%
  mutate("Aggregate Level"=case_when(`Aggregate Level`=="T" ~ "State", #replace aggregation level with actual words
                                     `Aggregate Level`=="C" ~ "County",
                                     `Aggregate Level`=="D" ~ "District",
                                     `Aggregate Level`=="S" ~ "School",
                                  is.na(`Aggregate Level`) ~ NA)) %>%
  mutate("Reporting Category"=case_when(`Reporting Category`=="RB" ~ "African American", #replace reporting categories with actual words
                                        `Reporting Category`=="RI" ~ "American Indian/Alaskan Native",
                                        `Reporting Category`=="RA" ~ "Asian",
                                        `Reporting Category`=="RF" ~ "Filipino",
                                        `Reporting Category`=="RH" ~ "Hispanic/Latino",
                                        `Reporting Category`=="RD" ~ "Not reported",
                                     `Reporting Category`=="RP" ~ "Pacific Islander",
                                     `Reporting Category`=="RT" ~ "Two or more races",
                                     `Reporting Category`=="RW" ~ "White",
                                     `Reporting Category`=="GM" ~ "Male",
                                     `Reporting Category`=="GF" ~ "Female",
                                     `Reporting Category`=="GX" ~ "Non-binary",
                                     `Reporting Category`=="GZ" ~ "Missing",
                                     `Reporting Category`=="SE" ~ "English Language Learners",
                                     `Reporting Category`=="SD" ~ "Students with Disbailities",
                                     `Reporting Category`=="SS" ~ "Socioeconomically Disadvantaged",
                                     `Reporting Category`=="SM" ~ "Migrant",
                                     `Reporting Category`=="SF" ~ "Foster",
                                     `Reporting Category`=="SH" ~ "Homeless",
                                     `Reporting Category`=="TA" ~ "Total",
                                     is.na(`Reporting Category`) ~ NA)) %>%
  mutate("Academic Year"=paste0(substr(`Academic Year`,1,5),"20",substr(`Academic Year`,6,7))) %>% #add a 20 before the second year in the academic year for cleanliness
  select(-c("County Code","District Code","School Code")) #might make matching easier

## Merge the two and some final cleaning
full_grad=bind_rows(full_grad_txt_long,full_grad_xlsx) %>% #merge
  relocate(`Aggregate Level`,.after="Academic Year") %>% #move a column for easier comparison
  arrange("Academic Year","Aggregate Level","County Name","District Name","School Name") #order the columns for cleanliness


# Data Export -------------------------------------------------------------

write.csv(full_grad, "Clean Data/CDE_Graduation_Counts.csv",row.names=FALSE)
