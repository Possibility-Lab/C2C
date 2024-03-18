# -------------------------------------------------------------------------
# Merging and Cleaning CDE Enrollment Data for C2C
# by Randy Clopton (Possibility Lab at UC Berkeley)
# Prepared March 2024
# -------------------------------------------------------------------------
#
# This file is designed to take the raw enrollment data collected from the 
# California Dept of Ed and merge it into one clean file for all years. The 
# code is designed to be run for as many years of data as necessary using CDE's
# current data formatting. To add new datafiles, go to the links below and
# download the year desired, then name it "CDE Enrollment YYYY-YYYY" or
# "CDE Cumulative Enrollment YYYY-YYYY" for the given schoolyear. This will 
# ensure the code runs properly and creates the proper datafile.
# 
# Links
# Enrollment - https://www.cde.ca.gov/ds/ad/filesenr.asp
# Cumulative Enrollment - https://www.cde.ca.gov/ds/ad/filesenrcum.asp

# Package Load ------------------------------------------------------------

library(tidyverse)


# Enrollment Data ---------------------------------------------------------

# Load and Merge Data
## Provided the filenaming system remains consistent, this should scan for all 
## of the CDE enrollment data files and create unique data.frames with correct 
## names for each of them.
##
## When downloaded, the files do not download as described in the documentation 
## (enYY.txt) but as the same filename. This is why the filenaming system is as 
## it is.

enrfiles=list.files(path="Raw Data", pattern="CDE Enrollment*", full.names=TRUE) #create a list of data files with "CDE Enrollment" at the start

get_enrfiles=lapply(X=enrfiles, FUN=read.table, sep="\t", header=TRUE, fill=TRUE, quote = "") #read in all the files in the list as dataframes. Fill and quote are set to fill in missing data with an empty string

names(get_enrfiles)=as.list(gsub("Raw Data/CDE Enrollment ","",gsub(".txt","",enrfiles))) #name each dataframe in the list with the year of the data

full_enr=bind_rows(get_enrfiles, .id = "Year") #merge list into a single dataframe with an ID column for the year of the data

# Clean Data
## This section is primarily devoted to creating a file with variables replaced with usable values

full_enr_clean=full_enr %>% 
  mutate(ETHNIC=case_when(ETHNIC==1 ~ "American Indian/Alaskan Native", #replace codes with actual ethnicity descriptions
                          ETHNIC==2 ~ "Asian",
                          ETHNIC==3 ~ "Pacific Islander",
                          ETHNIC==4 ~ "Filipino",
                          ETHNIC==5 ~ "Hispanic/Latino",
                          ETHNIC==6 ~ "African American",
                          ETHNIC==7 ~ "White",
                          ETHNIC==9 ~ "Two or more races",
                          ETHNIC==0 ~ "Not reported",
                          is.na(ETHNIC) ~ NA)) %>%
  mutate(GENDER=case_when(GENDER=="M" ~ "Male", #replace gender codes with actual gender names
                          GENDER=="F" ~ "Female",
                          GENDER=="X" ~ "Non-Binary",
                          GENDER=="Z" ~ "Missing",
                          is.na(GENDER) ~ NA))
## Export to .csv

write.csv(full_enr_clean, "Clean Data/CDE_Enrollment.csv",row.names=FALSE) #creates a csv in the project folder Clean Data called CDE_Enrollment.csv

# Cumulative Enrollment Data ----------------------------------------------

# Load and Merge Data
## Provided the filenaming system remains consistent, this should scan for all of the CDE enrollment data files and create unique data.frames with correct names for each of them

cumenrfiles=list.files(path="Raw Data", pattern="CDE Cumulative Enrollment*", full.names=TRUE) #create a list of data files with "CDE Enrollment" at the start

get_cumenrfiles=lapply(X=cumenrfiles, FUN=read.table, sep="\t", header=TRUE, fill=TRUE, quote = "") #read in all the files in the list as dataframes. Fill and quote are set to fill in missing data with an empty string

names(get_cumenrfiles)=as.list(gsub("Raw Data/CDE Cumulative Enrollment ","",gsub(".txt","",cumenrfiles))) #name each dataframe in the list with the year of the data

full_cum_enr=bind_rows(get_cumenrfiles, .id = "Year") %>% #merge list into a single dataframe with an ID column for the year of the data
  select(-Year) #the Year ID variable is extraneous in this dataset, so we're dropping it

# Clean Data
## This section is primarily devoted to creating a file with variables replaced with usable values
## This file contains suppressed information for populations less than 10

full_cum_enr_clean=full_cum_enr %>%
  mutate(AggregateLevel=case_when(AggregateLevel=="T" ~ "State", #replace aggregation level with actual words
                                  AggregateLevel=="C" ~ "County",
                                  AggregateLevel=="D" ~ "District",
                                  AggregateLevel=="S" ~ "School",
                                  is.na(AggregateLevel) ~ NA)) %>%
  mutate(ReportingCategory=case_when(ReportingCategory=="RB" ~ "African American", #replace reporting categories with actual words
                                     ReportingCategory=="RI" ~ "American Indian/Alaskan Native",
                                     ReportingCategory=="RA" ~ "Asian",
                                     ReportingCategory=="RF" ~ "Filipino",
                                     ReportingCategory=="RH" ~ "Hispanic/Latino",
                                     ReportingCategory=="RD" ~ "Not reported",
                                     ReportingCategory=="RP" ~ "Pacific Islander",
                                     ReportingCategory=="RT" ~ "Two or more races",
                                     ReportingCategory=="RW" ~ "White",
                                     ReportingCategory=="GM" ~ "Male",
                                     ReportingCategory=="GF" ~ "Female",
                                     ReportingCategory=="GX" ~ "Non-binary",
                                     ReportingCategory=="GZ" ~ "Missing",
                                     ReportingCategory=="SE" ~ "English Language Learners",
                                     ReportingCategory=="SD" ~ "Students with Disbailities",
                                     ReportingCategory=="SS" ~ "Socioeconomically Disadvantaged",
                                     ReportingCategory=="SM" ~ "Migrant",
                                     ReportingCategory=="SF" ~ "Foster",
                                     ReportingCategory=="SH" ~ "Homeless",
                                     ReportingCategory=="TA" ~ "Total",
                                     is.na(ReportingCategory) ~ NA))
## Export to .csv

write.csv(full_cum_enr_clean, "Clean Data/CDE_Cumulative_Enrollment.csv",row.names=FALSE) #creates a csv in the project folder Clean Data called CDE_Cumulative_Enrollment.csv
