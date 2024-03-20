# -------------------------------------------------------------------------
# Merging and Cleaning IPEDS Completions Data for C2C
# by Randy Clopton (Possibility Lab at UC Berkeley)
# Prepared March 2024
# -------------------------------------------------------------------------
#
# This file is designed to take the raw graduation rate data collected from 
# IPEDS on California's three public postsecondary school networks and merge it 
# into one clean file for all years. The code is designed to be run for as many 
# years of data as necessary using IPEDS's current data formatting. To add new 
# datafiles, go to the links below and download the year desired.
# 
# Links: https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?year=-1&sid=aab0fa38-211f-4be3-96f4-c4bc2a68acdb&rtid=7



# Package Load ------------------------------------------------------------

library(tidyverse)


# Load and Merge ----------------------------------------------------------
## Load in enrollment data
i_gradfiles=list.files(path="Raw Data", pattern="c20*", full.names=TRUE) #create a list of data files with "c2" at the start

i_gradfiles

get_i_gradfiles=lapply(X=i_gradfiles, FUN=read.csv, header=TRUE) #read in all the files in the list as dataframes

names(get_i_gradfiles)=as.list(gsub("Raw Data/c","",gsub(".csv","",gsub("_c_rv","",i_gradfiles)))) #name each dataframe in the list with the year of the data

data_i_grad=bind_rows(get_i_gradfiles, .id = "Year") #merge list into a single dataframe with an ID column for the year of the data

## Load in institution data
i_inst=read.csv("Raw Data/hd2022.csv", header=TRUE) %>% #load data
  select(UNITID,INSTNM,IALIAS,ADDR,CITY,STABBR,ZIP,FIPS) #we just need some basics from this file, so we're only storing the institution name and location info

full_i_grad=left_join(data_i_grad,i_inst, by="UNITID") %>% #join by UNITID
  filter(STABBR=="CA") #filter out all of the non-CA institutions

# Clean Data --------------------------------------------------------------
## Key things here are dropping unnecessary data, renaming, and recoding

#rename columns to something easier to understand
colnames(full_i_grad)=c("Year","UNITID","Award Level","XCSTOTLT","Grand Total","XCSTOTLM",
                        "Grand total men","XCSTOTLW","Grand total women","XCSAIANT",
                        "American Indian or Alaska Native total","XCSASIAT",
                        "Asian total","XCSBKAAT","Black or African American total",
                        "XCSHISP","Hispanic or Latino total","XCSNHPIT",
                        "Native Hawaiian or Other Pacific Islander total",
                        "XCSWHITT","White total","XCS2MORT","Two or more races total",
                        "XCSUNKNT","Race or ethnicity unknown total","XCSNRALT",
                        "US Nonresident total","XCSUND18","Age under 18",
                        "XCS18_24","Age 18-24","XCS25_39","Age 25-39",
                        "XCSABV40",'Age over 40',"XCSUNKN","Age unknown",
                        "Institution name","Institution alias","Street Address",
                        "City","State Abbreviation","ZIP","FIPS")

#create a list of community colleges for easier use later
#note that this dataset does not include Calbright College (fully online) or San Diego School of Continuing Education
ccc=c("Allan Hancock College","American River College","Antelope Valley Community College District",
      "Bakersfield College","Barstow Community College","Berkeley City College",
      "Butte College","Cabrillo College","Calbright College","Canada College","Cerritos College",
      "Cerro Coso Community College","Chabot College","Chaffey College",
      "Citrus College","City College of San Francisco","Clovis Community College",
      "Coastline Community College","College of Alameda","College of Marin",
      "College of San Mateo","College of the Canyons","College of the Desert",
      "College of the Redwoods","College of the Sequoias","College of the Siskiyous",
      "Columbia College","Compton College","Contra Costa College","Copper Mountain Community College",
      "Cosumnes River College","Crafton Hills College","Cuesta College",
      "Cuyamaca College","Cypress College", "De Anza College","Diablo Valley College",
      "East Los Angeles College","El Camino Community College District",
      "Evergreen Valley College","Feather River Community College District", "Folsom Lake College",
      "Foothill College","Fresno City College","Fullerton College","Gavilan College",
      "Glendale Community College","Golden West College","Grossmont College",
      "Hartnell College","Imperial Valley College","Irvine Valley College",
      "Lake Tahoe Community College","Laney College","Las Positas College","Lassen Community College",
      "Long Beach City College","Los Angeles City College","Los Angeles Harbor College",
      "Los Angeles Mission College","Los Angeles Pierce College","Los Angeles Southwest College",
      "Los Angeles Trade Technical College","Los Angeles Valley College","Los Medanos College",
      "Mendocino College","Merced College","Merritt College","MiraCosta College",
      "Mission College","Modesto Junior College","Monterey Peninsula College",
      "Moorpark College","Moreno Valley College","Mount San Antonio College",
      "Mt. San Jacinto College","Napa Valley College","Norco College","Ohlone College",
      "Orange Coast College","Oxnard College","Palo Verde College","Palomar College",
      "Pasadena City College","Porterville College","Reedley College","Rio Hondo College",
      "Riverside City College","Sacramento City College","Saddleback College",
      "San Bernardino Valley College","San Diego City College","San Diego Mesa College",
      "San Diego Miramar College","San Joaquin Delta College","San Jose City College",
      "Santa Ana College","Santa Barbara City College","Santa Monica College",
      "Santa Rosa Junior College","Santiago Canyon College","Shasta College",
      "Sierra College","Skyline College","Solano Community College",
      "Southwestern College","Taft College","Ventura College","Victor Valley College",
      "West Hills College-Coalinga","West Hills College-Lemoore","West Los Angeles College",
      "West Valley College","Woodland Community College","Yuba College","San Joaquin Valley College-Madera",
      "Mt San Antonio College","Mt San Jacinto Community College District")

full_i_grad_clean=full_i_grad %>%
  dplyr::select(-starts_with(c("X"))) %>% #select all the columns that don't start with X. X at the beginning indicates imputed data, which for the most part is not included.
  #create a variable to index UC system, CSU system, and CCC system
  mutate("System"=case_when(str_detect(`Institution name`, pattern="University of California") ~ "University of California",
                            str_detect(`Institution name`, pattern=" State University") ~ "California State University",
                            str_detect(`Institution name`, pattern="State Polytechnic") ~ "California State University",
                            `Institution name` %in% ccc ~ "California Community Colleges")) %>%
  filter(System %in% c("University of California","California State University","California Community Colleges")) %>% #subset to just public institutions
  #recode data to useful names
  mutate("Award Level"=case_when(`Award Level`==3 ~ "Associate's degree",
                                 `Award Level`==5 ~ "Bachelor's degree",
                                 `Award Level`==7 ~ "Master's degree",
                                 `Award Level`==9 ~ "Doctor's degree",
                                 `Award Level`==10 ~ "Postbaccalaureate or Post-master's certificate",
                                 `Award Level`==11 ~ "Certificate of less than 12 weeks",
                                 `Award Level`==12 ~ "Certificate of at least 12 weeks but less than 1 year",
                                 `Award Level`==2 ~ "Certificate of at least 1 but less than 4 years",
                                 `Award Level`==1 ~ "Award of less than 1 academic year"))


# Export to csv -----------------------------------------------------------

write.csv(full_i_grad_clean, "Clean Data/IPEDS_Postsecondary_Completions.csv",row.names=FALSE) #creates a csv in the project folder Clean Data called IPEDS_Postsecondary_Completions.csv



