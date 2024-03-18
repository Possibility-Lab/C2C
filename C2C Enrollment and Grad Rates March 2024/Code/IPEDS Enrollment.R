# -------------------------------------------------------------------------
# Merging and Cleaning IPEDS Enrollment Rate Data for C2C
# by Randy Clopton (Possibility Lab at UC Berkeley)
# Prepared March 2024
# -------------------------------------------------------------------------
#
# This file is designed to take the raw 12-month enrollment data collected from 
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
i_enrfiles=list.files(path="Raw Data", pattern="effy*", full.names=TRUE) #create a list of data files with "effy" at the start

get_i_enrfiles=lapply(X=i_enrfiles, FUN=read.csv, header=TRUE) #read in all the files in the list as dataframes

names(get_i_enrfiles)=as.list(gsub("Raw Data/effy","",gsub(".csv","",gsub("_rv","",i_enrfiles)))) #name each dataframe in the list with the year of the data

data_i_enr=bind_rows(get_i_enrfiles, .id = "Year") #merge list into a single dataframe with an ID column for the year of the data

## Load in institution data
i_inst=read.csv("Raw Data/hd2022.csv", header=TRUE) %>% #load data
  select(UNITID,INSTNM,IALIAS,ADDR,CITY,STABBR,ZIP,FIPS) #we just need some basics from this file, so we're only storing the institution name and location info

full_i_enr=left_join(data_i_enr,i_inst, by="UNITID") %>% #join by UNITID
  filter(STABBR=="CA") #filter out all of the non-CA institutions

# Clean Data --------------------------------------------------------------
## Key things here are dropping unnecessary data, renaming, and recoding

#rename columns to something easier to understand
colnames(full_i_enr)=c("Year","UNITID","Current Level of Study","Original Level of Study","XEYTOTLT","Grand total","XEYTOTLM","Grand total men",
                       "XEYTOTLW","Grand total women","XEFYAIAT","American Indian or Alaskan Native total","XEFYAIAM","American Indian or Alaskan Native men",
                       "XEFYAIAW","American Indian or Alaskan Native women","XEFYASIT","Asian total","XEFYASIM","Asian men","XEFYASIW","Asian women","XEFYBKAT",
                       "Black or African American total","XEFYBKAM","Black or African American men","XEFYBKAW","Black or African American women","XEFYHIST",
                       "Hispanic or Latino total","XEFYHISM","Hispanic or Latino men","XEFYHISW","Hispanic or Latino women","XEFYNHPT",
                       "Native Hawaiian or Other Pacific Islander total","XEFYNHPM","Native Hawaiian or Other Pacific Islander men","XEFYNHPW",
                       "Native Hawaiian or Other Pacific Islander women","XEFYWHIT","White total","XEFYWHIM","White men","XEFYWHIW","White women",
                       "XEFY2MOT","Two or more races total","XEFY2MOM","Two or more races men","XEFY2MOW","Two or more races women","XEYUNKNT",
                       "Race or ethnicity unknown total","XEYUNKNM","Race or ethnicity unknown men","XEYUNKNW","Race or ethnicity unknown women",
                       "XEYNRALT","US nonresident total","XEYNRALM","US Nonresident men","XEYNRALW","US Nonresident women","Level/Degree Status","XEFYGUUN",
                       "Gender unknown","XEFYGUAN","Another gender","XEFYGUTO","Total gender unknown and another gender","XEFYGUKN",
                       "Total gender reported as one of the mutually exclusive binary categories (Men/Women)","Institution name","Institution alias",
                       "Street Address","City","State Abbreviation","ZIP","FIPS")

colnames(full_i_enr)

head(full_i_enr)

#create a list of community colleges for easier use later
ccc=c("Allan Hancock College","American River College","Antelope Valley College",
      "Bakersfield College","Barstow Community College","Berkeley City College",
      "Butte College","Cabrillo College","Canada College","Cerritos College",
      "Cerro Coso Community College","Chabot College","Chaffey College",
      "Citrus College","City College of San Francisco","Clovis Community College",
      "Coastline Community College","College of Alameda","College of Marin",
      "College of San Mateo","College of the Canyons","College of the Desert",
      "College of the Redwoods","College of the Sequoias","College of the Siskiyous",
      "Columbia College","Compton College","Contra Costa College","Copper Mountain College",
      "Cosumnes River College","Crafton Hills College","Cuesta College",
      "Cuyamaca College","Cypress College", "De Anza College","Diablo Valley College",
      "East Los Angeles College","El Camino Community College District",
      "Evergreen Valley College","Feather River College", "Folsom Lake College",
      "Foothill College","Fresno City College","Fullerton College","Gavilan College",
      "Glendale Community College","Golden West College","Grossmont College",
      "Hartnell College","Imperial Valley College","Irvine Valley College",
      "Lake Tahoe Community College","Laney College","Las Positas College","Lassen College",
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
      "West Hills College Coalinga","West Hills College Lemoore","West Los Angeles College",
      "West Valley College","Woodland Community College","Yuba College")

full_i_enr_clean=full_i_enr %>%
  dplyr::select(-starts_with(c("X"))) %>% #select all the columns that don't start with X. X at the beginning indicates imputed data, which for the most part is not included.
  #create a variable to index UC system, CSU system, and CCC system
  mutate("System"=case_when(str_detect(`Institution name`, pattern="University of California") ~ "University of California",
                          str_detect(`Institution name`, pattern=" State University") ~ "California State University",
                          str_detect(`Institution name`, pattern="State Polytechnic") ~ "California State University",
                          `Institution name` %in% ccc ~ "California Community Colleges")) %>%
  filter(System %in% c("University of California","California State University","California Community Colleges")) %>% #subset to just public institutions
  #recode data to useful names
  mutate("Current Level of Study"=case_when(`Current Level of Study`==1 ~ "All students",
                                            `Current Level of Study`==2 ~ "Undergraduate",
                                            `Current Level of Study`==4 ~ "Graduate",
                                            `Current Level of Study`=="-2" ~ NA,
                                            is.na(`Current Level of Study`) ~ NA),

         "Original Level of Study"=case_when(`Original Level of Study`==1 ~ "All students",
                                             `Original Level of Study`==2 ~ "Undergraduate",
                                             `Original Level of Study`==3 ~ "Graduate",
                                             `Original Level of Study`==999 ~ "Generated total",
                                             is.na(`Original Level of Study`) ~ NA),

         "Level/Degree Status"=case_when(
           `Level/Degree Status`==1 ~ "All students total",
           `Level/Degree Status`==2 ~ "All students, Undergraduate total",
           `Level/Degree Status`==3 ~ "All students, Undergraduate, Degree/certificate-seeking total",
           `Level/Degree Status`==4 ~ "All students, Undergraduate, Degree/certificate-seeking, First-time",
           `Level/Degree Status`==5 ~ "All students, Undergraduate, Other degree/certificate-seeking",
           `Level/Degree Status`==19 ~ "All students, Undergraduate, Other degree/certificate-seeking, Transfer-ins",
           `Level/Degree Status`==20 ~ "All students, Undergraduate, Other degree/certificate-seeking, Continuing",
           `Level/Degree Status`==11 ~ "All students, Undergraduate, Non-degree/certificate-seeking",
           `Level/Degree Status`==12 ~ "All students, Graduate",
           `Level/Degree Status`==21 ~ "Full-time students total",
           `Level/Degree Status`==22 ~ "Full-time students, Undergraduate total",
           `Level/Degree Status`==23 ~ "Full-time students, Undergraduate, Degree/certificate-seeking total",
           `Level/Degree Status`==24 ~ "Full-time students, Undergraduate, Degree/certificate-seeking, First-time",
           `Level/Degree Status`==25 ~ "Full-time students, Undergraduate, Degree/certificate-seeking, Other degree/certificate-seeking",
           `Level/Degree Status`==39 ~ "Full-time students, Undergraduate, Other degree/certificate-seeking, Transfer-ins",
           `Level/Degree Status`==40 ~ "Full-time students, Undergraduate, Other degree/certificate-seeking, Continuing",
           `Level/Degree Status`==31 ~ "Full-time students, Undergraduate, Non-degree/certificate-seeking",
           `Level/Degree Status`==32 ~ "Full-time students, Graduate",
           `Level/Degree Status`==41 ~ "Part-time students total",
           `Level/Degree Status`==42 ~ "Part-time students, Undergraduate total",
           `Level/Degree Status`==43 ~ "Part-time students, Undergraduate, Degree/certificate-seeking total",
           `Level/Degree Status`==44 ~ "Part-time students, Undergraduate, Degree/certificate-seeking, First-time",
           `Level/Degree Status`==45 ~ "Part-time students, Undergraduate, Degree/certificate-seeking, Other degree/certificate-seeking",
           `Level/Degree Status`==59 ~ "Part-time students, Undergraduate, Other degree/certificate-seeking, Transfer-ins",
           `Level/Degree Status`==60 ~ "Part-time students, Undergraduate, Other degree/certificate-seeking, Continuing",
           `Level/Degree Status`==51 ~ "Part-time students, Undergraduate, Non-degree/certificate-seeking",
           `Level/Degree Status`==52 ~ "Part-time students, Graduate",
           is.na(`Level/Degree Status`) ~ NA))



# Export to csv -----------------------------------------------------------

write.csv(full_i_enr_clean, "Clean Data/IPEDS_Postsecondary_Enrollment.csv",row.names=FALSE) #creates a csv in the project folder Clean Data called IPEDS_Postsecondary_Enrollment.csv



