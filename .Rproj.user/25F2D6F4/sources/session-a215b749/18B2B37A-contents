# Load required packages
require(haven)
require(readr)
require(data.table)
require(dplyr)
require(stringr)

# Load helper functions
source(here::here("scripts/simulation-helper-functions.R"))

# Read in data ------------------------------------------------------------

## NOTE:
## .sav file for gum4 data contains demographics and IGT conventional score (Bechara 1994)
## .txt files contain trial level data
gum4.data.wide <- as.data.table(read_sav(here::here("data/actual", "Killgore2007_GUM4/raw/Killgore2007_GUM4.sav")))

txt_files <- list.files(path = "data/actual/Killgore2007_GUM4/raw", pattern = "\\.txt$", ignore.case = TRUE)


# Loop through all .txt files to record trial-level data for each participant

## NOTE:
## Subject 51's session 3 file was named "IGT_0051_.txt", but because they already have session 1 and 2 files
## present, it is assumed this file is session 3. File was renamed as "IGT_0051_3.txt". 

trial_data <- data.table()
for (file_name in txt_files){
  ## Grab subject id and session 
  PtID    <- str_sub(file_name, 5, 8 ) %>% as.integer()
  session <- str_sub(file_name, 10, 10 ) %>% as.integer()
  
  ## Grab subject demographic data
  sex <- gum4.data.wide[Subject_ID == PtID]$Gender %>% unique()
  age <- gum4.data.wide[Subject_ID == PtID]$Age %>% unique()
  
  ## Grab subject drug/placebo group status
  drug <- gum4.data.wide[Subject_ID == PtID]$Drug %>% unique()
    
  ## Read individual subject data
  subject_data <- data.table(
    read.table(here::here("data/actual/Killgore2007_GUM4/raw", file_name),
    skip = 21, sep = "",header = T,  na.strings=T, fill =T)
  )
  
  ## Add subject id and session identifier to data table
  subject_data[, PtID := PtID]
  subject_data[, session := session]
  subject_data[, sex := sex]
  subject_data[, age := age]
  subject_data[, drug := drug]
  
  ## Reorder cols such that ID and session are first two columns
  setcolorder(subject_data ,c('PtID', 'sex', 'age', 'session', 'drug', names(subject_data)[1:(length(subject_data)-5)]))
  
  ## Print message if participant's first trial is not denoted as 1
  if (subject_data[1, "Trial"] != 1){
    message(paste0("WARNING: Subject ", PtID, "'s data for Session ", session, " does not begin at Trial 1."))
    message("Check individual .txt file to verify number of lines to skip when using `read.table`")
  }
  
  ## Bind into trial data
  trial_data <- rbind(trial_data, subject_data)
}

# Get rid of "`" character in Deck column and change from alphabet to numerical index (1:4)
trial_data$Deck <- str_sub(trial_data$Deck, 1, 1 )
trial_data$Deck <- return_deck_index(trial_data$Deck)

# Rename "Deck" column to "choice"
names(trial_data)[names(trial_data) == "Deck"] <- "choice"

## Check for participants who didn't complete 3 sessions
temp <- distinct(trial_data, PtID, session)
for(id_x in unique(temp$PtID)){
  sessions.complete <- nrow(temp[PtID == id_x])
  if (sessions.complete != 3){
    message(paste0(
      "Subject ", id_x, " has completed ", sessions.complete, " session(s)."
    ))
  }
}


# Check if any sessions are < 100 trials ----------------------------------

temp <- trial_data[, max(Trial), by = c("PtID", "session")]
for (row_x in 1:nrow(temp)){
  if (temp[row_x, "V1"] != 100){
    message(paste0(
      "SUBJECT ", temp[row_x]$PtID, ", SESSION ", temp[row_x]$session, " has less than 100 completed trials.",
      " N trials = ", temp[row_x]$V1, "."
    ))
  }
}

# Calculate task score data from trial_data -----------------------------------

## Create new columns to hold scores for each block
trial_data[, c("b1.score", "b2.score", "b3.score", "b4.score", "b5.score") := as.numeric()]
trial_data[, total.score := as.numeric()]

for (id_x in unique(trial_data$PtID)){
  for (session_x in unique(trial_data[PtID==id_x]$session)){
    ## create temp subset of pt/session-level data
    temp <- trial_data[PtID==id_x & session==session_x]
    
    ## Calculate block scores and total score
    b1score <- nrow(temp[Trial %in% 1:20 & choice %in% 3:4])   - nrow(temp[Trial %in% 1:20   & choice %in% 1:2]) 
    b2score <- nrow(temp[Trial %in% 21:40 & choice %in% 3:4])  - nrow(temp[Trial %in% 21:40  & choice %in% 1:2]) 
    b3score <- nrow(temp[Trial %in% 41:60 & choice %in% 3:4])  - nrow(temp[Trial %in% 41:60  & choice %in% 1:2]) 
    b4score <- nrow(temp[Trial %in% 61:80 & choice %in% 3:4])  - nrow(temp[Trial %in% 61:80  & choice %in% 1:2]) 
    b5score <- nrow(temp[Trial %in% 81:100 & choice %in% 3:4]) - nrow(temp[Trial %in% 81:100 & choice %in% 1:2]) 
    totalscore  <- nrow(temp[choice %in% 3:4]) - nrow(temp[choice %in% 1:2])
    
    ## Write to trial data
    trial_data[PtID==id_x & session==session_x, c("b1.score", "b2.score", "b3.score", "b4.score", "b5.score") := .(b1score, b2score, b3score, b4score, b5score)]
    trial_data[PtID==id_x & session==session_x, total.score := totalscore ]
  }
  
  ## Remove temporary variables from global env
  rm(list = ls(pattern = "//b*score//")); rm(temp); rm(totalscore)
}

# Export merged data ------------------------------------------------------

save(trial_data, file = here::here("data", "actual", "Killgore2007_GUM4/Killgore2007_GUM4_merged.Rdata"))
