library(tidyverse)
library(here)
library(googlesheets4)

# Supreme Court decisions data
sc_decisions_raw <- read_csv(here("data/SCDB_2021_01_justiceCentered_Citation.csv"))

# Import external data
extra_data_sheet <- "https://docs.google.com/spreadsheets/d/1QzVsdjHLv5Mr3eSMi7CSVvd5z3qwUd7K349cAMwaqRg/edit?usp=sharing"

senate <- read_sheet(extra_data_sheet, sheet = "Senate", skip = 1)

justices_raw <- read_sheet(extra_data_sheet, sheet = "Justices", skip =1) %>%
  janitor::clean_names(case = "lower_camel") 

house <- read_sheet(extra_data_sheet, sheet = "House of Representatives", skip = 1)

presidents <- read_sheet(extra_data_sheet, sheet = "Presidency", skip = 1) %>%
  janitor::clean_names(case = "lower_camel")

# Characteristics relating to justices
justices <- justices_raw %>%
  select(justiceId = justice, yearAppointed, presidentId, gender, birthYear, state, religion, ethnicity) %>%
  left_join(select(presidents, -name, startYear, endYear), by = "presidentId") 
  
# Put data together
sc_decisions <- sc_decisions_raw %>%
  select(
    caseId, 
     dateDecision, 
     decisionType, 
     term, 
     naturalCourt, 
     chief, 
     dateArgument,
     dateRearg,
     petitioner,
     petitionerState,
     respondent,
     respondentState,
     adminAction,
     adminActionState,
     threeJudgeFdc,
     caseOrigin,
     caseOriginState,
     caseSource,
     caseSourceState,
     lcDisagreement,
     certReason,
     lcDisposition,
     lcDispositionDirection,
     voteUnclear,
     issueArea,
     justice,
     justiceName,
     direction) %>%
  left_join(justices, by = c("justiceName" = "justice"))

  
  