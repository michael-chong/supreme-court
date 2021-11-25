library(tidyverse)
library(here)
library(googlesheets4)
library(stringr)

# Supreme Court decisions data
sc_decisions_raw <- read_csv(here("data/SCDB_2021_01_justiceCentered_Citation.csv"))

# Import external data
extra_data_sheet <- "https://docs.google.com/spreadsheets/d/1QzVsdjHLv5Mr3eSMi7CSVvd5z3qwUd7K349cAMwaqRg/edit?usp=sharing"

senate <- read_sheet(extra_data_sheet, sheet = "Senate", skip = 1) %>%
  mutate(startDate = str_c(`Start Day`, `Start Month`, `Start Year`, sep = "/")) %>%
  mutate(startDate = as.Date(startDate, format = "%d/%m/%Y"))

justices_raw <- read_sheet(extra_data_sheet, sheet = "Justices", skip =1) %>%
  janitor::clean_names(case = "lower_camel") 

house <- read_sheet(extra_data_sheet, sheet = "House of Representatives", skip = 1) %>%
  mutate(startDate = str_c(`Start Day`, `Start Month`, `Start Year`, sep = "/")) %>%
  mutate(startDate = as.Date(startDate, format = "%d/%m/%Y"))

presidents <- read_sheet(extra_data_sheet, sheet = "Presidency", skip = 1) %>%
  mutate(startDate = str_c(`Start Day`, `Start Month`, `Start Year`, sep = "/")) %>%
  mutate(startDate = as.Date(startDate, format = "%d/%m/%Y")) %>%
  janitor::clean_names(case = "lower_camel") 

issueArea <- read_sheet(extra_data_sheet, sheet = "IssueArea", skip = 1)

state <- read_sheet(extra_data_sheet, sheet = "State", skip = 1)

party <- read_sheet(extra_data_sheet, sheet = "Party", skip = 1)

caseOrigin <- read_sheet(extra_data_sheet, sheet = "CaseOrigin", skip = 1)

lcDisposition <- read_sheet(extra_data_sheet, sheet = "LowerCourtDisposition", skip = 1)

certReason <- read_sheet(extra_data_sheet, sheet = "CertReason", skip = 1)

# Characteristics relating to justices
justices <- justices_raw %>%
  select(
    justiceId = justice, 
    justiceYearAppointed = yearAppointed, 
    justicePresidentId = presidentId, 
    justiceGender = gender, 
    justiceBirthYear = birthYear, 
    justiceState = state, 
    justiceReligion = religion, 
    justiceEthnicity = ethnicity,
    votesForAppointed = votesFor,
    votesAgainstAppointed = votesAgainst,
    justiceLowerCourt = lowerCourt) %>%
  left_join(
    select(presidents, -name, startYear, endYear), 
    by = c("justicePresidentId" = "presidentId")) %>%
  rename(justicePresidentParty = party) 

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
  left_join(justices, by = c("justiceName" = "justiceId"))

sc_decisions <- sc_decisions %>%
  # Filter for cases with decision date between 2000 to 2019
  mutate(yearDecision = str_sub(dateDecision, -4, -1)) %>%
  filter(yearDecision %in% 2000:2019) %>%
  # Turn dates into date objects
  mutate(dateDecision = as.Date(dateDecision, format = "%m/%d/%Y")) %>%
  # Filter for cases where decisions are liberal or conservative 
  filter(direction %in% 1:2) %>%
  mutate(direction = ifelse(direction == 1, "Conservative", "Liberal")) %>%
  # Remove columns that have been mapped or scrubbed
  select(-justice)

# Mapping issue area of the case
sc_decisions$issueArea <- with(issueArea, issueAreaName[match(sc_decisions$issueArea, issueArea)])

# Mapping states to columns with states
sc_decisions$petitionerState <- with(state, StateName[match(sc_decisions$petitionerState, StateValue)])
sc_decisions$respondentState <- with(state, StateName[match(sc_decisions$respondentState, StateValue)])
sc_decisions$adminActionState <- with(state, StateName[match(sc_decisions$adminActionState, StateValue)])
sc_decisions$caseOriginState <- with(state, StateName[match(sc_decisions$caseOriginState, StateValue)])
sc_decisions$caseSourceState <- with(state, StateName[match(sc_decisions$caseSourceState, StateValue)])

# Mapping parties to columns with parties
sc_decisions$petitioner <- with(party, PartyName[match(sc_decisions$petitioner, Party)])
sc_decisions$respondent <- with(party, PartyName[match(sc_decisions$respondent, Party)])
sc_decisions$adminAction <- with(party, PartyName[match(sc_decisions$adminAction, Party)])

# Mapping case origin columns
sc_decisions$caseOrigin <- with(caseOrigin, CaseOriginName[match(sc_decisions$caseOrigin, CaseOrigin)])
sc_decisions$caseSource <- with(caseOrigin, CaseOriginName[match(sc_decisions$caseSource, CaseOrigin)])

# Mapping lower court disposition column
sc_decisions$lcDisposition <- with(lcDisposition, lcDispositionName[match(sc_decisions$lcDisposition, lcDisposition)])

# Mapping cert reason column
sc_decisions$certReason <- with(certReason, CertReasonName[match(sc_decisions$certReason, CertReason)])

# Mapping lower court disposition direction
sc_decisions$lcDispositionDirection <- ifelse(sc_decisions$lcDispositionDirection == 1, "Conservative", 
                                              ifelse(sc_decisions$lcDispositionDirection == 2, "Liberal", "Unspecified"))

# Calculate judge's age at decision year
sc_decisions$justiceAge <- as.numeric(sc_decisions$yearDecision) - as.numeric(sc_decisions$justiceBirthYear)

# Map house, senate and presidential parties
house <- house %>%
  mutate(houseMajority = ifelse(houseDemocratSeats > houseRepublicanSeats, "Democrat", "Republican"))

# senate$senateMajority <- ifelse(senate$`Democrats Seats`/senate$`Total Seats` > 0.5, "Democrat", "Republican")
# Note: When Senate is tied 50-50, the Vice-President is the tie-breaker, so the majority is coded manually

sc_decisions <- sc_decisions %>%
  mutate(congress = as.Date(cut.Date(dateDecision, breaks = house$startDate))) %>%
  left_join(house, by = c("congress" = "startDate")) %>%
  select(-contains("start"), -contains("end"))
  
sc_decisions <- sc_decisions %>%
  mutate(congress = as.Date(cut.Date(dateDecision, breaks = senate$startDate))) %>%
  left_join(senate, by = c("congress" = "startDate")) %>%
  select(-contains("start"), -contains("end"))

presidents <- presidents %>% 
  rename(
    decisionPresidentParty = party,
    decisionPresident = presidentId
  ) %>%
  select(-name)

sc_decisions <- sc_decisions %>%
  mutate(presidentDate = as.Date(cut.Date(dateDecision, breaks = presidents$startDate))) %>%
  left_join(presidents, by = c("presidentDate" = "startDate")) 

sc_decisions <- sc_decisions %>%
  select(-contains("start"), -contains("end"))

