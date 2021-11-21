library(tidyverse)
library(here)
library(googlesheets4)
library(stringr)
library(fuzzyjoin)

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

issueArea <- read_sheet(extra_data_sheet, sheet = "IssueArea", skip = 1)

state <- read_sheet(extra_data_sheet, sheet = "State", skip = 1)

party <- read_sheet(extra_data_sheet, sheet = "Party", skip = 1)

caseOrigin <- read_sheet(extra_data_sheet, sheet = "CaseOrigin", skip = 1)

lcDisposition <- read_sheet(extra_data_sheet, sheet = "LowerCourtDisposition", skip = 1)

certReason <- read_sheet(extra_data_sheet, sheet = "CertReason", skip = 1)

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
  left_join(justices, by = c("justiceName" = "justiceId"))

# Filter for cases with decision date between 2000 to 2019
sc_decisions$yearDecision <- str_sub(sc_decisions$dateDecision,-4,-1)
sc_decisions <- sc_decisions %>% filter(sc_decisions$yearDecision %in% (2000:2019))

# Filter for cases where decisions are Liberal or Conservative
sc_decisions <- sc_decisions %>% filter(sc_decisions$direction %in% (1:2))
sc_decisions$direction <- ifelse(sc_decisions$direction == 1, "Conservative", "Liberal")

# Remove columns that have already been mapped or scrubbed
sc_decisions = sc_decisions[,!(names(sc_decisions) %in% c("dateDecision", "justice"))]

# Mapping issue area of the case
sc_decisions$issueArea <- with(issueArea, issueAreaName[match(sc_decisions$issueAre, issueArea)])

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
sc_decisions$age <- as.numeric(sc_decisions$yearDecision) - as.numeric(sc_decisions$birthYear)

# Map house, senate and presidential parties
house$houseMajority <- ifelse(house$`Democrats Seats`/house$`Total Seats` > 0.5, "Democrat", "Republican")
senate$senateMajority <- ifelse(senate$`Democrats Seats`/senate$`Total Seats` > 0.5, "Democrat", "Republican")

sc_decisions <- fuzzy_left_join(sc_decisions, house[,c("Start Year","End Year","houseMajority")], 
                  by = c("yearDecision" = "Start Year", "yearDecision" = "End Year"), 
                  match_fun = list(`>=`, `<=`))

sc_decisions = subset(sc_decisions, select = -c(`Start Year`,`End Year`))

sc_decisions <- fuzzy_left_join(sc_decisions, senate[,c("Start Year","End Year","senateMajority")], 
                                by = c("yearDecision" = "Start Year", "yearDecision" = "End Year"), 
                                match_fun = list(`>=`, `<=`))

sc_decisions = subset(sc_decisions, select = -c(`Start Year`,`End Year`))

names(presidents)[names(presidents) == "party"] <- "decisionPresident"
names(presidents)[names(presidents) == "startYear"] <- "presidentStartYear"
names(presidents)[names(presidents) == "endYear"] <- "presidentEndYear"

sc_decisions <- fuzzy_left_join(sc_decisions, presidents[,c("presidentStartYear","presidentEndYear","decisionPresident")], 
                                by = c("yearDecision" = "presidentStartYear", "yearDecision" = "presidentEndYear"), 
                                match_fun = list(`>=`, `<=`))

sc_decisions = subset(sc_decisions, select = -c(`presidentStartYear`,`presidentEndYear`))

