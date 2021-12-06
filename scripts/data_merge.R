library(tidyverse)
library(here)
library(googlesheets4)
library(stringr)
library(fuzzyjoin)
library(sqldf)

# Supreme Court decisions data
# sc_decisions_raw <- read_csv(here("data/SCDB_2021_01_justiceCentered_Citation.csv"))

# Alternate reading code in case here didn't work
 sc_decisions_raw <- read_csv("GitHub/supreme-court/data/SCDB_2021_01_justiceCentered_Citation.csv")

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

approval <- read_sheet(extra_data_sheet, sheet = "Approval", skip = 1)

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
    jurisdiction,
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

# If a case has no origin/source, then it was in the original jurisdiction
sc_decisions <- sc_decisions %>%
  mutate(
    caseOrigin = ifelse(is.na(caseOrigin), "No other court involved", caseOrigin),
    caseSource = ifelse(is.na(caseSource), "No other court involved", caseSource)) 

# Combine caseOrigin/caseOriginState, caseSource/caseSourceState
sc_decisions <- sc_decisions %>%
  mutate(caseOrigin = ifelse(
    !is.na(caseOriginState),
    str_c(caseOrigin, ", ", caseOriginState),
    caseOrigin
  )) %>%
  mutate(caseSource = ifelse(
    !is.na(caseSourceState),
    str_c(caseSource, ", ", caseSourceState),
    caseSource
  )) %>%
  select(-caseOriginState, -caseSourceState)

# Mapping lower court disposition column
sc_decisions$lcDisposition <- with(lcDisposition, lcDispositionName[match(sc_decisions$lcDisposition, lcDisposition)])

# if lcDisposition is NA, then check jurisdiction
# If jurisdiction = 9, then it is in Supreme Court's original jurisdiction
# If other, then it came from a trial court
sc_decisions <- sc_decisions %>%
  mutate(lcDisposition = ifelse(
    !is.na(lcDisposition),
    lcDisposition,
    ifelse(jurisdiction == 0, "NA: SC original jurisdiction", "NA: trial court")
  ))

# Mapping cert reason column
# If certReason is NA, assume it is "other reason" (13)
sc_decisions <- sc_decisions %>%
  mutate(certReason = ifelse(is.na(certReason), 13, certReason))

sc_decisions$certReason <- with(certReason, CertReasonName[match(sc_decisions$certReason, CertReason)])

# Mapping lower court disposition direction
sc_decisions$lcDispositionDirection <- ifelse(sc_decisions$lcDispositionDirection == 1, "Conservative", 
                                              ifelse(sc_decisions$lcDispositionDirection == 2, "Liberal", "Not applicable"))

sc_decisions <- sc_decisions %>%
  mutate(lcDispositionDirection = ifelse(is.na(lcDispositionDirection), "Not applicable", lcDispositionDirection))

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
  select(-contains("start"), -contains("end"), -congress)

# Other variable alterations
sc_decisions <- sc_decisions %>%
  # If threeJudgeFdc is missing, assume it was not heard at a 3 judge federal court
  mutate(threeJudgeFdc = ifelse(is.na(threeJudgeFdc), 0, threeJudgeFdc)) %>%
  # Turn adminAction into boolean 
  mutate(adminActionBool = !is.na(adminAction)) %>%
  # Were there oral arguments?
  mutate(oralArgBool = !is.na(dateArgument)) %>%
  # Was there re-argument?
  mutate(reargBool = !is.na(dateRearg)) %>%
  # Calculate time to decision 
  mutate(
    dateArgument = as.Date(dateArgument, format = "%m/%d/%Y"),
    decisionTime = ifelse(
      oralArgBool,
     dateDecision - dateArgument, 
      0)) %>%
  # Check whether case came from Justice's former circuit
  mutate(
    formerCourt = str_detect(caseSource, justiceLowerCourt),
    formerCourt = ifelse(is.na(formerCourt), FALSE, TRUE)
  ) %>%
  # remove columns not to be used in model
  select(-jurisdiction, 
         -adminAction, 
         -adminActionState, 
         -justiceLowerCourt, 
         -dateArgument, 
         -dateRearg,
         -petitionerState,
         -respondentState)

# Add Approval rating column
approval <- approval %>%
  select("yearDecision", "CourtApproval")

approval$yearDecision <- as.character(approval$yearDecision)

sc_decisions <- sc_decisions %>%
  left_join(approval, by = c("yearDecision"))

# Group Justice age to age band
sc_decisions <- sc_decisions %>% 
                  mutate(justiceDecisionAgeBand = ifelse(justiceAge <= 65 , "0-65", "65+"))


# There are many case origins so I'd like to map them to regions and court types
caseOrigin <- unique(sc_decisions$caseOrigin)
caseOrigin <- data.frame(caseOrigin)

region <- state %>%
  select("StateName", "RegionName")

caseOrigin <- caseOrigin %>%
                regex_inner_join(state, by = c(caseOrigin = "StateName")) %>%
                select(-StateValue) %>%
                rename(caseOriginRegion = RegionName, caseOriginSate = StateName)

caseOrigin <- caseOrigin %>% 
  mutate(caseOriginCourt = if_else(str_detect(caseOrigin, "District Court"), "District Court", 
                                   if_else(str_detect(caseOrigin, "Trial Court"), "Trial Court", 
                                           if_else(str_detect(caseOrigin, "Appellate Court"), "Appellate Court","Others"))))

sc_decisions <- sc_decisions %>%
  left_join(caseOrigin, by = c("caseOrigin"))

# Map justice's region too
justiceRegion <- state %>%
  select("StateName", "RegionName") %>%
  rename(justiceRegion = RegionName, justiceState = StateName)

sc_decisions <- sc_decisions %>%
  left_join(justiceRegion, by = c("justiceState"))

# Group Vote Appointed for each Justice
sc_decisions <- sc_decisions %>% 
  mutate(justiceVotesAppointed = if_else(votesForAppointed/(votesForAppointed+votesAgainstAppointed) >= 0.75, ">=75%", "<75%"))

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

# Gender column disappeared so I mapped it one more time
justiceGender <- sqldf('SELECT justiceID, justiceGender FROM justices')

sc_decisions <- sc_decisions %>%
  left_join(justiceGender, by = c("justiceName" = "justiceId"))
                
# Rearrange columns for model fitting
sc_decisions_final <- sqldf('
                            SELECT chief                    AS c_chief
                                   ,lcDisagreement          AS c_lcDisagreement
                                   ,lcDispositionDirection  AS c_lcDispositionDirection
                                   ,issueArea               AS c_issueArea
                                   ,adminActionBool         AS c_adminActionBool
                                   ,oralArgBool             AS c_oralArgBool
                                   ,reargBool               AS c_reargBool
                                   ,CASE 
                                      WHEN decisionTime <= 30 THEN "0-30 Days"
                                      WHEN decisionTime <= 180 THEN "31-180 Days"
                                      ELSE "181+ Days"
                                    END AS c_decisionTime
                                   ,formerCourt             AS c_formerCourt
                                   ,caseOriginRegion        AS c_caseOriginRegion
                                   ,caseOriginCourt         AS c_caseOriginCourt
                                   ,houseMajority           AS e_houseMajority
                                   ,senateMajority          AS e_senateMajority
                                   ,decisionPresidentParty  AS e_decisionPresidentParty
                                   ,courtApproval           AS e_JCApproval
                                   ,justiceGender           AS j_justiceGender
                                   ,justiceDecisionAgeBand  AS j_justiceDecisionAgeBand
                                   ,justiceRegion           AS j_justiceRegion
                                   ,justiceReligion         AS j_justiceReligion
                                   ,justiceEthnicity        AS j_justiceEthnicity
                                   ,justicePresidentParty   AS j_justicePresidentParty
                                   ,justiceVotesAppointed   AS j_justiceVotesAppointed
                                   ,direction
                            FROM sc_decisions
                            ')