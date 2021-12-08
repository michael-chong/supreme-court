# EDA and Logistic Regression
library(ggplot2)

# Percentage of cases by direction (48%)
sum(sc_decisions_final$direction == "Liberal", na.rm=TRUE)/nrow(sc_decisions_final)

# EDA
sc_decisions_eda <- sc_decisions_final  
sc_decisions_eda$caseCount <- 1  

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=c_issueArea, y=caseCount)) + 
         geom_bar(position="stack", stat="identity") + 
         ggtitle("Case Count by Issue Area and Direction") +
         xlab("Issue Area") + 
         ylab("Case Count") + 
         scale_fill_discrete(name = "Direction") +
         coord_flip()
         
ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=c_lcDispositionDirection, y=caseCount)) + 
         geom_bar(position="stack", stat="identity") + 
         ggtitle("Case Count by Lower Court Direction and Direction") +
         xlab("Lower Court Decition Direction") + 
         ylab("Case Count") + 
         scale_fill_discrete(name = "Direction")

sc_decisions_eda$c_decisionTime <- factor(sc_decisions_eda$c_decisionTime, levels=c("0-30 Days", "31-180 Days", "181+ Days"))
         
ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=c_decisionTime, y=caseCount)) + 
           geom_bar(position="stack", stat="identity") + 
           ggtitle("Case Count by Decision Time and Direction") +
           xlab("Decision Time") + 
           ylab("Case Count") + 
           scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=c_caseOriginRegion, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Case Origin Region and Direction") +
            xlab("Case Origin Region") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")
           
ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=e_houseMajority, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by House Majority and Direction") +
            xlab("House Majority") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")    

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=e_decisionPresidentParty, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Current President Party and Direction") +
            xlab("Current President Party") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")   

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=e_JCApproval, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Court Approval and Direction") +
            xlab("Justice Court Approval") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceGender, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Gender and Direction") +
            xlab("Gender") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceReligion, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Religion and Direction") +
            xlab("Justice Religion") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceEthinicity, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Religion and Direction") +
            xlab("Justice Religion") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceEthnicity, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Ethnicity and Direction") +
            xlab("Justice Ethinicity") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justicePresidentParty, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice President Party and Direction") +
            xlab("Justice President Party") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceVotesAppointed, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Votes Appointed and Direction") +
            xlab("Justice Votes Appointed") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

ggplot(data = sc_decisions_eda,
       aes(fill=direction, x=j_justiceDecisionAgeBand, y=caseCount)) + 
            geom_bar(position="stack", stat="identity") + 
            ggtitle("Case Count by Justice Decision Age and Direction") +
            xlab("Justice Decision Age") + 
            ylab("Case Count") + 
            scale_fill_discrete(name = "Direction")

# Logistic Regression (don't use, it's not ready)
## Code Liberal as 1 and conservative as 0
sc_decisions_final$direction = ifelse(sc_decisions_final$direction=="Liberal",1,0)

## Train test split
## 75% of the sample size
smp_size <- floor(0.75 * nrow(sc_decisions_final))

## set the seed to make your partition reproducible
set.seed(111111)
train_ind <- sample(seq_len(nrow(sc_decisions_final)), size = smp_size)

train <- sc_decisions_final[train_ind, ]
test <- sc_decisions_final[-train_ind, ]

logitmod <- glm(as.factor(direction) ~ ., family = "binomial", data=train)
summary(logitmod)

pred <- predict(logitmod, newdata = test, type = "response")


y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- train$direction

as.factor(sc_decisions_final$direction)