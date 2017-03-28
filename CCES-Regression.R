library(fmsb)
library(AER)
library(MASS)
library(QuantPsyc)
library(aod)
library(arm)
library(randomForest)
library(caret)
library(GGally) #should load ggplot with it
library(pROC)
library(dummies)

cces <- read.csv(file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-2016.csv", header=TRUE, row.names=1)

logit_trump <- glm(formula = Trump ~ Approve_Obama+PartyID+RepealACA+
 Ideo_Hillary+Ideo_Obama+Prim_Trump+Race_WhitePeopleAdv+Ideo_Trump+
 TimeCity+TaxIncreases_SpendingCuts+Sales_IncomeTaxes, 
 family = binomial(link = "logit"), data = cces, weights=weight)
summary (logit_trump)

NagelkerkeR2(logit_trump)
classDF <- data.frame(response = cces$Trump, predicted = round(fitted(logit_trump),0))
tabs <- xtabs(~ predicted + response, data = classDF)
frametabs <- as.data.frame(tabs)
mymatrix <- matrix(c(frametabs[,3]),nrow=2,byrow=TRUE)
colnames(mymatrix) <- c("Actual-No Trump","Actual-Trump")
rownames(mymatrix) <- c("Pred-No Trump","Pred-Trump")
print(mymatrix)
ratio <- mymatrix[1,1]*mymatrix[2,2]/(mymatrix[1,2]*mymatrix[2,1])
ratio
correct <- (mymatrix[1,1]+mymatrix[2,2])/(mymatrix[1,1]+mymatrix[2,2]+mymatrix[1,2]+mymatrix[2,1])
correct

logit_trump <- glm(formula = Trump ~ CountyType+Educ+Race+AgeGroup+
 Sex+Blog+TVNews+Newspaper+Radio+SocialMedia+NoMedia+Married+
 NationalEconomy+HHIncLastYear+HHIncNextYear+Past4_Married+Past4_Lostajob+
 Past4_Finishedschool+Past4_Retired+Past4_Divorced+Past4_Hadachild+
 Past4_Takenanewjob+Past4_Beenissuedatrafficticket+Past4_Beenavictimofacrime+
 Past4_Visitedanemergencyroom+Past4_Receivedaraiseatwork+PoliceSafe+
 PartyControl_USHouse+PartyControl_USSenate+PartyControl_SSenate+
 PartyControl_SHouse+Approve_Obama+Approve_USCongress+Approve_SCOTUS+
 Background_checks+Nopublishinggun+Ban_assault_rifles+
 Easier_concealed+Employment+HadJob+OwnHome+Child18+PartyID+AllUSA+
 Immig_Grand+Immig_Parent+BornAgain+Relig_Impt+ChurchAttendance+
 PrayerFrequency+Protestant+Catholic+Ideology+InMilitary+FamilyMil+PastMil+
 PastFamMil+NoMil+TimeCity+CurrentUnionHH+AnyUnionHH+ImmigP_LegalStatus+
 ImmigP_MoreBorderControl+ImmigP_Dreamers+ImmigP_Deport+Choice_FullChoice+
 Choice_RapeIncest+Choice_00WeekBan+Choice_EmployerBan+Choice_FedFundBan+
 Choice_Illegal+Envir_EPARegPowerCO0+Envir_RaiseFuelEffic+Envir_MinRenew+
 Envir_MoreEnforce+CJ_EndMandMin+CJ_BodyCameras+CJ_MorePolice+CJ_MorePrison+
 GayMarriage+Budget_CutDefense+Budget_CutDomestic+Budget_RaiseTaxes+Ideo_Obama+
 Ideo_Hillary+Ideo_Trump+Ideo_DemParty+Ideo_GOPParty+newsint+investor+
 sexuality+trans+HI_Employer+HI_Govt+HI_School+HI_Purchase+HI_NotSure+
 HI_None+TPP+EducationReform+HighwayTransportationFundingAct+
 IranSanctionsAct+MedicareAccountabilityAct+RepealACA+MinimumWage10+
 Time_Residence+Prim_Bernie+Prim_Hillary+Prim_Trump+Prim_OtherGOP+
 Troops_OilSupply+Troops_TerrorCamp+Troops_Genocide+Troops_SpreadDemocracy+
 Troops_ProtectAllies+Troops_HelpUN+Troops_None+TaxIncreases_SpendingCuts+
 Sales_IncomeTaxes+AttendPoliticalMeetings+PoliticalSign+WorkforCampaign+
 DonateMoney+DonateBlood+NoPoliticalActivities+ContactedbyCandidate+
 RunforOffice+Race_AngryRacism+Race_WhitePeopleAdv+Race_FearRaces+
 Race_Isolated+StateLeg_Welfare+StateLeg_HealthCare+StateLeg_Education+
 StateLeg_LawEnforcement+StateLeg_Infrastructure+LocalGrade_schools+
 LocalGrade_police+LocalGrade_roads+LocalGrade_Zoning+LocalGrade_Mayor+
 LocalGrade_Council+StudentLoanDebt, 
 family = binomial(link = "logit"), data = cces, weights=weight)
summary (logit_trump)

NagelkerkeR2(logit_trump)
classDF <- data.frame(response = cces$Trump, predicted = round(fitted(logit_trump),0))
tabs <- xtabs(~ predicted + response, data = classDF)


frametabs <- as.data.frame(tabs)
mymatrix <- matrix(c(frametabs[,3]),nrow=2,byrow=TRUE)
colnames(mymatrix) <- c("Actual-No Trump","Actual-Trump")
rownames(mymatrix) <- c("Pred-No Trump","Pred-Trump")
print(mymatrix)
ratio <- mymatrix[1,1]*mymatrix[2,2]/(mymatrix[1,2]*mymatrix[2,1])
ratio
correct <- (mymatrix[1,1]+mymatrix[2,2])/(mymatrix[1,1]+mymatrix[2,2]+mymatrix[1,2]+mymatrix[2,1])
correct

set.seed(6224)
trcontrol <- trainControl(classProbs = TRUE, 
                          verboseIter = FALSE,
                          number = 10, 
                          method = "repeatedcv", 
                          summaryFunction=twoClassSummary)

cces$set <- sample(c(1,2), nrow(cces), replace = TRUE, prob = c(0.15,0.85))
ccesTrain <- cces[cces$set == 1,]
ccesTest <- cces[cces$set == 2,]

head(ccesTest)

RFTrumpTrain <- train(Trump1 ~ CountyType+Educ+Race+AgeGroup+
 Sex+Blog+TVNews+Newspaper+Radio+SocialMedia+NoMedia+Married+
 NationalEconomy+HHIncLastYear+HHIncNextYear+Past4_Married+Past4_Lostajob+
 Past4_Finishedschool+Past4_Retired+Past4_Divorced+Past4_Hadachild+
 Past4_Takenanewjob+Past4_Beenissuedatrafficticket+Past4_Beenavictimofacrime+
 Past4_Visitedanemergencyroom+Past4_Receivedaraiseatwork+PoliceSafe+
 PartyControl_USHouse+PartyControl_USSenate+PartyControl_SSenate+
 PartyControl_SHouse+Approve_Obama+Approve_USCongress+Approve_SCOTUS+
 Background_checks+Nopublishinggun+Ban_assault_rifles+
 Easier_concealed+Employment+HadJob+OwnHome+Child18+PartyID+AllUSA+
 Immig_Grand+Immig_Parent+BornAgain+Relig_Impt+ChurchAttendance+
 PrayerFrequency+Protestant+Catholic+Ideology+InMilitary+FamilyMil+PastMil+
 PastFamMil+NoMil+TimeCity+CurrentUnionHH+AnyUnionHH+ImmigP_LegalStatus+
 ImmigP_MoreBorderControl+ImmigP_Dreamers+ImmigP_Deport+Choice_FullChoice+
 Choice_RapeIncest+Choice_00WeekBan+Choice_EmployerBan+Choice_FedFundBan+
 Choice_Illegal+Envir_EPARegPowerCO0+Envir_RaiseFuelEffic+Envir_MinRenew+
 Envir_MoreEnforce+CJ_EndMandMin+CJ_BodyCameras+CJ_MorePolice+CJ_MorePrison+
 GayMarriage+Budget_CutDefense+Budget_CutDomestic+Budget_RaiseTaxes+Ideo_Obama+
 Ideo_Hillary+Ideo_Trump+Ideo_DemParty+Ideo_GOPParty+newsint+investor+
 sexuality+trans+HI_Employer+HI_Govt+HI_School+HI_Purchase+HI_NotSure+
 HI_None+TPP+EducationReform+HighwayTransportationFundingAct+
 IranSanctionsAct+MedicareAccountabilityAct+RepealACA+MinimumWage10+
 Time_Residence+Prim_Bernie+Prim_Hillary+Prim_Trump+Prim_OtherGOP+
 Troops_OilSupply+Troops_TerrorCamp+Troops_Genocide+Troops_SpreadDemocracy+
 Troops_ProtectAllies+Troops_HelpUN+Troops_None+TaxIncreases_SpendingCuts+
 Sales_IncomeTaxes+AttendPoliticalMeetings+PoliticalSign+WorkforCampaign+
 DonateMoney+DonateBlood+NoPoliticalActivities+ContactedbyCandidate+
 RunforOffice+Race_AngryRacism+Race_WhitePeopleAdv+Race_FearRaces+
 Race_Isolated+StateLeg_Welfare+StateLeg_HealthCare+StateLeg_Education+
 StateLeg_LawEnforcement+StateLeg_Infrastructure+LocalGrade_schools+
 LocalGrade_police+LocalGrade_roads+LocalGrade_Zoning+LocalGrade_Mayor+
 LocalGrade_Council+StudentLoanDebt,
   data = ccesTrain,  ntree = 500, method="rf", metric="ROC", trControl = trcontrol)

imp <- varImp(RFTrumpTrain)
imp

RFTrumpTest <- predict(RFTrumpTrain , ccesTest, type="prob")
RFTrumpTrain <- predict(RFTrumpTrain , ccesTrain, type="prob")

ccesTrain$TrumpVoter <- RFTrumpTrain
ccesTest$TrumpVoter <- RFTrumpTest

write.csv(RFTrumpTrain, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-trump-rf-train.csv")
write.csv(RFTrumpTest, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-trump-rf-test.csv")

write.csv(ccesTrain, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-rf-train.csv")
write.csv(ccesTest, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-rf-test.csv")


RFClintonTrain <- train(Clinton1 ~ CountyType+Educ+Race+AgeGroup+
 Sex+Blog+TVNews+Newspaper+Radio+SocialMedia+NoMedia+Married+
 NationalEconomy+HHIncLastYear+HHIncNextYear+Past4_Married+Past4_Lostajob+
 Past4_Finishedschool+Past4_Retired+Past4_Divorced+Past4_Hadachild+
 Past4_Takenanewjob+Past4_Beenissuedatrafficticket+Past4_Beenavictimofacrime+
 Past4_Visitedanemergencyroom+Past4_Receivedaraiseatwork+PoliceSafe+
 PartyControl_USHouse+PartyControl_USSenate+PartyControl_SSenate+
 PartyControl_SHouse+Approve_Obama+Approve_USCongress+Approve_SCOTUS+
 Background_checks+Nopublishinggun+Ban_assault_rifles+
 Easier_concealed+Employment+HadJob+OwnHome+Child18+PartyID+AllUSA+
 Immig_Grand+Immig_Parent+BornAgain+Relig_Impt+ChurchAttendance+
 PrayerFrequency+Protestant+Catholic+Ideology+InMilitary+FamilyMil+PastMil+
 PastFamMil+NoMil+TimeCity+CurrentUnionHH+AnyUnionHH+ImmigP_LegalStatus+
 ImmigP_MoreBorderControl+ImmigP_Dreamers+ImmigP_Deport+Choice_FullChoice+
 Choice_RapeIncest+Choice_00WeekBan+Choice_EmployerBan+Choice_FedFundBan+
 Choice_Illegal+Envir_EPARegPowerCO0+Envir_RaiseFuelEffic+Envir_MinRenew+
 Envir_MoreEnforce+CJ_EndMandMin+CJ_BodyCameras+CJ_MorePolice+CJ_MorePrison+
 GayMarriage+Budget_CutDefense+Budget_CutDomestic+Budget_RaiseTaxes+Ideo_Obama+
 Ideo_Hillary+Ideo_Trump+Ideo_DemParty+Ideo_GOPParty+newsint+investor+
 sexuality+trans+HI_Employer+HI_Govt+HI_School+HI_Purchase+HI_NotSure+
 HI_None+TPP+EducationReform+HighwayTransportationFundingAct+
 IranSanctionsAct+MedicareAccountabilityAct+RepealACA+MinimumWage10+
 Time_Residence+Prim_Bernie+Prim_Hillary+Prim_Trump+Prim_OtherGOP+
 Troops_OilSupply+Troops_TerrorCamp+Troops_Genocide+Troops_SpreadDemocracy+
 Troops_ProtectAllies+Troops_HelpUN+Troops_None+TaxIncreases_SpendingCuts+
 Sales_IncomeTaxes+AttendPoliticalMeetings+PoliticalSign+WorkforCampaign+
 DonateMoney+DonateBlood+NoPoliticalActivities+ContactedbyCandidate+
 RunforOffice+Race_AngryRacism+Race_WhitePeopleAdv+Race_FearRaces+
 Race_Isolated+StateLeg_Welfare+StateLeg_HealthCare+StateLeg_Education+
 StateLeg_LawEnforcement+StateLeg_Infrastructure+LocalGrade_schools+
 LocalGrade_police+LocalGrade_roads+LocalGrade_Zoning+LocalGrade_Mayor+
 LocalGrade_Council+StudentLoanDebt,
   data = ccesTrain,  ntree = 500, method="rf", metric="ROC", trControl = trcontrol)

imp <- varImp(RFClintonTrain)
imp

RFClintonTest <- predict(RFClintonTrain , ccesTest, type="prob")
RFClintonTrain <- predict(RFClintonTrain , ccesTrain, type="prob")

ccesTrain$ClintonVoter <- RFClintonTrain
ccesTest$ClintonVoter <- RFClintonTest

write.csv(ccesTrain, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-clinton-rf-train.csv")
write.csv(ccesTest, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-clinton-rf-test.csv")


logit_trump1 <- glm(formula = Trump ~ Approve_Obama+PartyID+RepealACA+
 Ideo_Hillary+Prim_Trump+Ideo_Obama+
 Race_WhitePeopleAdv+Ideo_Trump+TimeCity+
 Sales_IncomeTaxes+Ideo_GOPParty+Educ+Ideology+StateLeg_LawEnforcement+
 ImmigP_Deport+NationalEconomy+Race_Isolated+Approve_USCongress, 
 family = binomial(link = "logit"), data = cces, weights=weight)
summary (logit_trump1)
NagelkerkeR2(logit_trump1)

TrumpProb <- predict(logit_trump, cces)
TrumpProb1 <- exp(TrumpProb)/(1+exp(TrumpProb))
cces$TrumpProb <- TrumpProb1

logit_clinton1 <- glm(formula = Clinton ~ Approve_Obama+
PartyID+RepealACA+Ideo_Hillary+Race_WhitePeopleAdv+
Prim_Hillary+TaxIncreases_SpendingCuts+
TimeCity+Sales_IncomeTaxes+Ideology+Race_Isolated+NationalEconomy+
Prim_Trump+Approve_USCongress+Educ, 
 family = binomial(link = "logit"), data = cces, weights=weight)
summary (logit_clinton1)
NagelkerkeR2(logit_clinton1)

ClintonProb <- predict(logit_clinton1, cces)
ClintonProb1 <- exp(ClintonProb)/(1+exp(ClintonProb))
cces$ClintonProb <- ClintonProb1

write.csv(cces, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-2.csv")
