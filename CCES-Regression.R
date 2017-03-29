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

write.csv(ccesTrain, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-rf-train.csv")
write.csv(ccesTest, file="C:/Documents and Settings/Stephen Clermont/My Documents/Personal/R Work/cces-rf-test.csv")
