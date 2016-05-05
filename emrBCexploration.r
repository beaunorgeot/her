setwd("/Users/beaunorgeot/Desktop")

#function to calc the mode b/c R is weird. Keep for reference. Use modeCount() instead most time
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#return the count of the mode in addition to the mode itself. 
modeCount <- function(v) {
  uniqv <- unique(v)
  mode = paste("mode=",uniqv[which.max(tabulate(match(v, uniqv)))],"count=",max(tabulate(match(v, uniqv))), sep = " ")
  mode
}

# How many total medications are women diagnosed with BC taking?
numMedsBefore = read.csv("numberMedicationsBeforeDiagnosis.csv")
# the above table was created on 4/8/2016 from the proj_emrUCSF2015Dec DB, using the query:
#select distinct `DIAGNOSES`.`Patient_ID`, COUNT(DISTINCT `MEDICATION_ORDERS`.`Medication_Name`) from `DIAGNOSES`, `MEDICATION_ORDERS` where `ICD9_Code` = "174.9" and `DIAGNOSES`.`Patient_ID` = `MEDICATION_ORDERS`.`Patient_ID` and `MEDICATION_ORDERS`.`Medication_Order_End_Date2` < `DIAGNOSES`.`Diagnosis_Start_Date2` GROUP BY `DIAGNOSES`.`Patient_ID`
dim(numMedsBefore) # 5322 x 2
View(numMedsBefore)
names(numMedsBefore) # Patient_ID, COUNT.DISTINCT..MEDICATION_ORDERS...MEDICATION_NAME..
names(numMedsBefore) = c("Patient_ID", "number_meds_before")
library(dplyr)
mean(numMedsBefore$number_meds_before) #28.8
median(numMedsBefore$number_meds_before) #19
min(numMedsBefore$number_meds_before) #1
max(numMedsBefore$number_meds_before) #225
library(ggplot2)
numMedBeforePlot = ggplot() + geom_bar(data = numMedsBefore, aes(number_meds_before), fill = "white", colour = "darkgreen") + geom_text(stat = 'bin', aes(label = ..count..),vjust = -1) + xlim(0,100) + ggtitle("Number of Distinct Medications Ordered Prior to Diagnosis")

numMedsAfter = read.csv("numberMedicationsAfterDiagnosis.csv")
# the above table was created on 4/8/2016 from the proj_emrUCSF2015Dec DB, using the query:
#select distinct `DIAGNOSES`.`Patient_ID`, COUNT(DISTINCT `MEDICATION_ORDERS`.`Medication_Name`) from `DIAGNOSES`, `MEDICATION_ORDERS` where `ICD9_Code` = "174.9" and `DIAGNOSES`.`Patient_ID` = `MEDICATION_ORDERS`.`Patient_ID` and `MEDICATION_ORDERS`.`Medication_Order_End_Date2` >= `DIAGNOSES`.`Diagnosis_Start_Date2` GROUP BY `DIAGNOSES`.`Patient_ID`
dim(numMedsAfter) # 6285 x 2
names(numMedsAfter) = c("Patient_ID", "number_meds_after")
mean(numMedsAfter$number_meds_after) #30.9
median(numMedsAfter$number_meds_after) #22
min(numMedsAfter$number_meds_after) #1
max(numMedsAfter$number_meds_after) #230
numMedAfterPlot = ggplot() + geom_bar(data = numMedsAfter, aes(number_meds_after), fill = "white", colour = "darkgreen") + geom_text(stat = 'bin', aes(label = ..count..),vjust = -1) + xlim(0,100) +  ggtitle("Number of Distinct Medications Ordered After to Diagnosis")
# On average, people take 3 additional medications after diagnosis

beforeAfterCombined = inner_join(numMedsBefore,numMedsAfter)
beforeAfterCombinedDiff = beforeAfterCombined %>% mutate(difference = number_meds_after - number_meds_before)
library(reshape2)
combinedMelt = melt(beforeAfterCombined, id.vars = c("Patient_ID"), variable.name = "status", value.name = "count")
combinedBoxPlot = ggplot() + geom_boxplot(notch = T, data = combinedMelt,aes(x = status, y = count, fill = status))
diffPlot = ggplot() + geom_bar(data = beforeAfterCombinedDiff, aes(x = difference))
boxplot(beforeAfterCombinedDiff$difference, horizontal = T)

# BC patient demogrphaics
bcPatientDemographics = read.csv("bcPatientDemographics.csv")
bcPatientDemographics$Patient_ID = as.character(bcPatientDemographics$Patient_ID)
bcPatientDemographics_distinct = distinct(bcPatientDemographics, Patient_ID)
sex = prop.table(table(bcPatientDemographics_distinct$Patient_Sex)) # 98.9% female (85 men)
age = summary(bcPatientDemographics_distinct$Patient_Age) # Min 5, Median and Mean = 61, Max = 90
smoke = prop.table(table(bcPatientDemographics_distinct$Patient_Smoking_Status)) # Former Smoker = 27%, Never Smoker = 64%, current everyday smoker = 2%. There are other categories
marital = prop.table(table(bcPatientDemographics_distinct$Patient_Marital_Status)) # single = 22%, Divorced = 9%, Widowed = 7%, Married = 57%
language = prop.table(table(bcPatientDemographics_distinct$Patient_Preferred_Language)) # there are 33 languages: English = 90%, spanish = 2%,Cantonese = 1.3%, Russian = 2.3%
race = prop.table(table(bcPatientDemographics_distinct$Patient_Race)) #7 races: White/Cauc = 64%, Asian = 13%, Black = 4%, Pacific Islander = 1.2%, 15% unknown/declined/other
bmi = prop.table(table(bcPatientDemographics_distinct$Vitals_BMI_Range))# 11% weren't recorded: Normal = 43%, Moderately_obsese = 12%, Severly_obsese ~8%, Overweight = 24%, underweight = 2%
bcPatientDemographics_distinct$timeSinceDiagnosis = as.numeric(bcPatientDemographics_distinct$timeSinceDiagnosis)
time_since_diagnosis = summary(bcPatientDemographics_distinct$timeSinceDiagnosis) # min =1, Median = 1574, Mean = 1142, Max = 1659

############ herceptin ########
# Query:
#>>>select distinct `DIAGNOSES`.`Patient_ID`, `DIAGNOSES`.`ICD9_Code`, COUNT(`DIAGNOSES`.`ICD9_Code`) from `MEDICATION_ORDERS`,`DIAGNOSES` where `Medication_Name` like "%trastuzu%" and `MEDICATION_ORDERS`.`Patient_ID` = `DIAGNOSES`.`Patient_ID` 
#GROUP BY `DIAGNOSES`.`Patient_ID`, `DIAGNOSES`.`ICD9_Code` ### end query
herTND = read.csv("herTotalNumberDiagnoses.csv")
herTND$Patient_ID = as.character(herTND$Patient_ID)
# how many comorbidities per person on average?
mean(herTND$totalNumberDiagnoses)
min(herTND$totalNumberDiagnoses)
max(herTND$totalNumberDiagnoses)  

# is anyone taking both tocilizumab and trastuzumab?
# Query: select `Patient_ID`, `Medication_Name` from `MEDICATION_ORDERS` where `Medication_Name` like "%tocilizu%" or `Medication_Name` like "%trastuzu%"
trasANDtoc = read.csv("trastORtoc.csv")
trasANDtoc$Patient_ID = as.character(trasANDtoc$Patient_ID)
both = trasANDtoc %>% mutate(tras = ifelse(grepl("TRAS",Medication_Name),1,0)) %>% mutate(toc = ifelse(grepl("TOC",Medication_Name),1,0)) %>% group_by(Patient_ID) %>% summarise(tras_total = sum(tras), toc_total = sum(toc)) %>% filter(tras_total > 0 & toc_total > 0)

#table(both$type_count)

# is anyone taking trastuzumab and leptinib?
trasANDlapat = read.csv("trastORlapat.csv")
trasANDlapat$Patient_ID = as.character(trasANDlapat$Patient_ID)
both_tras_lapat = trasANDlepat %>% mutate(tras = ifelse(grepl("TRAS",Medication_Name),1,0)) %>% mutate(lapat = ifelse(grepl("LAPAT",Medication_Name),1,0)) %>% group_by(Patient_ID) %>% summarise(tras_total = sum(tras), lapat_total = sum(lapat)) %>% filter(tras_total > 0 & lapat_total > 0)

# what 'medication_id' s are assoicated with trastuzumab?
#select `Medication_ID`, `Medication_Name` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%"
trasCodes = read.csv("trasCodes.csv")
trasCodesUnique = distinct(trasCodes,Medication_ID)

# Herceptin patient demogrphaics
herPatientDemographics = read.csv("herceptinPatientDemographics.csv")
herPatientDemographics$Patient_ID = as.character(herPatientDemographics$Patient_ID)
herPatientDemographics_distinct = distinct(herPatientDemographics, Patient_ID)
sex_her = prop.table(table(herPatientDemographics_distinct$Patient_Sex)) # 97% female, 3% male
age_her = summary(herPatientDemographics_distinct$Patient_Age) # Min 25, Median and Mean = 55, Max = 90
smoke_her = prop.table(table(herPatientDemographics_distinct$Patient_Smoking_Status)) # Former Smoker = 27%, Never Smoker = 69%, current everyday smoker = 2%. There are other categories
marital_her = prop.table(table(herPatientDemographics_distinct$Patient_Marital_Status)) # single = 24%, Divorced = 7%, Widowed = 2%, Married = 63%
language_her = prop.table(table(herPatientDemographics_distinct$Patient_Preferred_Language)) # there are 12 languages: English = 93%, Cantonese = 2%, Russian = 1.6%
race_her = prop.table(table(herPatientDemographics_distinct$Patient_Race)) #6 races: White/Cauc = 67%, Asian = 15%, Black = 4%, Pacific Islander = 1.3%, 11% unknown/declined/other
bmi_her = prop.table(table(herPatientDemographics_distinct$Vitals_BMI_Range))# 15% weren't recorded: Normal = 47%, Moderately_obsese = 10%, Severly_obsese ~4.5%, Overweight = 22%, underweight = 2%
herPatientDemographics_distinct$timeSinceDiagnosis = as.numeric(herPatientDemographics_distinct$timeSinceDiagnosis)
time_since_diagnosis_her = summary(herPatientDemographics_distinct$timeSinceDiagnosis) # min =170, Median = 1112, Mean = 1079, Max = 1764

# Did testing for her2 status happen?
her2testing = read.csv("testing_for_her2.csv")
# results from query: select distinct `DIAGNOSES`.`Patient_ID`, `Billing_CPT_Name`, `Billing_Service_Date2`, `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Start_Date2` from `DIAGNOSES`,`BILLING` where ( `Billing_Procedure_Code` = "86300" or `Billing_Procedure_Code` = "88368" or `Billing_Procedure_Code` = "88360" or `Billing_Procedure_Code`= "88342") and `BILLING`.`Patient_ID` = `DIAGNOSES`.`Patient_ID` and `DIAGNOSES`.`ICD9_Code` = "174.9"
# dim(her2testing) 564122
her2testing$Patient_ID = as.character(her2testing$Patient_ID)
# how many people received one of the tests?
n_people = her2testing %>% distinct(Patient_ID);dim(n_people)# 2513    5
#n_tests = her2testing %>% group_by(Patient_ID) %>% summarise(test_count = n()) #some people are geting a lot of tests
count_tests = her2testing %>% select(Patient_ID,Billing_CPT_Name) %>% group_by(Patient_ID, Billing_CPT_Name) %>% summarise(test_count = n())
min(count_tests$test_count) #1
max(count_tests$test_count) # 11,078 wtf?, patient_id = 105448618996888
which(count_tests$test_count > 1000) #there's 100 of these...
mean(count_tests$test_count) # ~120.00
median(count_tests$test_count) #26
test_plot = ggplot() + geom_bar(data = count_tests, aes(x = test_count)) #huge tail to the right
getmode(count_tests$test_count) #1
#Maybe people are getting these tests repeatedly
# also try looking by type of test, maybe only some tests are being reported a lot
by_category = her2testing %>% group_by(Billing_CPT_Name) %>% summarise(n = n()) %>% arrange(desc(n))
#CA 15-3 is by far the most commonly ordered, which makes sense since it's to be given regularly to watch for recurrence of BC
# perhaps some tests are given to more people, but other tests are given many times to the same person?
lots_o_tests = count_tests %>% filter(test_count > 100) %>% group_by(Billing_CPT_Name) %>% summarise(n = n()) %>% arrange(desc(n))
# same pattern exists, but CA-15-3 is less enriched when looking at mulitple retests. So the relative rate (in comparison to the other tests)
# is higher for ca-15 when less total tests are performed. 

#How many people do we actually have diagnosis starting date for?
real_date = her2testing %>% filter(Diagnosis_Start_Date2 != "0000-00-00") %>% distinct(Patient_ID)
dim(real_date)[1]/dim(n_people)[1] # 95.5% of people have an actual diagnosis date
her2testing$Billing_Service_Date2 = as.Date(her2testing$Billing_Service_Date2, format = "%Y-%m-%d")
her2testing$Diagnosis_Start_Date2 = as.Date(her2testing$Diagnosis_Start_Date2, format = "%Y-%m-%d")
# using the real date people, get rid of people whose tests were ordered before their BC diagnosis
real_date = her2testing %>% filter(!is.na(Diagnosis_Start_Date2),!is.na(Billing_Service_Date2), Billing_Service_Date2 > Diagnosis_Start_Date2)
real_date %>% distinct(Patient_ID) %>% dim(.) # there's 1935 people remaining
#before even working with dates, do the testing data change after filter?
real_date %>% group_by(Billing_CPT_Name) %>% summarise(n = n()) %>% arrange(desc(n))
#1  IMMUNOASSAY, TUMOR, CA 15-3 200063  YUP, CA 15-3 is even more prevalent
#2         IMMUNOHISTOCHEMISTRY  19373
#3 TUMOR IMMUNOHISTOCHEM/MANUAL  18876
#4 INSITU HYBRIDIZATION, MANUAL   9127

#average length of time between diagnosis and test
real_date = real_date %>% mutate(time_lag = difftime(Billing_Service_Date2,Diagnosis_Start_Date2))
real_date %>% summarise(avg = mean(time_lag), mode = modeCount(time_lag)) #avgs are pretty useless, there's crazy outliers
#avg = 314.259 days, mode: mode= 28 count= 1848
min(real_date$time_lag) #1 day
max(real_date$time_lag) #6079 days
#####
# gah: people actually get diagnosed w/BC multiple times:
r = real_date %>% group_by(Patient_ID,Diagnosis_Start_Date2) %>% summarise(n = n()) 
modeCount(r$n) # the most common number is 1 diagnosis, 
table(r$n)#but some have many more: max = 61

# what's the avg length of time between first diagnosis and first test?
# After, filtering to ensure that I'm only looking at tests that happen after the first BC diagnosis
# then select only person_id and diagnosis column and order the diagnosis col by date
di = real_date %>% select(c(Patient_ID,Diagnosis_Start_Date2)) %>% arrange(Diagnosis_Start_Date2)
# I could create a df that contains the first BC diagnosis for each person
di.first = di[match(unique(di$Patient_ID), di$Patient_ID),]
# then create a df that contains the first test ordered for each person
te = real_date %>% select(c(Patient_ID, Billing_Service_Date2, Billing_CPT_Name)) %>% arrange(Billing_Service_Date2)
te.first = te[match(unique(te$Patient_ID), te$Patient_ID),]
# then join the 2 on person_Id, then look at the differences in time between the 2. 
first_d_t = inner_join(di.first,te.first)
first_d_t = first_d_t %>% mutate(time_lag = difftime(Billing_Service_Date2,Diagnosis_Start_Date2))
first_d_t %>% summarise(avg = mean(time_lag), mode = modeCount(time_lag), med = median(time_lag)) #avgs are pretty useless, there's crazy outliers
# avg = 166.678 days, mode= 7 count= 48, med = 49
plot_first_d_t = ggplot() + geom_bar(data = first_d_t, aes(x = as.numeric(time_lag)))
first_d_t %>% group_by(Billing_CPT_Name) %>% summarise(n = n()) %>% arrange(desc(n)) #most people get IHC first
#1         IMMUNOHISTOCHEMISTRY  1012
#2 TUMOR IMMUNOHISTOCHEM/MANUAL   508
#3  IMMUNOASSAY, TUMOR, CA 15-3   403
#4 INSITU HYBRIDIZATION, MANUAL    12
first_d_t %>% group_by(Billing_CPT_Name) %>% summarise(avg_lag= mean(time_lag)) %>% arrange(desc(avg_lag)) #It takes a long time to get most of the tests, but people get FISH fast
#1  IMMUNOASSAY, TUMOR, CA 15-3 190.1687 days
#2         IMMUNOHISTOCHEMISTRY 170.7747 days
#3 TUMOR IMMUNOHISTOCHEM/MANUAL 143.0394 days
#4 INSITU HYBRIDIZATION, MANUAL  33.0000 days
#### end first occurence of bc and tests ###############

#next, run same query but include filter for patients on herceptin and include medication order date. Look at diff between diagnosis and her and test and her and diagnosis and test
# herceptin and tests
trast_tests = read.csv("trast_her2_tests.csv")
# select distinct `DIAGNOSES`.`Patient_ID`, `Billing_CPT_Name`, `Billing_Service_Date2`, `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Start_Date2`,`Medication_Order_Ordered_Date` from `DIAGNOSES`,`BILLING`,`MEDICATION_ORDERS` where ( `Billing_Procedure_Code` = "86300" or `Billing_Procedure_Code` = "88368" or `Billing_Procedure_Code` = "88360" or `Billing_Procedure_Code`= "88342") and `BILLING`.`Patient_ID` = `DIAGNOSES`.`Patient_ID` and `DIAGNOSES`.`Patient_ID` = `MEDICATION_ORDERS`.`Patient_ID` and `DIAGNOSES`.`ICD9_Code` = "174.9" and `Medication_Name` like "%trastuzu%"
# dim(tras_tests), 3651093
# there are more records in this vs her2testing b/c herceptin is being ordered (multiple times for the same person)
trast_tests$Patient_ID = as.character(trast_tests$Patient_ID)
trast_tests %>% distinct(Patient_ID) %>% dim(.) #217 people. Somehow 100 people are missing between this and the people taking herceptin
# above: there must be people taking herceptin that are missing a BC diagnosis or 1 of the 4 tests I'm looking at
count_herTest = trast_tests %>% select(Patient_ID,Billing_CPT_Name) %>% group_by(Patient_ID, Billing_CPT_Name) %>% summarise(test_count = n())
min(count_herTest$test_count) #4
modeCount(count_herTest$test_count) #"mode= 2340 count= 5"
max(count_herTest$test_count) #476,160
mean(count_herTest$test_count) #7590.63
# how is it possible that the max has gone up, tras patients are a subset of BC patients, the max should be less
# is this b/c rows are getting repeated as herceptin order dates are added?
# are all people in trast_tests in her2testing?
samePeeps = trast_tests %>% filter(Patient_ID %in% her2testing$Patient_ID); dim(samePeeps)/dim(trast_tests) # 1, Yes

herTest_plot = ggplot() + geom_bar(data = count_herTest, aes(x = test_count)) 

#### look just at first occurence of diagnosis, test, herceptin ###
trast_tests$Billing_Service_Date2 = as.Date(trast_tests$Billing_Service_Date2, format = "%Y-%m-%d")
trast_tests$Diagnosis_Start_Date2 = as.Date(trast_tests$Diagnosis_Start_Date2, format = "%Y-%m-%d")
dumb = trast_tests %>% select(Patient_ID, Billing_Service_Date2, Medication_Order_Ordered_Date)
dumb$test = as.Date(trast_tests$Medication_Order_Ordered_Date, format ="%m-%d-%Y")
#trast_tests$Medication_Order_Ordered_Date = as.Date(trast_tests$Medication_Order_Ordered_Date, format ="%m-%d-Y%")
trast_tests$Medication_Order_Date = dumb$test

her_real_date = trast_tests %>% filter(!is.na(Diagnosis_Start_Date2),!is.na(Billing_Service_Date2),!is.na(Medication_Order_Date), Billing_Service_Date2 > Diagnosis_Start_Date2)
her_real_date %>% distinct(Patient_ID) %>% dim(.) #only 206 patients pass this. Lost 11 people
her_di = her_real_date %>% select(c(Patient_ID,Diagnosis_Start_Date2)) %>% arrange(Diagnosis_Start_Date2)
# I could create a df that contains the first BC diagnosis for each person
her_di.first = her_di[match(unique(her_di$Patient_ID), her_di$Patient_ID),]
# then create a df that contains the first test ordered for each person
her_te = her_real_date %>% select(c(Patient_ID, Billing_Service_Date2, Billing_CPT_Name)) %>% arrange(Billing_Service_Date2)
her_te.first = her_te[match(unique(her_te$Patient_ID), her_te$Patient_ID),]
# then join the 2 on person_Id, then look at the differences in time between the 2. 
her_first_d_t = inner_join(her_di.first,her_te.first)

her_me = her_real_date %>% select(c(Patient_ID, Medication_Order_Date)) %>% arrange(Medication_Order_Date)
her_me.first = her_me[match(unique(her_me$Patient_ID), her_me$Patient_ID),]
her_first_m_d_t = inner_join(her_first_d_t,her_me.first)
#change date format for calculations
#her_first_m_d_t$Medication_Order_Date = format(as.Date(her_first_m_d_t$Medication_Order_Date),"%Y-%m-%d")
her_first_m_d_t$Medication_Order_Date = as.Date(her_first_m_d_t$Medication_Order_Date,format = "%Y-%m-%d")
her_first_m_d_t = her_first_m_d_t %>% mutate(di_test_diff = difftime(Billing_Service_Date2,Diagnosis_Start_Date2), test_med_diff = difftime(Medication_Order_Date,Billing_Service_Date2, units = "days"))
# theres a bunch of people who got herceptin prior to their first IHC/Fish test
her_first_m_d_t %>% select(test_med_diff) %>% filter(test_med_diff < 0) %>% dim(.) # 75 people
# look at length of time between BD diagnosis and first hereptin
her_first_m_d_t = her_first_m_d_t %>% mutate(di_med_diff = difftime(Medication_Order_Date,Diagnosis_Start_Date2, units = "days"))
# there are some that got herceptin before a BC diagnosis
her_first_m_d_t %>% select(di_med_diff) %>% filter(di_med_diff < 0) %>% dim(.) #19, nearly 20% fo people
# what's the avg for each time diff?
her_first_m_d_t %>% summarise(avg_di_test = mean(di_test_diff), avg_test_med = mean(test_med_diff), avg_di_med = mean(di_med_diff))
#   avg_di_test   avg_test_med    avg_di_med
#1 145.1942 days -26.98544 days 118.2087 days
# modes
her_first_m_d_t %>% summarise(mode_di_test = modeCount(di_test_diff), mode_test_med = modeCount(test_med_diff), mode_di_med = modeCount(di_med_diff))
#mode_di_test     mode_test_med      mode_di_med
#mode= 7 count= 8 mode= 27 count= 5 mode= 0 count= 6
# these might need to be plotted, but at least they're all above 0
plot_her_first_di_med = ggplot() + geom_bar(data = her_first_m_d_t, aes(x = as.numeric(di_med_diff)))

her_first_m_d_t %>% group_by(Billing_CPT_Name) %>% summarise(n = n()) %>% arrange(desc(n)) #most people get Tumor IHC first
#1 TUMOR IMMUNOHISTOCHEM/MANUAL    85
#2         IMMUNOHISTOCHEMISTRY    72
#3  IMMUNOASSAY, TUMOR, CA 15-3    45
#4 INSITU HYBRIDIZATION, MANUAL     4

her_first_m_d_t %>% group_by(Billing_CPT_Name) %>% summarise(avg_lag= mean(di_test_diff)) %>% arrange(desc(avg_lag)) #It takes a long time to get most of the tests, but people get FISH fast.
# This is true of normal BC patients too.
#1  IMMUNOASSAY, TUMOR, CA 15-3 201.6667 days
#2 TUMOR IMMUNOHISTOCHEM/MANUAL 140.9647 days
#3         IMMUNOHISTOCHEMISTRY 122.3889 days
#4 INSITU HYBRIDIZATION, MANUAL  10.2500 days

##### end first occurr####

##### outcomes below#########
#1. alive vs dead vs unknown
bc_death = read.csv("bc_death.csv")
bc_death %>% distinct(Patient_ID) %>% dim(.) #9455 people, got everyone
bc_death %>% filter(Patient_Status == "Deceased") %>% distinct(Patient_ID) %>% dim(.) #348 people died
bc_death %>% filter(Patient_Status == "Deceased",Patient_Death_Date2 == "0000-00-00") %>% distinct(Patient_ID) %>% dim(.) # we have actual death dates on 166/348 people
bc_death$Diagnosis_Start_Date2 = as.Date(bc_death$Diagnosis_Start_Date2, format = "%Y-%m-%d")
bc_death$Patient_Death_Date2 = as.Date(bc_death$Patient_Death_Date2, format = "%Y-%m-%d")
#get time from date of first diagnosis to death
# first, get first diagnosis date for each patient
bc_di = bc_death  %>% select(c(Patient_ID,Diagnosis_Start_Date2)) %>% filter(!is.na(Diagnosis_Start_Date2)) %>% arrange(Diagnosis_Start_Date2)
bc_di.first = bc_di[match(unique(bc_di$Patient_ID), bc_di$Patient_ID),]
dead_pats = bc_death %>% select(-Diagnosis_Start_Date2) %>% filter(Patient_Status == "Deceased", !is.na(Patient_Death_Date2)) %>% distinct(Patient_ID)
bc_diagnosis_death = inner_join(bc_di.first,dead_pats) # only 157 people have a valide diagnosis and death date
bc_diagnosis_death = bc_diagnosis_death %>% mutate(time_to_death = difftime(Patient_Death_Date2,Diagnosis_Start_Date2, units = "days"))
bc_diagnosis_death %>% summarise(avg_death = mean(time_to_death), med_death = median(time_to_death))
# avg_death med_death
#382.3376 days  350 days
#2. then do same but look for whether other cancer meds were entered after herceptin? Could be tricky, must have clear idea of when/where
# this would signal a non-response. 