setwd("/Users/beaunorgeot/Desktop") #note: this script lives in ~/her

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
# ifelse always converts dates to numeric b/c it's stupid. Create a function that preserves format (borrowed from Hadley Wickam)
safe.ifelse <- function(cond, yes, no) {structure(ifelse(cond, yes, no), class = class(yes))}

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
#boxplot(beforeAfterCombinedDiff$difference, horizontal = T)

# herceptin patients after diagnosis
# select `DIAGNOSES`.`Patient_ID`, COUNT(DISTINCT `MEDICATION_ORDERS`.`Medication_Name`) from `DIAGNOSES`, `MEDICATION_ORDERS` where `MEDICATION_ORDERS`.`Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%") and `DIAGNOSES`.`Patient_ID` = `MEDICATION_ORDERS`.`Patient_ID` and `MEDICATION_ORDERS`.`Medication_Order_End_Date2` >= `DIAGNOSES`.`Diagnosis_Start_Date2` GROUP BY `DIAGNOSES`.`Patient_ID`
herPatients_number_meds_after_diagnosis = read.csv("herPatients_number_meds_after_diagnosis.csv")
names(herPatients_number_meds_after_diagnosis) = c("Patient_ID", "her_pats_number_meds_after")
mean(herPatients_number_meds_after_diagnosis$her_pats_number_meds_after) #58
median(herPatients_number_meds_after_diagnosis$her_pats_number_meds_after) #54
max(herPatients_number_meds_after_diagnosis$her_pats_number_meds_after) #54
her_numMedAfterPlot = ggplot() + geom_bar(data = herPatients_number_meds_after_diagnosis, aes(her_pats_number_meds_after), fill = "white", colour = "darkgreen") + geom_text(stat = 'bin', aes(label = ..count..),vjust = -1) + xlim(0,100) +  ggtitle("Herceptin: Number of Distinct Medications Ordered After to Diagnosis")

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
both_tras_lapat = trasANDlapat %>% mutate(tras = ifelse(grepl("TRAS",Medication_Name),1,0)) %>% mutate(lapat = ifelse(grepl("LAPAT",Medication_Name),1,0)) %>% group_by(Patient_ID) %>% summarise(tras_total = sum(tras), lapat_total = sum(lapat)) %>% filter(tras_total > 0 & lapat_total > 0)
# there are 20 people on both


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

# Did receptor testing for her2 status happen?
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
plot_first_d_t = ggplot() + geom_bar(data = first_d_t, aes(x = as.numeric(time_lag)),fill = "white", colour = "darkgreen")
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
first_d_t %>% group_by(Billing_CPT_Name) %>% summarise(median_lag= median(time_lag)) %>% arrange(desc(median_lag)) 
#1  IMMUNOASSAY, TUMOR, CA 15-3  85.0 days
#2         IMMUNOHISTOCHEMISTRY  49.0 days
#3 TUMOR IMMUNOHISTOCHEM/MANUAL  35.5 days
#4 INSITU HYBRIDIZATION, MANUAL  11.0 days
first_d_t %>% group_by(Billing_CPT_Name) %>% summarise(mode_lag= modeCount(time_lag))
#  IMMUNOASSAY, TUMOR, CA 15-3 mode= 182 count= 9
#         IMMUNOHISTOCHEMISTRY  mode= 7 count= 22
#INSITU HYBRIDIZATION, MANUAL  mode= 11 count= 4
#TUMOR IMMUNOHISTOCHEM/MANUAL  mode= 7 count= 15


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

her_first_m_d_t %>% summarise(median_di_med = median(di_test_diff)) # 40 days
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
her_first_m_d_t %>% group_by(Billing_CPT_Name) %>% summarise(median_lag= median(di_test_diff)) %>% arrange(desc(median_lag)) 
#1  IMMUNOASSAY, TUMOR, CA 15-3    67 days
#2         IMMUNOHISTOCHEMISTRY    40 days
#3 TUMOR IMMUNOHISTOCHEM/MANUAL    30 days
#4 INSITU HYBRIDIZATION, MANUAL     8 days

##### end first occurr####

##### outcomes below#########
#1. alive vs dead vs unknown
bc_death = read.csv("bc_death.csv")
bc_death %>% distinct(Patient_ID) %>% dim(.) #9455 people, got everyone
bc_death %>% filter(Patient_Status == "Deceased") %>% distinct(Patient_ID) %>% dim(.) #348 people died: 3.6%
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

#2 How does this compare to herceptin?
her_death = read.csv("her_death.csv")
her_death %>% distinct(Patient_ID) %>% dim(.) #316 people, got everyone
her_death %>% filter(Patient_Status == "Deceased") %>% distinct(Patient_ID) %>% dim(.) #32 people died: 10.1%. her given to patients with worse prognosis
her_death$Medication_Order_Ordered_Date2 = as.Date(her_death$Medication_Order_Ordered_Date2, format = "%Y-%m-%d")
her_death$Patient_Death_Date2 = as.Date(her_death$Patient_Death_Date2, format = "%Y-%m-%d")
her_death %>% filter(Patient_Status == "Deceased",!is.na(Patient_Death_Date2)) %>% distinct(Patient_ID) %>% dim(.) # we have actual death dates on 12/316 people
#get time from date of first diagnosis to death
# first, get first medication date for each patient
her_di = her_death  %>% select(c(Patient_ID,Medication_Order_Ordered_Date2)) %>% filter(!is.na(Medication_Order_Ordered_Date2)) %>% arrange(Medication_Order_Ordered_Date2)
her_di.first = her_di[match(unique(her_di$Patient_ID), her_di$Patient_ID),]
# take all herceptin patients who have died
her_dead_pats = her_death %>% select(-Medication_Order_Ordered_Date2) %>% filter(Patient_Status == "Deceased", !is.na(Patient_Death_Date2)) %>% distinct(Patient_ID)
her_diagnosis_death = inner_join(her_di.first,her_dead_pats) # still 12 people
her_diagnosis_death = her_diagnosis_death %>% mutate(time_to_death = difftime(Patient_Death_Date2,Medication_Order_Ordered_Date2, units = "days"))
her_diagnosis_death %>% summarise(avg_death = mean(time_to_death), med_death = median(time_to_death))
#herceptin patients die faster (though there's only 12 of them)
#avg_death med_death
#288.25 days  207 days
her_death_plot = ggplot() + geom_bar(data = her_diagnosis_death, aes(x = as.numeric(time_to_death)))

#COULD PLOT THE DEATH DISTRIBUTIONS (BC VS HER) SIDE BY SIDE

##### prognostic testing, herceptin, etc
pid_her_and_bc = read.csv("pid_her_and_bc.csv")
pid_her_only = read.csv("pid_her_only.csv")
pid_her_not_bc = as.character(setdiff(pid_her_only,pid_her_and_bc)) #15 people getting her that aren't bc


#select `Patient_ID`, `ICD9_Code`,`Diagnosis_Name` from `DIAGNOSES` where `Patient_ID` in ("387982982676476", "401993526611477", "413887239061296", "435484920628369", "455471099819988", "476843455340713", "489194019231945", "519014782737941", "5201371852309", "532645433209837", "557798337657005", "571554773952812", "641475504729897", "673936441075057", "803078788332641") and (`Diagnosis_Name` like "%cancer%" or `Diagnosis_Name` like "%neoplasm%")
her_not_bc_otherDiagnosis = read.csv("her_not_bc_otherDiagnosis.csv")
her_not_bc_otherDiagnosis$Patient_ID = as.factor(as.character(her_not_bc_otherDiagnosis$Patient_ID))
#in general, how many diagnosis do people have?
her_not_bc_otherDiagnosis %>% group_by(Patient_ID) %>% summarise(n = n())
# one person has only one, half of people have more than 100, half have less
#what are the most frequent diagnosis for each person
bob = her_not_bc_otherDiagnosis %>% group_by(Patient_ID,Diagnosis_Name) %>% summarise(n = n()) %>% arrange(desc(n)) #%>% dim(.)
# all have metastic cancer, all associated w/digestive tract
#1  387982982676476   756 mostly colon
#2  401993526611477   129 colon, lung
#3  413887239061296   450 vast majoirty is stomach or gastric, but this one spread a lot: duedenom, pancrease, ovaries
#4  435484920628369    46  stomach, gastric
#5  455471099819988    92 stomach, gastric
#6  476843455340713    71  esophagus
#7  489194019231945   103  gastric, stomach
#8  519014782737941    16  stomach, gastric
#9    5201371852309   326  esophagus which spread to many other sites
#10 532645433209837   214 stomach, gastric, esophagus
#11 557798337657005   144  esophagus
#12 571554773952812   314  pancreas which spread to liver
#13 641475504729897    74  tongue
#14 673936441075057     1  breast (but with different icd9 then expected)
#15 803078788332641   193  gastro- esophagus
####

# what other meds are herceptin users taking?
her_other_drugs = read.csv("her_other_drugs.csv")
#select `Patient_ID`, `Medication_Name` from `MEDICATION_ORDERS` where `Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%")
her_other_drugs$Patient_ID = as.factor(as.character(her_other_drugs$Patient_ID))
her_other_drugs %>% distinct(Patient_ID) %>% dim(.) # got all 316
# What drugs is each person taking?
each_person = her_other_drugs %>% group_by(Patient_ID,Medication_Name) %>% summarise(n = n()) %>% arrange(desc(n))
# what drugs are prescribed most frequently, ignoring the volume/amount/frequency of the drug w/in a person, multiple orders of the same drug for the same person?
#mo = her_other_drugs %>% group_by(Patient_ID) %>% distinct(Medication_Name) %>% group_by(Medication_Name) %>% summarise(n = n()) %>% arrange(desc(n))
her_conco_drugs = her_other_drugs %>% group_by(Patient_ID) %>% distinct(Medication_Name) %>% group_by(Medication_Name) %>% summarise(n = n()) %>% arrange(desc(n))

### Multi-drug analysis and time lines #######
#######################
# ifelse always converts dates to numeric b/c it's stupid. Create a function that preserves format (borrowed from Hadley Wickam)
safe.ifelse <- function(cond, yes, no) {structure(ifelse(cond, yes, no), class = class(yes))}

get_date_range = function(drug1, drug2){
  # drug1, drug2 should be data frames.
  # each must have a col called med_date (for dates of medication orders). Obvisously they should be in same format
  # Patient_ID should be a factor
  first_drug1 = drug1 %>% select(c(Patient_ID,med_date)) %>% arrange(med_date)
  first_drug1= first_drug1[match(unique(first_drug1$Patient_ID),first_drug1$Patient_ID),]
  last_drug1 = drug1 %>% select(c(Patient_ID,med_date)) %>% arrange(desc(med_date))
  last_drug1= last_drug1[match(unique(last_drug1$Patient_ID),last_drug1$Patient_ID),]
  first_drug2 = drug2 %>% select(c(Patient_ID,med_date)) %>% arrange(med_date)
  first_drug2= first_drug2[match(unique(first_drug2$Patient_ID),first_drug2$Patient_ID),]
  last_drug2 = drug2 %>% select(c(Patient_ID,med_date)) %>% arrange(desc(med_date))
  last_drug2= last_drug2[match(unique(last_drug2$Patient_ID),last_drug2$Patient_ID),]
  first_date = safe.ifelse(first_drug1[1,2] < first_drug2[1,2], first_drug1[1,2], first_drug2[1,2])
  last_date = safe.ifelse(last_drug1[1,2] > last_drug2[1,2], last_drug1[1,2], last_drug2[1,2])
  date_range = c(first_date,last_date)
  return(date_range)
}
# example call:
#get_date_range(all_herceptin_order_dates,t_all_lapatinib_order_dates)

# data
###
#lapatinib w/herceptin
all_lapatinib_order_dates = read.csv("all_lapatinib_order_dates.csv")
#select `Patient_ID`, `Medication_Name`, `Medication_Order_Ordered_Date2` as med_date from `MEDICATION_ORDERS` where `Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%") and `Medication_Name` like "%lapatin%" 
colnames(all_lapatinib_order_dates)[3] = "med_date"
all_lapatinib_order_dates$Patient_ID = as.factor(as.character(all_lapatinib_order_dates$Patient_ID))
all_lapatinib_order_dates$med_date = as.Date(all_lapatinib_order_dates$med_date, format = "%Y-%m-%d")
all_lapatinib_order_dates %>% group_by(Patient_ID) %>% summarise(n = n())
# most people in here only received 1 single order of lapatinib. The max is 8
####
# herceptin w/lapatinib
all_herceptin_order_dates = read.csv("all_herceptin_order_dates.csv")
#select `Patient_ID`, `Medication_Name`, `Medication_Order_Ordered_Date2` as med_date from `MEDICATION_ORDERS` where `Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%lapatinib%") and `Medication_Name` like "%trastuzu%" 
all_herceptin_order_dates$Patient_ID = as.factor(as.character(all_herceptin_order_dates$Patient_ID))
all_herceptin_order_dates$med_date = as.Date(all_herceptin_order_dates$med_date, format = "%Y-%m-%d")
# are any patients ordered the same drug more than 1x a day?
#twoXDayHer = all_herceptin_order_dates %>% group_by(Patient_ID, med_date) %>% summarise(n = n())
#table(twoXDayHer$n) # nearly all only have 1x day, but 17 people have 2x on day, and 1 person has 3x
# remove duplicate dates for the same person
#single_all_herceptin_order_dates = all_herceptin_order_dates %>% distinct(Patient_ID, med_date)
#her_lap_time = full_join(single_all_herceptin_order_dates,all_lapatinib_order_dates, by = "Patient_ID") %>% mutate(time_diff = difftime(lapat_med_date,med_date, units = "days")) #%>% distinct(Patient_ID, lapat_med_date)
####

# pertuzumab order times with herceptin
all_pertuzumab_order_dates = read.csv("all_pertuzumab_order_dates.csv")
#select `Patient_ID`, `Medication_Name`, `Medication_Order_Ordered_Date2` as med_date from `MEDICATION_ORDERS` where `Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%") and `Medication_Name` like "%pertuzu%" 
colnames(all_pertuzumab_order_dates)[3] = "med_date"
all_pertuzumab_order_dates$Patient_ID = as.factor(as.character(all_pertuzumab_order_dates$Patient_ID))
all_pertuzumab_order_dates$med_date = as.Date(all_pertuzumab_order_dates$med_date, format = "%Y-%m-%d")
all_pertuzumab_order_dates %>% group_by(Patient_ID) %>% summarise(n = n()) %>% summarise(avg = mean(n), myMode = getmode(n), myMedian = median(n))
# the avg is about 13 orders per person, mode is 4, median is 6

# herceptin w/pertuzumab
all_herceptin_w_pertuzu_order_dates = read.csv("all_herceptin_w_pertuzu_order_dates.csv")
all_herceptin_w_pertuzu_order_dates$Patient_ID = as.factor(as.character(all_herceptin_w_pertuzu_order_dates$Patient_ID))
all_herceptin_w_pertuzu_order_dates$med_date = as.Date(all_herceptin_w_pertuzu_order_dates$med_date, format = "%Y-%m-%d")

####

plot_person = function(perID,drug1,drug2){
  d1 = drug1 %>% filter(Patient_ID == perID) %>% dim(.)
  y = rep(0,d1[1])
  the_drug1_dates = drug1 %>% filter(Patient_ID == perID) %>% cbind(.,y) %>% select(med_date,y)
  d2 = drug2 %>% filter(Patient_ID == perID) %>% dim(.)
  y1 = rep(1,d2[1])
  the_drug2_dates = drug2 %>% filter(Patient_ID == perID) %>% cbind(.,y1) %>% select(med_date,y1)
  #plot
  my_plot = plot(the_drug1_dates, pch = 19, xlim = get_date_range(drug1,drug2),ylim = c(-0.5, 1.5))
  points(the_drug2_dates, col = "red", pch = 19)
}
# there are also graphs w/o test_ that contain the data w/o the fixed time line. The fixed timeline provides consistency but does also compress the points
# to go back to the original plotting, simply remove the xlim argument in plot_person()
pdf("test_patient_her_lap.pdf", h = 17, w = 17) 
par(mfrow = c(10,2))
for (p in unique(all_herceptin_order_dates$Patient_ID)) {plot_person(p,all_herceptin_order_dates,all_lapatinib_order_dates)}
dev.off()

pdf("test_patient_her_pertuzu.pdf", h = 17, w = 17)
par(mfrow = c(10,2))
for (p in unique(all_herceptin_w_pertuzu_order_dates$Patient_ID)) {plot_person(p,all_herceptin_w_pertuzu_order_dates,all_pertuzumab_order_dates)}
dev.off()

#her_lap_first_plot = ggplot() + geom_histogram(data = first_her_lap, aes(x = as.numeric(time_diff))) + labs(x = "Number of Days", y = "Count", title = "Time Difference between First Herceptin Order and First Lapatinib Order")
#her_lap_diff_dotplot = ggplot() + geom_point(data = first_her_lap, aes(x = Patient_ID, y = as.numeric(time_diff))) + labs(x = "Distinct Patients", y = "Number of Days",title = "Time Difference between First Herceptin Order and First Lapatinib Order")


####

#todo:
#1. what is happening during the gaps in herceptin tx, surgery? -ignoring for now. Seems to speculative
#2. what does an order of her or laptininb mean? 1 dose, 21 doses etc?
her_between_orders = all_herceptin_order_dates %>% group_by(Patient_ID) %>% arrange(med_date) %>% mutate(or_diff = difftime(med_date,lag(med_date), units = "days"))
lap_between_orders = all_lapatinib_order_dates %>% group_by(Patient_ID) %>% arrange(med_date) %>% mutate(or_diff = difftime(med_date,lag(med_date), units = "days"))
per_between_orders = all_pertuzumab_order_dates %>% group_by(Patient_ID) %>% arrange(med_date) %>% mutate(or_diff = difftime(med_date,lag(med_date), units = "days"))
#4. albuterol
# albuterol could shrink tumors? http://www.ncbi.nlm.nih.gov/pubmed/22122228 
# http://www.cancernetwork.com/breast-cancer/dyspnea-common-symptom-lung-breast-cancer
# The second tumor group in which dyspnea most commonly occurs is breast cancer. In this group, breathing problems are caused by pleural effusions rather than by a blockage in the lungs.
# Chemotherapy drugs can cause pulmonary toxicity and produce dyspnea. Bleomycin (Blenoxane) may be particularly harmful because it can infiltrate interstitial tissues and lead to pulmonary fibrosis.
# Biologic response modifiers (such as interleukin-2) can cause a shift in the pulmonary fluids, which can lead to dyspnea.
# Radiation delivered to the chest can cause pulmonary compromise
# Dyspnea is typically treated with medications, such as albuterol, administered through a bronchial dilator

#bob = full_join(all_herceptin_order_dates,all_lapatinib_order_dates) %>% filter(Patient_ID == "629998774733394",med_date == lapat_med_date)

#############################
# cohort selection
reference_cohort_1 = read.csv("reference_cohort_1.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` not in ("Other", "Unknown/Declined", "*Unspecified") and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 6112
reference_cohort_1$Patient_ID = as.factor(as.character(reference_cohort_1$Patient_ID))

bc_cohort_1 = read.csv("bc_cohort_1.csv")
#select distinct `DIAGNOSES`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE`, `DIAGNOSES` where `DIAGNOSES`.`Patient_ID` in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9" group by `Patient_ID` having count(`Patient_ID`) > 1) and `PATIENTS`.`Patient_ID` = `DIAGNOSES`.`Patient_ID` and `DIAGNOSES`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` 
length(unique(bc_cohort_1$Patient_ID))
bc_cohort_1$Patient_ID = as.factor(as.character(bc_cohort_1$Patient_ID))
bc_cohort_1 = bc_cohort_1 %>% distinct(Patient_ID) #dim(bc_cohort_1) = 7528. Got correct number now. not sure what happened.
#remove patients that don't have values for race or
#REMOVE MALE PATIENTS FROM BC COHORT WHEN COMPARING TO GENERAL POPULATION
bc_cohort_general = bc_cohort_1 %>% filter(!Patient_Race %in% c("Other", "Unknown/Declined"), Patient_Sex == "Female") %>% select(-Patient_Sex)#6112 patients left 
bc_cohort_general = droplevels(bc_cohort_general)

#combine smoking levels into something that makes more sense: never, light, heavy, unknown
# heavy = (current every day, heavy tobacco smoker)
# light = (current some day, former smoker, light tobacco smoker, )  
# never = (Never smoker, passive smoke exposure never smoker)
# unknown = (Never assessed, smoker current status unknown, unknown if ever smoked)
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Current Every Day Smoker', 'Patient_Smoking_Status'] = "Heavy Tobacco Smoker"
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Current Some Day Smoker', 'Patient_Smoking_Status'] = "Light Tobacco Smoker"
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Former Smoker', 'Patient_Smoking_Status'] = "Light Tobacco Smoker"
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Passive Smoke Exposure - Never Smoker', 'Patient_Smoking_Status'] = "Never Smoker"
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Smoker, Current Status Unknown', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
reference_cohort_1[reference_cohort_1$Patient_Smoking_Status == 'Never Assessed', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
reference_cohort_1 = droplevels(reference_cohort_1)
#
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Current Every Day Smoker', 'Patient_Smoking_Status'] = "Heavy Tobacco Smoker"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Current Some Day Smoker', 'Patient_Smoking_Status'] = "Light Tobacco Smoker"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Former Smoker', 'Patient_Smoking_Status'] = "Light Tobacco Smoker"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Passive Smoke Exposure - Never Smoker', 'Patient_Smoking_Status'] = "Never Smoker"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Smoker, Current Status Unknown', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == 'Never Assessed', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == '*Unspecified', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
bc_cohort_general[bc_cohort_general$Patient_Smoking_Status == '', 'Patient_Smoking_Status'] = "Unknown If Ever Smoked"
bc_cohort_general = droplevels(bc_cohort_general)

bc_ids = bc_cohort_general %>% select(Patient_ID)
write.csv(bc_ids, file = "bc_ids.csv")
#######

# Next, run stat tests on age, race, and smoking status and age
#I'm dubious that the p values for age and smoke are identical. Ahh hah, 2.2e-16 is the limit for smallest floating point value to be printed
# smaller p-values can be obtained by calling p-value directly: t.test(bc_cohort_general$Patient_Age,reference_cohort_1$Patient_Age)$p.value
race_1 = as.data.frame(rbind(table(reference_cohort_1$Patient_Race), table(bc_cohort_general$Patient_Race)))
race_1 = race_1 %>% mutate(SUM = rowSums(.))
rownames(race_1) = c("reference", "bc")
chi_race_1 = chisq.test(race_1, correct = T) #p-value = 1.015e-12
#chi_race$expected
RACE = table(reference_cohort_1$Patient_Race,bc_cohort_general$Patient_Race)
# is bar plot failing b/c df and not table?
#TAB = table(reference_cohort_1$Patient_Race, reference_cohort_1$Patient_Smoking_Status)
#barplot(TAB, beside = T, legend = T)
barplot(RACE, beside = T, legend = T)
chi_RACE = chisq.test(RACE) #p-value = 0.1376

#
smoke_1 = as.data.frame(rbind(table(reference_cohort_1$Patient_Smoking_Status), table(bc_cohort_general$Patient_Smoking_Status)))
smoke_1 = smoke_1 %>% mutate(SUM = rowSums(.))
rownames(smoke_1) = c("reference", "bc")
chi_smoke = chisq.test(smoke_1, correct = T) #p-value < 2.2e-16

mean(bc_cohort_general$Patient_Age) #60.33655
mean(reference_cohort_1$Patient_Age) #52.8027
t.test(bc_cohort_general$Patient_Age,reference_cohort_1$Patient_Age) # p-value < 2.2e-16

############
#reference again:
American_Indian_reference_cohort = read.csv("American_Indian_reference_cohort.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` = "American Indian or Alaska Native" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 35
Asian_reference_cohort = read.csv("Asian_reference_cohort.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` = "Asian" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 1101
Black_reference_cohort = read.csv("Black_reference_cohort.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` = "Black or African American" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 455
Hawaiian_reference_cohort = read.csv("Hawaiian_reference_cohort.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` = "Native Hawaiian or Other Pacific Islander" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 147
White_reference_cohort = read.csv("White_reference_cohort.csv")
#select distinct `PATIENTS`.`Patient_ID`, `PATIENTS`.`Patient_Age`, `PATIENTS`.`Patient_Sex`, `PATIENT_RACE`.`Patient_Race`,`PATIENTS`.`Patient_Smoking_Status` from `PATIENTS`,`PATIENT_RACE` where `PATIENTS`.`Patient_ID` not in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9") and `Patient_Sex`= "Female" and `Patient_Age` > 25 and `PATIENTS`.`Patient_Smoking_Status` not in ("*Unspecified", "") and `PATIENT_RACE`.`Patient_Race` = "White or Caucasian" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` order by rand() limit 4374

races_reference_cohort = rbind_list(American_Indian_reference_cohort,Asian_reference_cohort,Black_reference_cohort,Hawaiian_reference_cohort,White_reference_cohort) %>% select(-Patient_Sex)
races_reference_cohort$Patient_Race = as.factor(races_reference_cohort$Patient_Race)
races_reference_cohort = races_reference_cohort %>% mutate(Patient_Smoking_Status = ifelse(Patient_Smoking_Status == 'Current Every Day Smoker', "Heavy Tobacco Smoker",Patient_Smoking_Status))
races_reference_cohort = races_reference_cohort %>% mutate(Patient_Smoking_Status = ifelse(Patient_Smoking_Status == 'Current Some Day Smoker'| Patient_Smoking_Status == "Former Smoker", "Light Tobacco Smoker",Patient_Smoking_Status))
races_reference_cohort = races_reference_cohort %>% mutate(Patient_Smoking_Status = ifelse(Patient_Smoking_Status == 'Passive Smoke Exposure - Never Smoker', "Never Smoker",Patient_Smoking_Status))
races_reference_cohort = races_reference_cohort %>% mutate(Patient_Smoking_Status = ifelse(Patient_Smoking_Status == 'Smoker, Current Status Unknown'| Patient_Smoking_Status == "Never Assessed" | Patient_Smoking_Status == "*Unspecified" | Patient_Smoking_Status == "", "Unknown If Ever Smoked",Patient_Smoking_Status))
races_reference_cohort$Patient_Smoking_Status = as.factor(races_reference_cohort$Patient_Smoking_Status)

reference_ids = races_reference_cohort %>% select(Patient_ID)
write.csv(reference_ids, file = "reference_ids.csv")
## stat tests on demographics
race = as.data.frame(rbind(table(races_reference_cohort$Patient_Race), table(bc_cohort_general$Patient_Race)))
race = race %>% mutate(SUM = rowSums(.))
rownames(race) = c("reference", "bc")
# breast cancer does not affect races proportionally
chi_race = chisq.test(race, correct = T) #p.value =1.757243e-09
# there are more whites in the BC group than expected, and less of every other race type than expected.
# PLOT race
library(reshape2)
race_m = race %>% mutate(isReference = c(1,0)) %>% select(-SUM)
race_m = melt(race_m, id.vars = "isReference")
names(race_m) = c("isReference", "race", "count")
race_m = race_m %>% mutate(race = ifelse(isReference == 1, paste(race, "ref", sep = "_"),as.character(race))) %>% select(-isReference)
#to arrange the bars to go from high --> low use reorder() to change the levels of the factor to be based on some other column
race_plot = ggplot() + geom_bar(data = race_m, aes(x = reorder(race,-count), count, fill = race), stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#smoking
smoke = as.data.frame(rbind(table(races_reference_cohort$Patient_Smoking_Status), table(bc_cohort_general$Patient_Smoking_Status)))
smoke = smoke %>% mutate(SUM = rowSums(.))
rownames(smoke) = c("reference", "bc")
#smoking status is not what would be expected
chi_smoke = chisq.test(smoke, correct = T) #p.value = 4.821095e-23
#less heavy smokers and never smokers than expected in BC, more light smokers than expected
#PLOT smoke
smoke_m = smoke %>% mutate(isReference = c(1,0)) %>% select(-SUM)
smoke_m = melt(smoke_m, id.vars = "isReference")
names(smoke_m) = c("isReference", "smoke", "count")
smoke_m = smoke_m %>% mutate(smoke = ifelse(isReference == 1, paste(smoke, "ref", sep = "_"),as.character(smoke))) %>% select(-isReference)
#to arrange the bars to go from high --> low use reorder() to change the levels of the factor to be based on some other column
smoke_plot = ggplot() + geom_bar(data = smoke_m, aes(x = reorder(smoke,-count), count, fill = smoke), stat = "identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#age
mean(bc_cohort_general$Patient_Age) #60.33655
mean(races_reference_cohort$Patient_Age) #53.02
age = t.test(bc_cohort_general$Patient_Age,races_reference_cohort$Patient_Age) #pvalue =  4.953749e-155
# with a few outliers, BC patients are older than the general audience. 
#PLOT age
bc_tmp = as.data.frame(bc_cohort_general$Patient_Age) %>%  mutate(study = rep("bc",length(bc_cohort_general$Patient_Age)))
names(bc_tmp) = c("age","study") # special character in name didn't work for rename()
ref_tmp = as.data.frame(races_reference_cohort$Patient_Age) %>% mutate(study = rep("ref",length(races_reference_cohort$Patient_Age)))
names(ref_tmp) = c("age","study")
age_df = rbind(bc_tmp, ref_tmp) %>% mutate(study = as.factor(study))
age_plot = ggplot() + geom_boxplot(data = age_df, aes(x = study, y = age, color = study), notch = T)

########### comorbidities ################
# comorbities for all BC and all reference (complete patient history)
all_comorbid_bc_cohort_general = read.csv("all_comorbid_bc_cohort_general.csv")
#select distinct `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Name`, COUNT(`DIAGNOSES`.`ICD9_Code`) as numOccur from `DIAGNOSES` where `DIAGNOSES`.`Patient_ID` in (select distinct PATIENTS.`Patient_ID` from `PATIENTS`,`PATIENT_RACE` where PATIENTS.`Patient_ID` in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9" group by `Patient_ID` having count(`Patient_ID`) > 1) and PATIENTS.`Patient_ID` not in ("796887201257050", "198259877972305", "211516019422561", "813369655981660", "573151760734618", "35229234490544",  "620538185350597", "259266504086554","189134215470403", "88734864722937",  "821811892092228", "770099472720176", "76906170696020" ) and `PATIENT_RACE`.`Patient_Race`not in ("Other", "Unknown/Declined") and `PATIENTS`.`Patient_Sex` = "Female" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` ) GROUP BY `DIAGNOSES`.`ICD9_Code` having numOccur > 5 ORDER BY numOccur desc 
# if you were a Dr, these would be the conditions that you would be listing the most often. 
# most frequently given diagnosis
byPatient_comorbid_bc_cohort_general = read.csv("byPatient_comorbid_bc_cohort_general.csv")
# select `DIAGNOSES`.`Patient_ID`, `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Name`, COUNT(`DIAGNOSES`.`ICD9_Code`) as numOccur from `DIAGNOSES` where `DIAGNOSES`.`Patient_ID` in (select distinct PATIENTS.`Patient_ID` from `PATIENTS`,`PATIENT_RACE` where PATIENTS.`Patient_ID` in (select distinct `Patient_ID` from `DIAGNOSES` where `ICD9_Code` = "174.9" group by `Patient_ID` having count(`Patient_ID`) > 1) and PATIENTS.`Patient_ID` not in ("796887201257050", "198259877972305", "211516019422561", "813369655981660", "573151760734618", "35229234490544",  "620538185350597", "259266504086554","189134215470403", "88734864722937",  "821811892092228", "770099472720176", "76906170696020" ) and `PATIENT_RACE`.`Patient_Race`not in ("Other", "Unknown/Declined") and `PATIENTS`.`Patient_Sex` = "Female" and `PATIENTS`.`Patient_ID` = `PATIENT_RACE`.`Patient_ID` ) GROUP BY `DIAGNOSES`.`Patient_ID`,`DIAGNOSES`.`ICD9_Code` having numOccur > 1
# For each patient, this is a list of all diagnosis and the number of time each diagnosis has been assigned to them (patients given a diagnosis less than 2 times had the diagnosis ignored)
# Diagnosis given to the most people
length(unique(byPatient_comorbid_bc_cohort_general$ICD9_Code)) #there are 4465 unique codes
# right now, diagnosis codes are counted multiple times for each patient. Instead just count a code 1 time for each patient %then% group by the code %and% count the number of patients for which that code was found, also display this count as a percent of the total patients that have it
icd9_prevelance = byPatient_comorbid_bc_cohort_general %>% mutate(counter = rep.int(1,length(byPatient_comorbid_bc_cohort_general$numOccur))) %>% select(Diagnosis_Name, ICD9_Code, counter) %>% group_by(ICD9_Code) %>% summarise(SUM = sum(counter), percentWith = round(100*SUM/6112, digits = 2)) %>% arrange(desc(SUM))
#create table that maps diagnosis name to diagnosis code
map_icd9 = byPatient_comorbid_bc_cohort_general %>% distinct(ICD9_Code) %>% select(-c(Patient_ID,numOccur))
#add diagnosis names back into the prevelance table
icd9_prevelance = inner_join(icd9_prevelance, map_icd9)
#reorder columns for easier viewing
bc_icd9_prevelance = icd9_prevelance[,c(4,1,2,3)]

# reference
all_comorbid_ref_cohort = read.csv("all_comorbid_ref_cohort.csv")
#select `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Name`, COUNT(`DIAGNOSES`.`ICD9_Code`) as numOccur from `DIAGNOSES` join user_norgeotb.`Reference_IDs`  b on `DIAGNOSES`.`Patient_ID`= b.`Patient_ID` GROUP BY `DIAGNOSES`.`ICD9_Code` having numOccur > 5 ORDER BY numOccur desc;
byPatient_comorbid_ref_cohort = read.csv("byPatient_ref_cohort.csv")
#select `DIAGNOSES`.`Patient_ID`, `DIAGNOSES`.`ICD9_Code`, `DIAGNOSES`.`Diagnosis_Name`, COUNT(`DIAGNOSES`.`ICD9_Code`) as numOccur from `DIAGNOSES` join user_norgeotb.`Reference_IDs`  b on `DIAGNOSES`.`Patient_ID`= b.`Patient_ID` GROUP BY `DIAGNOSES`.`Patient_ID`,`DIAGNOSES`.`ICD9_Code` having numOccur > 1
length(unique(byPatient_comorbid_ref_cohort$ICD9_Code)) # there are 4704 unique icd9 codes
#there's missing icd9_codes, when that happens, join the code to the Diagnosis name so that all empty codes aren't treated the same
ref_icd9_prevelance = byPatient_comorbid_ref_cohort %>% mutate(ICD9_Code = ifelse(ICD9_Code == "",paste(ICD9_Code,Diagnosis_Name, sep = "_"),as.character(ICD9_Code))) %>% mutate(counter = rep.int(1,length(byPatient_comorbid_ref_cohort$numOccur))) %>% select(Diagnosis_Name, ICD9_Code, counter) %>% group_by(ICD9_Code) %>% summarise(SUM = sum(counter), percentWith = round(100*SUM/6112, digits = 2)) %>% arrange(desc(SUM))
#ref_icd9_prevelance = byPatient_comorbid_ref_cohort %>% mutate(counter = rep.int(1,length(byPatient_comorbid_ref_cohort$numOccur))) %>% select(Diagnosis_Name, ICD9_Code, counter) %>% group_by(ICD9_Code) %>% summarise(SUM = sum(counter), percentWith = round(100*SUM/6112, digits = 2)) %>% arrange(desc(SUM))
# remove the missing icd9 things inorder to create a map, tmp1 removes them from reference file, tmp2 removes them from working file
tmp2 = ref_icd9_prevelance %>% filter(grepl('_',ICD9_Code)) 
tmp2 = tmp2 %>% mutate(Diagnosis_Name = rep("missing",times = length(tmp2$ICD9_Code)))
tmp2 = tmp2[,c(4,1,2,3)]
tmp1 = byPatient_comorbid_ref_cohort %>% mutate(ICD9_Code = ifelse(ICD9_Code == "",paste(ICD9_Code,Diagnosis_Name, sep = "_"),as.character(ICD9_Code))) %>% filter(!grepl("_",ICD9_Code))
tmp = ref_icd9_prevelance %>% filter(!grepl('_',ICD9_Code))

ref_map_icd9 = tmp1 %>% distinct(ICD9_Code) %>% select(-c(Patient_ID,numOccur))
ref_icd9_prevelance_j = inner_join(tmp, ref_map_icd9)
ref_icd9_prevelance_j = ref_icd9_prevelance_j[,c(4,1,2,3)]
ref_icd9_prevelance_j = rbind(ref_icd9_prevelance_j, tmp2) %>% arrange(desc(SUM))
#### old stuff, 3 lines, same as above but without filtering for the missing icd9's
#ref_map_icd9 = byPatient_comorbid_ref_cohort %>% distinct(ICD9_Code) %>% select(-c(Patient_ID,numOccur))
#ref_icd9_prevelance = inner_join(ref_icd9_prevelance, ref_map_icd9)
#ref_icd9_prevelance = ref_icd9_prevelance[,c(4,1,2,3)]

##### ICD9 code comparisons #############
#ref_icd9_prevelance_j
# bc_icd9_prevelance
bc_icd9_prevelance$ref_SUM = NA
bc_icd9_prevelance$ref_percent = NA
for (i in bc_icd9_prevelance$ICD9_Code){
  if (i %in% ref_icd9_prevelance_j$ICD9_Code){
    bc_icd9_prevelance[as.numeric(which(bc_icd9_prevelance$ICD9_Code == i)), 5] = as.numeric(ref_icd9_prevelance_j[which(ref_icd9_prevelance_j$ICD9_Code == i, arr.ind = T),3])
    bc_icd9_prevelance[as.numeric(which(bc_icd9_prevelance$ICD9_Code == i)), 6] = as.numeric(ref_icd9_prevelance_j[which(ref_icd9_prevelance_j$ICD9_Code == i, arr.ind = T),4])
  }
  else {bc_icd9_prevelance[as.numeric(which(bc_icd9_prevelance$ICD9_Code == i)), 5] = 0; bc_icd9_prevelance[as.numeric(which(bc_icd9_prevelance$ICD9_Code == i)), 6] = 0}
}

bc_icd9_prevelance = bc_icd9_prevelance %>% mutate(ratio = round(percentWith/ref_percent, digits = 2), ratio = ifelse(is.infinite(ratio),NA,ratio)) 
#---warn
total = 6112

c = bc_icd9_prevelance$SUM
d = bc_icd9_prevelance$ref_SUM

dumdum = list()
meme = list()
for(i in 1:length(c)){
  ap = c(c[i], total - c[i])
  dumdum[[i]] = ap
  for(j in 1:length(d)){
    bp = c(d[j], total - d[j])
    meme[[j]] = bp
  }}
pvalue = list()
for(i in 1:length(dumdum)){
  print(i)
  tmp = rbind(dumdum[[i]],meme[[i]])
  print(tmp)
  mychi = chisq.test(tmp)$p.value
  pvalue[[i]] = mychi
}
pvalue = as.numeric(pvalue)

bc_icd9_prevelance$pvalue = pvalue
number_icd9_tests = 4465
bonferroni_icd9 = .05/number_icd9_tests
bc_icd9_only = bc_icd9_prevelance %>% filter(ref_percent == 0.00)
# There are 902 icd9s found in BC cohort that are not seen in reference cohort
bc_icd9_enrichment = bc_icd9_prevelance %>% arrange(desc(ratio))
bc_icd9_significant = bc_icd9_enrichment %>% filter(pvalue <= bonferroni_icd9)
#there are 281 significant drugs
write.csv(bc_icd9_enrichment, file = "bc_icd9_enrichment.csv")
write.csv(bc_icd9_significant, file = "bc_icd9_significant.csv")
bc_nonBreast_icd9_enrichment = bc_icd9_significant %>% filter(!grepl('breast',tolower(Diagnosis_Name)))
bc_icd9_highly_enriched = bc_icd9_significant %>% filter(ratio >= 5.00)

#############----------------
#repeat for the reference:
#bc_icd9_prevelance, ref_icd9_prevelance_j
ref_icd9_prevelance_j$bc_SUM = NA
ref_icd9_prevelance_j$bc_percent = NA
for (i in ref_icd9_prevelance_j$ICD9_Code){
  if (i %in% bc_icd9_prevelance$ICD9_Code){
    ref_icd9_prevelance_j[as.numeric(which(ref_icd9_prevelance_j$ICD9_Code == i)), 5] = as.numeric(bc_icd9_prevelance[which(bc_icd9_prevelance$ICD9_Code == i, arr.ind = T),3])
    ref_icd9_prevelance_j[as.numeric(which(ref_icd9_prevelance_j$ICD9_Code == i)), 6] = as.numeric(bc_icd9_prevelance[which(bc_icd9_prevelance$ICD9_Code == i, arr.ind = T),4])
  }
  else {ref_icd9_prevelance_j[as.numeric(which(ref_icd9_prevelance_j$ICD9_Code == i)), 5] = 0; ref_icd9_prevelance_j[as.numeric(which(ref_icd9_prevelance_j$ICD9_Code == i)), 6] = 0}
}

ref_icd9_prevelance_j = ref_icd9_prevelance_j %>% mutate(ratio = round(percentWith/bc_percent, digits = 2), ratio = ifelse(is.infinite(ratio),NA,ratio)) 
total = 6112

c = ref_icd9_prevelance_j$SUM
d = ref_icd9_prevelance_j$bc_SUM

dumdum = list()
meme = list()
for(i in 1:length(c)){
  ap = c(c[i], total - c[i])
  dumdum[[i]] = ap
  for(j in 1:length(d)){
    bp = c(d[j], total - d[j])
    meme[[j]] = bp
  }}
pvalue = list()
for(i in 1:length(dumdum)){
  print(i)
  tmp = rbind(dumdum[[i]],meme[[i]])
  print(tmp)
  mychi = chisq.test(tmp)$p.value
  pvalue[[i]] = mychi
}
pvalue = as.numeric(pvalue)

ref_icd9_prevelance_j$pvalue = pvalue
number_icd9_tests = 4465
bonferroni_icd9 = .05/number_icd9_tests
ref_icd9_only = ref_icd9_prevelance_j %>% filter(bc_percent == 0.00)
# There are 1600 icd9s found in BC cohort that are not seen in reference cohort
ref_icd9_enrichment = ref_icd9_prevelance_j %>% arrange(desc(ratio))
ref_icd9_significant = ref_icd9_enrichment %>% filter(pvalue <= bonferroni_icd9)
#there are 268 significant icd9's
write.csv(ref_icd9_enrichment, file = "ref_icd9_enrichment.csv")
write.csv(ref_icd9_significant, file = "ref_icd9_significant.csv")
#ref_icd9_highly_enriched = ref_icd9_significant %>% filter(ratio >= 5.00)


################ DRUGS #########################
# most frequently ordered drugs
ref_most_freq_drugs = read.csv("ref_most_freq_drugs.csv")
#select `MEDICATION_ORDERS`.`Medication_Name` , COUNT(`MEDICATION_ORDERS`.`Medication_Name`) as numOccur from `MEDICATION_ORDERS` join user_norgeotb.`Reference_IDs`  b on `MEDICATION_ORDERS`.`Patient_ID`= b.`Patient_ID` GROUP BY `MEDICATION_ORDERS`.`Medication_Name` having numOccur > 5 ORDER BY numOccur desc;
bc_most_freq_drugs = read.csv("bc_most_freq_drugs.csv")
# select `MEDICATION_ORDERS`.`Medication_Name` , COUNT(`MEDICATION_ORDERS`.`Medication_Name`) as numOccur from `MEDICATION_ORDERS` join user_norgeotb.`bc_ids`  b on `MEDICATION_ORDERS`.`Patient_ID`= b.`Patient_ID` GROUP BY `MEDICATION_ORDERS`.`Medication_Name` having numOccur > 5 ORDER BY numOccur desc;

# drugs given to the most people
ref_drugs_to_most_people = read.csv("ref_drugs_to_most_people.csv")
#select `MEDICATION_ORDERS`.`Patient_ID`, `MEDICATION_ORDERS`.`Medication_Name` , COUNT(`MEDICATION_ORDERS`.`Medication_Name`) as numOccur from `MEDICATION_ORDERS` join user_norgeotb.`Reference_IDs`  b on `MEDICATION_ORDERS`.`Patient_ID`= b.`Patient_ID` GROUP BY `MEDICATION_ORDERS`.`Patient_ID`, `MEDICATION_ORDERS`.`Medication_Name` having numOccur > 1 
bc_drugs_to_most_people = read.csv("bc_drugs_to_most_people.csv")
#select `MEDICATION_ORDERS`.`Patient_ID`, `MEDICATION_ORDERS`.`Medication_Name` , COUNT(`MEDICATION_ORDERS`.`Medication_Name`) as numOccur from `MEDICATION_ORDERS` join user_norgeotb.`bc_ids`  b on `MEDICATION_ORDERS`.`Patient_ID`= b.`Patient_ID` GROUP BY `MEDICATION_ORDERS`.`Patient_ID`, `MEDICATION_ORDERS`.`Medication_Name` having numOccur > 1 

bc_drug_prevalence = bc_drugs_to_most_people %>% mutate(counter = rep.int(1,length(bc_drugs_to_most_people$numOccur))) %>% select(Medication_Name, counter) %>% group_by(Medication_Name) %>% summarise(SUM = sum(counter), percentWith = round(100*SUM/6112, digits = 2)) %>% arrange(desc(SUM))
ref_drug_prevalence = ref_drugs_to_most_people %>% mutate(counter = rep.int(1,length(ref_drugs_to_most_people$numOccur))) %>% select(Medication_Name, counter) %>% group_by(Medication_Name) %>% summarise(SUM = sum(counter), percentWith = round(100*SUM/6112, digits = 2)) %>% arrange(desc(SUM))

bc_drug_prevalence$ref_percent = NA
bc_drug_prevalence$ref_drug_SUM = NA
for (i in bc_drug_prevalence$Medication_Name){
  if (i %in% ref_drug_prevalence$Medication_Name){
    bc_drug_prevalence[as.numeric(which(bc_drug_prevalence$Medication_Name == i)), 4] = as.numeric(ref_drug_prevalence[which(ref_drug_prevalence$Medication_Name == i, arr.ind = T),3])
    bc_drug_prevalence[as.numeric(which(bc_drug_prevalence$Medication_Name == i)), 5] = as.numeric(ref_drug_prevalence[which(ref_drug_prevalence$Medication_Name == i, arr.ind = T),2]) 
  }
  else {bc_drug_prevalence[as.numeric(which(bc_drug_prevalence$Medication_Name == i)), 4] = 0; bc_drug_prevalence[as.numeric(which(bc_drug_prevalence$Medication_Name == i)), 5] = 0}
}

bc_drug_prevalence = bc_drug_prevalence %>% mutate(ratio = round(percentWith/ref_percent, digits = 2), ratio = ifelse(is.infinite(ratio),NA,ratio)) 
total = 6112

a = bc_drug_prevalence$SUM
b = bc_drug_prevalence$ref_drug_SUM

dumdum = list()
meme = list()
for(i in 1:length(a)){
  ap = c(a[i], total - a[i])
  dumdum[[i]] = ap
  for(j in 1:length(b)){
    bp = c(b[j], total - b[j])
    meme[[j]] = bp
  }}
pvalue = list()
san = for(i in 1:length(dumdum)){
  print(i)
  tmp = rbind(dumdum[[i]],meme[[i]])
  #print(tmp)
  mychi = chisq.test(tmp)$p.value
  pvalue[[i]] = mychi
}
pvalue = as.numeric(pvalue)
bc_drug_prevalence$pvalue = pvalue
number_drug_tests = 2482
bonferroni_drug = .05/number_drug_tests
bc_drug_only = bc_drug_prevalence %>% filter(ref_percent == 0.00)
# There are 751 drugs found in BC cohort that are not seen in reference cohort
bc_drug_enrichment = bc_drug_prevalence %>% arrange(desc(ratio))
bc_drug_significant = bc_drug_enrichment %>% filter(pvalue <= bonferroni_drug)
#there are 146 significant drugs
write.csv(bc_drug_significant, file = "bc_drug_significant.csv")
#bc_drug_highly_enriched = bc_drug_prevalence %>% filter(ratio >= 5.00)
# there are 104 that are 5x more common in BC

#NEXT: do exact same thing but start w/ reference and add numbers from BC
#ref_drug_prevalence, bc_drug_prevalence
ref_drug_prevalence$bc_percent = NA
ref_drug_prevalence$bc_drug_SUM = NA
for (i in ref_drug_prevalence$Medication_Name){
  if (i %in% bc_drug_prevalence$Medication_Name){
    ref_drug_prevalence[as.numeric(which(ref_drug_prevalence$Medication_Name == i)), 4] = as.numeric(bc_drug_prevalence[which(bc_drug_prevalence$Medication_Name == i, arr.ind = T),3])
    ref_drug_prevalence[as.numeric(which(ref_drug_prevalence$Medication_Name == i)), 5] = as.numeric(bc_drug_prevalence[which(bc_drug_prevalence$Medication_Name == i, arr.ind = T),2]) 
  }
  else {ref_drug_prevalence[as.numeric(which(ref_drug_prevalence$Medication_Name == i)), 4] = 0; ref_drug_prevalence[as.numeric(which(ref_drug_prevalence$Medication_Name == i)), 5] = 0}
}

ref_drug_prevalence = ref_drug_prevalence %>% mutate(ratio = round(percentWith/bc_percent, digits = 2), ratio = ifelse(is.infinite(ratio),NA,ratio)) 
total = 6112

a = ref_drug_prevalence$SUM
b = ref_drug_prevalence$bc_drug_SUM

dumdum = list()
meme = list()
for(i in 1:length(a)){
  ap = c(a[i], total - a[i])
  dumdum[[i]] = ap
  for(j in 1:length(b)){
    bp = c(b[j], total - b[j])
    meme[[j]] = bp
  }}
pvalue = list()
san = for(i in 1:length(dumdum)){
  print(i)
  tmp = rbind(dumdum[[i]],meme[[i]])
  #print(tmp)
  mychi = chisq.test(tmp)$p.value
  pvalue[[i]] = mychi
}
pvalue = as.numeric(pvalue)

ref_drug_prevalence$pvalue = pvalue
number_drug_tests = 2482
bonferroni_drug = .05/number_drug_tests
ref_drug_only = ref_drug_prevalence %>% filter(bc_percent == 0.00)
# There are 751 drugs found in BC cohort that are not seen in reference cohort
ref_drug_enrichment = ref_drug_prevalence %>% arrange(desc(ratio))
ref_drug_significant = ref_drug_enrichment %>% filter(pvalue <= bonferroni_drug)
#there are 129 significant drugs
write.csv(ref_drug_significant, file = "ref_drug_significant.csv")
#ref_drug_highly_enriched = ref_drug_enrichment %>% filter(ratio >= 5.00)
# there are 35 that are 5x more common in ref

#mean(ref_drug_prevalence$ratio, na.rm = T) 
#mean(bc_drug_prevalence$ratio, na.rm = T) #2.33, not sure how I want to interpret this


#---------------------------
#List of plots and tables
race_plot
smoke_plot
age_plot
bc_cohort_general #demographics
all_comorbid_bc_cohort_general # what icd9 is dr that sees bc patients most likely to give
bc_icd9_prevelance # what % of bc patients have a particular icd9
bc_icd_enrichment # icd9 prevelance, ordered on the odds ratio against reference
bc_most_freq_drugs # what medications is dr that sees bc patients most likely to give
bc_drug_prevalence # what % of bc patients have a particular medication
bc_drug_enrichment # drug prevelance, ordered on the odds ratio against reference
#write.csv(reference_ids, file = "reference_ids.csv")
#todo
# NOTE: MIGHT BE VERY INTERESTING TO SEE IF THERE IS ANYTHING ABOUT MALE BC WHEN COMPARING BC SUBTYPES (go back to bc_cohort_1)
# note: when looking w/in BC should I look at both complete patient history AND repeat looking only at comorbdidities/drugs after diagnosis?
