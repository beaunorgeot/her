setwd("/Users/beaunorgeot/Desktop")
bc_drugs_to_most_people = read.csv("bc_drugs_to_most_people.csv")
bc_drugs_to_most_people$numOccur = as.numeric(as.character(bc_drugs_to_most_people$numOccur))
t1 = dcast(bc_drugs_to_most_people, Patient_ID ~ Medication_Name, value.var = "numOccur", fill = 0)
library(reshape2)
library(dplyr)


#USE mutate_each with ifelse to create binary variables
#mutate_each(df,funs(ifelse(.>0,1,.)))
#For mutate_each see:
#http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
#and
#http://stackoverflow.com/questions/31718436/how-to-use-ifelse-inside-the-for-loop-in-r
bob = data.frame(replicate(10,sample(0:10,1000,rep=TRUE))) #10 columns, 1000 rows, numbers 1:10, sample with replacement
binary_bob = bob %>% mutate_each(funs(ifelse(.>0,1,.)))
###

#bc_drugs_to_most_people$Patient_ID = as.factor(as.character(bc_drugs_to_most_people$Patient_ID))


#### test to make sure that cast works way I want
bub = head(bc_drugs_to_most_people, n = 1000)
try1 = dcast(bub, as.factor(as.character(Patient_ID)) ~ Medication_Name, value.var = "numOccur")



bop = head(byPatient_comorbid_bc_cohort_general, n = 1000)
try2 = dcast(bop, as.factor(as.character(Patient_ID)) ~ ICD9_Code, value.var = "numOccur")

#####
drugs_cast = dcast(bc_drugs_to_most_people, Patient_ID ~ Medication_Name, value.var = "numOccur", fill = 0)
dim(drugs_cast) #3956 x 2483
# aside from the fact that something awful has happened envolving dropped patients, this is correct/good. Have checked individual patient numbers randomly too
#drugs_cast[is.na(drugs_cast)] = 0
# The Var.2 variable is weird, it's perhaps an empty string? but unique values do match that of bc_drugs_to_most_people. table(drugs_cast$Var.2) show that all values = 0. drop the column?
#drugs_cast = drugs_cast %>% select(-Var.2)
#drugs_cast_numeric = drugs_cast %>% mutate_each(funs(as.numeric),-Patient_ID)
ids_removed = drugs_cast %>% select(-Patient_ID)
row.names(ids_removed) = drugs_cast$Patient_ID

bc_cohort_general = read.csv("bc_cohort_general.csv")
small_bc_general = bc_cohort_general %>% filter(Patient_ID %in% bc_drugs_to_most_people$Patient_ID)
bob = setdiff(bc_drugs_to_most_people$Patient_ID, small_bc_general$Patient_ID) # bob = (2,3).. these are definetly not normal patient_ids
# how did patient_id (2,3) get into bc_drugs_to_most people
which(bc_drugs_to_most_people$Patient_ID == "2") # 1 observation, row 33177
which(bc_drugs_to_most_people$Patient_ID == "3") # 1 observation, row 52416
f_drugs_cast = drugs_cast %>% filter(!Patient_ID %in% bob)
f_ids_removed = f_drugs_cast %>% select(-Patient_ID)
row.names(f_ids_removed) = f_drugs_cast$Patient_ID

#drugs.prcomp = prcomp(ids_removed)
#plot(drugs.prcomp$x[,1:2])
library(ggfortify)
#autoplot(drugs.prcomp) # ooo, pretty!
#drugs.princomp = princomp(ids_removed)$scores[,1:2] #don't prefer this, use the ggplot method
#plot(drugs.princomp) #use gg
#drugs.princomp1 = princomp(ids_removed)
#autoplot(drugs.princomp1)

# If had kept patient_ids and then set rownames = patient_id column, then would have had the points in the pc plot be labeled with the patient_id
# as it is now, those points labeled 870 for example are probably the row numbers!
f_drugs.princomp1 = princomp(f_ids_removed)
save(f_drugs.princomp1, file = "f_drugs.princomp1") #load("f_drugs.princomp1")
# PC deconvolution
plot(f_drugs.princomp1) #barchart # how many pc's have variance/eigenvalue > 1
plot(f_drugs.princomp1, type = 'l') #points. There's a pretty huge elbow at 8 PC's. But the eigenvalues are still super high.
#1. get eigenvalues by (princomp1$sdev)^2 and filter for those over 1
# plot the first 2 pcs w/eigenvectors
f_drugs_eigenVector_plot = autoplot(f_drugs.princomp1, data = small_bc_general, loadings = T, loadings.label = T, loadings.label.size = 2)

# describe the first 2 pcs
#loadings are their own annoying class, must unclass to get what want
load_f_drugs.princomp1 =  as.data.frame(with(f_drugs.princomp1, unclass(loadings))) %>% select(1:10)
load_f_drugs.princomp1 = cbind(Medication_Name = rownames(load_f_drugs.princomp1), load_f_drugs.princomp1)
rownames(load_f_drugs.princomp1) = NULL

f_drugs.pc1_ordered = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.1) %>% arrange(Comp.1) #there are 6 above .2
f_drugs.pc1_desc = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.1) %>% arrange(desc(Comp.1))
f_drugs.pc1 = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.1) %>% filter(abs(Comp.1) >= .2) %>% arrange(Comp.1)

f_drugs.pc2_ordered = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.2) %>% arrange(Comp.2) # better correlates here range(-.24,.277) 
f_drugs.pc2_desc = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.2) %>% arrange(desc(Comp.2))
f_drugs.pc2 = load_f_drugs.princomp1 %>% select(Medication_Name,Comp.2) %>% filter(abs(Comp.2) >= .2) %>% arrange(Comp.2) # values

#pc plotting
race_princomp = autoplot(f_drugs.princomp1, data = small_bc_general, colour = 'Patient_Race')
smoke_princomp = autoplot(f_drugs.princomp1, data = small_bc_general, colour = 'Patient_Smoking_Status')
#younger than 40 seems rare: http://ww5.komen.org/KomenPerspectives/Breast-Cancer-in-Women-Younger-than-40.html
small_bc_general = small_bc_general %>% mutate(age_binary = ifelse(Patient_Age <= 40,"young","old"))
age_40_princomp = autoplot(f_drugs.princomp1, data = small_bc_general, colour = 'age_binary')

#specific drugs to look at: trastuzumab, pertuzumab, docetaxil?, cisplatin, what drugs for ER status?
#must get the bc_drugs list of medications for these people as being binary for they got the drug or not
f_bc_drugs_to_most_people = bc_drugs_to_most_people %>% filter(!Patient_ID %in% bob)
f_bc_drugs_to_most_people = f_bc_drugs_to_most_people %>% mutate(herceptin = ifelse(grepl("trastuzu",tolower(Medication_Name)),1,0), perjeta = ifelse(grepl("pertuzu",tolower(Medication_Name)),1,0),ibrance = ifelse(grepl("palbociclib",tolower(Medication_Name)),1,0),afinitor = ifelse(grepl("everolimus",tolower(Medication_Name)),1,0),tykerb = ifelse(grepl("lapatin",tolower(Medication_Name)),1,0))
f_bc_drugs_to_most_people$Patient_ID = as.character(f_bc_drugs_to_most_people$Patient_ID)
f_bc_drugs_to_most_people = f_bc_drugs_to_most_people %>% select(-c(Medication_Name,numOccur)) %>% group_by(Patient_ID) %>% summarise_each(funs(sum))
f_bc_drugs_to_most_people = f_bc_drugs_to_most_people %>% mutate_each(funs(ifelse(.>0,1,.)),-Patient_ID)
# back to plotting
autoplot(f_drugs.princomp1, data = f_bc_drugs_to_most_people, colour = 'herceptin')
autoplot(f_drugs.princomp1, data = f_bc_drugs_to_most_people, colour = 'perjeta')


###### TSNE() #########
f_tsne.drugs = tsne(f_ids_removed)
save(f_tsne.drugs, file = "f_binary_ids_removed.RData")
#autoplot(f_tsne.drugs, data = small_bc_general, colour = 'Patient_Race')
# don't know what the above did, but it took a long time to render and was un-discernable
#turn the tsne() results into a df, then combine them with demographics for plotting
f_tsne.drugs_df = as.data.frame(f_tsne.drugs)
f_tsne.drugs.demographics = cbind(small_bc_general,f_tsne.drugs_df)
ggplot(data = f_tsne.drugs.demographics, aes(x = V1, y = V2,colour = Patient_Race)) + geom_point()
#stopping here for now, the binary tsne() seems to have better clusters

f_binary_ids_removed = f_ids_removed %>% mutate_each(funs(ifelse(.>0,1,.)))
f_binary_tsne_drugs = tsne(f_binary_ids_removed)
save(f_binary_tsne_drugs, file = "f_binary_tsne_drugs.RData")
f_binary_tsne.drugs_df = as.data.frame(f_binary_tsne_drugs)
f_binary_tsne.drugs.demographics = cbind(small_bc_general,f_binary_tsne.drugs_df)
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = Patient_Race)) + geom_point()
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = Patient_Smoking_Status)) + geom_point()
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = age_binary)) + geom_point()

#join the drug classification table to demographics for plotting
f_binary_tsne.drugs.demographics$Patient_ID = as.character(f_binary_tsne.drugs.demographics$Patient_ID)
f_binary_tsne.drugs.demographics = inner_join(f_binary_tsne.drugs.demographics,f_bc_drugs_to_most_people)
#back to plotting
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = herceptin)) + geom_point()
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = perjeta)) + geom_point()

######## ICD9'S ###############################
#specific comorbidities to look at: ER status, gastric cancer
bc_icd9_to_most_people = read.csv("bc_icd9_to_most_people.csv")
bc_icd9_to_most_people$numOccur = as.numeric(as.character(bc_icd9_to_most_people$numOccur))
icd9_cast = dcast(bc_icd9_to_most_people, Patient_ID ~ ICD9_Code, value.var = "numOccur", fill = 0)
j = setdiff(icd9_cast$Patient_ID, bc_cohort_general$Patient_ID) # got everyone!

icd9_ids_removed = icd9_cast %>% select(-Patient_ID)
row.names(icd9_ids_removed) = icd9_cast$Patient_ID
icd9.princomp1 = princomp(icd9_ids_removed)
save(icd9.princomp1, file = "icd9.princomp1.RData") #load("icd9.princomp1.RData")
# PC deconvolution
plot(icd9.princomp1) #barchart # how many pc's have variance/eigenvalue > 1
plot(icd9.princomp1, type = 'l') #points. There's a pretty huge elbow at 8 PC's. But the eigenvalues are still super high.
#1. get eigenvalues by (princomp1$sdev)^2 and filter for those over 1
# plot the first 2 pcs w/eigenvectors
icd9_eigenVector_plot = autoplot(icd9.princomp1, data = bc_cohort_general, loadings = T, loadings.label = T, loadings.label.size = 4)
# describe the first 2 pcs
#loadings are their own annoying class, must unclass to get what want
load_icd9.princomp1 =  as.data.frame(with(icd9.princomp1, unclass(loadings))) %>% select(1:10)
load_icd9.princomp1 = cbind(ICD9_Code = rownames(load_icd9.princomp1), load_icd9.princomp1)
rownames(load_icd9.princomp1) = NULL

icd9.pc1_ordered = load_icd9.princomp1 %>% select(ICD9_Code,Comp.1) %>% arrange(Comp.1) #basically just 2, nothing interesting
icd9.pc1_desc = load_icd9.princomp1 %>% select(ICD9_Code,Comp.1) %>% arrange(desc(Comp.1))
icd9.pc1 = load_icd9.princomp1 %>% select(ICD9_Code,Comp.1) %>% filter(abs(Comp.1) >= .2) %>% arrange(Comp.1) #174.9, 199.0

icd9.pc2_ordered = load_icd9.princomp1 %>% select(ICD9_Code,Comp.2) %>% arrange(Comp.2) 
icd9.pc2_desc = load_icd9.princomp1 %>% select(ICD9_Code,Comp.2) %>% arrange(desc(Comp.2))
icd9.pc2 = load_icd9.princomp1 %>% select(ICD9_Code,Comp.2) %>% filter(abs(Comp.2) >= .2) %>% arrange(Comp.2) # V70.7, 197.7 plus 174.9 and 199.0

#pc plotting
icd9_race_princomp = autoplot(icd9.princomp1, data = bc_cohort_general, colour = 'Patient_Race')
icd9_smoke_princomp = autoplot(icd9.princomp1, data = bc_cohort_general, colour = 'Patient_Smoking_Status')
#younger than 40 seems rare: http://ww5.komen.org/KomenPerspectives/Breast-Cancer-in-Women-Younger-than-40.html
bc_cohort_general = bc_cohort_general %>% mutate(age_binary = ifelse(Patient_Age <= 40,"young","old"))
icd9_age_40_princomp = autoplot(icd9.princomp1, data = bc_cohort_general, colour = 'age_binary')

######## HERE #######
#Marina: 
#icd9: icd9_eigenVector_plot, plot(icd9.princomp1, type = 'l'), icd9.pc1, icd9.pc2
#drugs: 


f_binary_ids_removed = f_ids_removed %>% mutate_each(funs(ifelse(.>0,1,.)))
f_binary_tsne_drugs = tsne(f_binary_ids_removed)
save(f_binary_tsne_drugs, file = "f_binary_tsne_drugs.RData")
f_binary_tsne.drugs_df = as.data.frame(f_binary_tsne_drugs)
f_binary_tsne.drugs.demographics = cbind(small_bc_general,f_binary_tsne.drugs_df)
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = Patient_Race)) + geom_point()
#join the drug classification table to demographics for plotting
f_binary_tsne.drugs.demographics$Patient_ID = as.character(f_binary_tsne.drugs.demographics$Patient_ID)
f_binary_tsne.drugs.demographics = inner_join(f_binary_tsne.drugs.demographics,f_bc_drugs_to_most_people)
#back to plotting
ggplot(data = f_binary_tsne.drugs.demographics, aes(x = V1, y = V2,colour = herceptin)) + geom_point()

#-----------------------------------------------------------
#print(drugs.pca) lots of pcs, don't do this
View(drugs.pca$rotation) # much nicer
summary(drugs.pca) #mugly. Also the first pc only describes 1% of the data
plot(drugs.pca, type = "l")
biplot(drugs.pca)
str(drugs.pca)
plot(drugs.pca$x[,1:2])
dim(drugs.pca$x)
library(tsne)
tsne.drugs = tsne(ids_removed)  # this took >30mins to run
save(tsne.drugs, file = "tsne.drugs.RData")
plot(tsne.drugs) # saved at 1000x750 aspect ratio

binary_ids_removed = ids_removed %>% mutate_each(funs(ifelse(.>0,1,.)))
binary.drugs.prcomp = prcomp(binary_ids_removed)
binary.drugs.princomp1 = princomp(binary_ids_removed)
autoplot(binary.drugs.prcomp) #tmpResults
autoplot(binary.drugs.princomp1) #saved to ~/Desktop/tmpResults
binary.tsne.drugs = tsne(binary_ids_removed)
save(binary.tsne.drugs, file = "binary.tsne.drugs.RData")
plot(binary.tsne.drugs) #saved 1056x764

# Next try coloring by various different things such as race, smoking, ER status, etc
