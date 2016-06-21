setwd("/Users/beaunorgeot/Desktop/Clinical_BC") #note: the scripts are supposed to live in ~/her
library(dplyr)

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

plot_person = function(perID,drug1,drug2,drug3,status){
  # get the number of patients in the drug1 df
  d1 = drug1 %>% filter(Patient_ID == perID) %>% dim(.)
  # create vector with 0's and length of patients in drug1 df (this simply controls the y-value that this drug will be plotted at)
  y = rep(0,d1[1])
  # select the patients you want from a list, take their medication dates and the dummy variable (y) to give each medication_order_date (x,y) coordinates
  the_drug1_dates = drug1 %>% filter(Patient_ID == perID) %>% cbind(.,y) %>% select(med_date,y)
  d2 = drug2 %>% filter(Patient_ID == perID) %>% dim(.)
  y1 = rep(1,d2[1])
  the_drug2_dates = drug2 %>% filter(Patient_ID == perID) %>% cbind(.,y1) %>% select(med_date,y1)
  # d3
  d3 = drug3 %>% filter(Patient_ID == perID) %>% dim(.)
  y2 = rep(2,d3[1])
  the_drug3_dates = drug3 %>% filter(Patient_ID == perID) %>% cbind(.,y2) %>% select(med_date,y2)
  # patient alive or dead
  life = status %>% filter(Patient_ID == perID) %>% select(Patient_Status) %>% first(.)
  #plot
  my_plot = plot(the_drug1_dates, pch = 19, main = paste(perID,life, sep = ":"),xlim = get_date_range(drug1,drug2),ylim = c(-0.5, 2.5)) # set limits .5 below and above the first/final y values
  points(the_drug2_dates, col = "red", pch = 19) 
  points(the_drug3_dates, col = "blue", pch = 19)
}
# there are also graphs w/o test_ that contain the data w/o the fixed time line. The fixed timeline provides consistency but does also compress the points
# to go back to the original plotting, simply remove the xlim argument in plot_person()

# pertuzumab order times with herceptin
all_pertuzumab_order_dates = read.csv("all_pertuzumab_order_dates.csv")
#select `Patient_ID`, `Medication_Name`, `Medication_Order_Ordered_Date2` as med_date from `MEDICATION_ORDERS` where `Patient_ID` in (select `Patient_ID` from `MEDICATION_ORDERS` where `Medication_Name` like "%trastuzu%") and `Medication_Name` like "%pertuzu%" 
colnames(all_pertuzumab_order_dates)[3] = "med_date"
all_pertuzumab_order_dates$Patient_ID = as.factor(as.character(all_pertuzumab_order_dates$Patient_ID))
all_pertuzumab_order_dates$med_date = as.Date(all_pertuzumab_order_dates$med_date, format = "%Y-%m-%d")
#all_pertuzumab_order_dates %>% group_by(Patient_ID) %>% summarise(n = n()) %>% summarise(avg = mean(n), myMode = getmode(n), myMedian = median(n))
#

# herceptin w/pertuzumab
all_herceptin_w_pertuzu_order_dates = read.csv("all_herceptin_w_pertuzu_order_dates.csv")
all_herceptin_w_pertuzu_order_dates$Patient_ID = as.factor(as.character(all_herceptin_w_pertuzu_order_dates$Patient_ID))
all_herceptin_w_pertuzu_order_dates$med_date = as.Date(all_herceptin_w_pertuzu_order_dates$med_date, format = "%Y-%m-%d")

docetaxel_order_dates_on_her_per = read.csv("docetaxel_order_dates_on_her_per.csv")
length(unique(docetaxel_order_dates_on_her_per$Patient_ID)) #52
# there are only 52 people who were ordered doce, her, and per, how is this possible?
docetaxel_order_dates_on_her_per$Patient_ID = as.factor(as.character(docetaxel_order_dates_on_her_per$Patient_ID))
docetaxel_order_dates_on_her_per$med_date = as.Date(docetaxel_order_dates_on_her_per$med_date, format = "%Y-%m-%d")

####
pdf("threeD_test.pdf", h = 17, w = 17) # herceptin black, perjeta red, docetaxil blue
par(mfrow = c(10,2))
for (p in unique(docetaxel_order_dates_on_her_per$Patient_ID)) {plot_person(p,all_herceptin_w_pertuzu_order_dates,all_pertuzumab_order_dates,docetaxel_order_dates_on_her_per)}
dev.off()
###
# whose alive/dead?
her_per_dead_alive = read.csv("her_per_dead_alive.csv")
her_per_dead_alive$Patient_ID = as.factor(as.character(her_per_dead_alive$Patient_ID))
her_per_dead_alive %>% group_by(Patient_Status) %>% summarise(n = n()) # 15/122 recorded as dead

####
pdf("death_threeD_test.pdf", h = 17, w = 17) # herceptin black, perjeta red, docetaxil blue
par(mfrow = c(10,2))
for (p in unique(docetaxel_order_dates_on_her_per$Patient_ID)) {plot_person(p,all_herceptin_w_pertuzu_order_dates,all_pertuzumab_order_dates,docetaxel_order_dates_on_her_per,her_per_dead_alive)}
dev.off()
###
