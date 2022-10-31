## 2nd ukcogs survey

## Analysis - descriptive tables
## Adam Brentnall

##libraries
library("tidyverse")


##functions used
fn.sumstat.cat <- function(ind){

    mydat<- table(ind, useNA="always")
    mypc <- round(100*mydat/sum(mydat),0)
    myout<-paste0(mydat, " (", mypc, "%)")
    names(myout)<-names(mydat)
    myout
    }


fn.sumstat.cat.grp <- function(ind,grp){

    mydat<- table(ind, grp, useNA="always")[,1:2]
    mypc<- apply(mydat,2, function(ind) round(100*ind/sum(ind),0))
    myout<-matrix(paste0(mydat, " (", mypc, "%)"), ncol=ncol(mydat))
    rownames(myout)<-rownames(mydat)
    colnames(myout)<-colnames(mydat)
    myout
    }

fn.sumstat.cat.all <- function(ind, grp){
    col1<-fn.sumstat.cat.grp(ind, grp)
    col2<-fn.sumstat.cat(ind)
    myout<-cbind(col1    ,  col2)
    colnames(myout)[3]<-"Both"
    myout
}

##load data

mydta <- read_csv("../data/UKCOGS_Survey_2-header.csv")

## remove duplicates 
mydta <- mydta %>%
    filter(!is.na(Centre))

mydrop1 <- (mydta$name2 %in% c("The Christie Foundation Trust", "The Christie NHS FT"))

mydrop2 <- (duplicated(mydta$name1))& mydta$name1 !="Other"

mydta <- mydta %>%
    filter((mydrop1 + mydrop2) ==0)

## sites
allname<- mydta$name1
allname[allname=="Other"] <-mydta$name2[allname=="Other"]
##centres
sort(allname[mydta$Centre=="Centre"])
##units
sort(allname[mydta$Centre!="Centre"])



## 2. Regarding staffing, have you experienced significant reduction in staff numbers in your centre from 26/12/20 onwards?
fn.sumstat.cat.all(mydta$"2", mydta$Centre)

## not really - so not much point with below

## 2.a. Is this due to COVID related sickness and/or redeployment among junior doctors?
##2.a.i. Please select the overall percentage by which junior doctor numbers were reduced to the nearest percent. For example, if junior doctor staff numbers are reduced from 10 to 7 (i.e. 30%) then select 21-30%.
## 2.b. Is this due to COVID related sickness and/or redeployment among GO sub-specialty trainees?
## 2.b.i. Please select the overall percentage by which GO sub-specialty trainee numbers were reduced to the nearest percent.
##2.c. Is this due to COVID related sickness and/or redeployment among consultant staff?
## 2.c.i. Please select the overall percentage by which consultant staff numbers were reduced to the nearest percent.
##2.d. Is this due to COVID related sickness and/or redeployment among CNS staff?
## 2.d.i. Please select the overall percentage by which CNS staff numbers were reduced to the nearest percent.




##3. Have you changed the way your MDT function due to COVID?
fn.sumstat.cat.all(mydta$"3", mydta$Centre)

##3.a. Please indicate all changes to MDT functioning you have implemented.
mycats<-unique(unlist(str_split(mydta$"3a",",")))
mycats<- mycats[!is.na(mycats)]

my3a <-str_split(mydta$"3a",",")
mydta$mixed<-unlist(lapply(my3a, function(ind) sum(ind %in% mycats[1])))
mydta$virtual<-unlist(lapply(my3a, function(ind) sum(ind %in% mycats[2])))
mydta$fewerppl<-unlist(lapply(my3a, function(ind) sum(ind %in% mycats[3])))

##Undertaking mixed virtual and face to face MDT
fn.sumstat.cat.all(mydta$mixed, mydta$Centre)

##Undertaking virtual MDT"
fn.sumstat.cat.all(mydta$virtual, mydta$Centre)

##Reduced number of attendees at MDT
fn.sumstat.cat.all(mydta$fewerppl, mydta$Centre)
##3.a.i. If the number of attendees at MDT are reduced please select % reduced
fn.sumstat.cat.all(mydta$"3ai", mydta$Centre)



## 4. What proportion of your out-patient clinic is currently remote consultation?

fn.sumstat.cat.all(mydta$"41-50%", mydta$Centre)

## 5. Please answer the following questions about the proportion reduction of Gynae Oncology related activity:
##5.1.a. How much has theatre time reduced? - % Reduced

fn.sumstat.cat.all(mydta$"51a", mydta$Centre)

##5.2.a. What is the proportion of surgical cases postponed? - % Reduced
fn.sumstat.cat.all(mydta$"52a", mydta$Centre)

##5.3.a. How much has medical oncology access/capacity been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"53a", mydta$Centre)

##5.4.a. How much has clinical oncology access/capacity been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"54a", mydta$Centre)

##5.5.a. How much has theatre-based (intrauterine) brachytherapy been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"55a", mydta$Centre)

##5.6.a. How much have outpatient brachytherapy services been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"56a", mydta$Centre)

##5.7.a. How much has radiology access/capacity been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"57a", mydta$Centre)

##5.8.a. How much has pathology access/capacity been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"58a", mydta$Centre)

##5.9.a. How much has palliative care access/capacity been reduced? - % Reduced
fn.sumstat.cat.all(mydta$"59a", mydta$Centre)

##5.10.a. How much have your rapid access referrals dropped by? - % Reduced
fn.sumstat.cat.all(mydta$"510a", mydta$Centre)

##5.11.a. How much has your weekly MDT list/workload reduced by? - % Reduced
fn.sumstat.cat.all(mydta$"511a", mydta$Centre)

##6. Please tick all applicable boxes regarding PPE use in theatre:
mycats<-unique(unlist(str_split(mydta$"6",",")))
mycats<- mycats[!is.na(mycats)]

my6 <-str_split(mydta$"6",",")
mydta$ffp<-unlist(lapply(my6, function(ind) sum(ind %in% mycats[1])))
mydta$visor<-unlist(lapply(my6, function(ind) sum(ind %in% mycats[2])))
mydta$dglove<-unlist(lapply(my6, function(ind) sum(ind %in% mycats[3])))
mydta$anas<-unlist(lapply(my6, function(ind) sum(ind %in% mycats[4])))
## "FFP3 masks"
fn.sumstat.cat.all(mydta$ffp, mydta$Centre)
## Visor
fn.sumstat.cat.all(mydta$visor, mydta$Centre)
## Double glove
fn.sumstat.cat.all(mydta$dglove, mydta$Centre)
## "Anaesthesia administered in operating theatre"
fn.sumstat.cat.all(mydta$anas, mydta$Centre)


##7. Please complete the following boxes regarding patient pre-operative prep for major and minor procedures
##7.1.a. Major Procedures - Duration of patient self-isolation prior to procedure
fn.sumstat.cat.all(mydta$"71a", mydta$Centre)

##7.1.b. Major Procedures - Number of COVID swabs prior to procedure
fn.sumstat.cat.all(mydta$"71b", mydta$Centre)

##7.1.c. Major Procedures - Are patients required to be vaccinated for COVID prior to procedure?
fn.sumstat.cat.all(mydta$"71c", mydta$Centre)

##7.2.a. Minor Procedures - Duration of patient self-isolation prior to procedure
fn.sumstat.cat.all(mydta$"72a", mydta$Centre)

##7.2.b. Minor Procedures - Number of COVID swabs prior to procedure
fn.sumstat.cat.all(mydta$"72b", mydta$Centre)

##7.2.c. Minor Procedures - Are patients required to be vaccinated for COVID prior to procedure?
fn.sumstat.cat.all(mydta$"72c", mydta$Centre)

##8. Have you needed to move activity off site to another hospital due to the recent surge in cases? (e.g. Independent sector)


##8.1.a. Moved operating lists
fn.sumstat.cat.all(mydta$"81a", mydta$Centre)

##8.2.a. Moved clinics
fn.sumstat.cat.all(mydta$"82a", mydta$Centre)

##8.3.a. Moved (theatre based) intrauterine and interstitial brachytherapy
fn.sumstat.cat.all(mydta$"83a", mydta$Centre)

##8.4.a. Moved outpatient (vaginal) brachytherapy
fn.sumstat.cat.all(mydta$"84a", mydta$Centre)

##8.5.a. Moved other activity
fn.sumstat.cat.all(mydta$"85a", mydta$Centre)

##8.6.a. Not yet moved but are planning to move
fn.sumstat.cat.all(mydta$"86a", mydta$Centre)

##8.7.a. Do you need to go via a central hub or committee to access operating?
fn.sumstat.cat.all(mydta$"87a", mydta$Centre)

##8.8.a. If you are working in the independent sector because you previously moved activity to the independent sector (prior to the current 2021 surge) and have not moved back to the NHS site please tick 'yes' here
fn.sumstat.cat.all(mydta$"88a", mydta$Centre)

##8.a. When operating at different locations than normal, does your normal in-house pathology team report the specimens or are they reported by another team?
fn.sumstat.cat.all(mydta$"8a", mydta$Centre)

##9. Are you working in a COVID-free hospital?
fn.sumstat.cat.all(mydta$"9", mydta$Centre)
##9.a. If no- Do you have COVID-free/green zones within the non-COVID-free hospital?
fn.sumstat.cat.all(mydta$"9a", mydta$Centre)

##10. Are you undertaking minimal access procedures?
fn.sumstat.cat.all(mydta$"10", mydta$Centre)

##11. How would you describe yourself?
fn.sumstat.cat.all(mydta$"11", mydta$Centre)
##11.a. Other:
fn.sumstat.cat.all(mydta$"11a", mydta$Centre)

##12. Please use this box to provide any free text comments
print("===========Centre============")
unique(mydta$"12"[mydta$Centre=="Centre"])
print("===========Unit============")
unique(mydta$"12"[mydta$Centre=="Unit"])
