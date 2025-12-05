
#Script til MatA-dag

install.packages("readr")  # hvis ikke installeret
library(readr)

library(tidyverse)


#Indlæsning af data fra github

ISSP_2002 <- read_csv("https://raw.githubusercontent.com/clahanse/gymdag/refs/heads/main/ISSP_2002.csv")

ISSP_2022 <- read_csv("https://raw.githubusercontent.com/clahanse/gymdag/refs/heads/main/ISSP_2022.csv")

FV2022 <- read_csv("https://raw.githubusercontent.com/clahanse/gymdag/refs/heads/main/FV2022.csv")

# Info om spørgeskemadata

https://github.com/clahanse/gymdag

# Decimaltal

options(scipen = 999)

#Valgdata
FV2022$q22a_23[FV2022$q22a_23 %in% c(6)] <- NA
tab_kød <- table(FV2022$q22a_23) 
tab_kød

kod_gender <- table(FV2022$gender, FV2022$q22a_23)
round(prop.table(kod_gender, margin=1)*100, 2)
chisq.test(kod_gender)

model_FV2022 <- lm(q22a_23 ~ as.factor(gender) , data = FV2022)
summary(model_FV2022) 

FV2022$household_income[FV2022$household_income %in% c(12,13)] <- NA
tab_inc <- table(FV2022$household_income) 
tab_inc

model_FV2022_2 <- lm(q22a_23 ~ as.factor(gender) + as.factor(household_income), data = FV2022)
summary(model_FV2022_2) 


# data=ISSP_2002
tab_v8 <- table(ISSP_2002$v8)
tab_v8

# oplysningerne 8 og 11 bliver smidt ud
ISSP_2002$v8[ISSP_2002$v8 %in% c(8, 11, 99)] <- NA

tab_v8 <- table(ISSP_2002$v8)
tab_v8

#fordeling på den afhængige variabel v8
prop.table(tab_v8) * 100

#kryds med baggrundsvariabel
#v77 - kønsvariabel 1=mand, 2=kvinde

arb_kon <- table(ISSP_2002$v77, ISSP_2002$v8)
arb_kon
#rækkeprocenter
round(prop.table(arb_kon, margin=1)*100, 2)
#kolonneprocenter
round(prop.table(arb_kon, margin=2)*100, 2)
chisq.test(arb_kon) 

#alder - ubesvaret er blevet smidt ud
ISSP_2002$recodetalder[ISSP_2002$recodetalder %in% c(6)] <- NA
prop.table(table(ISSP_2002$recodetalder)) * 100

arb_alder <- table(ISSP_2002$recodetalder, ISSP_2002$v8)
arb_alder
#rækkeprocenter
round(prop.table(arb_alder, margin=1)*100, 2)

chisq.test(arb_alder)
#afrunding af decimaler
chi_aa <- chisq.test(arb_alder) 
cat("p-værdi:", sprintf("%.4f", chi_aa$p.value), "\n")
round(chi_aa$p.value, 7) 

#regressionsanalyse
model_ISSP_2002 <- lm(v8 ~ as.factor(v77) + as.factor(recodetalder), data = ISSP_2002)
summary(model_ISSP_2002) 



# data=ISSP_2022
tab_s1_2 <- table(ISSP_2022$s1_2)
tab_s1_2

# oplysningerne 8 og 11 bliver smidt ud
ISSP_2022$s1_2[ISSP_2022$s1_2 %in% c(8, 11, 99)] <- NA

tab_s1_2 <- table(ISSP_2022$s1_2)
tab_s1_2

prop.table(tab_s1_2) * 100

arb_kon22 <- table(ISSP_2022$bvq_01, ISSP_2022$s1_2)
arb_kon22
#rækkeprocenter
round(prop.table(arb_kon22, margin=1)*100, 2)
#kolonneprocenter
round(prop.table(arb_kon22, margin=2)*100, 2)
chisq.test(arb_kon22) 


