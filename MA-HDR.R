############################################
##### MortBM-MA-HDR ########################
############################################

# Liechti FD, van Ettekoven CN, Bijlsma MW, Brouwer MC, van de Beek D (2025) 
# BMJ Global Health, doi: 10.1136/bmjgh-2024-016802

# Preparation ##################################################################
library(tidyverse)
library(meta)
library(metafor)
library(sf)
library(httr)
library(gtsummary)


## Definitions and lists =======================================================
tab_save <- FALSE # Should tables be saved?
img_save <- FALSE # Should plots be saved?
pt <- FALSE # Should permutation tests be performed?

'%!in%' <- function(x,y)!('%in%'(x,y))

list_Agegroup <- c(
  "Neonates", "Neonates and children", "Children", "Children and adults",
  "Adults", "Not specified")

list_age <- c(1,3,5)


# List of multinational studies attributable to a specific UNDP region
list_SSA <- c("m139", "m144", "m25", "m26", "u36")
list_LAC <- c("145")

data_merge <- read_rds("data_merge.RDS") # Dataset contains all studies, used for presentation of study characteristics
data <- read_rds("data.RDS") # Dataset contains individual study periods, used for further analyses
data1960 <- read_rds("data1960.RDS") # Dataset contains studies with their mean period in 1960 or later only
data1990 <- read_rds("data1990.RDS") # Dataset contains studies with their mean period in 1990 or later only


# Study characteristics ########################################################
data_merge %>%
  mutate(Inclusion = paste(year(Periodfrom), "-", year(Periodto))) %>%
  mutate(Firstauthor = paste0(Firstauthor, " ")) %>%
  select(Firstauthor, Year, Country, Income, Inclusion, Agegroup, 
         Patients, Deaths, Pathogen, Mergesplit) %>%
  arrange(Year, Firstauthor) 

data_merge %>%
  group_by(MultiPathogen) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

print("Total studies included")
data_merge %>%
  group_by() %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

print("Total study periods included")
data %>%
  group_by() %>%
  summarise(k = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

data_merge %>%
  group_by(Agegroup) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

data %>%
  group_by(Interval) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

data %>%
  group_by(region_wb) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

data %>%
  group_by(region_un) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))

tab_region_undp <- data %>%
  group_by(region_undp) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))
tab_region_undp

tab_region_wb <- data %>%
  group_by(region_wb) %>%
  summarise(n = n(), Episodes = sum(Patients), Deaths = sum(Deaths))
tab_region_wb

### Number of studies reporting multiple time periods
print("Multiple time periods")
data %>%
  filter(grepl("split time", Mergesplit)) %>%
  group_by(Firstauthor) %>%
  summarise(n = n()) %>%
  summarise(k = sum(n), n = n())

### Number of countries
print("Countries and multinational")
length(levels(factor(data$Country)))-1
length(data$Country[data$Country == "Multinational"])


### Study type
knitr::kable(
  data_merge %>%
    select(Periodfrom, Periodto, Studytype, SingleMulticentre) %>%
    tbl_summary() %>%
    add_n())

knitr::kable(
  data %>%
    select(Periodfrom, Periodto, Studytype, SingleMulticentre) %>%
    tbl_summary() %>%
    add_n())


### Table 1
{tab1_01 <- data_merge %>%
    group_by(Agegroup) %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    rename(Group = Agegroup) %>%
    arrange(Group)
  
  tab1_02 <- data_merge %>%
    group_by(Region) %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    rename(Group = Region) %>%
    arrange(desc(n))
  
  tab1_03 <- data_merge %>%
    group_by(Income) %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    rename(Group = Income) %>%
    arrange(desc(n))
  
  tab1_04 <- data_merge %>%
    group_by(Interval) %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    rename(Group = Interval)
  
  tab1_05 <- data_merge %>%
    group_by(Pathogen) %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    rename(Group = Pathogen)
  
  tab1 <- data_merge %>%
    group_by() %>%
    summarise(n = n(), Patients = sum(Patients), Deaths = sum(Deaths)) %>%
    add_column(Group = "All studies", .before = "n")  %>%
    add_row(tab1_01) %>%
    add_row(tab1_02) %>%
    add_row(tab1_03) %>%
    add_row(tab1_04) %>%
    add_row(tab1_05) %>%
    mutate(CfrCalc = Deaths/Patients*100) %>%
    mutate(CfrLci = (1 / Patients)*(Deaths - (1.96*Deaths^(1/2)))*100) %>%
    mutate(CfrUci = (1 / Patients)*(Deaths + (1.96*Deaths^(1/2)))*100) %>%
    mutate(MortalityRateSE = (CfrUci - CfrLci) / 3.92) %>%
    mutate_at(c("CfrCalc", "CfrUci", "CfrLci"), round, 1) %>%
    mutate("Case fatality ratio (95% CI) [%]" = paste0(CfrCalc, " (", CfrLci, "-", CfrUci, ")")) %>%
    select(-c(CfrCalc, CfrUci, CfrLci, MortalityRateSE))
}
tab1 %>% print(n = nrow(.))

### Studies conducted in high-income countries
print("High-income countries, studies")
paste0(round(1/tab1$n[tab1$Group =="All studies"]*tab1$n[tab1$Group == "high"]*100, 1), "%")
print("High-income countries, patients")
paste0(round(1/tab1$Patients[tab1$Group == "All studies"]*tab1$Patients[tab1$Group == "high"]*100, 1), "%")


#### Studies reporting sex proportions
data %>%
  group_by(SexProp) %>%
  summarise(n = n(), Nall = sum(Patients), 
            Nmale = sum(PatientsMales, na.rm = TRUE),
            Nfemale = sum(PatientsFemales, na.rm = TRUE))

data_merge %>%
  group_by(SexProp) %>%
  summarise(k = n(), Nall = sum(Patients), 
            Nmale = sum(PatientsMales, na.rm = TRUE),
            Nfemale = sum(PatientsFemales, na.rm = TRUE))

data_merge %>%
  mutate(period = paste0(year(Periodfrom), " \u2013 ", year(Periodto))) %>%
  filter(SexProp == "Yes") %>%
  select(Firstauthor, Year, Country, period, Agegroup, 
         Patients, PatientsMales, PatientsFemales, SexPropMalesCalc) %>%
  arrange(Year, Firstauthor) 

#### Studies reporting sexspecific CFRs
data %>%
  mutate(CfrSex = case_when(
    !is.na(CfrMales) ~ "Yes",
    is.na(CfrMales) ~ "No")) %>%
  group_by(CfrSex) %>%
  summarise(n = n(), Nall = sum(Patients), 
            Nmale = sum(PatientsMales, na.rm = TRUE), 
            Nfemale = sum(PatientsFemales, na.rm = TRUE))

data_merge %>%
  mutate(CfrSex = case_when(
    !is.na(CfrMales) ~ "Yes",
    is.na(CfrMales) ~ "No")) %>%
  group_by(CfrSex) %>%
  summarise(n = n(), Nall = sum(Patients), 
            Nmale = sum(PatientsMales, na.rm = TRUE), 
            Nfemale = sum(PatientsFemales, na.rm = TRUE))

data_merge %>%
  mutate(CfrSex = case_when(
    !is.na(CfrMales) ~ "Yes",
    is.na(CfrMales) ~ "No")) %>%
  filter(CfrSex == "Yes") %>%
  mutate(CfrMales = round(CfrMales, 2)) %>%
  mutate(CfrFemales = round(CfrFemales, 2)) %>%
  mutate(period = paste0(year(Periodfrom), " \u2013 ", year(Periodto))) %>%
  
  select(Firstauthor, Year, Country, period, Agegroup, 
         PatientsMales, DeathsMales, PatientsFemales, DeathsFemales,
         CfrMales, CfrFemales) %>%
  arrange(Year, Firstauthor) 


#### Studies reporting CFR-M:F ratio
data %>%
  mutate(CfrSex = case_when(
    !is.na(CfrMales) | !is.na(CfrFemales)~ "Yes",
    is.na(CfrMales) ~ "No")) %>%
  group_by(CfrSex) %>%
  summarise(n = n())

data_merge %>%
  mutate(CfrSex = case_when(
    !is.na(CfrMales) | !is.na(CfrFemales)~ "Yes",
    is.na(CfrMales) ~ "No")) %>%
  group_by(CfrSex) %>%
  summarise(n = n())


tab1 %>%
  rename("Mean study periods (k)" = n) %>%
  rename("Episodes (n)" = Patients) %>%
  rename("Deaths (n)" = Deaths) %>%
  filter(Group %in% c("All studies", list_Agegroup[list_age], "high", "low",
                      levels(data_merge$Interval)))

data_merge %>%
  group_by(Pathogen) %>%
  summarise(n = n(), Patients = sum(Patients))


## CFR =========================================================================
hist(data$CfrCalc) # explore distribution -> not normally distributed

for (k in c(list_Agegroup[c(1,3,5)], "All")) {
  
  if(k != "All")  {
    data_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data, measure = "PLO") %>% # logit transformation
      filter(Agegroup == k)  }
  
  if(k == "All")  {
    data_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data, measure = "PLO") # logit transformation
  }
  
  ma_all <- metaprop(Deaths, Patients, studlab = Studyauthor, data = data_ma, 
                     sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                     #control = list(maxiter = 1000),
                     prediction = TRUE, subgroup = region_wb)
  
  if(tab_save)  {  sink(file = paste0("ma_cfr_", k, ".txt"))  }
  print(summary(ma_all))
  if(tab_save)  {  sink(file = NULL)  }
  
  ### Forest plot
  if(img_save)  {
    tiff(paste0("Rplot_Forest_cfr_region_", k, "_wb.tif"), unit = "mm",
         width = 2.5*107, height = 3.5*80, res = 300)  }
  metafor::forest(ma_all, 
                  sortvar = Periodmean,
                  study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                  print.subgroup.name = FALSE, pooled.events = TRUE,
                  leftlabs = c("Subgroup", "Deaths", "Total"),
                  rightlabs = c("CFR", "[95% CI]"),
                  prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                  print.tau2 = TRUE, common = FALSE, smlab = "",
                  col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                  digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                  prediction.subgroup = k.w >= 5,
                  fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
  if(img_save)  { dev.off() }
  
  if(img_save)  {
    pdf(paste0("Rplot_Forest_cfr_region_", k, ".pdf"), 
        width = 10, height = 10)
    metafor::forest(ma_all, 
                    sortvar = Periodmean,
                    study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                    print.subgroup.name = FALSE, pooled.events = TRUE,
                    leftlabs = c("Subgroup", "Deaths", "Total"),
                    rightlabs = c("CFR", "[95% CI]"),
                    prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                    print.tau2 = TRUE, common = FALSE, 
                    smlab = "",
                    
                    col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                    digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                    
                    
                    prediction.subgroup = k.w >= 5,
                    fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
    dev.off() 
  }
  
  ### Funnel plot
  if(img_save)  {
    tiff(paste0("Rplot_Funnel_cfr_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  meta::funnel(ma_all, studlab = FALSE, yaxis = "size", common = FALSE)
  title(k)
  if(img_save)  { dev.off() }
  
  
  ### HDI (meta-regression) ------------------------------------------------------
  metareg_all <- meta::metareg(ma_all, hdi)
  if(pt) {  metareg_all_pt <- permutest(metareg_all)}
  
  if(tab_save)  {  sink(file = paste0("metareg_cfr_hdi_", k, ".txt"))  }
  print(summary(metareg_all))
  print("Permutation test:")
  if(pt) {  print(metareg_all_pt)  }
  
  if(tab_save)  {  sink(file = NULL)   }
  if(img_save)  {
    tiff(paste0("Rplot_metareg_cfr_hdi_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  metafor::regplot(metareg_all, mod = "hdi", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                   xlim = c(0, 1), pi = TRUE, 
                   #ylim = c(0, 1),
                   ylab = "Case fatality ratio", xlab = "Human Development Index")
  title(k)
  if(img_save)  { dev.off() }
  
  if(img_save)  {
    pdf(paste0("Rplot_metareg_cfr_hdi_", k, ".pdf"), 
        width = 10, height = 7.5)
    metafor::regplot(metareg_all, 
                     mod = "hdi", transf = transf.ilogit, 
                     legend = FALSE, bg = scales::alpha("#00A087B2", .3), pi = TRUE,
                     #ylim = c(0, 1), xlim = c(0, 1),  
                     ylab = "Case fatality ratio", xlab = "Human Development Index")
    dev.off() 
  }
}


### Gini index (meta-regression) ----------------------------------------------- 
hist(data1960$CfrCalc) # explore distribution -> not normally distributed

for (k in c(list_Agegroup[c(1,3,5)], "All")) {
  
  if(k != "All")  {
    data1960_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data1960, measure = "PLO") %>% # logit transformation
      filter(Agegroup == k)  }
  
  if(k == "All")  {
    data1960_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data1960, measure = "PLO") %>% # logit transformation
      filter(!is.na(region_wb)) }
  
  ma_1960 <- metaprop(
    Deaths, Patients, 
    studlab = Studyauthor, data = data1960_ma, 
    sm = "PLO",
    method.tau = "REML", method.ci = "NAsm", 
    #control = list(maxiter = 1000),
    prediction = TRUE, subgroup = region_wb)
  
  if(tab_save)  {  sink(file = paste0("ma_cfr1960_", k, ".txt"))  }
  print(summary(ma_1960))
  if(tab_save)  {  sink(file = NULL)  }
  
  
  #### Forest plot
  if(img_save)  {
    tiff(paste0("Rplot_Forest_cfr1960_", k, "_region.tif"), 
         unit = "mm", width = 2.5*107, height = 3.5*80, res = 300)  }
  metafor::forest(ma_1960, 
                  sortvar = Periodmean,
                  study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                  print.subgroup.name = FALSE, pooled.events = TRUE,
                  leftlabs = c("Subgroup", "Deaths", "Total"),
                  rightlabs = c("CFR", "[95% CI]"),
                  prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                  print.tau2 = TRUE, common = FALSE, 
                  smlab = "",
                  
                  col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                  digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                  
                  
                  prediction.subgroup = k.w >= 5,
                  fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1)
  )
  if(img_save)  { dev.off() }
  
  if(img_save)  {
    pdf(paste0("Rplot_Forest_cfr1960_", k, "_region.pdf"), 
        width = 10, height = 10)  
    metafor::forest(ma_1960, 
                    sortvar = Periodmean,
                    study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                    print.subgroup.name = FALSE, pooled.events = TRUE,
                    leftlabs = c("Subgroup", "Deaths", "Total"),
                    rightlabs = c("CFR", "[95% CI]"),
                    prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                    print.tau2 = TRUE, common = FALSE, 
                    smlab = "",
                    
                    col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                    digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                    
                    
                    prediction.subgroup = k.w >= 5,
                    fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1)
    )
    dev.off() 
  }
  
  
  #### Funnel plot
  if(img_save)  {
    tiff(paste0("Rplot_Funnel_cfr1960_", k, ".tif"),
         unit = "mm", width = 2*107, height = 2*80, res = 300)  }
  meta::funnel(ma_1960, studlab = FALSE, yaxis = "size", common = FALSE)
  title("Mean study period 1960 or later")
  if(img_save)  { dev.off() }
  
  
  #### Meta-regression
  metareg_gini <- meta::metareg(ma_1960, gini)
  if(pt) {  metareg_gini_pt <- permutest(metareg_gini)  }
  if(tab_save)  {  
    sink(file = paste0("metareg_cfr1960_gini_", k, ".txt"))  }
  print(summary(metareg_gini))
  print("Permutation test:")
  if(pt) {  print(metareg_gini_pt)  }
  if(tab_save)  {  sink(file = NULL)   }
  
  if(img_save)  {
    tiff(paste0("Rplot_metareg_cfr1960_gini_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  metafor::regplot(metareg_gini, pi = TRUE, mod = "gini", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), #label = TRUE,
                   #xlim = c(0, 100), 
                   #ylim = c(0, 1),
                   ylab = "Case fatality ratio", xlab = "Gini index")
  title(paste0("Mean study period 1960 or later, ", k))
  if(img_save)  { dev.off() }
}



### GII - (meta-regression) ----------------------------------------------------
hist(data1990$CfrCalc) # explore distribution -> not normally distributed

data1990_ma <- metafor::escalc(
  xi = PatientsMales, ni = Patients, data = data1990, measure = "PLO") %>% # logit transformation
  filter(!is.na(gii)) 

for (k in c(list_Agegroup[c(1,3,5)], "All")) {
  
  if(k != "All")  {
    data1990_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data1990, measure = "PLO") %>% # logit transformation
      filter(Agegroup == k)  
  }
  
  if(k == "All")  {
    data1990_ma <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data1990, measure = "PLO") # logit transformation
  }
  
  ma_1990 <- metaprop(
    Deaths, Patients, 
    studlab = Studyauthor, data = data1990_ma, 
    sm = "PLO",
    method.tau = "REML", method.ci = "NAsm", 
    #control = list(maxiter = 1000),
    prediction = TRUE, subgroup = region_wb)
  
  if(tab_save)  {  sink(file = paste0("ma_cfr1990_", k, ".txt"))  }
  print(summary(ma_1990))
  if(tab_save)  {  sink(file = NULL)  }
  
  
  #### Forest plot
  if(img_save)  {
    tiff(paste0("Rplot_Forest_cfr1990_", k, "_region.tif"), 
         unit = "mm", width = 2.5*107, height = 3.5*80, res = 300)  }
  metafor::forest(ma_1990, 
                  sortvar = Periodmean,
                  study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                  print.subgroup.name = FALSE, pooled.events = TRUE,
                  leftlabs = c("Subgroup", "Deaths", "Total"),
                  rightlabs = c("CFR", "[95% CI]"),
                  prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                  print.tau2 = TRUE, common = FALSE, 
                  smlab = "",col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                  digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                  prediction.subgroup = k.w >= 5,
                  fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
  if(img_save)  { dev.off() }
  
  if(img_save)  {
    pdf(paste0("Rplot_Forest_cfr1990_", k, "_region.pdf"), 
        width = 10, height = 10)  
    metafor::forest(ma_1990, 
                    study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                    print.subgroup.name = FALSE, pooled.events = TRUE,
                    leftlabs = c("Subgroup", "Deaths", "Total"),
                    rightlabs = c("CFR", "[95% CI]"),
                    prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                    print.tau2 = TRUE, common = FALSE, 
                    smlab = "",
                    col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                    digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                    prediction.subgroup = k.w >= 5,
                    sortvar = Periodmean,
                    fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
    dev.off() 
  }
  
  
  #### Funnel plot
  if(img_save)  {
    tiff(paste0("Rplot_Funnel_cfr1990_", k, ".tif"),
         unit = "mm", width = 2*107, height = 2*80, res = 300)  }
  meta::funnel(ma_1990, studlab = FALSE, yaxis = "size", common = FALSE)
  title("Mean study period 1990 or later")
  if(img_save)  { dev.off() }
  
  
  #### Meta-regression
  metareg_gii <- meta::metareg(ma_1990, gii)
  if(pt) {  metareg_gii_pt <- permutest(metareg_gii)  }
  if(tab_save)  {  
    sink(file = paste0("metareg_cfr1990_gii_", k, ".txt"))  }
  print(summary(metareg_gii))
  print("Permutation test:")
  if(pt) {  print(metareg_gii_pt)  }
  if(tab_save)  {  sink(file = NULL)   }
  
  if(img_save)  {
    tiff(paste0("Rplot_metareg_cfr1990_gii_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  metafor::regplot(metareg_gii, pi = TRUE, mod = "gii", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), #label = TRUE,
                   #xlim = c(0, 100), 
                   #ylim = c(0, 1),
                   ylab = "Case fatality ratio", xlab = "Gender Inequality Index")
  title(paste0("Mean study period 1990 or later, ", k))
  if(img_save)  { dev.off() }
}



## Gender inequality I (sex proportions) =======================================
hist(data$SexPropMales) # explore distribution -> not normally distributed


dataSex <- data %>%
  filter(!is.na(PatientsMales) & !is.na(PatientsFemales) & !is.na(Patients)) 

data_ma_sex <- metafor::escalc(xi = PatientsMales, ni = Patients, data = dataSex, 
                               measure = "PLO")  # logit transformation

ma_sexprop <- metaprop(PatientsMales, Patients, studlab = Studyauthor, 
                       data = data_ma_sex,
                       sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                       #control = list(maxiter = 1000),
                       prediction = TRUE, subgroup = region_wb)


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_sexprop_region.tif", unit = "mm",
       width = 2.5*107, height = 3.5*80, res = 300)  }
metafor::forest(ma_sexprop, ref = 0.5,
                study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup", "Males", "Total"),
                rightlabs = c("Proportion of\n male patients", "[95% CI]"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5, 
                fs.hetstat = 8, col.subgroup = "black", details = TRUE)
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_sexprop.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_sexprop, studlab = FALSE, yaxis = "size", common = FALSE)
title("All studies")
if(img_save)  { dev.off() }


### Meta-regression sex proportions vs year ------------------------------------
metareg_sexprop_periodmean <- meta::metareg(ma_sexprop, Periodmean)
if(pt) {  metareg_sexprop_periodmean_pt <- permutest(metareg_sexprop_periodmean)  }
if(tab_save)  {  
  sink(file = paste0("metareg_sexprop_periodmean_", k, ".txt"))  }
print(summary(metareg_sexprop_periodmean))
print("Permutation test:")
if(pt) {  print(metareg_sexprop_periodmean_pt)  }
if(tab_save)  {  sink(file = NULL)   }
if(img_save)  {
  tiff("Rplot_metareg_sexprop_periodmean.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_sexprop_periodmean, pi = TRUE, mod = "Periodmean", 
                 transf = transf.ilogit, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                 #xlim = c(0, 100), 
                 ylim = c(0, 1), refline = 0.5,
                 ylab = "Proportion of males", xlab = "Year")
title("All studies")
if(img_save)  { dev.off() }


### HDI (meta-regression) ------------------------------------------------------
metareg_sexprop <- meta::metareg(ma_sexprop, hdi)

if(pt) {  metareg_sexprop_pt <- permutest(metareg_sexprop)  }
if(tab_save)  {  sink(file = "metareg_sexprop_hdi.txt")  }
print(summary(metareg_sexprop))
print("Permutation test:")
if(pt) {  print(metareg_sexprop_pt)  }
if(tab_save)  {  sink(file = NULL)   }

if(img_save)  {
  tiff("Rplot_metareg_sexprop_hdi.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_sexprop, pi = TRUE, mod = "hdi", transf = transf.ilogit, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                 #xlim = c(0, 100), 
                 ylim = c(0, 1), refline = 0.5,
                 ylab = "Proportion of males", xlab = "Human Development Index")
title("All studies")
if(img_save)  {    dev.off() }

if(img_save)  {  pdf(paste0("Rplot_metareg_sexprop_hdi.pdf"), 
                     width = 10, height = 7.5)  
  metafor::regplot(metareg_sexprop, pi = TRUE, mod = "hdi", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                   #xlim = c(0, 100), 
                   ylim = c(0, 1), refline = 0.5,
                   ylab = "Proportion of males", xlab = "Human Development Index")
  dev.off() }


### Meta-analysis and meta-regression with sex proportion and GII --------------

ma_1990 <- metaprop(PatientsMales, Patients, studlab = Studyauthor, data = data1990_ma, 
                    sm = "PLO", method.tau = "DL", method.ci = "NAsm", 
                    #control = list(maxiter = 1000),
                    prediction = TRUE, subgroup = region_wb)

if(tab_save)  {  sink(file = "ma_sexprop_gii.txt")  }
summary(ma_1990)
if(tab_save)  {  sink(file = NULL)  }

### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_sexprop_gii1990.tif", unit = "mm",
       width = 2.5*107, height = 3.5*80, res = 300)  }
metafor::forest(ma_1990, 
                sortvar = Periodmean, ref = 0.5,
                study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup", "Deaths", "Total"),
                rightlabs = c("M:F ratio", "[95% CI]"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_sexprop_gii1990.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_1990, studlab = FALSE, yaxis = "size", common = FALSE)
title("Mean study period 1990 and later")
if(img_save)  { dev.off() }

### Meta-regression with GII ---------------------------------------------------
metareg_gii <- meta::metareg(ma_1990, gii)

if(pt) {  metareg_gii_pt <- permutest(metareg_gii)  }
if(tab_save)  {  sink(file = "metareg_sexprop_gii1990.txt")  }
summary(metareg_gii)
print("Permutation test:")
if(pt) {  print(metareg_gii_pt)  }
if(tab_save)  {  sink(file = NULL)   }

if(img_save)  {
  tiff("Rplot_metareg_sexprop_gii1990.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_gii, pi = TRUE, mod = "gii", transf = transf.ilogit, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                 ylim = c(0, 1), refline = 0.5, #xlim = c(1, 0), 
                 ylab = "Proportion of males", xlab = "Gender Inequality Index")
title("Mean study period 1990 or later")
if(img_save)  { dev.off() }

if(img_save)  {  pdf("Rplot_metareg_sexprop_gii1990.pdf", 
                     width = 10, height = 7.5)  
  metafor::regplot(metareg_gii, pi = TRUE, mod = "gii", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                   ylim = c(0, 1), refline = 0.5, #xlim = c(1, 0), 
                   ylab = "Proportion of males", xlab = "Gender Inequality Index")
  #title("Mean study period 1990 or later")
  dev.off() 
}

## Gender inequality II (CFR-M:F ratio) ========================================
hist(data$CfrMales) # explore distribution -> not normally distributed
hist(data$CfrFemales) # explore distribution -> not normally distributed

dataSex <- data %>%
  filter(!is.na(PatientsMales) & !is.na(DeathsMales))

### CFR males by region --------------------------------------------------------
data_ma_males <- metafor::escalc(xi = DeathsMales, ni = PatientsMales, 
                                 data = dataSex, 
                                 measure = "PLO")  # logit transformation

ma_males <- metaprop(DeathsMales, PatientsMales, studlab = Studyauthor, data = data_ma_males, 
                     sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                     #control = list(maxiter = 1000),
                     prediction = TRUE, subgroup = region_wb)

if(tab_save)  {  sink(file = "ma_cfrMales_region.txt")  }
summary(ma_males)
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrMales_region.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_males$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_males, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup\nAuthor (year) [study period]", "Deaths", "Total"),
                rightlabs = c("CFR", "[95% CI]", "Weight"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, 
                smlab = "", 
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfrMales.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_males, studlab = FALSE, yaxis = "size", common = FALSE)
title("Male patients")
if(img_save)  { dev.off() }


### CFR males by age group -----------------------------------------------------
ma_males_agegroup <- metaprop(DeathsMales, PatientsMales, studlab = Studyauthor, 
                              data = data_ma_males, prediction = TRUE, 
                              sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                              #control = list(maxiter = 1000),
                              subgroup = Agegroup)

if(tab_save)  {  sink(file = "ma_cfrMales_agegroup.txt")  }
print(summary(ma_males_agegroup))
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrMales_agegroup.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_males_agegroup$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_males_agegroup, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftcols = c("studlab"), 
                leftlabs = "Subgroup\nAuthor (year) [study period]",
                rightlabs = c("Case Fatality Ratio", "[95% CI]", "Weight"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }



### CFR females by region ------------------------------------------------------
data_ma_females <- metafor::escalc(xi = DeathsFemales, ni = PatientsFemales, 
                                   data = dataSex,
                                   measure = "PLO")  # logit transformation

ma_females <- metaprop(DeathsFemales, PatientsFemales, 
                       data = data_ma_females,
                       sm = "PLO", method.tau = "REML", method.ci = "NAsm",
                       #control = list(maxiter = 1000),
                       prediction = TRUE, subgroup = region_wb,
                       studlab = Studyauthor)

if(tab_save)  {  sink(file = "ma_cfrFemales_region.txt")  }
summary(ma_females)
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrFemales_region.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_females$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_females, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup\nAuthor (year) [study period]", "Deaths", "Total"),
                rightlabs = c("CFR", "[95% CI]", "Weight"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, 
                smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }


### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfrFemales.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_females, studlab = FALSE, yaxis = "size", common = FALSE)
title("All studies, female patients")
if(img_save)  { dev.off() }


### CFR females by age group ---------------------------------------------------
ma_females_agegroup <- metaprop(DeathsFemales, PatientsFemales, studlab = Studyauthor, 
                                data = data_ma_females, prediction = TRUE, 
                                sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                                #control = list(maxiter = 1000),
                                subgroup = Agegroup)

if(tab_save)  {  sink(file = "ma_cfrFemales_agegroup.txt")  }
print(summary(ma_females_agegroup))
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrFemales_agegroup.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_females_agegroup$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_females_agegroup, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = "Subgroup\nAuthor (year) [study period]",
                rightlabs = c("Case Fatality Ratio", "[95% CI]", "Weight"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "", 
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }


### CFR-M:F ratio by region_wb -------------------------------------------------
ma_CfrMtoF <- metabin(event.e = DeathsMales, n.e = PatientsMales,
                      event.c = DeathsFemales, n.c = PatientsFemales,
                      data = dataSex, 
                      studlab = Studyauthor, prediction = TRUE,
                      sm = "RR", subgroup = region_wb,
                      MH.exact = TRUE)

if(tab_save)  {  sink(file = "ma_cfrMtoF_region.txt")  }
summary(ma_CfrMtoF)
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrMtoF_region.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_CfrMtoF$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_CfrMtoF, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup\nAuthor (year) [study period]", "Deaths", "Total", "Deaths", "Total"),
                rightlabs = c("CFR ratio", "[95% CI]", "Weight"),
                label.e = "Males", label.c = "Females",
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, 
                smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0.25,4),
                label.right = "Male CFR higher",
                label.left = "Female CFR higher")
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfrMtoF.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_CfrMtoF, studlab = FALSE, yaxis = "size", common = FALSE)
title("All studies")
if(img_save)  { dev.off() }


### CFR-M:F ratio by age group -------------------------------------------------
ma_CfrMtoF_agegroup <- metabin(event.e = DeathsMales, n.e = PatientsMales,
                               event.c = DeathsFemales, n.c = PatientsFemales,
                               data = dataSex, studlab = Studyauthor, prediction = TRUE,
                               sm = "RR", subgroup = Agegroup, incr = 0.5,
                               MH.exact = TRUE)

if(tab_save)  {  sink(file = "ma_cfrMtoF_agegroup.txt")  }
print(summary(ma_CfrMtoF_agegroup))
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrMtoF_agegroup.tif", unit = "mm",
       width = 2.5*107, height = (round((ma_CfrMtoF_agegroup$k.all)/12, 0)+2)*80+20, 
       res = 300)  }
metafor::forest(ma_CfrMtoF_agegroup, 
                sortvar = Periodmean,
                study.results = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup\nAuthor (year) [study period]", "Deaths", "Total", "Deaths", "Total"),
                rightlabs = c("CFR ratio", "[95% CI]", "Weight"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, 
                smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0.25,4),
                label.right = "Male CFR higher",
                label.left = "Female CFR higher")
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfrMtoF_agegroup.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_CfrMtoF_agegroup, studlab = FALSE, yaxis = "size", common = FALSE)
title("All studies")
if(img_save)  { dev.off() }


### HDI (meta-regression) ------------------------------------------------------
metareg_cfrMtoF_hdi <- meta::metareg(ma_CfrMtoF, hdi)
if(pt) {  metareg_cfrMtoF_hdi_pt <- permutest(metareg_cfrMtoF_hdi)  }
if(tab_save)  {  sink(file = "metareg_cfrMtoF_hdi.txt")  }
summary(metareg_cfrMtoF_hdi)
print("Permutation test:")
if(pt) {  print(metareg_cfrMtoF_hdi_pt)  }
if(tab_save)  {  sink(file = NULL)   }
if(img_save)  {
  tiff("Rplot_metareg_cfrMtoF_hdi.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_cfrMtoF_hdi, pi = TRUE, mod = "hdi", transf = exp, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3),
                 refline = 1, #ylim = c(0, 1), xlim = c(0, 1), 
                 ylab = "M:F CFR ratio", xlab = "Human Development Index"
)
title("All studies")
if(img_save)  { dev.off() }

if(img_save)  {  pdf("Rplot_metareg_cfrMtoF_hdi.pdf", 
                     width = 10, height = 7.5) 
  metafor::regplot(metareg_cfrMtoF_hdi, 
                   mod = "hdi", transf = exp, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3),
                   refline = 1, pi = TRUE, #ylim = c(0, 1), xlim = c(0, 1), 
                   ylab = "Male-to-female CFR ratio", 
                   xlab = "Human Development Index")
  dev.off() }


### GII (meta-regression) ------------------------------------------------------
ma_CfrMtoF_gii <- metabin(event.e = DeathsMales, n.e = PatientsMales,
                          event.c = DeathsFemales, n.c = PatientsFemales,
                          data = data1990, studlab = Studyauthor, prediction = TRUE,
                          sm = "RR", subgroup = region_wb, incr = 0.5,
                          MH.exact = TRUE)

if(tab_save)  {  sink(file = "ma_cfrMtoF_gii_region.txt")  }
summary(ma_CfrMtoF_gii)
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_cfrMtoF_gii_region.tif", unit = "mm",
       width = 2.5*107, height = 3.5*80, res = 300)  }
metafor::forest(ma_CfrMtoF_gii, 
                sortvar = Periodmean,
                study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup", "Deaths", "Total"),
                rightlabs = c("M:F CFR ratio", "[95% CI]"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE)
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfrMtoF_gii.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_CfrMtoF_gii, studlab = FALSE, yaxis = "size", common = FALSE)
title("Mean study period 1990 or later")
if(img_save)  { dev.off() }

### Meta-regression with GII
metareg_cfrMtoF_gii <- meta::metareg(ma_CfrMtoF_gii, ~gii)
if(pt) {  metareg_cfrMtoF_gii_pt <- permutest(metareg_cfrMtoF_gii)  }
if(tab_save)  {  sink(file = "metareg_cfrMtoF_gii.txt")  }
summary(metareg_cfrMtoF_gii)
print("Permutation test:")
if(pt) {  print(metareg_cfrMtoF_gii_pt)  }
if(tab_save)  {  sink(file = NULL)   }
if(img_save)  {
  tiff("Rplot_metareg_cfrMtoF_gii.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_cfrMtoF_gii, pi = TRUE, mod = "gii", transf = exp, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                 refline = 1, #xlim = c(1, 0), ylim = c(0, 1), 
                 #lcol = c("black", "black", "black", "blue"),
                 ylab = "M:F CFR ratio", xlab = "Gender Inequality Index",
                 #label = "all", labsize = 0.9, offset = 0.9
)
title("Mean study period 1990 or later")
if(img_save)  { dev.off() }





## Sensitivity analysis (studies after 2000 only) ==============================
data2000 <- data %>%
  filter(Periodmean > 2000)

### Sex proportions ------------------------------------------------------------
dataSex2000 <- data2000 %>%
  filter(!is.na(PatientsMales)&!is.na(PatientsFemales)&!is.na(Patients)) 

data_ma_sex2000 <- metafor::escalc(xi = PatientsMales, ni = Patients, 
                                   data = dataSex2000, 
                                   measure = "PLO")  # logit transformation

ma_sexprop2000 <- metaprop(PatientsMales, Patients, studlab = Studyauthor, 
                           data = data_ma_sex2000,
                           sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                           #control = list(maxiter = 1000),
                           prediction = TRUE, subgroup = region_wb)

if(tab_save)  {  sink(file = "ma_sexprop2000_region.txt")  }
print(summary(ma_sexprop2000))
if(tab_save)  {  sink(file = NULL)  }


### Forest plot
if(img_save)  {
  tiff("Rplot_Forest_sexprop2000_region.tif", unit = "mm",
       width = 2.5*107, height = 3.5*80, res = 300)  }
metafor::forest(ma_sexprop2000, ref = 0.5,
                study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w > 4,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup", "Deaths", "Total"),
                rightlabs = c("Proportion of\n male patients", "[95% CI]"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE)
if(img_save)  { dev.off() }

### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_sexprop2000.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_sexprop, studlab = FALSE, yaxis = "size", common = FALSE)
title("All studies")
if(img_save)  { dev.off() }

### CFR ------------------------------------------------------------------------
hist(data2000$CfrCalc) # explore distribution -> not normally distributed

for (k in c(list_Agegroup[c(1,3,5)], "All")) {
  
  if(k != "All")  {
    data_ma2000 <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data2000, measure = "PLO") %>% # logit transformation
      filter(Agegroup == k)  }
  
  if(k == "All")  {
    data_ma2000 <- metafor::escalc(
      xi = Deaths, ni = Patients, data = data2000, measure = "PLO")
  }
  
  ma_2000 <- metaprop(Deaths, Patients, studlab = Studyauthor, data = data_ma2000, 
                      sm = "PLO", method.tau = "REML", method.ci = "NAsm", 
                      #control = list(maxiter = 1000),
                      prediction = TRUE, subgroup = region_wb)
  
  if(tab_save)  {  sink(file = paste0("ma_cfr2000_", k, ".txt"))  }
  print(summary(ma_2000))
  if(tab_save)  {  sink(file = NULL)  }
  
  
  ### Forest plot
  if(img_save)  {
    tiff(paste0("Rplot_Forest_cfr2000_region_", k, "_.tif"), unit = "mm",
         width = 2.5*107, height = 3.5*80, res = 300)  }
  metafor::forest(ma_2000, 
                  sortvar = Periodmean,
                  study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                  print.subgroup.name = FALSE, pooled.events = TRUE,
                  leftlabs = c("Subgroup", "Deaths", "Total"),
                  rightlabs = c("CFR", "[95% CI]"),
                  prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                  print.tau2 = TRUE, common = FALSE, smlab = "",
                  col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                  digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                  prediction.subgroup = k.w >= 5,
                  fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
  if(img_save)  { dev.off() }
  
  if(img_save)  {
    pdf(paste0("Rplot_Forest_cfr2000_region_", k, ".pdf"), 
        width = 10, height = 10)  
    metafor::forest(ma_2000, 
                    sortvar = Periodmean,
                    study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                    print.subgroup.name = FALSE, pooled.events = TRUE,
                    leftlabs = c("Subgroup", "Deaths", "Total"),
                    rightlabs = c("CFR", "[95% CI]"),
                    prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                    print.tau2 = TRUE, common = FALSE, smlab = "",
                    col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                    digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                    prediction.subgroup = k.w >= 5,
                    fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
    dev.off() 
  }
  
  ### Funnel plot
  if(img_save)  {
    tiff(paste0("Rplot_Funnel_cfr2000_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  meta::funnel(ma_2000, studlab = FALSE, yaxis = "size", common = FALSE)
  title(k)
  if(img_save)  { dev.off() }
  
  
  ### HDI (meta-regression) ------------------------------------------------------
  metareg_all2000 <- meta::metareg(ma_2000, hdi)
  if(pt) {  metareg_all2000_pt <- permutest(metareg_all2000)}
  
  if(tab_save)  {  sink(file = paste0("metareg_cfr2000_hdi_", k, ".txt"))  }
  print(summary(metareg_all2000))
  print("Permutation test:")
  if(pt) {  print(metareg_all2000_pt)  }
  if(tab_save)  {  sink(file = NULL)   }
  if(img_save)  {
    tiff(paste0("Rplot_metareg_cfr2000_hdi_", k, ".tif"), unit = "mm",
         width = 2*107, height = 2*80, res = 300)  }
  metafor::regplot(metareg_all2000, pi = TRUE, mod = "hdi", transf = transf.ilogit, 
                   legend = FALSE, bg = scales::alpha("#00A087B2", .3), 
                   xlim = c(0, 1), 
                   #ylim = c(0, 1),
                   ylab = "Case fatality ratio", xlab = "Human Development Index")
  title(paste0(k, ", mean study period 2001 or later"))
  if(img_save)  { dev.off() }
}


### Gini index (meta-regression) ----------------------------------------------- 
data2000_gini <- data1960 %>%
  filter(Periodmean > 2000)

ma_2000_gini <- metaprop(
  Deaths, Patients, 
  data = data2000_gini, 
  sm = "PLO", method.tau = "REML", method.ci = "NAsm", subgroup = region_wb, 
  #control = list(maxiter = 1000),
  prediction = TRUE, studlab = Studyauthor)

if(tab_save)  {  sink(file = "ma_cfr1960.txt")  }
print(summary(ma_2000_gini))
if(tab_save)  {  sink(file = NULL)  }


#### Forest plot
if(img_save)  {
  tiff(paste0("Rplot_Forest_cfr2000_gini_region.tif"), unit = "mm",
       width = 2.5*107, height = 3.5*80, res = 300)  }
metafor::forest(ma_2000_gini, 
                sortvar = Periodmean,
                study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                print.subgroup.name = FALSE, pooled.events = TRUE,
                leftlabs = c("Subgroup", "Deaths", "Total"),
                rightlabs = c("CFR", "[95% CI]"),
                prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                print.tau2 = TRUE, common = FALSE, smlab = "",
                col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                prediction.subgroup = k.w >= 5,
                fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
if(img_save)  { dev.off() }

if(img_save)  {
  pdf(paste0("Rplot_Forest_cfr2000_gini_region.pdf"), 
      width = 10, height = 10)  
  metafor::forest(ma_2000_gini, 
                  sortvar = Periodmean,
                  study.results = FALSE, sort.subgroup = TRUE, subgroup = k.w >= 5,
                  print.subgroup.name = FALSE, pooled.events = TRUE,
                  leftlabs = c("Subgroup", "Deaths", "Total"),
                  rightlabs = c("CFR", "[95% CI]"),
                  prediction = TRUE, print.I2 = TRUE, print.Q = TRUE,  digits.pval.Q = 3,
                  print.tau2 = TRUE, common = FALSE, smlab = "",
                  col.diamond = "grey", col.diamond.lines = "navy", digits = 2,
                  digits.tau2 = 2, digits.TE = 2, digits.se = 2, digits.Q = 1,
                  prediction.subgroup = k.w >= 5,
                  fs.hetstat = 8, col.subgroup = "black", details = TRUE, xlim = c(0,1))
  dev.off() 
}


#### Funnel plot
if(img_save)  {
  tiff("Rplot_Funnel_cfr2000_gini.tif", unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
meta::funnel(ma_2000_gini, studlab = FALSE, yaxis = "size", common = FALSE)
title("Mean study period 2001 or later")
if(img_save)  { dev.off() }


#### Meta-regression
metareg_gini_2000 <- meta::metareg(ma_2000_gini, gini)
if(pt) {  metareg_gini_2000_pt <- permutest(metareg_gini_2000)  }
if(tab_save)  {  sink(file = paste0("metareg_cfr2000_gini_gini.txt"))  }
print(summary(metareg_gini_2000))
print("Permutation test:")
if(pt) {  print(metareg_gini_2000_pt)  }
if(tab_save)  {  sink(file = NULL)   }

if(img_save)  {
  tiff(paste0("Rplot_metareg_cfr2000_gini.tif"), unit = "mm",
       width = 2*107, height = 2*80, res = 300)  }
metafor::regplot(metareg_gini_2000, pi = TRUE, mod = "gini", transf = transf.ilogit, 
                 legend = FALSE, bg = scales::alpha("#00A087B2", .3), #label = TRUE,
                 #xlim = c(0, 100), 
                 #ylim = c(0, 1),
                 ylab = "Case fatality ratio", xlab = "Gini index")
title("Mean study period 2001 or later")
if(img_save)  { dev.off() }

