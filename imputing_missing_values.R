#Drops columns that have 75% or more missing values, and imputes the remaining missing values


#source("https://bioconductor.org/biocLite.R")
#biocLite("RDRToolbox")
library("RDRToolbox")
install.packages("dr")
library("dr")
depression<- read.csv("depression_new.csv")

#categorical variables:
#Gender
depression$Female=0;depression$Male=0
depression$Female=as.numeric(depression$Gendername=="Female")
depression$Male=as.numeric(depression$Gendername=="Male")

#Age
depression$Adult=0;depression$Geriatric=0;depression$Pediatric=0
depression$Adult=as.numeric(depression$Age_Group=="Adult")
depression$Geriatric=as.numeric(depression$Age_Group=="Geriatric")
depression$Pediatric=as.numeric(depression$Age_Group=="Pediatric")


#Function to impute missing values and drop columns that have 75% or more missing values:
library("mi")
impute_missing=function(df,rate=0.25){
  df=df[, unlist(lapply(df, function(x) (sum(is.na(x))/length(x)<rate )))]
  mdf=missing_data.frame(df)
  imputations <- mi(mdf)
  dfs=complete(imputations)
  for (i in 1:dim(df)[2]){ 
    df[,i]=rowMeans(cbind(dfs$`chain:1`[,i],dfs$`chain:2`[,i],dfs$`chain:3`[,i],dfs$`chain:4`[,i]))}
  return(df)
}



#group1:
vars1=c("Female","Adult","Geriatric", "Pediatric", "race_id", "location_id", "veteran",
        "doctor_id","previous_clinicians", "Pre_QOLI", "Pre_QOLI_Number_Class", "Pre_GSI", "PRE_PST",
        "Pre_PSDI","Pre_BDI", "PRE_DEPRESSION_lEVEL")
gr1<- depression[vars1]
gr1=impute_missing(gr1)
#group2:
vars2=c("Adjustment_Disorder", "AnxietyDisorder", "dementia", "Amnestic_disorders", 
        "Dementia_Alzheimers_Type", "Dementia_due_to", "Other_cognitive_disorders", "Vascular_dementia", 
        "ChildHoodDisorder", "Attention_Deficit_Disruptive_Behavior", "ADHD", "Communication_Disorder", 
        "Elimination_Disorder", "Learning_Disorder", "Mental_Retardation", "Motor_Skill_Disorder", 
        "Other_disorders_infancy_childhood_adolesc", "Pervasive_developmental_disorders", "Tic_Disorder", 
        "Dissociative_Disorder", "Eating_Disorder", "Factitious_disorders", "Impulse_Control_Disorders", 
        "Mental_Disorder_due_to", "MoodDisorder", "Bipolar", "Depressed", "Personality_disorders", "Personallity_Cluster_A", 
        "Personallity_Cluster_B", "Personallity_Cluster_C", "Personality_Disorder_NOS", "SchizoPsycho" ,
        "Sexual_gender_identity_disorders", "Paraphilias", "Sexual_disorder_NOS", "Sleep_Disorders", "Other_Sleep_Disorders",
        "Parasomnia", "Primary_Sleep_Disorder", "Somatoform_Disorder", "SubstanceAbuseDisorder", "Alcohol_Related_Disorder", 
        "Amphetamine_Related_Disorder", "Caffeine_related_disorders", "Cannabis_Related_Disorder", 
        "Cocaine_Related_Disorder", "Hallucinogen_related_disorders", "Nicotine_related_disorders", "Opioid_Related_Disorder", 
        "Other_Substance_Related_Disorder", "Polysubstance_related_disorder", "Sedative_hypnotic_anxiolytic_related_disorder", 
        "TemporalDysfunction", "Frontal_Lobe_Dysfunction", "Toxicicity_Encephalopathy", "Diagnosed_Brain_Trauma", 
        "GAD", "OCD", "Autism", "PTSD", "Epilepsy", "Baseline_location",	"Concentration_location")
gr2<- depression[vars2]
gr2=impute_missing(gr2)

#group3: columns that start with "T_Concentration"
gr3<-depression[,grepl("T_Concentration",names(depression))]
gr3=impute_missing(gr3,rate=0.1)

# group4: columns that start with "Activation_"
gr4<-depression[,grepl("Activation_",names(depression))]
gr4=impute_missing(gr4,rate=0.15)

# group5: columns that start with "Baseline_"
gr5<-depression[,grepl("Baseline_",names(depression))]
dim(gr5)
gr5=gr5[, unlist(lapply(gr5, function(x) (sum(is.na(x))/length(x)<0.1 )))]
dim(gr5)

gr5=impute_missing(gr5,rate=0.1)

# group6: columns that start with "Concentration_"
gr6<-depression[,grepl("Concentration_",names(depression))]

gr6=impute_missing(gr6,rate=0.1)

# group7: columns that start with "max_cluster_size"
gr7<-depression[,grepl("max_cluster_size",names(depression))]
gr7=impute_missing(gr7,rate=0.25)

# group8: columns that start with "min_cluster_size"
gr8<-depression[,grepl("min_cluster_size",names(depression))]
gr8=impute_missing(gr8,rate=0.25)

# group9: columns that start with "max_cluster_T_Baseline"
gr9<-depression[,grepl("max_cluster_T_Baseline",names(depression))]
gr9=impute_missing(gr9,rate=0.25)
# group10: columns that start with "min_cluster_T_Baseline"
gr10<-depression[,grepl("min_cluster_T_Baseline",names(depression))]
gr10=impute_missing(gr10,rate=0.25)

#group11: columns that start with "T_Baseline"
gr11<-depression[,grepl("T_Baseline",names(depression))]
gr11=impute_missing(gr11,rate=0.1)


depression$outcome <- (depression$Pre_BDI-depression$POST_BDI)>0.5*depression$Pre_BDI

no_na_depression=cbind(depression$outcome, gr1, gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10, gr11)

write.csv(no_na_depression, file = "no_na_depression.csv")
