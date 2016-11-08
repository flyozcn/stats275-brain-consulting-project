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


#Function to impute missing values:
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



library(mi)
z=depression[,grepl("T_Baseline",names(depression))]

mdf=missing_data.frame(z)
image(mdf)
show(mdf)
imputations <- mi(mdf)

y=complete(imputations)
y=as.matrix(y)
image(imputations)
dfs=complete(imputations)


mdf_all=missing_data.frame(gr11)
image(mdf_all)

# y=as.matrix(gr10)
# #Choose the dimension p and k for number of nearest neighbors
# p=10; k=3
# #Locally Linear Embedding:
# depression_LLE <- LLE(data=y, dim=p, k=k)
# plotDR(data=depression_LLE, labels=simLabels)
# #Isomap
# depression_IM <- Isomap(data=y, dims=p, k=k, plotResiduals=TRUE)




# #####T-Distributed Stochastic Neighbor Embedding
# install.packages("tsne")
# library("tsne")
# tsne_gr1=tsne(gr1)
# tsne_gr2=tsne(gr2)
# tsne_gr3=tsne(gr3)
# tsne_gr4=tsne(gr4)
# tsne_gr5=tsne(gr5)
# tsne_gr6=tsne(gr6)
# tsne_gr7=tsne(gr7)
# tsne_gr8=tsne(gr8)
# tsne_gr9=tsne(gr9)
# tsne_gr10=tsne(gr10)
# no_missing_data=cbind(tsne_gr1,tsne_gr2,tsne_gr3,tsne_gr4,tsne_gr5,tsne_gr6,tsne_gr7,tsne_gr8,
#                       tsne_gr9,tsne_gr10)




###############################################################


###
library(tsne)
library(rgl)
library(FactoMineR)
library(vegan)
#1. k-means on comorbidities
km_comorbidities <- kmeans(gr2,5,10000)
# run principle component analysis
pc_comorbidities<-prcomp(gr2)
# plot dots
plot(pc_comorbidities$x[,1], pc_comorbidities$x[,2],col=km_comorbidities$cluster,pch=16)
pc3d_comorbidities<-cbind(pc_comorbidities$x[,1], pc_comorbidities$x[,2], pc_comorbidities$x[,3])
plot3d(pc3d_comorbidities, col = km_comorbidities$cluster,type="s",size=1,scale=0.2)

#2. k-means on each brain group:
km_gr3 <- kmeans(gr3,5,10000)
gr3=cbind(gr3,km_gr3$cluster)
km_gr4 <- kmeans(gr4,5,10000)
gr4=cbind(gr4,km_gr4$cluster)
km_gr5 <- kmeans(gr5,5,10000)
gr5=cbind(gr5,km_gr5$cluster)
km_gr6 <- kmeans(gr6,5,10000)
gr6=cbind(gr6,km_gr6$cluster)
km_gr37 <- kmeans(gr7,5,10000)
gr7=cbind(gr7,km_gr7$cluster)
km_gr8 <- kmeans(gr8,5,10000)
gr8=cbind(gr8,km_gr8$cluster)
km_gr9 <- kmeans(gr9,5,10000)
gr9=cbind(gr9,km_gr9$cluster)
km_gr10 <- kmeans(gr10,5,10000)
gr10=cbind(gr10,km_gr10$cluster)
km_gr11 <- kmeans(gr11,5,10000)
gr11=cbind(gr11,km_gr11$cluster)

no_missing_data=cbind(gr1,gr2,gr3,gr4,gr5,gr6,gr7,gr8,gr9,gr10, gr11)
colnames801=names(no_missing_data)
colnames(no_missing_data)=c(1:801)#change column names for rfe to work

#####Recursive Feature Elimination
install.packages("mlbench");library(mlbench)
install.packages("caret");library(caret)
install.packages("e1071");library(e1071)
install.packages("randomForest");library(randomForest)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

#rfe of "brain region groups with kmeans" on "kmeans on comorbidities"
subsets=c(1:30)
colnames_3=names(gr3)
colnames(gr3)=c(1:dim(gr3)[2])
results_3 <- rfe(gr3, km_comorbidities$cluster, sizes=subsets, rfeControl=control)
ind_3=predictors(results_3)
colnames_3[as.numeric(unlist(t(strsplit(ind_3," "))))]

rfe_on_df=function(df,outcome,  subsets=c(1:30)){
  colnames_=names(df)
  colnames(df)=c(1:dim(df)[2])
  results_ <- rfe(df, outcome, sizes=subsets, rfeControl=control)
  ind_=predictors(results_)
  colnames_[as.numeric(unlist(t(strsplit(ind_," "))))]
}

subset3=rfe_on_df(gr3,km_comorbidities$cluster)
subset4=rfe_on_df(gr4,km_comorbidities$cluster)
subset5=rfe_on_df(gr5,km_comorbidities$cluster)
subset6=rfe_on_df(gr6,km_comorbidities$cluster)
subset7=rfe_on_df(gr7,km_comorbidities$cluster)
subset8=rfe_on_df(gr8,km_comorbidities$cluster)
subset9=rfe_on_df(gr9,km_comorbidities$cluster)
subset10=rfe_on_df(gr10,km_comorbidities$cluster)
subset11=rfe_on_df(gr11,km_comorbidities$cluster)

merged_subset=cbind(t(unlist(subset3)),t(unlist(subset4)),t(unlist(subset5)),
          t(unlist(subset6)),t(unlist(subset7)),t(unlist(subset8)),
          t(unlist(subset9)),t(unlist(subset10)),t(unlist(subset11)))

idx=match(unlist(merged_subset),colnames801)
idx <- sort(idx)
no_missing_subset=no_missing_data[,idx]
#now use this subset on logistic regression

# split training and test data
n=dim(depression)[1]
indices <- sample((1:n),n)
x_test <- no_missing_subset[indices[1:200],]
y_test=depression$outcome[indices[1:200]]
x_train <- no_missing_subset[indices[201:n],]
y_train=depression$outcome[indices[201:n]]

fit_all <- glm(as.factor(depression$outcome)~.,data=no_missing_subset,family='binomial')
summary(fit_all)

significant=c(98,134,153,155,174,187,188,206,208,211,214,216,218,225,226,231,233,234,
239,245,248,254,257,261,263,264,267,268,270,274,275,283,288,295,298,301,302,
306,312,313,316,319,320,323,324,327,328,336,342,351,354,357,359,361,382,
385,394,397,404,406,414,415,416,423,425,426,428,429,431,433,436,437,438,
439,440,446,452,456,462,463,464,468,469,612,624,629,631,636,644,659,661,662,
800,801)


colnames801[t(significant)]

# [1] "T_Concentration_Cerebellum_7b_R"                    
# [2] "T_Concentration_Fusiform_R"                         
# [3] "T_Concentration_Paracentral_Lobule_L"               
# [4] "T_Concentration_ParaHippocampal_L"                  
# [5] "T_Concentration_Supp_Motor_Area_R"                  
# [6] "T_Concentration_Temporal_Mid_Post_L"                
# [7] "T_Concentration_Temporal_Mid_Post_R"                
# [8] "T_Concentration_Vermis_8"                           
# [9] "max_cluster_size_T_Concentration_cerebellum_6_L"    
# [10] "max_cluster_size_T_Concentration_Thalamus_L"        
# [11] "max_cluster_T_Concentration_Putamen_R"              
# [12] "km_gr3$cluster"                                     
# [13] "Activation_Amygdala_R"                              
# [14] "Activation_Cerebellum_10_L"                         
# [15] "Activation_Cerebellum_10_R"                         
# [16] "Activation_Cerebellum_6_L"                          
# [17] "Activation_Cerebellum_7b_L"                         
# [18] "Activation_Cerebellum_7b_R"                         
# [19] "Activation_Cerebellum_Crus1_L"                      
# [20] "Activation_Cingulum_Mid_L"                          
# [21] "Activation_Cingulum_Post_R"                         
# [22] "Activation_Frontal_Inf_Orb_R"                       
# [23] "Activation_Frontal_Mid_L"                           
# [24] "Activation_Frontal_Mid_Orb_R_10"                    
# [25] "Activation_Frontal_Sup_L"                           
# [26] "Activation_Frontal_Sup_Medial_L"                    
# [27] "Activation_Frontal_Sup_Orb_R"                       
# [28] "Activation_Frontal_Sup_R"                           
# [29] "Activation_Fusiform_R"                              
# [30] "Activation_Hippocampus_R"                           
# [31] "Activation_Insula_L"                                
# [32] "Activation_Occipital_Sup_L"                         
# [33] "Activation_Pallidum_R"                              
# [34] "Activation_Parietal_Sup_L"                          
# [35] "Activation_Postcentral_R"                           
# [36] "Activation_Precuneus_L"                             
# [37] "Activation_Precuneus_R"                             
# [38] "Activation_Rectus_R"                                
# [39] "Activation_SupraMarginal_R"                         
# [40] "Activation_Temporal_Inf_Ant_L"                      
# [41] "Activation_Temporal_Inf_Mid_R"                      
# [42] "Activation_Temporal_Mid_Ant_L"                      
# [43] "Activation_Temporal_Mid_Ant_R"                      
# [44] "Activation_Temporal_Mid_Post_L"                     
# [45] "Activation_Temporal_Mid_Post_R"                     
# [46] "Activation_Temporal_Pole_Sup_L"                     
# [47] "Activation_Temporal_Pole_Sup_R"                     
# [48] "Activation_Thalamus_R"                              
# [49] "Activation_Vermis_8"                                
# [50] "T_Baseline_Calcarine_L"                             
# [51] "T_Baseline_Caudate_R"                               
# [52] "T_Baseline_Cerebellum_4_5_R"                        
# [53] "T_Baseline_Cerebellum_6_R"                          
# [54] "T_Baseline_Cerebellum_7b_R"                         
# [55] "T_Baseline_Frontal_Inf_Tri_L"                       
# [56] "T_Baseline_Frontal_Mid_Orb_L"                       
# [57] "T_Baseline_Frontal_Sup_Orb_R"                       
# [58] "T_Baseline_Fusiform_R"                              
# [59] "T_Baseline_Lingual_L"                               
# [60] "T_Baseline_Occipital_Inf_L"                         
# [61] "T_Baseline_Pallidum_L"                              
# [62] "T_Baseline_Pallidum_R"                              
# [63] "T_Baseline_Paracentral_Lobule_L"                    
# [64] "T_Baseline_Parietal_Sup_R"                          
# [65] "T_Baseline_Postcentral_R"                           
# [66] "T_Baseline_Precentral_L"                            
# [67] "T_Baseline_Precuneus_L"                             
# [68] "T_Baseline_Precuneus_R"                             
# [69] "T_Baseline_Putamen_R"                               
# [70] "T_Baseline_Rectus_R"                                
# [71] "T_Baseline_Supp_Motor_Area_L"                       
# [72] "T_Baseline_Supp_Motor_Area_R"                       
# [73] "T_Baseline_SupraMarginal_L"                         
# [74] "T_Baseline_SupraMarginal_R"                         
# [75] "T_Baseline_Temporal_Inf_Ant_L"                      
# [76] "T_Baseline_Temporal_Mid_Ant_L"                      
# [77] "T_Baseline_Temporal_Pole_Mid_L"                     
# [78] "T_Baseline_Temporal_Sup_Ant_L"                      
# [79] "T_Baseline_Thalamus_L"                              
# [80] "T_Baseline_Thalamus_R"                              
# [81] "T_Baseline_Vermis_3"                                
# [82] "T_Baseline_Vermis_8"                                
# [83] "T_Baseline_Vermis_9"                                
# [84] "max_cluster_size_T_Baseline_cerebellum_6_L"         
# [85] "max_cluster_size_T_Concentration_cerebellum_Crus1_L"
# [86] "max_cluster_size_T_Concentration_Putamen_L"         
# [87] "max_cluster_size_T_Concentration_Thalamus_L"        
# [88] "min_cluster_size_T_Baseline_Postcentral_R"          
# [89] "min_cluster_size_T_Concentration_cerebellum_Crus2_R"
# [90] "max_cluster_T_Baseline_Calcarine_L"                 
# [91] "max_cluster_T_Baseline_cerebellum_6_R"              
# [92] "max_cluster_T_Baseline_cerebellum_Crus1_L"          
# [93] "T_Baseline_Vermis_9"                                
# [94] "km_gr11$cluster"



fit <- glm(as.factor(y_train)~.,data=x_train,family='binomial')
summary(fit)

######################################################################

# run the RFE algorithm
subsets=c(1:30)*10
results <- rfe(no_missing_data, depression$Treatment_Group2, sizes=subsets, rfeControl=control)
# summarize the results
print(results)
# 
# 
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables Accuracy   Kappa AccuracySD KappaSD Selected
# 10   0.5409 0.04598   0.016720 0.04291         
# 20   0.5482 0.04392   0.008991 0.02166         
# 30   0.5514 0.04981   0.018472 0.04219         
# 40   0.5494 0.04645   0.024219 0.04595         
# 50   0.5410 0.02628   0.020600 0.03311         
# 60   0.5431 0.02692   0.015868 0.02728         
# 70   0.5410 0.02146   0.017355 0.02802         
# 80   0.5420 0.02202   0.010046 0.01557         
# 90   0.5514 0.04067   0.015367 0.02964         
# 100   0.5483 0.03462   0.015670 0.02886         
# 110   0.5398 0.02065   0.014276 0.02852         
# 120   0.5462 0.03278   0.015066 0.03111         
# 130   0.5483 0.03346   0.017527 0.03050         
# 140   0.5462 0.03206   0.016474 0.03149         
# 150   0.5462 0.03140   0.017066 0.02551         
# 160   0.5472 0.03113   0.015174 0.02863         
# 170   0.5472 0.02899   0.016087 0.02666         
# 180   0.5493 0.03756   0.015072 0.02856         
# 190   0.5483 0.03198   0.018677 0.03807         
# 200   0.5514 0.03926   0.018308 0.03293         
# 210   0.5483 0.03182   0.015183 0.02742         
# 220   0.5462 0.03375   0.019857 0.03664         
# 230   0.5494 0.03682   0.019170 0.03676         
# 240   0.5462 0.03071   0.017512 0.03331         
# 250   0.5462 0.03363   0.016347 0.02789         
# 260   0.5483 0.03385   0.019147 0.03450         
# 270   0.5483 0.03372   0.016728 0.03215         
# 280   0.5483 0.03502   0.015870 0.03023         
# 290   0.5504 0.03694   0.015598 0.02729         
# 300   0.5472 0.03034   0.014819 0.02284         
# 310   0.5493 0.03751   0.017398 0.03019         
# 320   0.5504 0.03643   0.017213 0.02889         
# 330   0.5472 0.02962   0.017153 0.03086         
# 340   0.5461 0.02999   0.015738 0.03252         
# 350   0.5504 0.03489   0.015250 0.02439         
# 360   0.5525 0.04154   0.017286 0.02992         
# 370   0.5514 0.04316   0.019932 0.03777         
# 380   0.5483 0.03243   0.020316 0.03530         
# 390   0.5494 0.03797   0.019776 0.03438         
# 400   0.5472 0.03226   0.016366 0.02455         
# 410   0.5472 0.03436   0.016805 0.02793         
# 420   0.5493 0.03630   0.016625 0.03110         
# 430   0.5515 0.03874   0.018809 0.03387         
# 440   0.5451 0.02893   0.017536 0.03269         
# 450   0.5472 0.02974   0.016366 0.02342         
# 460   0.5483 0.03186   0.016093 0.02740         
# 470   0.5472 0.03295   0.016366 0.02515         
# 480   0.5493 0.03678   0.019030 0.03313         
# 490   0.5504 0.03900   0.016482 0.03179         
# 500   0.5514 0.03988   0.016080 0.03522         
# 510   0.5567 0.05415   0.018608 0.03664        *
#   520   0.5514 0.04261   0.018437 0.03557         
# 530   0.5493 0.03634   0.021810 0.03743         
# 540   0.5462 0.02816   0.015081 0.02745         
# 550   0.5494 0.03856   0.022092 0.03745         
# 560   0.5483 0.03601   0.018639 0.03366         
# 570   0.5524 0.04470   0.016448 0.03226         
# 580   0.5504 0.03696   0.018197 0.03327         
# 590   0.5472 0.03170   0.017835 0.03102         
# 600   0.5483 0.03107   0.013870 0.02557         
# 610   0.5514 0.04141   0.017623 0.03506         
# 620   0.5514 0.04060   0.016604 0.03003         
# 630   0.5504 0.03573   0.015699 0.02523         
# 640   0.5535 0.04279   0.018327 0.03430         
# 650   0.5472 0.03228   0.016366 0.02755         
# 660   0.5525 0.04288   0.019265 0.04020         
# 670   0.5504 0.03544   0.012118 0.02269         
# 
# The top 5 variables (out of 510):
#   12, 13, 14, 15, 344
colnames670[c(12, 13, 14, 15, 344)]

# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


#### T-Distributed Stochastic Neighbor Embedding using a Barnes-Hut Implementation
install.packages("Rtsne")
library(Rtsne)
set.seed(42)
tsne_out <- Rtsne(as.matrix(no_missing_data), theta=0.0, pca=FALSE,max_iter=1000) 
plot(tsne_out$Y, col=depression$Treatment_Group2) 

###
library(tsne)
library(rgl)
library(FactoMineR)
library(vegan)


km <- kmeans(no_missing_data,5,10000)
# run principle component analysis
pc<-prcomp(no_missing_data)
# plot dots
plot(pc[,1], pc[,2],col=km$cluster,pch=16)
# plot spiderweb and connect outliners with dotted line
pc<-cbind(pc[,1], pc[,2])
ordispider(pc, factor(km$cluster), label = TRUE)
ordihull(pc, factor(km$cluster), lty = "dotted")

tsne_data <- tsne(no_missing_data, k=3, max_iter=500, epoch=500)
plot(tsne_data[,1], tsne_data[,2], col=km$cluster, pch=16)
ordispider(tsne_data, factor(km$cluster), label = TRUE)
ordihull(tsne_data, factor(km$cluster), lty = "dotted")



