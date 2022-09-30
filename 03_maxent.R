library(terra)
library(dismo)
library(tidyverse)
library(multcompView)
# next is to run maxent.
#give java extra memory
options(java.parameters = "-Xmx4g" )

#load rasters and points
predictors_final <- rast('./outputs/predictors_final.tiff')
bee_occ <- read.csv('./outputs/bee_occ.csv')

#list of species
bee_occ$Species <- as.factor(bee_occ$Species)
sp_ls <- levels(bee_occ$Species)

#turn the spatraster to rast for maxent
predictors_final <- stack(predictors_final)

#create a loop to run through all species

#create empty list to save the MaxEnt objects if running in a loop
MaxEnt_list <- list()

#looped this, but each one should have its optimized RM and features post-ENMeval. Also set random number of points for training as 30% of total point number

#this is example code for the loop

for (i in sp_ls) {
  points <- bee_occ %>%
    dplyr::filter(Species == i) %>%
    dplyr::select(X, Y)

  output_model <-
    maxent(x=predictors_final,
           p=points,
           removeDuplicates=TRUE, args=c(
             'maximumbackground=10000',
             'defaultprevalence=1.00',
             'betamultiplier=1',
             'plots=true',
             'pictures=true',
             'linear=true',
             'quadratic=true',
             'product=true',
             'threshold=true',
             'hinge=true',
             'threads=4',
             'responsecurves=true',
             'jackknife=true',
             'askoverwrite=false',
             'replicates=4',
             'replicatetype=crossvalidate'),
           path = paste("./outputs/maxent/",i, sep =''))

  MaxEnt_list[[i]] <- output_model
}

#extract variable importance
pi_table <- data.frame()

#need number of reps and species for loops
rep_num <- c(1:4)
sp_num <- c(1:length(sp_ls))

for(i in sp_num){
  for(j in rep_num){
  pi1 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_10.permutation.importance',]
  pi2 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_13.permutation.importance',]
  pi3 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_15.permutation.importance',]
  pi4 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_16.permutation.importance',]
  pi5 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_7.permutation.importance',]
  pi6 <- MaxEnt_list[[i]]@models[[j]]@results['wc2.1_2.5m_bio_8.permutation.importance',]
  
  
  pi_table_part <- data.frame(value = c(pi1, pi2, pi3, pi4, pi5, pi6), var = c('wc2.1_2.5m_bio_10.permutation.importance','wc2.1_2.5m_bio_13.permutation.importance','wc2.1_2.5m_bio_15.permutation.importance','wc2.1_2.5m_bio_16.permutation.importance','wc2.1_2.5m_bio_7.permutation.importance','wc2.1_2.5m_bio_8.permutation.importance'))
  
  pi_table_part$Species <- paste(sp_ls[i])
  
  pi_table_part$Replicate <- paste(j)
  
  pi_table <- rbind(pi_table, pi_table_part)
  }}

auc_table <- data.frame()

for(i in sp_num){
  for(j in rep_num){
    auc_train <- MaxEnt_list[[i]]@models[[j]]@results['Training.AUC',]
    auc_test <- MaxEnt_list[[i]]@models[[j]]@results['Test.AUC',]
    
    auc_table_part <- data.frame(value = c(auc_train, auc_test), var = c('AUC Train', 'AUC Test'))
    
    auc_table_part$Species <- paste(sp_ls[i])
    
    auc_table_part$Replicate <- paste(j)
    
    auc_table <- rbind(auc_table, auc_table_part)
  }}


#sort to highlight / look for consistent
pi_table_sorted <- pi_table %>%
  arrange(Species, desc(value))

#make things prettier for pub table
pi_table_sorted_pub <- data.frame(Species = pi_table_sorted$Species, var = pi_table_sorted$var, `Permutation Importance` = pi_table_sorted$value)
pi_table_sorted_pub <- pi_table_sorted_pub %>% separate(var, c('junk1', 'junk2', 'junk3', 'BIOCLIM Number'), sep = "_")
pi_table_sorted_pub <- pi_table_sorted_pub %>% separate(`BIOCLIM Number`, c('BIOCLIM Number', 'junk4', 'junk5'), sep = "\\.")
pi_table_sorted_pub$junk1 <- NULL
pi_table_sorted_pub$junk2 <- NULL
pi_table_sorted_pub$junk3 <- NULL
pi_table_sorted_pub$junk4 <- NULL
pi_table_sorted_pub$junk5 <- NULL

#best thing to do is visualize though - looking at the histogram of the AUC values
ggplot(auc_table, aes(x=value)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 12)+
  #geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~var) +
  scale_x_continuous(limits=c(.4, 1)) +
  labs(x = "Value", y = "Count") +
  theme_bw(base_size = 14)

#next is the permutation importance for the different variables with some stats
pi_table_sorted_pub$`BIOCLIMNumber` <- as.factor(pi_table_sorted_pub$`BIOCLIM Number`)

anova <- aov(data = pi_table_sorted_pub, Permutation.Importance ~ `BIOCLIMNumber`)
tukey <- TukeyHSD(anova)
cld <- multcompLetters4(anova, tukey)

# table with factors and 3rd quantile
Tk <- pi_table_sorted_pub %>% group_by(BIOCLIMNumber) %>%
  summarise(mean=mean(Permutation.Importance), quant = quantile(Permutation.Importance, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$BIOCLIMNumber)
Tk$cld <- cld$Letters

ggplot(pi_table_sorted_pub, aes(x = BIOCLIMNumber, y = Permutation.Importance)) +
  geom_boxplot() +
  geom_point()+
  labs(x = "BIOCLIM Variable", y = "Permutation Importance") +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = BIOCLIMNumber, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)

#last is visualize maxent models
output_predict <- predict(predictors_final, MaxEnt_list[[1]]@models[[1]], progress='text')
plot(output_predict)