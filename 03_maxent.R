library(terra)
library(dismo)
library(tidyverse)
# next is to run maxent.
#give java extra memory
options(java.parameters = "-Xmx4g" )

#load rasters and points
predictors_final <- rast('./outputs/predictors_final.tiff')
bee_occ <- read.csv('./outputs/bee_occ.csv')

#list of species
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
  
  points = points[,2:3]

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
             'threads=6',
             'responsecurves=true',
             'jackknife=true',
             'askoverwrite=false',
             'replicates=10',
             'replicatetype=crossvalidate'),
           path = paste("./outputs/maxent/",i, sep =''))

  MaxEnt_list[[i]] <- output_model
}

i = sp_ls[1]

points <- bee_occ %>%
  dplyr::filter(Species == i) %>%
  dplyr::select(X, Y)

points = points[,2:3]

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
           'threads=6',
           'responsecurves=true',
           'jackknife=true',
           'askoverwrite=false',
           'replicates=10',
           'replicatetype=crossvalidate'),
         path = paste("./outputs/maxent/",i, sep =''))

MaxEnt_list[[i]] <- output_model