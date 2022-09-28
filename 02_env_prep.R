#env data prep
library(dismo)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(tidyverse)
library(conflicted)
library(terra)
library(corrplot)
library(caret)
library(sp)

#load raster and clip
#create list of env data
bio_files <- list.files(path = './data/wc2.1_2.5m_bio', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#load in the rasters
bio_layers <- rast(bio_files)

#load ND shapefile from TIGER for extent
nd <- vect('./data/tl_2016_38_place.shp')
bio_layers <- crop(bio_layers, nd)

#check correlations across the state
extracted_df <- terra::extract(bio_layers, bee_occ[,2:3])

#remove ID column
extracted_df <- extracted_df[,-1]

#calculate the correlation among our variables at our points
mydata.cor <- cor(extracted_df, method = 'spearman', use = 'complete.obs')

#make plot
corrplot(mydata.cor, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE)

#set up the correlation value cutoff you want to use to make the list of highly correlated variables
hc <- findCorrelation(mydata.cor, cutoff = 0.75)

#sort the list of highlight correlated variables
hc = sort(hc)

#remove the correlated ones from the data frame. This will create what df for
predictors_final_list = extracted_df[,-c(hc)]

#cut out correlated from the raster stack, then save it
predictors_final <- subset(bio_layers, names(predictors_final_list))

#names of rasters in the stack
bio_names <- names(predictors_final)

#save
terra::writeRaster(predictors_final, './outputs/predictors_final.tiff', filetype = 'GTiff',names = bio_names, overwrite=TRUE)

