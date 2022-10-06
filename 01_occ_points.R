#for bringing in points, starting with just CK's ND data, but could expand to include gbif at a later date.

library(rnaturalearthdata)
library(rnaturalearth)
library(dismo)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(tidyverse)
library(conflicted)
library(readxl)
library(terra)
library(tidyterra)
library(ggrepel)
#note that older version of feddata has missing tile issue. dev says to pull dev version
devtools::install_github("ropensci/FedData")
library(FedData)

#read in net and traps and then combine after adding row for collection type
bee_netting <- read.csv("data/XYBeeData_27_Sept_2022_netting.csv")
bee_passive <- read.csv("data/XYBeeData_27_Sept_2022_passive.csv")

#need to remove males
bee_netting <- bee_netting %>% dplyr::filter(Male. != 'x')
bee_passive <- bee_passive %>% dplyr::filter(Male. != 'x')


#note only need a few columns
bee_netting <- as.data.frame(cbind('X' = bee_netting$X_Site, 'Y' = bee_netting$Y_Site, 'Species' = bee_netting$Bee.Species, 'Count' = bee_netting$Bee.Count))
bee_passive <- as.data.frame(cbind('X' = bee_passive$X_Site, 'Y'= bee_passive$Y_Site, 'Species' = bee_passive$Bee.Species, 'Count' = c('1')))

bee_netting$Method <- c('netting')
bee_passive$Method <- c('passive')

bee_data <- rbind(bee_netting, bee_passive)

#turn individual lines to count data
#loses the count and methods, but keeping in mind here that anything at the same site is going to be lost anyway
bee_sum_loc <- bee_data %>%
  dplyr::filter(Species != 0 & Species != ' ' & Species != '\n') %>%
  group_by(Species, X, Y) %>%
  tally()

#this can then be subset for later pieces, but also getting total n
bee_sum <- bee_data %>%
  dplyr::filter(Species != 0 & Species != ' ' & Species != '\n') %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n))

#take top 10
bees_common <- bee_sum[1:36,]
bees_common <- bees_common$Species

#sort the location data for just those bees
bee_occ <- bee_sum_loc %>%
  dplyr::filter(Species %in% bees_common)

bee_occ$Species <- as.factor(bee_occ$Species)
bee_occ$X <- as.numeric(bee_occ$X)
bee_occ$Y <- as.numeric(bee_occ$Y)

write.csv(bee_occ, './outputs/bee_occ.csv', row.names = FALSE)

#to plot map of the sample sizes by species
plot_df <- bee_occ %>% 
  dplyr::filter(Species == 'Agapostemon virescens' | Species == 'Lasioglossum albipenne') %>%
  dplyr::filter(n > 14)

#drop unused levels
plot_df <- droplevels(plot_df)

#bring in county map
counties <- vect('./data/County_Boundaries.shp')

#get landcover
#template raster
r <- rast(counties)
r <- raster::raster(r)

#load data
nlcd <- get_nlcd(
  r,
  label = 'ND',
  year = 2019,
  dataset = c("landcover"),
  extraction.dir = "./data/NLCD/FedData",
  #raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
  force.redo = F)

#can also load with the main function
nlcd_plot <- rast('./data/NLCD/FedData/ND_NLCD_Land_Cover_2019.tif')

#csv of the colors to use
nlcd_legend <- read.csv('./data/NLCD/nlcd_legend.csv')

#quick rename
nlcd_legend$value <- nlcd_legend$ï..labels
nlcd_legend$ï..labels <- NULL

#need to add unclassified
nlcd_legend2 <- rbind(data.frame(colors = '0000ffff', value = 'Unclassified'), nlcd_legend)

#remove pren snow and ice
nlcd_legend2 <- nlcd_legend2[c(-3),]

#fix projection issue
nlcd_plot2 <- terra::project(nlcd_plot, counties)

#although labels are correct, the colors are not quite right from that csv I found online, maybe an older version of nlcd
#pull out from second position perrenial snow/ice
#also change herbaceous to a slightly more green color original was '#edeccd'
nlcd_legend3 <- data.frame(nlcd_legend2$value, colors = c('0000ffff', '#486da2', '#e1cdce', '#dc9881', '#f10100', '#ad0101', '#b3afa4', '#6ba966','#1d6533', '#bdcc93', '#d1bb82','#a4cc51', '#ddd83e','#ae7229','#bbd7ed', '#71a4c1'))

#bring in REC
rec <- read.csv('./data/rec_sites.csv', fileEncoding = 'UTF-8-BOM')

#crop the nlcd to better match the counties file
nlcd_plot3 <- mask(nlcd_plot2, counties)

#this version doesn't have 'unclassified'
nlcd_legend3 <- nlcd_legend3[c(-1),]

#plot by species
ggplot() +
  geom_spatraster(data = nlcd_plot3) + scale_fill_manual(values = nlcd_legend3$colors, na.value = NA)+ 
  tidyterra::geom_spatvector(data = counties, fill = NA, colour = "black", size = 0.75)+
  geom_point(data = plot_df, aes(x = X, y = Y, color = Species), size = 2.5) +
  geom_label_repel(data = plot_df, aes(x = X, y = Y, label = n, color = Species), min.segment.length = 0.05, size = 3, force = 10, max.overlaps = 30) +
  geom_point(data = rec, aes(x = Longitude, y = Latitude), fill = 'black', size = 7, pch = 23) +
  geom_label_repel(data = rec, aes(x = Longitude, y = Latitude, label = Location), size = 4, color = 'black', min.segment.length = 0, size = 3, force = 50) +
  scale_colour_manual(values=c('darkgreen', 'blue')) +
  theme_void()
