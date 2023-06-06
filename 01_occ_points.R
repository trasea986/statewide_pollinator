#for bringing in points, starting with just CK's ND data, but could expand to include gbif at a later date.
#test
library(rnaturalearthdata)
library(rnaturalearth)
library(dismo)
library(maps)
library(mapproj)
library(ggthemes)
library(sf)
library(tidyverse)
library(conflicted)
library(terra)
library(tidyterra)
library(ggrepel)
#note that older version of feddata has missing tile issue. dev says to pull dev version
#devtools::install_github("ropensci/FedData")
library(FedData)
library(ggspatial)

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

bee_data %>%
  group_by(Species) %>%
  tally() %>%
  nrow()

#take top 10
bees_common <- bee_sum[1:36,]
bees_common <- bees_common$Species

#sort the location data for just those bees
bee_occ <- bee_sum_loc %>%
  dplyr::filter(Species %in% bees_common)

bee_occ$Species <- as.factor(bee_occ$Species)
bee_occ$X <- as.numeric(bee_occ$X)
bee_occ$Y <- as.numeric(bee_occ$Y)

#write.csv(bee_occ, './outputs/bee_occ.csv', row.names = FALSE)

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
nlcd_legend2 <- rbind(data.frame(colors = '0000ffff', labels = 'Unclassified'), nlcd_legend)

#remove pren snow and ice
nlcd_legend2 <- nlcd_legend2[c(-3),]

#fix projection issue
nlcd_plot2 <- terra::project(nlcd_plot, counties)

#although labels are correct, the colors are not quite right from that csv I found online, maybe an older version of nlcd
#pull out from second position perrenial snow/ice
#also change herbaceous to a slightly more green color original was '#edeccd'
nlcd_legend3 <- data.frame(nlcd_legend2$labels, colors = c('0000ffff', '#486da2', '#e1cdce', '#dc9881', '#f10100', '#ad0101', '#b3afa4', '#6ba966','#1d6533', '#bdcc93', '#d1bb82','#a4cc51', '#ddd83e','#ae7229','#bbd7ed', '#71a4c1'))

#quick rename
nlcd_legend3$value <- nlcd_legend3$labels

#bring in REC
rec <- read.csv('./data/rec_sites.csv', fileEncoding = 'UTF-8-BOM')

#crop the nlcd to better match the counties file
nlcd_plot3 <- mask(nlcd_plot2, counties)

#this version doesn't have 'unclassified'
nlcd_legend3 <- nlcd_legend3[c(-1),]

#to plot map of the sample sizes by species
#other bombus: | Species == 'Bombus fervidus'  | Species == 'Bombus rufocinctus'
plot_df <- bee_occ %>% 
  dplyr::filter(Species == 'Bombus ternarius' | Species == 'Bombus griseocollis') #%>%
  #  | Species == 'Bombus borealis') %>%
  #dplyr::filter(n > 10)

#drop unused levels
plot_df <- droplevels(plot_df)

#plot by species
ggplot() +
  geom_spatraster(data = nlcd_plot3) + scale_fill_manual(values = nlcd_legend3$colors, na.value = NA)+ 
  tidyterra::geom_spatvector(data = counties, fill = NA, colour = "black", size = 0.75)+
  geom_point(data = plot_df, aes(x = X, y = Y, color = Species), size = 2.5) +
  geom_label_repel(data = plot_df, aes(x = X, y = Y, label = n, color = Species), min.segment.length = 0.05, size = 3, force = 10, max.overlaps = 100) +
  #these two add the RECs, but because focus likely NE ND, not needed
  #geom_point(data = rec, aes(x = Longitude, y = Latitude), fill = 'black', size = 7, pch = 23) +
  #geom_label_repel(data = rec, aes(x = Longitude, y = Latitude, label = Location), size = 4, color = 'black', min.segment.length = 0, size = 3, force = 50) +
  annotation_scale(location = c("bl"))+
  scale_colour_manual(values=c('darkgreen', 'blue', 'purple', "orange", "darkred")) +
  facet_wrap(~Species, nrow = 2) +
  #xlim(c(-100.4815, -96.52885))+
  #ylim(c(47.46399, 49.22939))+
  theme_void(base_size = 16)

#ggsave('./outputs/sample_size_all_sites.jpeg', p1, units = 'in', dpi = 1200, width = 11, height = 8.5)

#pull out list by filtering within those limits
proposed_samples <- bee_occ %>% 
  dplyr::filter(Species == 'Bombus ternarius' | Species == 'Bombus griseocollis')%>% # if wanting sp 3  | Species == 'Bombus borealis') %>%
  dplyr::filter(n >= 7) %>%
  dplyr::filter(X > -100.4815 && X < -96.52885) %>%
  dplyr::filter(Y > 47.46399 && Y < 49.488497)

sum(proposed_samples$n) #514 total samples

#add column that takes the max number of samples we may be interested in
proposed_samples$n_seq <- with(proposed_samples, ifelse(n > 20, 20, n))

sum(proposed_samples$n_seq) #294! total samples

ggplot() +
  geom_spatraster(data = nlcd_plot3) + scale_fill_manual(values = nlcd_legend3$colors, na.value = NA)+ 
  tidyterra::geom_spatvector(data = counties, fill = NA, colour = "black", size = 0.75)+
  geom_point(data = proposed_samples, aes(x = X, y = Y, color = Species), size = 2.5) +
  geom_label_repel(data = proposed_samples, aes(x = X, y = Y, label = n_seq, color = Species), min.segment.length = 0.05, size = 3, force = 10, max.overlaps = 100) +
  annotation_scale(location = c("bl"))+
  scale_colour_manual(values=c('darkgreen', 'blue', 'purple', "orange", "darkred")) +
  facet_wrap(~Species, nrow = 2) +
  #xlim(c(-101, -96.5544916699999))+
  #ylim(c(47, 49.000594494))+
  theme_void(base_size = 16)

#ggsave('./outputs/sample_size.jpeg', p1, units = 'in', dpi = 1200, width = 11, height = 8.5)

proposed_samples %>%
  group_by(Species) %>%
  summarize(average_pop = mean(n_seq))

proposed_samples %>%
  group_by(Species) %>%
  tally()

sum(proposed_samples$n_seq)

proposed_samples %>%
  group_by(Species) %>%
  summarize(total_samp = sum(n_seq))


#check variation in environment
#load raster and clip
#create list of env data
bio_files <- list.files(path = './data/wc2.1_2.5m_bio', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#load in the rasters
bio_layers <- rast(bio_files)

#load ND shapefile from TIGER for extent
nd <- vect('./data/tl_2016_38_place.shp')
bio_layers <- crop(bio_layers, nd)

#extract values across the state
extracted_df <- terra::extract(bio_layers, proposed_samples[,2:3])

#min, max, mean
summary_df1 <- extracted_df[,-1] %>%
  summarize_all(mean)
summary_df1$calc <- "Mean"
summary_df2 <- extracted_df[,-1] %>%
  summarize_all(min)
summary_df2$calc <- "Min"
summary_df3 <- extracted_df[,-1] %>%
  summarize_all(max)
summary_df3$calc <- "Max"
summary_df_final <- rbind(summary_df1, summary_df2, summary_df3)

#note that I ran two versions of this, one with NE ('small') and one over the whole state to compare in Excel quickly
write.csv(summary_df_final, file = "./outputs/summary_env_small.csv", row.names = FALSE)
