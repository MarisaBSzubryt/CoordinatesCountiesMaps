
# Code written by Marisa Szubryt 2024/01/22, modified from links below
# Package upgraded by 'fipio' creator, Justin Singh-M.
# See bottom of document for list of sources for various code snippets

# Marisa's emails: marisabszubryt@gmail.com or marisabszubryt@ou.edu
# Marisa's website: https://marisabszubryt.wordpress.com/
# Marisa's github account: https://github.com/MarisaBSzubryt

################################################################################
################################################################################
################################################################################

# Set working directory and clear environment
setwd("/Users/mbs/Desktop/Heterotheca/Heterotheca-RCode/Coordinate_county_maps")
rm(list=ls()); graphics.off(); gc()

# Load in map-making packages for coordinate to county conversion
install.packages('dyplr')
install.packages('fipio')
install.packages('stringr')
install.packages('totalcensus')
install.packages('usdata')
library(dplyr)
library(fipio)
library(stringr)
library(totalcensus)
library(usdata)

# Read in CSV with coordinates
# The below CSV works
points <- read.csv('example_dataset.csv'); attach(points); points; names(points)

# Add in necessary columns
points$fips<-'';points$state_fips<-'';points$county_fips<-'';points$state_name<-'';points$county_name<-''; points$SC_name<-''

# Extract latitude and longitude
points_longitude <- points$longitude; points_longitude <- as.numeric(points_longitude); points_longitude
points_latitude <- points$latitude; points_latitude <- as.numeric(points_latitude); points_latitude

# Add FIPS codes (2 numbers for state, 3 numbers for county)
fips_codes <- c()
for (i in c(1:length(points_longitude))) {
  fips_iteration <- fipio::coords_to_fips(x = data.frame(points_longitude[i], points_latitude[i], coords = c("X", "Y")))
  fips_codes <- c(fips_codes, fips_iteration)}
fips_codes <- fips_codes[seq(1,length(fips_codes),2)]
points$fips <- fips_codes; points$fips <- as.character(points$fips); points$fips; points

# Split 5 number FIPS codes into state and county fip codes
points$state_fips<- substr(points$fips,1,2); fips_states <- points$state_fips; fips_states <- as.character(fips_states); fips_states
points$county_fips<- substr(points$fips,3,5); fips_counties <- points$county_fips; fips_counties <- as.character(fips_counties); fips_counties

# Generate state and county names from FIPS codes 
states_names <- convert_fips_to_names(fips_states); states_names
counties_names <- convert_fips_to_names(fips_counties, states=states_names, geo_header='COUNTY'); counties_names

# Add state/county names into dataframe
points$state_name <- states_names; points$county_name <- counties_names; points

# Convert state abbreviations to full name
points$state_name <- abbr2state(points$state_name); points

# Remove the 'County' portion of the counties_names vector
points <- points %>% mutate(county_name=word(county_name, 1, -2)); points

# Convert state and county to lower case
points$state_name <- tolower(points$state_name); points$county_name <- tolower(points$county_name); points

# Convert the state and county names into a format usable by the county-highlighting code
points$SC_name <- paste(points$state_name, ",", points$county_name); points
points$SC_name <- gsub(" , (?! )", ",", points$SC_name, perl=TRUE); points

# Extract out the list of "state,county" to insert into the county-highlighting code
plantCounties <- points$SC_name; plantCounties

# Export the amended dataframe for reference
write.csv(points, "example_dataset_annotated.csv", row.names=FALSE)

################################################################################
################################################################################
################################################################################

# Code written by Patrick Alexander (2024/01/02), modified by Marisa Szubryt
# Plot all counties in a single color

# Load in map-making packages
install.packages('ggplot2')
install.packages('maps')
install.packages('sf')
install.packages('tidyverse')
library(ggplot2)
library(sf)
library(maps)
library(tidyverse)

# Read in list of plant counties
points <- read.csv('example_dataset_annotated.csv'); attach(points); points; names(points)
plantCounties <- points$SC_name; plantCounties

# Run code to automatically set up map-making - all as one color
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
plantStates <- substring(plantCounties, 1, regexpr(",", plantCounties) - 1) %>% unique()
counties <- counties %>% 
  mutate(state=substring(ID, 1, regexpr(',', ID)-1)) %>%
  filter(state %in% plantStates)
plantStates <- states %>% filter(ID %in% plantStates)
plantCounties <- counties %>% filter(ID %in% plantCounties)
bbox <- st_bbox(plantStates)
zone <- floor(((mean(c(bbox$xmin,bbox$xmax))+180)/6) +1)
projBbox <- st_bbox(st_transform(plantStates, crs = paste('+proj=utm +zone=',zone,' +datum=WGS84', sep='')))

# Can change thickness/color of state/county boundaries and fill color of selected counties
ggplot(NULL) + geom_sf(data=counties, lwd=0.33, color='black', fill='white') +
  geom_sf(data=plantCounties, lwd=0.33, fill='steelblue') +
  geom_sf(data = states, lwd=0.75, color='black', fill='transparent') +
  coord_sf(crs=paste('+proj=utm +zone=',zone,' +datum=WGS84',sep=''),
           xlim=c(projBbox$xmin, projBbox$xmax), ylim=c(projBbox$ymin, projBbox$ymax), expand=TRUE) +
  theme_void()

################################################################################
################################################################################
################################################################################

# Code written by Patrick Alexander (2024/01/02), modified by Marisa Szubryt
# Plot counties in a multiple colors - note that counties must be mutually exclusive
# or else will only include the first species/color mapped

# Patrick's email: paalexan@polyploid.net
# Patrick's website: https://polyploid.net/?fbclid=IwAR3zZuiAaKfrAs_HjPCM2Rgy6lLfdb5nHGLmJ3Ct6V2m_icfAmWh-xBLAJM
# Patrick's github account: https://github.com/aspidoscelis/



# Set total map extent
pcTotal <- read.csv('example_dataset_annotated.csv')
plantCounties <- pcTotal$SC_name; plantCounties
plantCounties <- unique(plantCounties); plantCounties

# Set up species 1 ('spA')
pc1 <- read.csv('example_dataset_annotated.csv'); pc1 <- pc1[pc1$species == 'spA', ]; pc1
plantCounties1 <- pc1$SC_name; plantCounties1 <- unique(plantCounties1); plantCounties1
# Set up species 2 ('spB')
pc2 <- read.csv('example_dataset_annotated.csv'); pc2 <- pc2[pc2$species == 'spB', ]; pc2
plantCounties2 <- pc2$SC_name; plantCounties2 <- unique(plantCounties2); plantCounties2
# Set up species 3 ('spC')
pc3 <- read.csv('example_dataset_annotated.csv'); pc3 <- pc3[pc3$species == 'spC', ]; pc3
plantCounties3 <- pc3$SC_name; plantCounties3 <- unique(plantCounties3); plantCounties3

# Set up joint species 1 and 2 counties
plantCounties1_2 <- intersect(plantCounties1, plantCounties2); plantCounties1_2
# Set up joint species 1 and 3 counties
plantCounties1_3 <- intersect(plantCounties1, plantCounties3); plantCounties1_3
# Set up joint species 2 and 3 counties
plantCounties2_3 <- intersect(plantCounties2, plantCounties3); plantCounties2_3
# Set up joint species 1 and 2 and 3 counties
plantCounties1_2_3 <- intersect(plantCounties1, plantCounties2_3); plantCounties1_2_3

# Generate list of unique counties for plant species 1
plantCounties1_unique <- setdiff(plantCounties1, plantCounties1_2); plantCounties1_unqiue
plantCounties1_unique <- setdiff(plantCounties1_unqiue, plantCounties1_3); plantCounties1_unqiue
# Generate list of unique counties for plant species 1
plantCounties2_unique <- setdiff(plantCounties2, plantCounties1_2); plantCounties2_unqiue
plantCounties2_unique <- setdiff(plantCounties2_unqiue, plantCounties2_3); plantCounties2_unqiue
# Generate list of unique counties for plant species 1
plantCounties3_unique <- setdiff(plantCounties3, plantCounties1_3); plantCounties3_unqiue
plantCounties3_unique <- setdiff(plantCounties3_unqiue, plantCounties2_3); plantCounties3_unqiue

# Total extent set up
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
plantStates <- substring(plantCounties, 1, regexpr(",", plantCounties) - 1) %>% unique()
counties <- counties %>% mutate(state=substring(ID, 1, regexpr(',', ID)-1)) %>% filter(state %in% plantStates)
plantStates <- states %>% filter(ID %in% plantStates)
plantCounties_1u <- counties %>% filter(ID %in% plantCounties1_unique)
plantCounties_2u <- counties %>% filter(ID %in% plantCounties2_unique)
plantCounties_3u <- counties %>% filter(ID %in% plantCounties3_unique)
plantCounties_1_2 <- counties %>% filter(ID %in% plantCounties1_2)
plantCounties_1_3 <- counties %>% filter(ID %in% plantCounties1_3)
plantCounties_2_3 <- counties %>% filter(ID %in% plantCounties2_3)
plantCounties_1_2_3 <- counties %>% filter(ID %in% plantCounties1_2_3)
bbox <- st_bbox(plantStates)
zone <- floor(((mean(c(bbox$xmin,bbox$xmax))+180)/6) +1)
projBbox <- st_bbox(st_transform(plantStates, crs = paste('+proj=utm +zone=',zone,' +datum=WGS84', sep='')))

# Can change thickness/color of state/county boundaries and fill color of selected counties
ggplot(NULL) + geom_sf(data=counties, lwd=0.33, color='black', fill='white') +
  geom_sf(data=plantCounties_1u, lwd=0.33, fill='steelblue') +
  geom_sf(data=plantCounties_2u, lwd=0.33, fill='darkred') +
  geom_sf(data=plantCounties_3u, lwd=0.33, fill='gold') +
  geom_sf(data=plantCounties_1_2, lwd=0.33, fill='purple') +
  geom_sf(data=plantCounties_1_3, lwd=0.33, fill='darkgreen') +
  geom_sf(data=plantCounties_2_3, lwd=0.33, fill='orange') +
  geom_sf(data=plantCounties_1_2_3, lwd=0.33, fill='darkgray') +
  geom_sf(data = states, lwd=0.75, color='black', fill='transparent') +
  coord_sf(crs=paste('+proj=utm +zone=',zone,' +datum=WGS84',sep=''),
           xlim=c(projBbox$xmin, projBbox$xmax), ylim=c(projBbox$ymin, projBbox$ymax), expand=TRUE) +
  theme_void()

################################################################################
################################################################################
################################################################################

# https://community.rstudio.com/t/deleting-values-in-a-vector/125796 
# https://cran.r-project.org/web/packages/fipio/readme/README.html
# https://www.digitalocean.com/community/tutorials/unique-function-r-programming
# https://github.com/program--/fipio/issues/15
# https://rdrr.io/cran/fipio/f/README.md
# https://www.reddit.com/r/Rlanguage/comments/se5w7b/how_to_replace_comma_that_is_not_followed_by_a/ 
# https://search.r-project.org/CRAN/refmans/totalcensus/html/convert_fips_to_names.html 
# https://search.r-project.org/CRAN/refmans/usdata/html/abbr2state.html
# https://sparkbyexamples.com/r-programming/rename-column-in-r/#google_vignette 
# https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
# https://stackoverflow.com/questions/10841511/removing-elements-in-one-vector-from-another-in-r
# https://stackoverflow.com/questions/13093931/remove-last-word-from-string 
# https://stackoverflow.com/questions/18115550/combine-two-or-more-columns-in-a-dataframe-into-a-new-column-with-a-new-name 
# https://stackoverflow.com/questions/72408598/r-extract-first-two-characters-from-a-column-in-a-dataframe
