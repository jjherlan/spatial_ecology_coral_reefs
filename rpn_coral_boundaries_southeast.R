## Testing script on FR9
## Author Sarah McTague
##07/25/24

# Load necessary packages.
require(tidyverse)
require(purrr)
library(sp)
library(sf)
library(terra)
library(units)
library(tidyr)
library(dplyr)
library(data.table)

# Set working directory.
# wd <- "C:/Users/ssmct/Documents/Sandin Lab/FR9/vector"
# wd <- "D:/sp_ecology_FR9/vector"
wd <- "D:/sp_ecology_FR9/vector"
# get(wd)
setwd(wd)

## Add boundary of site. 
# boundary_dir <- "C:/Users/ssmct/Documents/Sandin Lab/FR9/vector/boundary"
boundary_dir <- "D:/sp_ecology_FR9/vector/boundary"
boundary <- st_read(boundary_dir)

## Set boundary to correct CRS depending on location of site. 
boundary <- st_transform(boundary, crs = 32606)

# Store shapefile names as a list.
shp_files <- list.files(wd, pattern = "\\.shp$")

# Batch shapefile loading function.
rgdal_batch_shp <- function(shp_path) {
  shp_sf <- st_read(shp_path, stringsAsFactors = FALSE)
  return(shp_sf)
}

# Pass batch function to shapefile list.
batch_shp_list <- lapply(shp_files, rgdal_batch_shp)

# batch_shp_list <- lmap(shp_files, rgdal_batch_shp)
# 
# batch_shp_list <- map(shp_files, rgdal_batch_shp)
# list2env(batch_shp_list, .GlobalEnv)

# Merge the shapefiles.
# merged_shapefile <- do.call(rbind, batch_shp_list)
merged_shapefile2 <- do.call(rbind, batch_shp_list)

## Set merged_shapefile to correct CRS depending on location of site. 
# merged_shapefile <- st_transform(merged_shapefile, crs = 32606)
merged_shapefile2 <- st_transform(merged_shapefile2, crs = 32606)

# merged_shapefile.tibble <-
# merged_shapefile %>%
#   as_tibble()

merged_shapefile.tibble2 <-
  merged_shapefile2 %>%
  as_tibble()

# Calculate the area of each polygon in square meters.
merged_shapefile$coralarea <- st_area(merged_shapefile, byid = TRUE)

# Remove those features that are smaller than 9 square centimeters (0.0009 square meters)
merged_shapefile_filter <- merged_shapefile[merged_shapefile$coralarea >= units::set_units(0.0009, "m^2"), ]

## Create buffer of 10 cm. The byid ensures there is no merging of overlapping buffers.
buffered_data <- st_buffer(merged_shapefile_filter, dist = 0.1, byid = TRUE)

# Remove the original polygons from the buffered data output.
buffered_ring_list <- vector("list", length = nrow(buffered_data))
for (i in seq_len(nrow(buffered_data))) {
  buffered_ring_list[[i]] <- st_difference(buffered_data[i, ], merged_shapefile_filter[i, ])
}

# Combine the buffered rings into a single sf object
buffered_ring <- do.call(rbind, buffered_ring_list)

# Identify and remove features that overlap the boundary of the site.
overlap_indices <- st_intersects(buffered_ring, boundary, by_element = TRUE)
buffers_f <- buffered_ring[!apply(overlap_indices, 1, any), ]

# Create a new attribute of Taxa_ID to represent the attribute Buffer.
buffers_f$Buffer <- paste(buffers_f$Taxa, buffers_f$Id, sep = "_")

# Calculate the total area of each buffer polygon.
buffers_f$bufferarea <- st_area(buffers_f, byid = TRUE)

# Create an empty data frame to store the results.
overlap_data <- data.frame(Taxa = character(),
                           Buffer = character(),
                           Neighbor = character(),
                           bufferarea = numeric(),
                           intersectionarea = numeric(),
                           ratio = numeric(),
                           stringsAsFactors = FALSE)


# Iterate over each buffer polygon in buffers_f.
for (i in 1:nrow(buffers_f)) {
  #for (i in 1:20) {
  # Extract the Taxa name and buffer ID.
  taxa <- buffers_f$Taxa[i]
  buffer_id <- buffers_f$Buffer[i]
  
  # Find intersecting polygons between buffer and merged_shapefile_filter.
  intersecting_indices <- which(st_intersects(buffers_f[i, ], merged_shapefile_filter, sparse = FALSE))
  
  # Initialize a list to store the raw intersection data.
  intersection_data <- data.frame(Neighbor = character(), intersectionarea = numeric(), stringsAsFactors = FALSE)
  
  # Loop through each intersecting polygon.
  for (j in intersecting_indices) {
    # Calculate the intersection between the buffer polygon and the intersecting polygon
    intersection <- st_intersection(merged_shapefile_filter[j, ], buffers_f[i, ])
    
    # Calculate the area of the intersection
    intersection_area <- st_area(intersection)
    #print(intersection_area)
    
    # Extract the Taxa name of the intersecting polygon
    taxa_original <- merged_shapefile_filter[j, ]$Taxa
    
    # Extract the total area of the buffer polygon
    buffer_area <- buffers_f[i, ]$bufferarea
    
    # Store the intersection data
    intersection_data <- rbind(intersection_data, data.frame(Neighbor = taxa_original, intersection_area = intersection_area))
    #}
  }
  
  # Summarize the intersection data by Neighbor (taxa_original)
  intersection_summary <- intersection_data %>%
    group_by(Neighbor) %>%
    summarise(intersection_area = sum(intersection_area))
  
  # Add the buffer area and other details to the summary data frame
  intersection_summary <- intersection_summary %>%
    mutate(Taxa = taxa, 
           Buffer = buffer_id,
           bufferarea = buffer_area)
  
  # Calculate ratio
  intersection_summary <- transform(intersection_summary, ratio = intersection_area / bufferarea)
  
  # Add the summarized data to the overlap_data data frame
  overlap_data <- rbind(overlap_data, intersection_summary)

}

# Remove features that are equal to 0. 
overlap_data <- overlap_data[overlap_data$ratio > units::set_units(0, "1"), ]

# Print the overlap_data data frame.
print(overlap_data)

# Remove 'intersection_area' and 'bufferarea' columns from the overlap_data data frame.
overlap_data <- subset(overlap_data, select = -c(intersection_area, bufferarea))

# Pivot the overlap_data dataframe.
overlap_data_piv <- pivot_wider(overlap_data, names_from = Neighbor, values_from = ratio)

# Convert pivoted data to data.table.
overlap_data_piv <- as.data.table(overlap_data_piv)

# Remove rows with all NAs in the pivoted columns.
overlap_data_piv <- overlap_data_piv[!apply(overlap_data_piv[, -c("Taxa", "Buffer"), with = FALSE], 1, function(row) all(is.na(row)))]

# Replace remaining NAs with zeros.
overlap_data_piv[is.na(overlap_data_piv)] <- 0

# Extract the first two columns.
first_columns <- overlap_data_piv[, .(Taxa, Buffer)]

# Extract and alphabetize the remaining columns.
remaining_columns <- overlap_data_piv[, .SD, .SDcols = sort(setdiff(names(overlap_data_piv), c("Taxa", "Buffer")))]

# Combine the first two columns with the alphabetized columns.
final_data <- cbind(first_columns, remaining_columns)

# Print the final data.table with alphabetized columns (except first two)
print(final_data)

# Setting up environment for bootstrapping process. 
# taxa: Extracts the column names of taxa from the data excluding the first two columns (containing "Taxa" identifiers).
taxa <- colnames(final_data)[3:ncol(final_data)]
# n_taxa: Computes the number of taxa.
n_taxa <- length(taxa)
# n_colonies: Determines the number of rows in the data.
n_colonies <- dim(final_data)[1]
# count_focal_taxa: Calculates the count of each taxa in the dataset.
count_focal_taxa <- tapply(final_data$Taxa, final_data$Taxa, length)
n_focal_taxa <- length(count_focal_taxa)

## BOOTSTRAPPING
# Sample size of 10000
B <- 10000    

# Initialize array to store bootstrap samples for all taxa
taxa_resample <- array(dim = c(B, n_taxa, n_focal_taxa), dimnames = list(NULL, taxa, NULL))

for (i in 1:B) {
  for (j in 1:length(count_focal_taxa)) {
    # Bootstrap sampling for each taxa
    sample_rows <- sample(c(1:n_colonies), count_focal_taxa[j], replace = TRUE)
    # Store bootstrap sample for each taxa separately
    taxa_resample[i, , j] <- colMeans(final_data[sample_rows, ..taxa])
  }
}
print(taxa_resample)

# Print average intersection of buffer and neighbor taxa data into table. 
final_data <- final_data[,-c("Buffer")]
data_final_mean <- final_data %>% group_by(Taxa) %>% summarise_each(funs(mean(.,na.rm=FALSE)))
data_final_mean <- as.data.table(data_final_mean)
print(data_final_mean)

## Calculate the lower quantile and upper quantile values for each column within each array slice. 
lower_quantiles <- apply(taxa_resample, MARGIN = c(2,3), function(x) quantile(x, probs = 0.025))
upper_quantiles <- apply(taxa_resample, MARGIN = c(2,3), function(x) quantile(x, probs = 0.975))

# Convert to table and add column for taxa names.
lower_quantiles_dt <- as.data.table(lower_quantiles)
upper_quantiles_dt <- as.data.table(upper_quantiles)
lower_quantiles_dt$Taxa <- data_final_mean$Taxa
upper_quantiles_dt$Taxa <- data_final_mean$Taxa
lower_quantiles_dt <- lower_quantiles_dt %>% select(Taxa, everything())  
upper_quantiles_dt <- upper_quantiles_dt %>% select(Taxa, everything()) 

data_final_mean <- drop_units(data_final_mean)
upper_quantiles_dt <- drop_units(upper_quantiles_dt)
lower_quantiles_dt <- drop_units(lower_quantiles_dt)


# Create final results table based on upper and lower datatables. 
result <- data.frame(ifelse(data_final_mean > upper_quantiles_dt, 1, ifelse(data_final_mean < lower_quantiles_dt, -1, 0)))
result$Taxa <- data_final_mean$Taxa

# Print or use 'result' as needed
print(result)
