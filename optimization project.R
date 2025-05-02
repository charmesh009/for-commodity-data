#1
# Load required libraries
library(dplyr)
library(geosphere)
library(readr)

# Load FPS data
fps_data <- read_csv("https://github.com/charmesh009/for-commodity-data/raw/refs/heads/main/shop-status-details_3_2025.csv")

# Load district centroid data (godown locations)
godown_data <- read_csv("https://github.com/charmesh009/for-commodity-data/raw/refs/heads/main/telangana_district_centroids.csv")

# Standardize column names
fps_data <- fps_data %>%
  rename(shop_lat = latitude, shop_lon = longitude, district = distName)

godown_data <- godown_data %>%
  rename(godown_lat = latitude, godown_lon = longitude)

# Merge on district (assumption: each FPS gets its district godown)
merged_data <- fps_data %>%
  inner_join(godown_data, by = "district") %>%
  filter(!is.na(shop_lat), !is.na(shop_lon), !is.na(godown_lat), !is.na(godown_lon))

# Compute Haversine distance in kilometers between godown and FPS
merged_data$distance_km <- distHaversine(
  cbind(merged_data$godown_lon, merged_data$godown_lat),
  cbind(merged_data$shop_lon, merged_data$shop_lat)
) / 1000

# Create edge list: from godown to shop (used for network visualization or LP modeling)
edge_list <- merged_data %>%
  transmute(
    from_node = paste0("godown_", district),
    to_node = paste0("shop_", shopNo),
    distance_km = round(distance_km, 3)
  )

# Preview result
head(edge_list)

# Save as CSV (optional: use this as input for later stages or visualizations)
write_csv(edge_list, "D:/fps_godown_edge_list.csv")




#2
# Load required packages
library("dplyr")
library("geosphere")  # for distance calculation
library("lpSolve")    # for linear programming

# STEP 1: Calculate Haversine distance between each FPS and its godown
merged_data <- merged_data %>%
  mutate(
    distance = distHaversine(cbind(shop_lon, shop_lat), cbind(godown_lon, godown_lat)) / 1000  # in km
  )

# STEP 2: Simulate demand at each FPS
# Assumption: Demand follows Poisson distribution with mean 100
set.seed(42)
merged_data <- merged_data %>%
  mutate(demand = rpois(n(), lambda = 100))

# STEP 3: Define the cost function
# Assumption: cost is proportional to distance × demand
merged_data <- merged_data %>%
  mutate(cost = distance * demand)

# STEP 4: Set up LP model for assignment
# Note: Here, each FPS has only one godown (district level), so this is trivial binary selection
num_fps <- nrow(merged_data)
objective <- merged_data$cost

# Constraints: each FPS is assigned exactly once (already 1-to-1 in this model)
const.mat <- diag(num_fps)
const.dir <- rep("==", num_fps)
const.rhs <- rep(1, num_fps)

# Solve LP with binary decision variables
solution <- lp(
  direction = "min",
  objective.in = objective,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs,
  all.bin = TRUE
)

# STEP 5: View results
solution$objval           # Total transportation cost
assigned <- solution$solution  # Binary assignment (1 = assigned)

# Add assignment column to merged_data
merged_data$assigned <- assigned

# View only assigned routes (should be all rows here due to 1-to-1 structure)
assigned_routes <- merged_data %>% filter(assigned == 1)

head(assigned_routes)     # Preview assigned routes

# Optional summary stats for report
summary(assigned_routes$distance)
sum(assigned_routes$demand)



# Visualization section (Leaflet map of connections)

# Check and install leaflet if not present (optional improvement)
if (!require(leaflet)) install.packages("leaflet")

# Load required libraries
library(dplyr)
library(readr)
library(leaflet)

# Reload data for visualization (alternative to passing merged_data forward)
fps_data <- read_csv("https://raw.githubusercontent.com/charmesh009/for-commodity-data/refs/heads/main/shop-status-details_3_2025.csv")
godown_data <- read_csv("https://github.com/charmesh009/for-commodity-data/raw/refs/heads/main/telangana_district_centroids%20(2).csv")

# Rename columns
fps_data <- fps_data %>%
  rename(shop_lat = latitude, shop_lon = longitude, district = distName)

godown_data <- godown_data %>%
  rename(godown_lat = latitude, godown_lon = longitude)

# Merge on district
merged_data <- fps_data %>%
  inner_join(godown_data, by = "district") %>%
  filter(!is.na(shop_lat), !is.na(shop_lon), !is.na(godown_lat), !is.na(godown_lon))

# Sample to limit overcrowding on map (50 FPS per district)
set.seed(123)
sampled_data <- merged_data %>% group_by(district) %>% slice_sample(n = 50) %>% ungroup()

# Initialize leaflet map with base tiles
m <- leaflet() %>%
  addProviderTiles("CartoDB.Positron")

# Add red godown markers
m <- m %>%
  addCircleMarkers(
    data = sampled_data,
    lng = ~godown_lon, lat = ~godown_lat,
    color = "red", radius = 5, stroke = FALSE, fillOpacity = 0.9,
    label = ~paste("Godown:", district)
  )

# Add blue FPS markers
m <- m %>%
  addCircleMarkers(
    data = sampled_data,
    lng = ~shop_lon, lat = ~shop_lat,
    color = "blue", radius = 2, stroke = FALSE, fillOpacity = 0.6,
    label = ~paste("Shop:", shopNo)
  )

# Add gray lines connecting each shop to its godown
for (i in 1:nrow(sampled_data)) {
  m <- m %>% addPolylines(
    lng = c(sampled_data$godown_lon[i], sampled_data$shop_lon[i]),
    lat = c(sampled_data$godown_lat[i], sampled_data$shop_lat[i]),
    color = "gray", weight = 1, opacity = 0.5
  )
}

# Optional: add legend for clarity
m <- m %>%
  addLegend("bottomright", colors = c("red", "blue"), labels = c("Godown", "FPS"), title = "Node Type")

# Display the interactive map
m
