### Estimating traffic flow between observation points in the UK
# Using DfT bulk data: https://roadtraffic.dft.gov.uk/downloads
library(downloader)
library(magrittr)
library(dplyr)
library(sf)

if(!file.exists('traffic_dir.zip')){
  downloader::download('http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf_by_direction.zip',
                     dest = 'traffic_dir.zip', mode = 'wb')
}
if(!file.exists('dft_traffic_counts_aadf_by_direction.csv')){
  unzip('traffic_dir.zip')
}

traffic_data <- read.csv('dft_traffic_counts_aadf_by_direction.csv') %>%
  # Only keep major, non-ring roads in England
  dplyr::filter(road_type == 'Major' & grepl('^[^NSW]', region_ons_code) & direction_of_travel != 'C') %>%
  dplyr::mutate(direction_of_travel = toupper(direction_of_travel),
                local_authority_code = as.character(local_authority_code)) %>%
  dplyr::select(count_point_id, year, local_authority_name, local_authority_code,
                road_name, latitude, longitude, link_length_km, 
                estimation_method, direction_of_travel, all_motor_vehicles)

other_authorities <- traffic_data %>%
  dplyr::group_by(local_authority_code,
                  count_point_id) %>%
  dplyr::summarise(lat_unique = mean(latitude),
                   lon_unique = mean(longitude)) %>%
  dplyr::ungroup() %>%
  dplyr::select(local_auth = local_authority_code,
                lat_unique, lon_unique)

all_combs <- merge(traffic_data, other_authorities, all = T)

east_combs <- all_combs %>%
  dplyr::filter(longitude < lon_unique & direction_of_travel == 'E') %>%
  # Crude distance to make calculation less complicated
  dplyr::mutate(distance = sqrt( (latitude - lat_unique)^2 + (longitude - lon_unique)^2)) %>%
  # Cut-off of distance to make data manageable (and network halfway across the country is less strong)
  dplyr::filter(distance < median(distance)) %>%
  dplyr::mutate(cars_by_distance = all_motor_vehicles / distance)
west_combs <- all_combs %>%
  dplyr::filter(longitude > lon_unique & direction_of_travel == 'W') %>%
  # Crude distance to make calculation less complicated
  dplyr::mutate(distance = sqrt( (latitude - lat_unique)^2 + (longitude - lon_unique)^2)) %>%
  # Cut-off of distance to make data manageable (and network halfway across the country is less strong)
  dplyr::filter(distance < median(distance)) %>%
  dplyr::mutate(cars_by_distance = all_motor_vehicles / distance)
north_combs <- all_combs %>%
  dplyr::filter(latitude < lat_unique & direction_of_travel == 'N') %>%
  # Crude distance to make calculation less complicated
  dplyr::mutate(distance = sqrt( (latitude - lat_unique)^2 + (longitude - lon_unique)^2)) %>%
  # Cut-off of distance to make data manageable (and network halfway across the country is less strong)
  dplyr::filter(distance < median(distance)) %>%
  dplyr::mutate(cars_by_distance = all_motor_vehicles / distance)
south_combs <- all_combs %>%
  dplyr::filter(latitude > lat_unique & direction_of_travel == 'S') %>%
  # Crude distance to make calculation less complicated
  dplyr::mutate(distance = sqrt( (latitude - lat_unique)^2 + (longitude - lon_unique)^2)) %>%
  # Cut-off of distance to make data manageable (and network halfway across the country is less strong)
  dplyr::filter(distance < median(distance)) %>%
  dplyr::mutate(cars_by_distance = all_motor_vehicles / distance)

test <- sf::st_linestring(x = matrix(c(north_combs$longitude, north_combs$latitude, 
                                       north_combs$lon_unique, north_combs$lat_unique),
                             ncol = 2))

