########## Short introduction to creating choropleth maps in R ##########

library(ggmap)
library(rgeos)
library(maptools)
library(basemaps)

# Read the census tracts shapefile data and plot the boundaries
shpfile = "./data/census_tracts_wgs84/census_tracts_wgs84.shp"
sh = readShapePoly(shpfile)
plot(sh)


# Creating a choropleth map based on the values of total population
# Set the palette
n=64
p = colorRampPalette(c("white", "red"))(n)  # check color codes: https://html-color.codes/
palette(p)
# Scale the total population to the palette
pop = sh@data$POP_TOT
cols = (pop - min(pop))/diff(range(pop))*(n-1)+1
plot(sh, col=cols)



########################################################################################################

### A cooler way to create the same map ############

library(sf)
library(tidyverse)
library(classInt)
library(viridis)

sf_gb  = read_sf(dsn = "./data/census_tracts_wgs84", layer = "census_tracts_wgs84")
glimpse(sf_gb)
st_geometry(sf_gb)
plot(st_geometry(sf_gb))


# Create a choropleth map using a discrete palette (based on quantile intervals)
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sf_gb,                                                             # add a simple features (sf) object
          aes(fill = cut_number(POP_TOT, 5)),                                       # group variable into equal number of observation per interval and use it for fill
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'white',                                                         # make polygon boundaries white
          size = 0.3) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu",                                             # choose a http://colorbrewer2.org/ palette
                    name = "Residents (n)"                                          # add legend title
                    , direction = -1                                                # choose the palette direction
  ) +                               
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Residents by Census Tracts, Milan",                                 # add title
       subtitle = "Source: ISTAT, Census 2011",                                     # add subtitle
       caption = "Statistics for Marketing @ LUISS 2022/2023") +                    # add caption
  theme(panel.background = element_blank())                                         # remove background gridlines



# Create a choropleth map using a continuous viridis colour palette

ggplot(sf_gb, aes(fill = POP_TOT)) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Residents (n)",
                     direction = 1
  ) +
  labs(x = NULL, y = NULL,
       title = "Residents by Census Tracts, Milan",
       subtitle = "Source: ISTAT, Census 2011",
       caption = "Statistics for Marketing @ LUISS 2021/2022") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


# Try to change "POP_TOT" with "log(POP_TOT+1)" to handle extreme values


# Create a choropleth map using discrete variables
ggplot(sf_gb, aes(fill = factor(FLAG_COVER))) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_manual(values = c("grey", "dark red")) +
  labs(x = NULL, y = NULL,
       title = "Residents by Census Tracts, Milan",
       subtitle = "Source: ISTAT, Census 2011",
       caption = "Statistics for Marketing @ LUISS 2022/2023") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))



########################################################################################################
# Overlay point feature with polygon feature


# Import Point of Interest table with latitude and longitude
poi = read.table("./data/Nidi_WGS84.csv", header = TRUE, dec = ",", sep = ";")


# Overlay point feature (in red) on choropleth census tracts
ggplot(sf_gb, aes(fill = log(POP_TOT+1))) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Residents (n)",
                     direction = 1
  ) +
  labs(x = NULL, y = NULL,
       title = "Residents by Census Tracts, Milan",
       subtitle = "Source: ISTAT, Census 2011",
       caption = "Statistics for Marketing @ LUISS 2021/2022") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_point(data=poi, aes(lon, lat), inherit.aes = FALSE, alpha = 1, size = 2, colour="red")


# Overlay point feature - colored by column values -  on choropleth census tracts
ggplot(sf_gb, aes(fill = log(POP_TOT+1))) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Residents (n)",
                     direction = 1
  ) +
  labs(x = NULL, y = NULL,
       title = "Residents by Census Tracts, Milan",
       subtitle = "Source: ISTAT, Census 2011",
       caption = "Statistics for Marketing @ LUISS 2021/2022") +
  coord_sf(datum = NA) +
  geom_point(data=poi, aes(lon, lat, color=factor(ETA)), inherit.aes = FALSE, alpha = 1, size = 2) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        #legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


######################################################################################
# SPATIAL JOIN / INTERSECT

# Objective: 
# Add the census tract data to each kindergarten


sf_poi = read_sf(dsn = "./data/Asili_nido_Milano", layer = "Nidi_WGS84")

sj_poi = st_join(sf_poi, sf_gb[, c("CODICE01", "POP_TOT", "NEIGHBORHO")], left=TRUE)


# Extracting data frame from simple feature object
df_sj_poi = sj_poi %>% st_set_geometry(NULL)

# Print
head(df_sj_poi)



######################################################################################
# BUFFER

# Objective: 
# Create a 300 meters buffer for each point
#example I want to create a 300 buffer starting from all the kindergardens present in Milan. we want to create a new flag variable inside the table.
#I want to perform the spatial operation between the buffer previously created. for each kindergarten we have the number of metro stations inside its buffer.
sf_buffer = st_buffer(x=sf_poi, dist=300)

ggplot(sf_gb, aes(fill = log(POP_TOT+1))) +
  geom_sf(alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_viridis(discrete = F,
                     name = "Residents (n)",
                     direction = 1
  ) +
  labs(x = NULL, y = NULL,
       title = "Residents by Census Tracts, Milan",
       subtitle = "Source: ISTAT, Census 2011",
       caption = "Statistics for Marketing @ LUISS 2021/2022") +
  coord_sf(datum = NA) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.2, 0.09),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  geom_sf(data=sf_buffer, aes(fill = 7), color="orange", alpha = 0.2)

?geom_sf 



######################################################################################
# FLAG CREATION

# Objective: 
# Create a new flag variable:
# 1 = There is at least one metro station within 300 meters
# 0 = There isn't any metro station within 300 meters


sf_metro = read_sf(dsn = "./data/metro_mi", layer = "MM_FERMATE_WGS84")
sj_buffer_metro = st_join(sf_buffer, sf_metro[, c("ID", "FERMATA")], left=FALSE)
# Extracting data frame from simple feature object
df_sj_buffer_metro = sj_buffer_metro %>% st_set_geometry(NULL)

# Group data
metro_gr = df_sj_buffer_metro %>% 
  group_by(SEDE) %>%
  summarise(n_metro = n())

# Add n_metro variable to "poi" dataframe
poi2 = poi %>%
  left_join(metro_gr, by="SEDE")

# fill missing values as 0
poi2[is.na(poi2$n_metro), c("n_metro")] = 0

# Create flag variable
poi2 = poi2 %>%
  mutate(flag_metro = ifelse(n_metro>0, 1, 0))
#i
