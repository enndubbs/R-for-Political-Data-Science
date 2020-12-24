#16 Maps and Spatial Data ####

library(tidyverse)
library(politicalds)
library(sf)
library(ggrepel)
library(gridExtra)
#install.packages("rnaturalearthhires", repos="http://packages.ropensci.org", type = "source")  
library(rnaturalearthhires)
library(spdep)
library(pinochet)
library(spdep)

#Spatial data files can be found at 'https://github.com/arcruz0/arcruz0.github.io/blob/master/politicalds/spatial_data_politicalds.zip'

#16.2   Spatial Data in R ####

#16.2.2    Structure 

shp_brazil <- read_sf("shp_brazil/shp_brazil.shp")
  
class(shp_brazil)

st_geometry(shp_brazil)

#16.3   Spatial Data Management ####

#16.3.1     Modiﬁcations 

#16.3.1.1      Filter and select by geographical units 

shp_brazil %>% 
  filter(state == "São Paulo")

shp_brazil <- shp_brazil %>%  
  filter(state != " State District of Fernando de Noronha (PE)")  

ggplot(data = shp_brazil) +
  geom_sf()

#16.3.1.2      Generate new units with st_union()Generate new units with st_union() 

shp_brazil <- shp_brazil %>% 
  mutate(region = case_when(
    state %in% c("Goiás", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal") ~ "Center-West",
    state %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins") ~ "North",
    state %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Distrito estadual de Fernando de Noronha (PE)", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe", "Maranhão") ~ "Northeast",
    state %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Southeast",
    state %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "South")
    )

ggplot(data = shp_brazil) +
  geom_sf(aes(fill = region)) +
  labs(fill = "")
  
shp_brazil_regions <- shp_brazil %>% 
  group_by(region) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()

ggplot(data = shp_brazil_regions) +
  geom_sf(aes(fill = region)) +
  labs(fill = "")

#16.3.1.3      Create new shapefiles with st_write() 

dir.create("shp_brazil_regions/")  

st_write(shp_brazil_regions, "shp_brazil_regions/shp_brazil_regions.shp")  

#16.3.2    Add data from other datasets with left_join()Add data from other datasets with left_join() 

data("brazil_states")
  
head(brazil_states)

head(brazil_states$state)

head(shp_brazil$state)

shp_brazil_data <- shp_brazil %>% 
  left_join(brazil_states)

head(shp_brazil_data)

#16.4   Mapping in R ####

#16.4.1    Generating centroids 

shp_brazil <- shp_brazil %>% mutate(centroid = map(geometry, st_centroid),  
        coords = map(centroid, st_coordinates), coords_x = map_dbl(coords, 1),  
        coords_y = map_dbl(coords, 2))  
   
head(shp_brazil)  

#16.4.2   Mapping variables 

ggplot(data = shp_brazil) +
  geom_sf() +
  geom_text_repel(mapping = aes(coords_x, coords_y, label = state),
                  size = 4, min.segment.length = 0) +
  labs(x = "", y = "")

   
ggplot(shp_brazil_data) + 
  geom_sf(aes(fill = gini)) +  
  scale_fill_gradient(low = "white", high = "black") +  
  theme(legend.key.width = unit(2, "cm")) + # make legend readable  
  labs(fill = "Gini index")  

ggplot(shp_brazil_data %>% filter(year == 2009)) + 
  geom_sf(aes(fill = gini)) +  
  scale_fill_gradient(low = "white", high = "black") +  
  theme(legend.key.width = unit(2, "cm")) + # make legend readable  
  labs(fill = "Gini index") 

#16.4.3    Mapping points 

chile <- rnaturalearthhires::countries10 %>% 
  st_as_sf() %>% 
  filter(SOVEREIGNT %in% c("Chile", "Argentina", "Peru", "Bolivia"))

pinochet <- pinochet::pinochet %>%  
     select(last_name, first_name, place_1, longitude_1, latitude_1,  
           location_1)  
   
head(pinochet)  

pinochet_sf <- pinochet %>% 
  filter(place_1 != "NA" & !is.na(longitude_1) & !is.na(latitude_1)) %>% 
  st_as_sf(coords = c("longitude_1", "latitude_1"), crs = 4326, remove = F)

head(pinochet_sf)

class(pinochet_sf)  

ggplot() +  
   geom_sf(data = chile) +  
   geom_sf(data = pinochet_sf, size = 1) +  
   coord_sf(xlim = c(-75.6, -67), ylim = c(-55, -19)) +  
   scale_x_continuous(breaks = c(-76, -67)) # also clean the x scale 
   
ggplot() +  
   geom_sf(data = chile) +  
   geom_sf(data = pinochet_sf,  
        mapping = aes(shape = place_1),  
        size = 1) +  
  coord_sf(xlim = c(-75.6, -67), ylim = c(-55, -19)) +  
  scale_x_continuous(breaks = c(-76, -67)) +  
  scale_shape_manual(values = 0:6) +  
  labs(shape = "")  

#16.5   Inference from Spatial Data ####

#16.5.2    Spatial Weights Matrix 

coords <- coordinates(as((shp_brazil), 'Spatial'))

shp_brazil_data <- shp_brazil_data %>% 
  filter(year == 2009)

#16.5.2.1     Rook criterion 

rook_brazil <- poly2nb(as(shp_brazil_data, 'Spatial'), queen = FALSE)  
   
nb_to_df <- function(nb, coords){  
    x <- coords[, 1]  
    y <- coords[, 2]  
    n <- length(nb)  
   
   
    cardnb <- card(nb)  
    i <- rep(1:n, cardnb)  
    j <- unlist(nb)  
    return(data.frame(x = x[i], xend = x[j],  
                      y = y[i], yend = y[j]))  
  }  

rook_brazil_df <- nb_to_df(rook_brazil, coords)

ggplot(shp_brazil_data) +
  geom_sf() +
  geom_point(data = rook_brazil_df,
             mapping = aes(x = x, y = y)) +
  geom_segment(data = rook_brazil_df,
               mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
  labs(x = "", y = "")

#16.5.2.2  Queen criterion 

queen_brazil <- poly2nb(as(shp_brazil_data, 'Spatial'), queen = T)

queen_brazil_df <- nb_to_df(queen_brazil, coords)

ggplot(shp_brazil_data) +
  geom_sf() +
  geom_point(data = queen_brazil_df,
             mapping = aes(x = x, y = y)) +
  geom_segment(data = queen_brazil_df,
               mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
  labs(x = "", y = "")

#16.5.2.3      K-nearest criterion 

kn_brazil <- knn2nb(knearneigh(coords, k = 6))
kn_brazil_df <- nb_to_df(kn_brazil, coords)

ggplot(shp_brazil_data) +
  geom_sf() +
  geom_point(data = kn_brazil_df, mapping = aes(x = x, y = y)) +
  geom_segment(data = kn_brazil_df,
               mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
  labs(x = "", y = "")

#16.5.3    Moran’s I 

queen_brazil_lw <- nb2listw(queen_brazil)  

moran.test(shp_brazil_data$gini, listw = queen_brazil_lw)

moran.plot(shp_brazil_data$gini, listw = queen_brazil_lw,
           xlab = "Gini",
           ylab = "Gini (spatially lagged)")

queen_brazil_b_lw <- nb2listw(queen_brazil, style = "B")

shp_brazil_data <- shp_brazil_data %>% 
  mutate(lmoran = localmoran(x = gini, listw = queen_brazil_b_lw)[, 1],
         lmoran_pval = localmoran(x = gini, listw = queen_brazil_b_lw)[, 5]
         )

ggplot(shp_brazil_data) +  
   geom_sf(aes(fill = lmoran))+  
   labs(fill = "Local Moran Statistic") +  
   scale_fill_gradient(low = "white", high = "black") 

shp_brazil_data <- shp_brazil_data %>% 
  mutate(
    # Standardize the Gini and Local Moran's to their means:
    st_gini = gini - mean(gini),
    st_lmoran = lmoran - mean(lmoran),
    # Create the new categorical variable:
    quadrant = case_when(
      lmoran_pval > 0.05 ~ "Insignificant",
      st_gini > 0 & st_lmoran > 0 ~ "High-High",
      st_gini < 0 & st_lmoran < 0 ~ "Low-Low",
      st_gini < 0 & st_lmoran > 0 ~ "Low-High",
      st_gini > 0 & st_lmoran < 0 ~ "High-Low"
  )
)

ggplot(shp_brazil_data) +  
  geom_sf() +  
  labs(fill = "Quadrant") +  
  scale_fill_manual(values = c("white", "lightgray","black"))