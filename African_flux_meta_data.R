#loading packages ----
library(tidyverse)  
library(rgdal)  
library(raster)  
library(ggsn)  
library(rworldmap)
library(rworldxtra)
library(ggplot2)
library(countrycode)
library(patchwork)
library(cowplot)
library(RColorBrewer)

#Importing data ----
Fluxnet<- read_csv("African_flux_meta_data.csv")

##Data cleaning -Keep only the needed columns and add a new column----
fluxdata <- c("Site_Code", "Site_Name", "Site_Location", "Site_Latitude", "Site_Longitude", "IGBP_Code", "Year_Start", "Date_Stop", "Record_Length")
flux_trim <- Fluxnet %>% dplyr::select(one_of(fluxdata))

sites_with_data <- c("BJ-BIF", "BJ-BEF", "BJ-NAF", "BW-Gum", "BW-Nxr", "CG-Tch", "GH-Ank", "ML-AgG", "NE-WaM", "NE-WaF", "SN-Dhr", "SN-RAG", "SN-Nkr", "ZA-Kru", "ZA-XxD", "ZA-Wgn", "SD-Dem", "UG-Jin", "ZM-Mon", "EG-Urb", "EG-Agri", "EG-Dst")
flux_trim$Data_available <- ifelse(flux_trim$Site_Code %in% sites_with_data, "Yes", "No")



#Preliminary plot ----
(prelim_plot <- ggplot(flux_trim, aes(x = Site_Longitude, y = Site_Latitude, 
                                      colour = IGBP_Code, shape = Data_available)) +
   scale_shape_manual(values = c("Yes" = 16, "No" = 1)) +
   geom_point())

(prelim_plot <- ggplot(flux_trim, aes(x = Site_Longitude, y = Site_Latitude, 
                                      colour = IGBP_Code, shape = Data_available)) +
    geom_point())

world <- getMap(resolution = "low")

#Subset a library dataframe into the list of countries and their respective continent ----
countries<-codelist[,c("continent","country.name.en")]

#Subset that dataframe into just countries in Africa----
countries<-subset(countries,continent=='Africa')

#Some of the countries in the dataframe have alternate names to their official ones----
#Use the below code to change them-----
countries[countries=='Congo - Brazzaville']<-("Republic of the Congo")
countries[countries=='Congo - Kinshasa']<-("Democratic Republic of the Congo")


#GGPlot has global map data, loading into a variable ----
mapdata<-map_data('world')

#Renaming some of the options so they correspond with the "countries" database ----
mapdata[mapdata=="Swaziland"]<-"Eswatini"
mapdata[mapdata=="Ivory Coast"]<-"Côte d’Ivoire"
mapdata[mapdata=="Republic of Congo"]<-"Republic of the Congo"


#Filtering the mapdata (using magrittr's pipes) to include only the countries from the list of African countries
mapdata<-mapdata%>%filter(region %in% countries$country.name.en)



#plot map----
(prelim_plot <- ggplot(flux_trim, aes(x = Site_Longitude, y = Site_Latitude, 
                                      colour = IGBP_Code, shape = Data_available)) +
    geom_point())

# Define the number of colors needed
num_colors <- length(unique(flux_trim$IGBP_Code))

# Select a color palette from RColorBrewer
color_palette <- brewer.pal(num_colors, "Set3")

p1 <- ggplot() +
  geom_polygon(data = mapdata, 
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black") + 
  geom_point(data = flux_trim,
             aes(x = Site_Longitude, y = Site_Latitude, colour = IGBP_Code, shape = Data_available, fill = IGBP_Code),
             alpha = 8, size = 5) +
  scale_color_manual(values = color_palette) +
  coord_quickmap() +  
  theme_void() +  
  xlab("Longitude") +
  ylab("Latitude") + 
  theme(legend.position = "left")  # Adjust legend position

p1






#extracting the frequency of ecosystem ----
ecofreq <- c("IGBP_Code")
ecofreq_trim <- Fluxnet %>% dplyr::select(one_of(ecofreq))

# compute unique levels in data frame
lvls <- unique(unlist(ecofreq_trim))

# apply the summation per value
freq <- sapply(ecofreq_trim,
               function(x) table(factor(x, levels = lvls,
                                        ordered = TRUE)))

# making a barplots ----
# Site per ecosystem type ----
SAV <- (15)
CRO <- (9)
GRA <- (8)
OSH <- (1)
WET <- (4)
EBF <- (5)
DBF <- (2)
BSV <- (2)
URB <- (1)


# Chain them together in a vector
sitesfreq <- c(SAV, CRO, DBF, OSH, WET, EBF, GRA, BSV, URB)


# Create an ecosystem vector
ecosystem <- c("Savanna", "Cropland", "Deciduous broadleaf forest", "Open shrubland", "Wetland", "Evergreen braodleaf forest", "Grassland", "Barren spare vegetation", "Urban")

# changing ecosystem from character form to factor 
ecosystem <- as.factor(ecosystem)
class(ecosystem)

# combining the two vectors in a data frame

SitesPerIGBP <- data.frame(ecosystem, sitesfreq)


# Plot the bar plot 
p2 <- ggplot(SitesPerIGBP) + 
  geom_bar(aes(x = reorder(ecosystem, -sitesfreq), y = sitesfreq, fill = ecosystem), stat = "identity", colour = 'black') +
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  xlab("Ecosystem") +
  ylab('Number of sites') +
  ylim(0, 14) +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1), 
        panel.grid = element_blank(), legend.position = "none")
p2


#plot to visualize record length
recordlength<- c("Site_Latitude", "Site_Longitude", "Year_Start", "Date_Stop", "Record_Length", "Site_Code", "IGBP_Code")
recordlength_trim <- Fluxnet %>% dplyr::select(one_of(recordlength))

is.numeric(recordlength_trim$Record_Length)
recordlength_trim <- recordlength_trim %>% replace(is.na(.), 0)

p3 <- ggplot(recordlength_trim) + 
  geom_bar(aes(x = reorder(Site_Code, -Record_Length), y = Record_Length), 
           stat = "identity", fill = 'blue') +
  theme_bw() +
  xlab("Sites") +
  ylab('Record length (in years)') +
  ylim(0, 14) +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme(axis.text.x = element_text(size = 11, angle = 90, vjust = 1, hjust = 1), 
        panel.grid = element_blank(), legend.position = "left")

p3




#Plot to visualize years observation for each ecosystem type
Savanna <- (27)
Wetlands <- (7)
Evergreenbroadleafforest <- (20)
Grasslands <- (24)
DeciduousbroadleafForests <- (10)
Cropland <- (39)

# Chain them together in a vector
ecoyears <- c(Savanna, Wetlands, Evergreenbroadleafforest, Grasslands, DeciduousbroadleafForests, Cropland)


# Create an ecosystem vector
ecosystem_vector <- c("Savanna", "Wetlands", "Evergreen braodleaf forest", "Grassland", "Deciduous broadleaf forest", "Cropland")

# changing ecosystem from character form to factor 
ecosystem_vector <- as.factor(ecosystem_vector)
class(ecosystem_vector)

# combining the two vectors in a data frame
SitesyearsPerIGBP <- data.frame(ecosystem_vector, ecoyears)


# Plot the bar plot 
p4 <- ggplot(SitesyearsPerIGBP) + 
  geom_bar(aes(x = reorder(ecosystem_vector, -ecoyears), y = ecoyears), 
           stat = "identity", fill = 'blue') +
  theme_bw() +
  xlab("Ecosystem") +
  ylab('site-years') +
  ylim(0, 35) +
  theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1), 
        panel.grid = element_blank(), legend.position = "left")


p4



# Combine the plots using patchwork and add tags ----
##p1 and p2----
combined_plot1 <- (p1 + labs(tag = "a") +
                     theme(plot.tag = element_text(face = "bold"))) /
  (p2 + labs(tag = "b") +
     theme(plot.tag = element_text(face = "bold")))

# Arrange the plots side by side
combined_plot1 <- combined_plot1 + plot_layout(ncol = 2)

# Display the combined plot
combined_plot1

##p3 and p4----
combined_plot2 <- (p3 + labs(tag = "a") +
                     theme(plot.tag = element_text(face = "bold"))) /
  (p4 + labs(tag = "b") +
     theme(plot.tag = element_text(face = "bold")))

# Arrange the plots side by side
combined_plot2 <- combined_plot2 + plot_layout(ncol = 2)

# Display the combined plot
combined_plot2

### Alternative to combine plot objects with Patchwork ----
pall <- (p1 + p2)
plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))
filename = "plots/Figure_1a"
ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 30,
  height = 18,
  units = "cm"
)

pall <- (p3 + p4)
plot_annotation(tag_levels = 'a') & theme(plot.tag.position = c(0.0, 0.97))
filename = "plots/Figure_1b"
ggsave(
  pall,
  filename = paste0(filename, ".png"),
  width = 30,
  height = 18,
  units = "cm"
)






# EXTRA CODE FOR MAP --AI did this for the ORC 2024 conference presentation ----

# Filter out NA values in IGBP_Code
flux_trim_filtered <- flux_trim %>% filter(!is.na(IGBP_Code))

# Create the plot with the requested changes
p1 <- ggplot() +
  geom_polygon(data = mapdata, 
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black") + 
  geom_point(data = flux_trim_filtered,
             aes(x = Site_Longitude, y = Site_Latitude, colour = IGBP_Code, shape = Data_available, fill = IGBP_Code),
             alpha = 8, size = 5, show.legend = c(shape = FALSE)) +  # Remove Data_available legend
  scale_color_manual(values = color_palette) +
  coord_quickmap() +  
  theme_void() +  
  xlab("Longitude") +
  ylab("Latitude") + 
  ggtitle("Flux tower sites") +  # Add title
  labs(color = "Ecosystem", fill = "Ecosystem") +  # Change legend title
  theme(
    legend.position = "left",  # Adjust legend position
    plot.title = element_text(size = 22, face = "bold"),  # Increase title size
    legend.title = element_text(size = 15),  # Increase legend title size
    legend.text = element_text(size = 14)  # Increase legend text size
  )

# Display the plot
print(p1)
