
# Load counties data
counties <- readRDS("data/counties.rds") %>%

# Separate state name and county name
  mutate(
    state = gsub( ",.*$", "", name),
    
    county = gsub(":.*", "", str_remove(name, ".*,"))
    
  )



# Merge counties data with map data for a given state

get_county_geodat <- function(state){
  
  ## Filter out the state of interest
  county_dat <- counties %>% 
    filter(state == tolower(state))
  
  ## Obtain map data of the state of interest
  county_map <- maps::map(
    database = "county",
    regions = tolower(state),
    plot = FALSE,
    fill = TRUE
  ) %>%
    st_as_sf() %>%
    mutate(county = str_remove(ID, ".*,"))
  
  
  # Merge county_dat and county_map for the state of interest
    county_geodat <- left_join(county_dat, county_map, by = "county") %>%
      # Select columns that will be used
      select(state, county, total.pop, white, black, hispanic, asian, geom)
 
   
  return(county_geodat)
  
}



# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(state, var, color, legend_title, min = 0, max = 100) {
      
  
  
      # Load data
      county_geodat <- get_county_geodat(state)
      
      var_fill <- case_when(
        var == "white" ~ county_geodat$white,
        var == "black" ~ county_geodat$black,
        var == "hispanic" ~ county_geodat$hispanic,
        var == "asian" ~ county_geodat$asian
      )
      
      # Generate legend key labels
      key_labs = seq(min, max, length.out = 5)
      
      for(i in 1 : length(key_labs)){
        
        key_labs[i] = paste(key_labs[i], "%", sep = "")
      }
      
      
    
      
      ## Deal with two special cases
      if((min != 0)){
        key_labs[1] = paste(key_labs[1], " or less", sep = "")
      }
      
      if((max != 100)){
        key_labs[length(key_labs)] = paste(key_labs[length(key_labs)], " or more", sep = "")
      }
     
   
      
      # Generate legend colors
      ## Generate vector of fill colors for map
      shades <- colorRampPalette(c("white", color))(100)
      
      # constrain gradient to percents that occur between min and max
      var_fill <- pmax(var_fill, min)
      var_fill <- pmin(var_fill, max)
      percents <- as.integer(cut(var_fill, 100, 
                              include.lowest = TRUE, ordered = TRUE))
      
      fills <- shades[percents]
      
      print(key_labs)
      
      print(shades[c(1, 25, 50, 75, 100)])
      
      ggplot(county_geodat) + 
        geom_sf(aes(fill = var_fill, 
                    geometry = geom),
                color = NA) + 
        
        scale_fill_gradientn(name = legend_title,
                             limits = c(min, max),
                             breaks = seq(min, max, length.out = 5),
                             labels = key_labs,
                             colors = shades[c(1, 25, 50, 75, 100)],
                             na.value = "#7F7F7F") + 
        
        ggtitle(state)+
        
        theme_minimal() + 
        
        theme(
          
          ### Plot ###
          plot.background = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size = 45),
        
          ### Panel ###
          panel.grid = element_blank(),
          
          
          ### Axis ###
          axis.text = element_blank(),
          
          
          ### Legend ###
          legend.title = element_text(size = 25),
          legend.key.height = unit(1.5, "cm"),
          legend.text = element_text(size = 18)
          
        )
      
}
