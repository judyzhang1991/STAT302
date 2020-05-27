
# ---------------- HELPER FUNCTIONS ----------- #
# Capitalize every word in a string

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# ------------------------------------------- #




# Load counties data
counties <- readRDS("data/counties.rds") %>%

# Separate state name and county name
  mutate(
    state = gsub( ",.*$", "", name),
    
    county = gsub(":.*", "", str_remove(name, ".*,"))
    
  ) 



# Add category: 
## Determine which percentage category the value belongs to
## And adds it to the given dataframe
add_categ <- function(var, dat, min, max){
  
  # Calculate interval
  interval = (max - min) / 4

  dat$categ <- case_when(
    dat[,var] >= min & dat[,var] < min + interval ~ if(min == 0){paste(min, "%", sep = "")} else{paste(min, "% or less", sep = "")},
    dat[,var] >= min + interval & dat[,var] < min + 2 * interval ~ paste(min + interval, "%", sep = ""),
    dat[,var] >= min + 2 * interval & dat[,var] < min + 3 * interval ~ paste(min + 2 * interval, "%", sep = ""),
    dat[,var] >= min + 3* interval & dat[,var] < min + 4 * interval ~ paste(min + 3 * interval, "%", sep = ""),
    dat[,var] >= min + 4 * interval ~ if(max == 100){paste(min, "%", sep = "")} else{paste(max, "% or more", sep = "")}
  )
  
  return(dat)
}



# Get cartogram dataframe: create dataframe for cartogram for contiguous 48 states

get_cartogramdat <- function(min, max){
  
  state_cartogram_dat <- counties %>%
    mutate(
      white_ct = total.pop * as.numeric(white) / 100,
      
      black_ct = total.pop * as.numeric(black) / 100,
      
      hispanic_ct = total.pop * as.numeric(hispanic) / 100,
      
      asian_ct = total.pop * as.numeric(asian) / 100
      
    ) %>% group_by(state) %>%
    summarise(
      state_white = (sum(white_ct, na.rm = TRUE) / sum(total.pop, na.rm = TRUE)) * 100,
      
      state_black = (sum(black_ct, na.rm = TRUE) / sum(total.pop, na.rm = TRUE)) * 100,
      
      state_hispanic = (sum(hispanic_ct, na.rm = TRUE) / sum(total.pop, na.rm = TRUE)) * 100,
      
      state_asian = (sum(asian_ct, na.rm = TRUE) / sum(total.pop, na.rm = TRUE)) * 100
      
    ) %>%
    ## Capitalize the first letter of each word in state names
    mutate(
      state = sapply(state, simpleCap)
      
    )
  
  ## Remove DC 
  state_cartogram_dat <- state_cartogram_dat[!(state_cartogram_dat$state == "District Of Columbia"), ]
  
  return(state_cartogram_dat)
}












# Merge counties data with map data for a given state

get_county_geodat <- function(state_input){

  
  ## Filter out the state of interest
  county_dat <- counties %>% 
    filter(state == tolower(state_input))
  
  
  ## Obtain map data of the state of interest
  county_map <- maps::map(
    database = "county",
    region = tolower(state_input),
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


# Get usa geo data
get_usageodat <- function(){
  
  
  
  ## Obtain map data of the state of interest
  county_map <- maps::map(
    database = "county",
    plot = FALSE,
    fill = TRUE
  ) %>%
    st_as_sf() %>%
    mutate(county = str_remove(ID, ".*,"))
  
  
  
  # Merge county_dat and county_map for the state of interest
  usa_geodat <- left_join(counties, county_map, by = "county") %>%
    # Select columns that will be used
    select(state, county, total.pop, white, black, hispanic, asian, geom)
  
  
  return(usa_geodat)
}


# Get outline data for a given state
get_state_outline <- function(state_input){
  
  states_outline <- maps::map(
    database = "state",
    plot = FALSE,
    fill = TRUE
  ) %>%
    st_as_sf() 
  
  state_outline <- subset(states_outline, ID == tolower(state_input))
  return(state_outline)
}


# Get legend labels for a given set of min and max
get_keylabs <- function(min, max){
  
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
  
  
  return(key_labs)
  
}


# Get shades for a given color

get_shadecolor <- function(color){
  # Generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  return(shades)
}

# Get fill color for a given variable, a color and a set of min and max
get_fillcolor <- function(color, var_fill, min, max){
 
  # Get shade colors
  shades <- get_shadecolor(color)
  
  # constrain gradient to percents that occur between min and max
  var_fill <- pmax(var_fill, min)
  var_fill <- pmin(var_fill, max)
  percents <- as.integer(cut(var_fill, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  
  fills <- shades[percents]
  
  
  return(fills)
}




# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does 
# not exactly match the order in which the maps package plots counties
percent_map <- function(state_input, var, color, legend_title, min = 0, max = 100) {
      
     if(grepl(",", state_input, fixed = TRUE)){
       plot_dat <- get_usageodat()
     }
   
    else{
      # Load data
      plot_dat <- get_county_geodat(state_input)
      
      # Load state outline
      state_outline <- get_state_outline(state_input)
      
    }
  
     
      
      var_fill <- case_when(
        var == "white" ~ plot_dat$white,
        var == "black" ~ plot_dat$black,
        var == "hispanic" ~ plot_dat$hispanic,
        var == "asian" ~ plot_dat$asian
      )
      
      
      # Get legend key labs
      key_labs <- get_keylabs(min, max)
      
      # Get shades
      shades <- get_shadecolor(color)
      
      fills <- get_fillcolor(color, var_fill, min, max)
      
      var_fill <- pmax(var_fill, min)
      var_fill <- pmin(var_fill, max)
      
    
      ggplot(plot_dat) + 
        
        geom_sf(aes(fill = var_fill,
                    geometry = geom),
                color = NA,
                size = 0.5) + 
        
       # geom_sf(aes(geometry = geom), 
             #   fill = "transparent",
              #  color = "gray90",
              #  size = 1,
              #  data = . %>% group_by(state) %>% summarise()) +
        
       
        
        scale_fill_gradientn(name = legend_title,
                             limits = c(min, max),
                             breaks = seq(min, max, length.out = 5),
                             labels = key_labs,
                             colors = shades[c(1, 25, 50, 75, 100)],
                             na.value = "#7F7F7F") + 
        
        ggtitle(state_input)+
        
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




# Cartogram Heatmap for Contiguous 48 States
state_cartogram <- function(var, color, legend_title, min, max){
  
  # Get data for plotting
  state_cartogram_dat <- get_cartogramdat(min, max)
  
  var_fill <- case_when(
    var == "white" ~ "state_white",
    var == "black" ~ "state_black",
    var == "hispanic" ~ "state_hispanic",
    var == "asian" ~ "state_asian"
  )
  
  # Add category to the data

  state_cartogram_dat <- add_categ(var_fill, state_cartogram_dat, min, max)
  
  
  # Get shade colors
  shades <- get_shadecolor(color)
  
  # Get legend key labels
  key_labs <- get_keylabs(min, max)
  
  
  # Calculate interval
  
  interval <- (max - min) / 4
  
  shade_seq <- c(min + 1, min + interval, min + 2 * interval, min + 3 * interval, max)
  
  
  
  
  ggplot(state_cartogram_dat, aes(state = state, 
                       fill = categ))  + 
 
    scale_fill_manual(name = legend_title,
                      values = shades[c(1, 25, 50, 75, 100)],
                      limits = key_labs,
                      breaks = key_labs,
                      labels = key_labs,
                      guide =guide_legend(reverse = TRUE)) + 
    
    
    geom_statebins() +
    
    theme_statebins() + 
    
    theme(
      ### Legend ###
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(size = 25),
      legend.key.height = unit(1.5, "cm"),
      legend.text = element_text(size = 18)
    )
    
}
