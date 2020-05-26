library(tidyverse)
# Extract states from text file ----
strings <- readChar("data/states.txt", file.info("data/states.txt")$size)%>%
  str_split_fixed("</div>", n = Inf) 


states <- c("Alabama")

for(i in 2:length(strings)){
  states[i] <- sub(".*>", "", strings[i])
}


# Remove last two empty strings
states <- head(states, -2)
