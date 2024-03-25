#2
install.packages("palmerpenguins")
install.packages('tidyverse')
install.packages('dplyr')
library(palmerpenguins)
library(tidyverse)
library(dplyr)

# Subset penguins dataframe to the five heaviest penguins
big_penguins <- penguins %>%
  filter(species == "Gentoo",!is.na(body_mass_g)) %>% 
  arrange(body_mass_g) %>% tail(n = 5L)
# only includes Gentoo species and removes NAs in the data of body mass then arranges them in order of increasing body mass 

# Add a column with names to big_penguins
big_penguins$names <- c("Dwayne", "Hulk", "Giant", "Gwendoline", "Usain")

# Plot all Gentoo penguins and use big_penguins dataframe for labels
penguins %>% filter(species == "Gentoo") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(colour = flipper_length_mm)) +
  geom_text(
    data = big_penguins,
    mapping = aes(label = names),
    nudge_x = -1.5,
    nudge_y = -0.5,
    colour = "red"
  ) +
  xlim(3900, 6400)
#this is where we plot the data - flipper length has been added to the y axis
#geom_text is used to add text to a plot and it also allows you to use specific colours for the labels

#Important details:

#1) For geom_text() we’re switching to different data, namely our big_penguins dataframe. Nevertheless, geom_text() inherits the position mappings from ggplot(). That’s how geom_text() knows where to put the labels.

#2) We use the nudge parameters to push the labels down and left a bit, so that they don’t sit right on top of the dots they are labeling.

#3) We’ve made the x-axis a bit longer with xlim(), so that the names don’t get cut off.


#To highlight the home islands of Adelie penguins with flipper lengths over 200mm, we can filter within the 'data' argument of geom_text:

penguins %>% filter(species == 'Adelie') %>% ggplot(aes(x = body_mass_g, y = flipper_length_mm)) + geom_point() + geom_text(data = filter(penguins, species == 'Adelie' ,flipper_length_mm > 200), aes(label = island), nudge_y = -0.7)

#the species has to be filtered again in geom_text, because otherwise it will include all the species of penguins that have a flipper length of >200 and add labels for them too 










