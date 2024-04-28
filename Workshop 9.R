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

#3 - Facets

malariamodel<- read.table('wmr_modelling.txt', sep = '\t', header = T)

df_short <- head(malariamodel, n = 506L)
#subsetted to 506 rows (slightly more than half of the original data)

df_short %>% drop_na() %>% filter(year>2018) %>% ggplot(aes(x = year, y = deaths)) + geom_col(fill = 'firebrick') + facet_wrap(~country, ncol = 5, dir = 'v')

#NAs were dropped and only data from 2019-2021 was included 
#facet_wrap created a graph for each country looking at the number of deaths in each year 
# ~ decides the variable by which we want to split our data , ncol allows us to choose the number of columns, dir controls the direction of the wrap - in this case vertical (horizontal is the default)


df_short %>% drop_na() %>% filter(year>2018) %>% ggplot(aes(x = year, y = deaths)) + geom_col(fill = 'firebrick') + facet_wrap(~reorder(country, deaths), ncol = 5, dir = 'v', scales = 'free_x')

# what does as.table do? - it is meant to move the separate graphs in terms of highest value but it didn't work so I used reorder
#scales = 'free' - for each graph, the y and x axis are 'freed' meaning their scales are all different depending on what their highest value is 

penguins %>% drop_na() %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) + geom_point() + facet_grid(sex ~ species)
#facet_grid separates 2 variables by 2 categorical values - in this case it is sex and species
#sex being the row headers and species being the column headers 

p_plot <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point()

p_plot + facet_grid(. ~ species)
#this way we can control how we want the plots separated by just one variable (species)

#4 - Patchwork
#allows us to combine several panels into a larger figure 
install.packages("patchwork")
library(patchwork)

#here we made some plots:
p1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() + facet_grid(. ~ species)

p2 <- penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity")

p3 <- penguins %>% drop_na() %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex))

#the first one is the biggest graph and the other 2 are smaller and at the bottom
p1/(p2+p3)

#using | will put p2 as the bigger graph on the left and the other 2 on the right 
p2 | (p1/p3) 

# %in% checks if a value is   
p_deaths <- malariamodel %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = deaths, colour = country)) +
  geom_point() +
  geom_line() +
  xlim(1999,2022)

p_pop <- malariamodel %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = population, fill = country)) + 
  geom_col(position = "dodge") +
  xlim(1999,2022)

malaria_plot <- p_deaths/p_pop

ggsave(filename = 'malaria_plot.png', plot = malaria_plot, width = 300, height = 200, units = 'mm')

#here we made 2 graphs on one page with p_death on top and p_pop at the bottom

#5 - Colours
#Discrete colour scales
s_counts <- penguins %>% ggplot(aes(x = species, fill = species)) +
  geom_bar()

s_counts + scale_fill_manual(values = c("yellow2", "magenta", "darkblue"))

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
#these are colours that have already been decided 
brew_1 <- s_counts + scale_fill_brewer(palette = "Set1")
brew_2 <- s_counts + scale_fill_brewer(palette = "Dark2", direction = -1)
#these colours arent great becaue the red and green will be hard for people with colour blindness to distinguish 
brew_1 + brew_2

viri_1 <- s_counts + scale_fill_viridis_d() #Uses default option viridis
viri_2 <- s_counts + scale_fill_viridis_d(option = "plasma")

viri_1 + viri_2

#Continuous colour scales 
con_plot_1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(size = body_mass_g, colour = body_mass_g))
#scale_colour_viridis_c()is the function for continuous virdis scales
#scale_colour_distiller is the function for ColourBrewer

con_plot_2 <- con_plot_1 + scale_colour_viridis_c(option = "magma")

con_plot_1 + con_plot_2

#NA values
#it is recommended that if you are displaying NA values in your graph, you should give it colour

#6 - Themes
#ggplots default theme is theme_grey(), which determines the overall look of your plot (grey)
#there are other themes available
con_plot_3 <- con_plot_1 + theme_classic()

con_plots <- con_plot_1 + con_plot_3 + plot_annotation(title = "Default theme on the left, theme_classic() on the right")

ggsave(filename = 'con_plots.png', plot = con_plots, width = 300, height = 300, units = 'mm')

?theme()
#this function has so many arguments, and shows how you can adjust every small detail 
?theme_grey
#you can see how these arguments are used to set the theme 

#the elements of a plot are divided into 3 types: lines, text and rectangles 
#there are 3 functions that are used to manipulate them: element_line(), element_text(), element_rect()
#to remove an element entirely, element_blank() is used 

#this is an ugly plot but just to get an idea of all the possibilities
ugly_plot <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(colour = body_mass_g)) +
  labs(title = "My ugly plot") +
  scale_colour_viridis_c(option = "magma") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(colour = "red", size = 14, hjust = 1),
        axis.title.y = element_blank(),
        axis.line.y = element_line(colour = "cornflowerblue", size = 4),
        axis.text.y = element_text(size = 20, angle = 45),
        panel.background = element_rect(colour = "green", fill = "yellow", size = 10),
        plot.title = element_text(family = "Times", face = "italic",  hjust = 0.5, size = 18))

ggsave(filename = 'Ugly_plot.png', plot = ugly_plot, width = 300, height = 300, units = 'mm')

#Q7 - 1
penguins %>% filter(island == 'Biscoe') %>% ggplot(aes(y = bill_length_mm, x = bill_depth_mm)) + geom_point(aes(colour = species))+ geom_text(data = filter(penguins, island == 'Biscoe' & (bill_length_mm > 54 | bill_depth_mm > 20)), aes(label = sex)) + labs(title = 'Penguins on the island Biscoe')

#Q7 - 2

cases_deaths <- read.table('wmr_cases_deaths_modelling_summaries.txt', sep = '\t', header = TRUE)

cases_deaths %>% ggplot(aes(y = 'deaths', x = 'year')) + geom_col(fill = 'blue') + facet_wrap(~region) + xlim(2019,2021)
