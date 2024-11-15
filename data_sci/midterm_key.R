# midterm practice test answer key 

# libraries
library(tidyverse)

# Smith et al. (2003) compiled a database of the body mass of mammals
# of the late Quaternary period. Your goal is to calculate the average body mass
# of the species in each family.
  
# Q1: Read the file `Smith2003_data.txt`, 
# note that the file doesn’t have column names; 
# column 7 contains the data on body mass (in log kilograms)
# The authos used -999 to mark missing data. Use NA instead.
# Add column names by carefully read Smith2003_about.txt.

# hint
#read.table(file, header = FALSE, sep=, na.strings = )

setwd('~/../Downloads/midterm_practice')
smith = read.table(file = 'Smith2003_data.txt', header = F, sep = '\t', na.strings = -999)

# add column names
names(smith) = c('Continent', 'Status', 'Order', 'Family', 
                    'Genus', 'Species', 'Log_mass', 
                    'Combined_mass', 'Reference')

# lets turn to tibble (dont have to do this, but i like to)
smith = as_tibble(smith)

# Q2: Average body mass
# a. Calculate the average body mass per order 
# (using the combined mass column instead of the log mass)

# hint: group_by, summarise, mean
avg_bm = smith %>% 
  group_by(Order) %>% 
  summarise(mean_mass = mean(Combined_mass, na.rm = T)) %>% 
  arrange(desc(mean_mass))


# b. Plot the mean body mass per mammal order in a decreasing order
# When plotting the mean body mass values, take the log scale 
# Produce a figure similar to "bodyMass_perOrder.pdf"
# Hint: remember na.rm=T when calculating mean or median values

# can set order as a factor her, or use fct_reorder in ggplot
avg_bm$Order = factor(avg_bm$Order, levels = avg_bm$Order)

# can take log of mean_mass here, or take log() in ggplot
avg_bm$log = log(avg_bm$mean_mass)

ggplot(avg_bm, aes(Order, log))+geom_col()+
  # rotate x to 90 degrees
  theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))+
  ##... change axis labels
  xlab('Mammal Orders') + ylab('Body mass (Log[Kg])')

# Hint: for rotating the labels on x-axis, add the following line
# theme(axis.text.x = element_text(angle = 45,vjust=1,hjust=1))

# c. What are the top three and botoom three orders of body mass?
# What are the common names? Does the pattern make sense to you?

# since i ordered the dataset i can just use head and tail
head(avg_bm, 3)

# top body mass: Cetacea, Proboscidea, Notoungulata

tail(avg_bm, 3)

# bottom body mass: Chiroptera, Paucituberculata, Microbiotheria

# does it make sense - yes it does
# - cetacea are large aquatic mammals,
# - proboscidea are large terrestrial mammals
# - notoungulata are extinct herbivores
# --------------------------------------------- #
# - chiroptera are bats, small
# - paucituberculata are shrew opossums, small
# - microbiotheria are marsupials, small

# Q3. Extinction patterns of mammals in relation to their body mass.

# a. Each record of species is associated with a status 
# extant: the species is currently present in the wild; 
# extinct: extinct as of late Pleistocene
# historical: extinct within the last 300 years; 
# introduction: an introduction to the continent;
# convert the column of Status to factor,
# with an ordered levels of extinct, historical, extant, introduction
# and calculate the median log body mass per status

smith$Status = factor(smith$Status, 
                      levels = c('extinct', 'historical', 
                                 'extant', 'introduction'))

median_bm = smith %>% 
  group_by(Status) %>% 
  summarise(median_mass = median(Log_mass, na.rm = T)) %>% 
  arrange(median_mass)

# b. Plot the histogram of log body mass per status
# (remove records of introductions)
# Add the vertical red lines to indicate the median values 
# produce the figure similar to `bodyMass_extinctionStatus.pdf`
# hint: geom_vline(aes(xintercept=))
# hint: when using facet_grid, we can change whether y axis scale
# is fixed or not, scales="free_y" for each subgraph to have its own
# scale of bar length

# remove introduct
df = smith %>% filter(Status != 'introduction')

# just doing this again without introduction
median_bm = df %>% 
  group_by(Status) %>% 
  summarise(median_mass = median(Log_mass, na.rm = T)) %>% 
  arrange(median_mass)

ggplot(df, aes(Log_mass))+
  geom_histogram()+
  # add vertical lines based on median_bm
  geom_vline(data = median_bm, aes(xintercept = median_mass),color = 'red')+
  facet_grid(rows = vars(Status), scales = 'free_y')+
  theme_bw()+
  xlab('Body Mass (Log[Kg])')+ 
  ylab('number of species')+
  scale_x_continuous(breaks = c(0, 2.5, 5.0, 7.5))
  # don't worry about scaling y axis
  # don't worry about bins numbers either

# From the pattern of body mass distribution per status, 
# what can you conclude regarding the extinction patterns of mammals
# in Pleistocene and Anthropocene?

# current species are smaller than extinct species
# anthropocene aka historical species are smaller
# pleistocene aka extinct are the largest

# Q4: Which genus, on average, 
# is found across the largest number of continents?

# first find continents per species
hold = smith %>% 
  group_by(Genus, Species) %>% 
  summarise(n_continent = n_distinct(Continent)) %>% 
  arrange(desc(n_continent))

# now find average for genus
hold2 = hold %>% group_by(Genus) %>% 
  summarise(mean_continent = mean(n_continent)) %>% 
  arrange(desc(mean_continent))

# -- finding a lot of genus that are found on 3
hold2 %>% filter(mean_continent == 3) %>% 
  select(Genus) %>% unlist() %>% unname() %>% 
  paste(., collapse = ', ')

# here is the list #
# Ametrida, Centurio, Cuon, Desmodus, Diaemus,
# Furipterus, Mesophylla, Oryctolagus, Rhynchonycteris, 
# Trachops, Vampyrodes, Vampyrum
