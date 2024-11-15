# Header ------------------------------------------------------------------
# Assignment name: Assignment 8 -- key
# Author name: Brad Broyles
# Date: 10/25/24
# Notes: 

# File setup --------------------------------------------------------------

# set working directory below
setwd('~/../DS4Bio_labs/')

# load libraries below
library(tidyverse)


# Exercise 2 --------------------------------------------------------------

# q2.1 for each herd, show histogram of week in which snow melts

# read in data
snow = read_tsv('tidyDat/tidyDat/FauchaldEtAl2017/snow.csv')

ggplot(snow, aes(Week_snowmelt))+
  geom_histogram()+
  facet_wrap(~Herd)

# q2.2
# Produce a scatter plot showing the Perc_snowcover vs. Week_snowmelt.
# Facet using Herd, and add a smoothing line to each panel.

# plot
ggplot(snow, aes(Perc_snowcover, Week_snowmelt))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Herd)+
  theme_bw()

# q2.3
# A geometry we didn't explore is geom_tile, which can be used to produce heat maps. 
# Explore its documentation" and draw a heat map having Year as the x-axis,
# Herd as the y-axis, and Week_snowmelt as the color of each cell.

# plot
ggplot(snow, aes(Year, Herd, fill = Week_snowmelt))+
  geom_tile()


# Exercise 3 --------------------------------------------------------------

# q3.1
bolstad <- read_csv("ggplot_ExerciseIII/lab15/Bolstad2015_data.csv")

# make a summarized version of the data
bolstad_sum <- bolstad %>% 
  group_by(Species, Sex) %>% 
  summarise(avg.WS = mean(WingSize),
            avg.L2L = mean(L2Length))

# you can plot multiple data types in a ggplot, I will use the sum data for geom_point and the full data for geom_smooth
# notice how the order here matters (points are laid on top of lines when it comes after in order)
ggplot() +
  geom_smooth(data = bolstad, aes(x = WingSize, y = L2Length, group = Species), method = "lm", se = FALSE) +
  geom_point(data = bolstad_sum, aes(x = avg.WS, y = avg.L2L)) +
  facet_wrap(~Sex) +
  labs(x = "Wing size (In mm)", y =  "Vein L2 lenght (In mm)") +
  theme_bw()


# **** Another way to do this is use groupby() %>% mutate() ****
bolstad_sum <- bolstad %>% 
  group_by(Species, Sex) %>% 
  mutate(avg.WS = mean(WingSize),
         avg.L2L = mean(L2Length))

ggplot(bolstad_sum) +
  geom_smooth(aes(x = WingSize, y = L2Length, group = Species), method = "lm", se = FALSE, size = .75) +
  geom_point(aes(x = avg.WS, y = avg.L2L)) +
  facet_wrap(~Sex) +
  labs(x = "Wing size (In mm)", y =  "Vein L2 lenght (In mm)") +
  theme_bw()


# ------------- #

# q3.2
urban <- read_tsv("ggplot_ExerciseIII/lab15/Urban2015_data.csv")

## need to make our own bins to recreate this plot
urban_tally <- urban %>%  
  group_by(Author, Year) %>% 
  summarise(avg.Percent =  mean(Percent)) %>% 
  mutate(cut = cut(avg.Percent, 
                   breaks = c( -1, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 1), 
                   labels= c("0-0", "0-0.05", "0.05-0.1", "0.1-0.15", "0.15-0.2", "0.2-0.25",
                             "0.25-0.3", "0.3-0.35", "0.35-0.4", "0.4-0.45", "0.45-0.5", "0.5-1")))

### this is very close, but maybe not exact, hard to know without seeing author's exact binning
ggplot(data = urban_tally, aes(x= cut)) +
  geom_bar() +
  labs(x = "Proportion species at risk", y = "Number of studies") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # this rotates axis labels


# Answer Key Assignment 3 - Dataframes

# Exercise 3 (4.7.1) ----

# 4.7.1.1 - what is average height of cherry tree?
data(trees)         # trees dataset is built into R
head(trees)         # use head to peak into dataset
mean(trees$Height)
mean(trees$Height, na.rm = T)   # average height is 76 and this
                                # na.rm is useful to clear out any
                                # NA's that might be in the dataset


# 4.7.1.2 - average girth for trees taller than 75 ft
tall_tree = trees[trees$Height > 75,]     # this filters the data
mean(tall_tree$Girth, na.rm = T)          # average is 14.511 


# 4.7.1.3 - max tree height of these with volume between 15 and 35
tree2 = trees[trees$Volume > 15 & trees$Volume < 35,]  # we can combine logical statements with &
heights = trees[trees$Volume > 15 & trees$Volume < 35,'Height'] # we can also go ahead and grab Height while filtering

max(tree2$Height)   # max height is 86
max(heights)        # max height is 86


# 4.7.1.4 - Read Gesquiere data, max GC and mean T for male ID == 3
ges = read.table('lab1_unix/data/Gesquiere2011_data.csv', header = TRUE)   # we see file was actually tsv

m3 = ges[ges$maleID==3,]
max(m3$GC)   # 136.31 is max GC
mean(m3$T)   # 149.04 is mean T

# 4.7.1.5 - how many GC > 50 & T > 100
ges2 = ges[ges$GC > 50 & ges$T > 100,]
nrow(ges2)    # looks like 1117 rows fit this criteria

nrow(m3[m3$GC > 50 & m3$T > 100,])  # only 38 when male ID == 3
# homework 4 - answer key

# exercise 1 ----------------------------------------

# 5.3.1 - saved as a different script

# in terminal run this command "Rscript check_z.R 131"

# I will just paste what that other script contained here:
args = commandArgs(trailingOnly = T)
z = as.numeric(args[1])

# condition 1
if(z > 100){
  print(z^3)
} 

# condition 2
if(z %% 17 == 0){
  print(sqrt(z))
}

# condition 3
if(z < 100){
  print(1:z)
} else {
  print(z:1)
}


# exercise 2 -----------------------------------

# 5.4.1

# q1 what does this code do?
# for values between 1 and 1000 with step of 3 (1, 4, 7, 10, 13 ...),
# print the values that are divisible by 4
z <- seq(1, 1000, by = 3)
for (k in z) {
  if (k %% 4 == 0) {
    print(k)
  }
}


# q2 what does this code do?
# this code checks if a number is prime
#     check if any integer from 2 to (z-1) is a factor of z
#     if any factor is found, exit the loop
#     if no factor is found print z

z <- readline(prompt = "Enter a number: ")
z <- as.numeric(z)
isthisspecial <- TRUE
i <- 2
while (i < z) {
  if (z %% i == 0) {
    isthisspecial <- FALSE
    break
  }
  i <- i + 1
}
if (isthisspecial == TRUE) {
  print(z)
}



# exercise 3 -----------------------------------------

#q1 finish these functions
tell_fortune <- function(){
  # you can use runif(1) 
  if (runif(1) < 0.3) {
    print("Yes")
  } else {
    print("No")
  }
}

replicate(10, tell_fortune())

# q2 finish this function
order_three <- function(x, y, z){
  return(sort(c(x, y, z)))
}

order_three(19, 12, 95)


# q3 finish this function
order_three <- function(x, y, z){
  my_ord <- sort( c(x, y, z) )
  return(list("first" = my_ord[1],
              "second" = my_ord[2],
              "third" = my_ord[3]
  ))
}

order_three(19, 12, 95)


# q4 - load trees add random (1,2) to Exp column
data("trees")

# make it a function
random_value_col = function(df){
  # function takes a data.frame as input
  # and adds a random 1 or 2 columng
  
  df$Exp = sample(1:2, nrow(df), replace = T)
  
  return(df)
}

trees = random_value_col(trees)

# q5 -- call this function 100 times - run a t-test on volume between the groups
p_values = c()
for (i in 1:100) {
  # create new samples
  trees = random_value_col(trees)
  
  # extract volumes
  set1 = trees$Volume[trees$Exp == 1]
  set2 = trees$Volume[trees$Exp == 2]
  
  # run t.test
  res = t.test(set1, set2)
  
  # store results
  p_values = c(p_values, res$p.value)
}

length(which(p_values < 0.05)) / length(p_values)

# another way to run t-test
p_values = c()
for (i in 1:100) {
  # create new samples
  trees = random_value_col(trees)
  
  # run t.test
  res = t.test(Volume ~ Exp, data = trees)
  
  # store results
  p_values = c(p_values, res$p.value)
}

length(which(p_values < 0.05)) / length(p_values)





# Quiz 4: Group challenge
# Practice writing fuctions, conditional branching and loops
# Practice generating random numbers or sampling numbers

{
# Question 1: Generate allele frequency change under selection pressure
# Write a function that simulates allele frequency changes under selection pressure. 
# There are two alleles of the gene: B and b, 
# with starting frequency p_B defined by the user, thus p_b = 1- p_B
# fitness of B is fit_B, and fitness of b is fit_b
# in the next generation, the frequency of B is 
# its relative fitness over average fitness times its current frequency
# p_B (next generation) = fit_B * p_B / (fit_B * p_B + fit_b * p_b)
# The initial frequencies are considered Generation 0
# Given user defined generations,
# output a dataframe, first column is the generation number, second and third column 
# is the frequency of allele B and b at that generation
} 

simulate_selection <- function(p_B, fit_B, fit_b, total_generations){
  # stop the execution if any input is not numeric:
  stopifnot(is.numeric(p_B),is.numeric(fit_B),is.numeric(fit_b),is.numeric(total_generations))
  # stop if p_B is not between 0 and 1
  stopifnot(p_B<1, p_B>0)
  # stop if fit_B and fit_b are not positive
  stopifnot(fit_B>0, fit_b>0)
  # initialize a data frame for output, dimension: nrow = total_generations+1, ncol = 3
  # generation records the generation number, p_B_t records frequencies of allele B at the current generation
  # p_b_t records frequencies of allele B at the current generation
  out_df <- data.frame(generation = 0:total_generations,p_B_t = p_B,p_b_t = 1-p_B)
  
  # start the loop from generation 1, which will be the second row of the output
  for (i in 2:nrow(out_df)){
    # get previous generation proportions
    B = out_df$p_B_t[i-1]
    b = out_df$p_b_t[i-1]
    
    # calculate new B and b proportions
    new_B = fit_B * B / (fit_B * B + fit_b * b)
    new_b = 1 - new_B
    
    # save to out_df
    out_df[i, 2:3] = c(new_B, new_b)
  }
  
  return(out_df)
}

simulate_selection(0.3, 1.1, 0.9, 10)

# values 0.5, 1, 1.1, 10 (0.278 - 0.722)
# values 0.8, 1, 1.01, 100 (0.597 - 0.403)
# values 0.3, 1.1, 0.9, 10 (0.761 - 0.239)


{
# Question 2: modify the function in Question 1 to allow for random changes in each generation
# Assume we start with a total gene copy of user-defined N
# There are two alleles of the gene: B and b, 
# with starting frequency p_B defined by the user, thus p_b = 1- p_B
# fitness of B is fit_B, and fitness of b is fit_b
# There are N_B = N*p_B gene copies of B at generation g
# In the next generation, the possible gene copies of B is 
# sampled from the fitness advantage of B
# N_B  = sum(rbinom(n=N,1,prob = fit_B * p_B / (fit_B * p_B + fit_b * p_b)))
# N_b= 1 - N_B
# Given user defined generations and total gene copies,
# output a dataframe, first column is the generation number, second and third column 
# is the copy numbers of allele B and b at that generation
} 

stochastic_simulate_selection <- function(p_B, fit_B, fit_b, total_generations,total_gene_copies){
  # stop the execution if any input is not numeric:
  stopifnot(is.numeric(p_B),is.numeric(fit_B),is.numeric(fit_b),is.numeric(total_generations), is.numeric(total_gene_copies))
  # stop if p_B is not between 0 and 1
  stopifnot(p_B<1, p_B>0)
  # stop if fit_B and fit_b are not positive
  stopifnot(fit_B>0, fit_b>0, total_gene_copies > 0)
  
  # set N to total_gene_copies number
  N = total_gene_copies
  
  # initialize a data frame for output, dimension: nrow = total_generations+1, ncol = 3
  # generation records the generation number, p_B_t records frequencies of allele B at the current generation
  # p_b_t records frequencies of allele B at the current generation
  out_df <- data.frame(generation = 0:total_generations,
                       n_B_t = p_B * N,
                       n_b_t = (1-p_B) * N)  # this time columns are tracking number instead of proportion
  
  # start the loop from generation 1, which will be the second row of the output
  for (i in 2:nrow(out_df)){
    # get proportions from previous generation
    B = out_df$n_B_t[i-1] / N
    b = out_df$n_b_t[i-1] / N
    
    # calculate new B and b counts
    new_B  = sum(rbinom(n=N, 1, prob = fit_B * B / (fit_B * B + fit_b * b)))
    new_b = N - new_B
    
    # assign to out_df
    out_df[i, 2:3] = c(new_B, new_b)
  }
  
  return(out_df)
}

stochastic_simulate_selection(0.3, 1.1, 0.9, 10, 10000)

# values 0.5, 1, 1.1, 10, 1000 
# (253 - 747), (293 - 707), (291 - 709)

# values 0.8, 1, 1.01, 100, 100
# (100 - 0), (100 - 0), (43 - 57)

# values 0.3, 1.1, 0.9, 10, 10000
# (7407 - 2593), (7509 - 2491), (7607 - 2393)

# Chapter 7 exercises + Relational Databases

# Brad Broyles 10/6/24

# SET UP
library(tidyverse)
setwd('~/../DS4Bio_labs/tidyDat/tidyDat/FauchaldEtAl2017/')


# Exercise 1 ----

# q1: read in pop_size extract unique years in ascending order
popsize = read_tsv('pop_size.csv')

y = popsize %>% 
  select(Year) %>% 
  arrange(Year) %>% 
  distinct()
  

y 
# start with 1970, 71, 72 ... and has 44 entries


# q2: read in ndvi as tibble
#     extract row with largest NDVI_may
#     list three years with largest popsize for WAH herd (2 ways)
ndvi = read_tsv('ndvi.csv')

nm = ndvi %>% 
  arrange(desc(NDVI_May)) %>% 
  dplyr::slice(1)

nm  # WAH has largest NDVI_May

wa = popsize %>% 
  filter(Herd == 'WAH') %>% 
  top_n(3, Pop_Size) %>% 
  select(Year)

wa # 1993, 96, 03

wa2 = popsize %>% 
  filter(Herd == 'WAH') %>% 
  arrange(desc(Pop_Size)) %>% 
  dplyr::slice(1:3) %>% 
  select(Year)

wa2 # 03, 93, 96

# Exercise 2 ----

# q1: avg popsize per herd
popsize %>% group_by(Herd) %>% 
  summarise(avg_size = mean(Pop_Size))

# q2: top sd for NDVI_May per herd
ndvi %>% group_by(Herd) %>% 
  summarise(sd_may = sd(NDVI_May)) %>% 
  top_n(1, sd_may)

# TCH has largest sd of NDVI_May

# q3: relative popsize
relpop = popsize %>% group_by(Herd) %>% 
  mutate(
    rel_pop = Pop_Size / mean(Pop_Size)
    )

head(relpop)

# for this one it is also full points if you find Relative pop size to the mean of the full dataset
relpop = popsize %>% mutate(rel_pop = Pop_Size / mean(Pop_Size))
head(relpop)


########### RELATIONAL DATABASE ############

# THE FOLLOWING IS THE QUESTION #
# Assume you are collecting insects at three different locations, three
# times a year, over the span of three years. For each day/location of
# sampling, you record

  # Site data: site of the sampling, its geographic
  # coordinates, description of the site

  # Sampling data: day, month, and year, weather
  # conditions, temperature, humidity, etc.;

  # Species data: classification according to species
  # and stage of development of all the insects
  # collected, their total, and their measurements

# Write out the plan of your design of all the tables you are
# making, and how you are going to relate them together



# Here is my plan. Four tables (site_data, sample_data, collection_data, taxon_data)
#
# For site_data: 
#     Primary_key - Site_ID (unique id for each site)
#       Additional columns: Latitude, Longitude, Description
#
# For sample_data:
#     Primary_key - Sample_ID (unique id for each sampling day)
#     Foreign_key - Site_ID  
#       Additional columns: Day, Month, Year, Weather_conditions, Temperature, Humidity
#
# For Collection_data:
#     Primary_key - Collection_ID (Unique ID for each insect collected [insect_1, insect_2 ...])
#     Foreign_key - Sample_ID
#     Foreign_key - Taxon_ID
#       Additional columns: Developmental_stage, Measurements
#       *** Total for each developmental stage can be calculated by summarizing counts of taxon_id and developmental stage ***
#
# For Taxon_data:
#     Primary_key - Taxon_ID
#       Additional columns: Genus, Species, Common_name
#
# Brad Broyles
# Answer Key -- Homework 6 (ch7 exercise 3)

# set up ---
library(tidyverse)

setwd('~/../DS4Bio_labs/')

# q1 ----
#Compute the average sea-ice cover for each month and herd, 
#by averaging over the years. (hint: pivoting longer first) 
# [requirement: fulfill the task with one command using the pipe %>%]

avg_sea_ice = read_tsv('tidyDat/tidyDat/FauchaldEtAl2017/sea_ice.csv') %>%
  pivot_longer(cols = -c(1:2)) %>%  
  group_by(Herd, name) %>% 
  summarise(avg_cover = mean(value))

head(avg_sea_ice)



# q2 ----
#convert table2 and table4a into tidy data format 
# [requirement: fulfill each task with one command using the pipe %>%]

table2  # we need to split this case column
table2 %>% pivot_wider(names_from = type, values_from = count)

table4a # we need to gather these years into one column
table4a %>% pivot_longer(cols = -1, names_to = 'Year', values_to = 'Count')



# q3 ----
# Produce a tibble (avg_Perc_seaicecover) containing the average population 
# size and the average seaice cover for each combination of Herd and Year.

pop = read_tsv('tidyDat/tidyDat/FauchaldEtAl2017/pop_size.csv')
si = read_tsv('tidyDat/tidyDat/FauchaldEtAl2017/sea_ice.csv')

# pop already has pop per herd and year (don't need to average)
# work on si to get avg per herd and year
avg_Perc_seaicecover = si %>%
  pivot_longer(cols = -c(1:2)) %>% 
  group_by(Herd, Year) %>% 
  summarise(avg_cover = mean(value)) %>% 
  inner_join(pop, by = c('Herd', 'Year'))

head(avg_Perc_seaicecover)



# q4 ----
#redo Chapter 5, 5.12.1, question 2, using the function separate and summarise

gold = read_csv('~/../Downloads/R_exerciseV/R_exerciseV/Q1/Goldberg2010_data.csv')

# use separate and summarize - how many species in each status per genus
df = gold %>% 
  separate(Species, into = c('Genus', "Species"), sep = '_') %>% 
  group_by(Genus, Status) %>% 
  summarise(sp_count = n())

head(df)



# q5 ----
# Take a look at the documentation of dplyr::join and identify the command 
# that provides a tibble with the 17 rows that are present in popsize but not 
# in avg_Perc_seaicecover (i.e., that do not have data for seaice cover)

?dplyr::join # go to See Also, then filter-joins

anti_join(pop, si)



# q6 ----
# For each Herd, compute Kendall’s rank correlation (cor(x,y,method = "kendall")) 
# between the percentage of seaice cover in March and the week in which the ground 
# snow melted (contained in snow.csv) for each of the years.

sn = read_tsv('tidyDat/tidyDat/FauchaldEtAl2017/snow.csv')

# look at seaice in march and week snow melted
df = inner_join(sn, si %>% select(Herd, Year, Mar))

# looks like some NA's get in the way lets remove them
df = df %>% filter(!is.na(Week_snowmelt), !is.na(Mar))

df %>% group_by(Herd) %>% 
  summarise(cor = cor(Mar, Week_snowmelt, method = 'kendall'))
# assignment 7 hmwk key

# exercise 1 -- 

setwd('~/../DS4Bio_labs/tidyDat/tidyDat/FauchaldEtAl2017/')

# q1 ----
snow = read_tsv('snow.csv')

# box plot is a decent geom
ggplot(snow, aes(Week_snowmelt, Perc_snowcover, group = Week_snowmelt))+
  geom_boxplot()
  

# q2 ----
# avg cover and week snow melted, per year
df = snow %>% group_by(Year) %>% 
  summarise(avg_cover = mean(Perc_snowcover, na.rm = T),
            avg_week = mean(Week_snowmelt, na.rm  = T))

ggplot(df, aes(avg_cover, avg_week))+
  geom_point()


# q3 ----
# boxplot popsize for each herd
popsize = read_tsv('pop_size.csv')

ggplot(popsize, aes(Herd, Pop_Size, group = Herd))+
  geom_boxplot()


# q4 ----
# scatter plot
df = popsize %>% filter(Year >= 2008, Year <= 2014)


df2 = df %>% group_by(Year) %>% 
  summarise(avg = mean(Pop_Size, na.rm = T),
            sd = sd(Pop_Size, na.rm = T),
            c = n())

ggplot(df2, aes(Year, avg))+
  geom_point()+
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd))



























# assignment 9 key

# load packages
library(tidyverse)
library(modelr)


# Chapter 12 exercise 1 ----

# q1 -
# repeat model fitting, grid generation, 
# predictions and visualization on sim1 using loess
grid = sim1 %>% data_grid(x)

# train model
sim1_mod = loess(y ~ x, sim1)

grid = grid %>% add_predictions(sim1_mod)

ggplot(sim1) +
  geom_point(aes(x = x, y = y)) +
  geom_line(aes(x=x, y = pred), data = grid, colour = "red", size = 1)

# how does this result compare to geom smooth 
ggplot(sim1) +
  geom_point(aes(x = x, y = y)) +
  geom_line(aes(x=x, y = pred), data = grid, colour = "red", size = 1)+
  geom_smooth(aes(x = x, y = y), linetype = 'dashed', se = T)


# ** model fit with loess generates same curve as geom_smooth
# this is because geom_smooth uses loess by default

#q2 - 
# what does geom_ref_line() do. Why is it helpful?

# geom_ref_line() adds a reference line to a plot. 
# it will help vizualize where model is over/under predicting target values
# if there is a pattern in the residuals, it may indicate a problem with the model

# chapter 12 exercise 2 ----

# q1 - Evaluate the performance of mod1 and mod2 for the dataset sim4 above. 
# Which model is better? Demonstrate with clear plots and statistical tests.

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)

grid

# plot
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~model)

# ** From the above plot we can see the influence of interacting terms

# statistical test
summary(mod1)$r.squared  # 0.51
summary(mod2)$r.squared  # 0.52 -- mod2 is slightly better

AIC(mod1) # 1304
AIC(mod2) # 1296 -- mod2 is slightly better

BIC(mod1) # 1319
BIC(mod2) # 1315 -- mod2 is slightly better

# All three agree mod2 is better (slightly)

# q2 - use MASS:rlm() with same variables
v1 = MASS::rlm(y ~ x1 + x2, data = sim4)  # i am making a version of both models
v2 = MASS::rlm(y ~ x1 * x2, data = sim4)

# gather predictios for mod1, mod2, v1, v2
df = sim4 %>% gather_residuals(mod1, mod2, v1, v2) %>% gather_predictions(mod1, mod2, v1, v2)

# plot residuals
ggplot(df, aes(resid, fill = model))+
  geom_histogram(position = 'dodge', alpha = 0.5)

# try boxplot
ggplot(df, aes(resid, model, group = model))+
  geom_boxplot()

summary(mod1) # rse 2.11 on 297
summary(v1)   # rse 2.109 on 297 -- maybe v1 is better than mod1

summary(mod2) # rse 2.078 on 296
summary(v2)   # rse 2.05 on 296 -- maybe v2 is better than mod2

AIC(mod1) # 1304.5 -- lookd like mod1 is ever so slighlty better
AIC(v1)   # 1304.7 

AIC(mod2) # 1296.04 -- looks like mod2 is better
AIC(v2)   # 1296.38

BIC(mod1) # 1319.28 -- looks like mod1 is better
BIC(v1)   # 1319.50

BIC(mod2) # 1314.56 -- looks like mod2 is better
BIC(v2)   # 1314.90

# ** rlm() is robust to outliers, but in this case it did not make a difference
# ** mod2 remains the best model though there is not much difference# Header ------------------------------------------------------------------
# Assignment name: Homework 1 -- Chapter 4
# Author name: Brad Broyles
# Date: 8/30/24
# Notes: Hopefully this is an answer key 

# File setup --------------------------------------------------------------

# set working directory below
# setwd('~/ds4-bio-fall/')   #Don't need to set anything this week

# load libraries below
# library(dplyr)             #DOn't need to load anything this week either

# Exercise 1 --------------------------------------------------------------

### Question 1 ~ create even number vector
z = seq(2,100,2)          # assign with =    -- or --
z <- seq(2, 100, 2)       # assign with <-
z

#I am just showing two ways of assigning data in R here 


### Question 2 ~ get elements of z divisble by 12
div12 = z[z %% 12 == 0]   # use modular division
length(div12)             # 8 elements


### Question 3 ~ sum of z?
sum(z)                    # sum is 2550


### Question 4 ~ is sum equal to 51*50
sum(z) == (51 * 50)       # these two are the same


### Question 5 ~ what is product of 5, 10, 15 element
z[5] * z[10] * z[15]      # product is 6000
prod(z[c(5,10,15)])       # another way to do this


### Question 6 ~ what does z^2 do
z^2                       # squares each element in the vector


### Question 7 ~ get 0-30 divisible by 3, what is overlap with z
y = seq(0, 30, 3)         # here is the vector
y[which(y %in% z)]        # also in z (6, 12, 18, 24, 30)


### Question 8 ~ does command1 produce same results as command2
seq(2, 100, 2) == (1:50) * 2    # here we see each element is TRUE

  # another way to do this
  a = seq(2, 100, 2)
  b = (1:50) * 2
  all(a == b)                   # this all() function checks if each element is TRUE
  

# Exercise 2 --------------------------------------------------------------

### Question 1 ~ create a matrix with 10 rows, 5 columns, and each column is 1:10
A = matrix(1:10, nrow = 10, ncol = 5)
A

### Question 2 ~ create array Z [5,5,2] so Z[,,1] is first 5 rows of A, and Z[,,2] is A rows 6-10 
Z = array(data = c(rep(1:5, 5), rep(6:10, 5)), dim = c(5,5,2))
Z

  # -- or -- #
  Z = array(NA, c(5,5,2))
  Z[,,1] = A[1:5,]
  Z[,,2] = A[6:10,]
  Z
  
### Question 3 ~ take character vector, remove "n" and make numeric
x = c("n30","n101","n140")
x = sub('n', '', x)
x = as.numeric(x)
x                               # x is now 30, 101, 140



# Helpful info ------------------------------------------------------------

# Using Ctrl+Shift+R (Cmd+Shift+R on the Mac) creates new sections which are an easy way to organize
# scripts. You can also use them to navigate around very large scripts whith the stacked line icon in
# the top right of the script window.


# Using Ctrl+Shift+C (Cmd+Shift+C on the Mac) creates multiple commented out lines (e.g., # ) and allows you
# to type longer text segments and then comment them out as a group.
