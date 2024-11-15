#' ---
#' Course: "Bio 495/695 Data Science for Biologists"
#' Title: "Midterm2-mock"
#' Student name:______________
#' PUID:_____________

library(tidyverse)
#' 
#' All the questions are based on one database from Jiang et al. (2013) that studied assortative mating in animals. 
#' They compiled a large database, reporting the results of many experiments on mating. 
#' These experiments measured that whether mating would be observed more
#' if the traits between male and female are more similar than randomly expected.
#' In particular, `r` represents mating correlation regarding different traits;
#' a positive value of `r` stands for assortative mating (e.g., large animals tend to mate with large animals), 
#' and a negative value for disassortative mating.
#' 
#' Let's first load the dataset of "Jiang2013_data.csv" under "data" folder:

assortMating <- read_csv('/Users/natal/Documents/Purdue/Data_science_biol/midterm2_mock/data/Jiang2013_data.csv')

head(assortMating)
View(assortMating)

#' 
#' - Explanation of each column in the dataset:
#' 
#' 1. ScientificName: The scientific name of the species
#' 2. Taxon: Taxonomic Groups
#' 3. Phylum: Which phylum does the species belong to
#' 4. AssortmentTrait: Specific traits measured
#' 5. TraitCategory: Broader category of the traits
#' 6. r: mating correlation
#' 7. SampleSize: how many pairs were observed in the experiment
#' 8. Sign_of_r: Whether the correlation is positive or negative; if p-value >0.05, Sign_of_r will be stored as "none"
#' 9. DiscreteTrait: Whether the traits are discrete or not
#' 10. Source: citation for the data source
#' 

## Mock question:-----

### A ----------
#' Create a subset of the data, which filters out the rows with "Sign_of_r" being "not available"
#' 
#' 

filtered_rows <- assortMating %>%
  filter(Sign_of_r == "not available")

# print the rows
print(filtered_rows)

# filter out the rows
filtered_Mating <- assortMating %>% 
  filter(Sign_of_r != "not available")
glimpse(filtered_Mating)



### B ----------
#' - Change the column "Sign_of_r" to factors, with the order of levels following c("negative", "none", "positive")

# convert "Sign_of_r" to factor with ordered levels
filtered_Mating <- filtered_Mating %>%
  mutate(Sign_of_r = factor(Sign_of_r, levels = c("negative", "none", "positive")))

glimpse(filtered_Mating)


### C -------------
#' 
#' - Plot a histogram of the strength of assortative mating (measured in the column "r") showing the number  of cases across the range of "r" with bins = 20
#' 
#' - Fill the bars with "sign_of_r"
#' 
#' -  change x-axis label to "Strength of Assortative Mating (r)"; y-axis label to Number of Cases
#' 
#' -  add a vertical line which shows the mean value of r across all samples.
#' 
#' -  See the example plot at examplePlots/mock.pdf


# plot histogram of the strength of assortative mating
ggplot(filtered_Mating, aes(x = r, fill = Sign_of_r)) +
  geom_histogram(bins = 18) +
  geom_vline(aes(xintercept = mean(r)), 
             color = "black", linetype = "dashed") + # add mean line
  labs(
    x = "Strength of Assortative Mating (r)", # x-axis label
    y = "Number of Cases", # y-axis label
    fill = "Sign of r" # legend title
  )
  #theme_minimal() # clean theme
