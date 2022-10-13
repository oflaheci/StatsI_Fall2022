#################################
# Tutorial 5: Data Manipulation #working with dplyr in practical way
#################################

## Packages
library(tidyverse) # Load our packages here
library(broom) # If not installed - function for installing?
# used for functional models - tidies lists into rectangular objects 
# considered bad form to include install.packages() if distributing
# can change the user's system
# install.packages("broom")

?tidyverse
browseVignettes(package = "tidyverse") # manifesto etc

## Assign data
# The tidyverse has its own package for reading in data: readr
?readr

# readr uses a very similar format to base r functions. For example, 
# we can read in a csv file using the read_csv() function, which is 
# similar to base R's read.csv() function.
dat <- read.csv("movies.csv")
data <- read_csv("movies.csv") # gives a summary output of data
dat
data # immediately rectangular, cyrtails printout of the values and the rows
# better grasp of what the data is 
class(dat) # data.frame
class(data) # gives 4 outputs # data.frame is one of them
# which means functions not built for the tidyverse will still work on
# the data.frame attribute
# rest are attributes of a tibble - which is the tidyverse version of a data.fra,e

##########
# Exercise
##########

# Change the above code to use readr's read_csv() function. Assign
# the output to a different object. What do you notice is different 
# about the two functions?

vignette("tibble") # more info

#######
# dplyr
#######

vignette("dplyr")

# we use the dplyr package to manipulate our data. Manipulation 
# operations can generally be broken down into three basic steps:

## (Filtering on) rows
filter(dat, title_type == "Feature Film") #not a tibble

## (Selecting or mutating on) columns
select(dat, thtr_rel_month)
mutate(dat, rel_mon = month.abb[thtr_rel_month]) #month.abb is base R built in
# like 'letters' or 'LETTERS' - try in console
# so transforming the numbers in month into 3 letter abbreviations
# but adds on as an additional columns at the end 
# transmute() to replace original, but don't recommend 

## Group by and summarise into a single row
by_month <- group_by(dat, thtr_rel_month) #group_by object 
# call by_month to see structure
by_month # see Groups at top [12] 
summarise(by_month, n = n()) # summarise according to the number of 
# observations in each group
# so raw number/frequency of releases in each month 

##########
# The pipe
##########

# Filtering, selecting and summarising aren't very useful on their 
# own. Their true power comes from combining them together into a 
# single operation. This is where the pipe comes in.

dat %>%
  filter(title_type == "Feature Film") %>% # filter on the rows
  select(thtr_rel_month) %>% # select one column
  mutate(month = month.abb[thtr_rel_month]) %>% # change to month abbreviation
  group_by(month) %>% # group data by month
  summarise(n = n()) %>% # perform a summary operation (count the n per month)
  arrange(desc(n)) # sort in descending order

# Here, we perform 6 operations, one after the other, to manipulate 
# our dataset.

##########
# Exercise
##########

# Using the dplyr functions above with the pipe operator %>% 
# 1. Filter the movies dataset for "Horror" films in the genre 
# column. Which is the most popular month for Horror films to be 
# released?

dat %>%
  filter(genre == "Horror") %>% # filter on the rows
  select(thtr_rel_month) %>% # select one column
  mutate(month = month.abb[thtr_rel_month]) %>% # change to month abbreviation
  group_by(month) %>% # group data by month
  summarise(n = n()) %>% # perform a summary operation (count the n per month)
  arrange(desc(n))
# June

# 2. Using the dplyr commands you have learned, find the actor 
# (actor1) with the most award wins. 

dat %>%
  filter(best_actor_win == "yes") %>% # filter on the rows
  group_by(actor1) %>% # group data by actors
  summarise(n = n()) %>% # perform a summary operation (count the n awards per actor)
  arrange(desc(n))

# NO mutate(dat, wins = actor1[best_actor_win == "yes"])

#######################
# Complex operations...
#######################

# As we improve our dplyr skills, we can perform more complex 
# operations. Let's say we have a hunch about horror films, i.e.
# that, compared with other films, they're more likely to be 
# released in October for Halloween. How might we check this hunch?
# First, we'd need to check the underlying pattern of releases for 
# all films across the year. Let's recycle our code from above, but
# convert the n into proportions rather than getting a raw frequency

dat %>%  
  mutate(month = month.abb[thtr_rel_month]) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>%
  mutate(prop_month = round(n / sum(n), 3)) %>% # mutate after 
  # our summarise to find the proportion
  arrange(desc(prop_month))
# not filtered because checking for absolute proportions so can control for it


##########
# Exercise
##########

# Using the code above as a template, perform the same operation on 
# a subset of horror films

dat %>%  
  filter(genre == "Horror") %>%
  mutate(month = month.abb[thtr_rel_month]) %>% 
  group_by(month) %>% 
  summarise(n = n()) %>%
  mutate(prop_month = round(n / sum(n), 3)) %>% 
  arrange(desc(prop_month))

#############
# Visualising
#############

# Now that we know the relative frequency of releases for films, it 
# would be good to be able to visualise these. We could perform 
# some of these grouping and summarise operations directly within 
# ggplot. However, we'll use dplyr here to show how we can 
# manipulate data and then output the results into visualisations.

# Note: try breaking the code below at different points to see what
# is happening to the data at each stage.

dat %>%
  select(genre, thtr_rel_month) %>% # just keep the two relevant cols
  mutate(horror = genre == "Horror") %>% # make a new logical col for horror films
  group_by(thtr_rel_month, horror) %>% # perform a nested grouping operation (release month, then T/F horror)
  summarise(n = n()) %>% # get a raw count for each group
  pivot_wider(names_from = horror, values_from = n) %>% # change the shape of our data
  ungroup() %>%
  mutate(All = round(`FALSE` / sum(`FALSE`), 2), # calculate proportions for all films
         Horror = `TRUE` / sum(`TRUE`, na.rm = TRUE)) %>% # calculate proportions for horror films
  select(thtr_rel_month, All, Horror) %>% # drop all other columns
  pivot_longer(cols = c("All", "Horror"), names_to = "film_type") %>% # change the shape again!
  mutate(month = factor(month.abb[thtr_rel_month], levels = month.abb)) %>% # create a factor for months
  ggplot(aes(month, value)) + # plot the data
  geom_col(aes(fill = film_type), position = "dodge") +
  labs(title = "Proportion of Theatrical Releases by Month", y = "proportion") 

# The code above is quite complex: not only are we filtering and 
# grouping, we are also spreading and gathering our data using the 
# pivot longer and pivot wider functions. These functions change 
# the shape of our dataset, and are necessary here because of the 
# way dplyr performs different calculations. For now though, we 
# don't need to worry too much about them. What's important to note 
# is the way we have used the pipe %>% to chain different operations 
# together, to create a visualisation of our initial question. How 
# would you interpret the bar plot?

##########
# Exercise
##########

# Are feature films getting longer? Use the dplyr functions you've 
# learned about today to find out whether the average running time 
# of feature films has increased in recent years.

.....

# compare 2 means
# 1. two means
    # average running time of all films vs average of best picture winners
# 2. two sample sizes 
    # overall n (minus missing values for run time )
    nrow(is.na(dat$runtime)) # no
    dat %>%
      summarise(mean_run_time = mean(runtime, na.rm = TRUE),
                n = n())
# 3. standard deviations



