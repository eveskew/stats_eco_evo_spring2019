

# Reminder: subsetting in base R. We can subset vectors 
# or data frames using brackets

x <- c(25.4, 26.3, 23.2, 28)

x[1]

#==========================================================


# Installing packages 

install.packages("dplyr")

# Or you can do this through the RStudio "Packages" tab

# Loading the package

library(dplyr)

# Getting help

?dplyr()

#==========================================================


# Importing Ellenton Bay snake capture data

d <- read.csv("data/ebay_snake_captures.csv",
              stringsAsFactors = FALSE)

head(d)

colnames(d)

# Rename columns to be more informative

colnames(d) <- c("date", "time", "trap_type", 
                 "species", "num", "comments")

head(d)

#==========================================================


# Introduction to dplyr

# There are four major dplyr verbs: 
# arrange(), select(), filter(), mutate()

# All of these functions take a data frame as the first
# argument


# 1) arrange(): sorts the data frame according to the 
# values of a given variable

arrange(d, time)

# Use the desc() function to get the reverse sort order
# (i.e., from high to low)

arrange(d, desc(time))

# You can arrange by multiple variables simultaneously

arrange(d, species, time)

# sort() in base R is similar, but it only acts on a 
# vector and returns a vector

sort(d$time)

rev(sort(d$time)) # get the reverse sort order with base R


# 2) select(): pulls out only named variables from 
# the data frame

select(d, species)

select(d, time, species)

# The analogous base R functionality uses brackets and
# indexing to subset

d[ , "species"] # all rows, "species" column

d[ , c("time", "species")] # all rows, "time" and "species" column

# You can also select() based on column number

select(d, 1)

select(d, 1:3)


# 3) filter(): allows us to filter the data frame based 
# on logical tests

filter(d, time > 1200)

filter(d, time > 2300)

filter(d, species == "Nerodia fasciata")

filter(d, species == "Nerodia fasciata", time >= 1200, time <= 1300)

filter(d, trap_type == "coffee can")

# The analogous base R functionality uses brackets 
# and indexing to subset

d[d$time > 1200, ] 
# "d$time > 1200" returns a TRUE/FALSE vector that 
# specifies the rows we want to keep (i.e., the rows 
# that meet the desired condition)

d[d$species == "Nerodia fasciata", ]

d[d$species == "Nerodia fasciata" & d$time >= 1200 & d$time <= 1300, ]


# 4) mutate(): allows us to create new variables

mutate(d, multiple_individuals = ifelse(num > 1, "yes", "no"))

# In base R:

# d$multiple_individuals <- ifelse(d$num > 1, "yes", "no")

#==========================================================


# distinct() is also useful


# distinct(): when given only one variable, shows us 
# the distinct values of that variable

distinct(d, species)

# The base R analog is unique()

unique(d$species)

#==========================================================


# The pipe operator


# The "%>%" operator places what's on the left side of 
# the operator as the first argument to the subsequent
# function

select(d, species)

d %>% select(species) # same as above

d %>%
  select(species) # same as above with different styling


# The power of this approach is that it allows us to 
# chain together different functions very easily

# start with the data frame "d"
d %>% 
  # only want observations from coffee can traps
  filter(trap_type == "coffee can") %>% 
  # only return the "species" and "time" columns
  select(species, time) %>%
  # sort the rows by "species"
  arrange(species)

# This is (absurdly) equivalent to:

arrange(select(filter(d, trap_type == "coffee can"), species, time), species)

# We can use the pipe operator with other functions as 
# well. We just want to be sure that whatever is on the 
# left side of the pipe should be the first argument in 
# the next function

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, time) %>%
  arrange(species) %>%
  View()

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, time) %>%
  arrange(species, time) %>%
  View()
# this one is also sorted by time, early to late

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, time) %>%
  arrange(species, desc(time)) %>%
  View()
# this one is sorted by time, late to early

d %>%
  filter(trap_type == "coffee can") %>%
  select(species) %>%
  arrange(species, desc(time)) %>%
  View()
# throws an error because we're not passing it the 
# two columns we're asking to arrange by

#==========================================================


# Commenting out lines is a powerful exploratory tool 
# with this general data filtering pipeline


d %>%
  filter(trap_type == "coffee can") %>%
  select(species, time) %>%
  arrange(species, time) %>%
  View()

# Commenting allows us to easily remove lines of code from
# our pipeline and see the result

d %>%
  filter(trap_type == "coffee can") %>%
  select(species, time) %>%
  # arrange(species, time) %>%
  View()

d %>%
  filter(trap_type == "coffee can") %>%
  # select(species, time) %>%
  # arrange(species, time) %>%
  View()

#==========================================================


# group_by() then summarize()


# group_by() sets up all future operations to be done on 
# a group-wise basis

group_by(d, species)


# Not too exciting on its own, but with summarize(), 
# it's very powerful

d %>%
  # group by snake species
  group_by(species) %>%
  # summarize the total number of captures
  summarize(sum(num)) %>%
  View()

d %>%
  group_by(species) %>%
  # name this new variable "total_captures"
  summarize(total_captures = sum(num)) %>%
  View()

d %>%
  group_by(species) %>%
  summarize(total_captures = n()) %>%
  # arrange the data frame by "total_captures"
  arrange(desc(total_captures)) %>%
  View()

d %>%
  # group_by(species) %>%
  summarize(total_captures = n()) %>%
  arrange(desc(total_captures)) %>%
  View()
# with no grouping, the entire data frame is considered 
# a group

d %>%
  group_by(species, trap_type) %>%
  summarize(total_captures = n()) %>%
  arrange(species, trap_type) %>%
  View()

#==========================================================


# Loops: common programming construction that allows us
# to perform repeated operations


beginning <- "Stats"

endings <- c("is useful!", "is fun!", 
             "is great to talk about at social events!")


for (x in 1:3) {
  
  paste(beginning, endings[x])
}
# Why didn't anything happen? We didn't ask for output...


for (x in 1:3) {
  
  print(paste(beginning, endings[x]))
}
