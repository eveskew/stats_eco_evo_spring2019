

# Explain relationship between R and RStudio, scripts, etc.

#==========================================================


# Basic mathematical operations in R

2+2 # addition

2-1 # subtraction

2*3 # multiplication

2/4 # division

2^4 # exponents

#==========================================================


# Assignment 

# In R, as in programming languages generally, we often 
# want to "assign" certain values to objects in order to
# reference those objects (and values) later in a script 

x = 2 + 2 # you can assign using the "=" operator

x # then type the name of the object to see the value(s)

# You can also see existing objects in your "Environment" 
# tab within RStudio

y <- 3 + 3 # or you can use the "<-" operator to assign

y

x <- y 
# This is clearer than "x = y", so the "<-" operator is
# often preferred. With the "<-" operator, we see 
# visually that x takes on the value of y. The "=" operator
# can seem ambiguous

x # x now has the value previously assigned to y

y # y retains its value

#==========================================================


# Commenting

# You can use the "#" sign to write portions of your 
# script that will not be interpreted as code, and 
# effectively ignored. You should comment liberally to
# remind yourself of code purpose and to document your
# ideas for any other readers

#==========================================================


# Get and set working directories

getwd()

# Anything followed by parentheses are functions in R. 
# A function usually takes a series of arguments and 
# performs some operation, but sometimes they don't need 
# any arguments

setwd()

# Why didn't this work?

# Get help in R using the "?" operator preceding the 
# function in question

?setwd()

# The "Help" tab in RStudio can also be navigated 
# interactively

setwd("~/Desktop/")

?list.files()

list.files() # lists all files in the working directory

setwd("~/Documents/Statistics and Coding/stats_eco_evo_spring2019/")

list.files()

#==========================================================


# Vectors (with numeric data)

# A vector in R is a collection of values of the same type

c(5.9, 7.2, 6.3)

weights <- c(5.9, 7.2, 6.3)

weights

# What is "c()" doing?

?c() # concatenates values into a vector or list

# We can perform operations on the resulting object 
# using functions

sum(weights) # sum all values in "weights"

mean(weights) # find the mean of all values in "weights"

# Note that most basic mathematical operations will be
# performed on each element of a vector

weights + 10 # all elements of "weights" with 10 added

#==========================================================


# Vectors (with character data)

# We are not limited to using numbers. R also supports 
# character data (i.e., text) 

names <- 
  c("Hyla avivoca", "Hyla gratiosa", "Hyla versicolor")

names

sum(names) 
# this function doesn't make any sense with character data

mean(names) 
# this function doesn't make any sense with character data

#==========================================================


# Inspecting objects in R

str(weights) # structure of the data

summary(weights) # summary of the data

View(weights) # view the data in a new tab

str(names)

summary(names)

View(names)

#==========================================================


# Sequences

# Use the ":" operator to generate a sequence of 
# consecutive integers

1:3

1:10

1:(3^6)

# Use "seq()" to generate more complex sequences

?seq()

seq(from = 1, to = 3, by = 1) 
# within functions, argument values must be assigned 
# with the "=" operator

seq(from = 1, to = 4, by = 1.5)

my.seq <- seq(from = 0, to = 100, by = 0.5)

str(my.seq)

summary(my.seq)

length(my.seq) # how many values are in the object?

#==========================================================


# Indexing

# Indexing is the process of subsetting your data to 
# obtain only specific values

my.seq[1] # returns the first value of the vector

my.seq[1, 2] 
# invalid because the vector only has one dimension

my.seq[c(1, 2)] # returns the first and second values

my.seq[1:20] # returns the first twenty values

my.seq[101]

my.seq[500] # invalid index value; only 201 "my.seq" values

my.short.seq <- my.seq[1:101]

#==========================================================


# What happens if we try to combine data types into 
# one vector?

mashup <- c(16.9, "Hyla gratiosa")

str(mashup) # the number has been coerced into a character 

summary(mashup)

#==========================================================


# Data frames

# Data frames can hold collections of different data types

d <- data.frame(names, weights, stringsAsFactors = FALSE) 

str(d)

summary(d)

View(d)

# Inspect and change column names in a data frame

?colnames()

colnames(d)

colnames(d)[1]

colnames(d)[1] <- "species"

colnames(d)

View(d)

# You can refer to specific columns within a data frame
# using the "$" operator

d$species

d$weights

# Indexing with data frames

d[1, 1] 
# refers to row and column values, separated by a comma

d[1, 2] # first row, second column

d[1, 3] # first row, third column (doesn't exist)

d[1, ] 
# if you leave the value blank for a given dimension, 
# you'll get all values

d[ , 2] # all rows, second column

d[3, 2] # third row, second column

# Another way to do this is to call the second column 
# directly and then index

d$weights[3]

# What if we want to add data to the data frame?

names <- c(names, "Hyla chrysoscelis")

names

d <- data.frame(names, weights, stringsAsFactors = FALSE) 
# Nope! Every column of the data frame must have the same 
# number of observations

weights <- c(weights, NA) 
# add a value to the "weights" vector indicating 
# missingness

mean(weights)
# our weights vector now has NA values, so mean() 
# returns NA

mean(weights, na.rm = TRUE) # need an additional argument

d <- data.frame(names, weights)

View(d)
