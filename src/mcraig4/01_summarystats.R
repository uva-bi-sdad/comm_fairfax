library(maps)
library(ggplot2)

# This file is the summary file from the ATUS data with each respondent's state included, as well as a grouping variable for their age. Note that I've removed hawaii and alaska since the maping software I'm using doesn't have shapefiles for them, not for any good reason.

actSum = read.csv("./data/comm_fairfax/working/ATUS_data_working/ATUSsummaryAgeState.csv")

####################
# Here are some examples of how to produce summaries.
####################

# The basic summary function is table

# This is a table of ages
table(actSum$TEAGE)

# Age groups
table(actSum$agegrp)

# By giving multiple columns this will make cross tabulations. I'll also use with() so I don't have to put actSum in front of everything
# Ages by states
with(actSum, table(state, agegrp))

# For continuous variables we can use tapply to apply whatever function we want to subgroups of the data. with() is a very convenient function here.
# tapply has 3 components: the column you want to summarize, a list of factors across which to summarize, and a function to do the summarizing, such as 'mean'.

# t010101 is how many minutes people sleep. This will apply 'mean' to 't010101' broken out by 'state'

with(actSum, tapply(t010101, state, mean))

# Do it by age group

with(actSum, tapply(t010101, agegrp, mean))

# You can do multiple grouping variables by putting them in a list. This does age by state

with(actSum, tapply(t010101, list(agegrp, state), mean))

# It's pretty easy to get a serviceable bar plot by sticking this code inside a barplot() function and adding beside = T. This isn't always the best option, but we can discuss visualization if you have something you'd like to show.

barplot(with(actSum, tapply(t010101, list(agegrp, state), mean)), beside = T)





