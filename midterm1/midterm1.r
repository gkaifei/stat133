# Please load in the Kaiser babies dataset included in your midterm1
# directory. This dataset includes information on mothers and the children born
# to those mothers. You will need this data to perform the tasks for this quiz.


# calculate the mean and standard deviation of birthweights (bwt) for all
# observations in the dataset. Store these as the variables <mean.bwt> and
# <sd.bwt> respectively.

# mean.bwt <- your code here
# sd.bwt <- your code here



# For each observation in the dataset, subtract <mean.bwt> from the observations
# bwt and divide by <sd.bwt>. Store this as the variable <std.bwts>. Note that
# this should be a numeric vector whose length is equal to the number of
# observations in the dataset.

# std.bwts <- your code here



# Create the following two subsets and store them as variables with the
# indicated names:
# 1) Mothers whose smoking status is never: <subset.nonsmoke>
# 2) Mothers whose smoking status is now: <subset.smoke>

# subset.nonsmoke <- your code here
# subset.smoke <- your code here



# For each of your subsets, create a vector giving the age of the mother. Store
# these as variables <subset.nonsmoke.age> and <subset.smoke.age>.

# subset.smoke.age <- your code here
# subset.nonsmoke.age <- your code here



# Implement the function gestByAge. Your function should take the following
# arguments:
#
# <age.cutoff>: a numeric constant giving a cutoff to subset by
# <ages>: a numeric vector of ages for each observation
# <gestation>: a numeric vector of gestation period length for each observation
#   (this should be the same length as <ages>)
#
# Your function should return the average gestation period for every observation
# whose value in <ages> is strictly less that <age.cutoff>.

gestByAge <- function(age.cutoff, ages, gestation){

    # your code here

}


# Please produce a plot of birthweight (y-axis) against gestation period. Your
# plot should contain the following features:
# 1) the title: "Birthweight v gestation"
# 2) points of mothers whose smoking status is never should be colored red


