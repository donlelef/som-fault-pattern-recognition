# This script computes three different bidimensional probability distribution, uses it as a
# probability function to create a faultMap, and then tries to estimate
# the original distributions from the faults on the map using KDE algorithm.
# Different bandwidth are used and, for each number of faults, the best one is 
# saved. The best bandwidth is the one which causes the minimum error, defined as
# the sum of the extimation errors on the different distribution. The weight of the
# single distribution is arbitrary. 
# After that, the relationship between the amounts of faults and the 
# best bandwith is represented through a linear model.

# TODO