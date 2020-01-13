1-(0.95)^(20)
all_experiments <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Problem_Sets/13/jellybeans.csv")
control <- all_experiments[all_experiment$colour=="control","score"]
purple <- all_experiments[all_experiment$colour=="purple","score"]
t.test(control,purple)
# create compare_pairs function.
# Inputs: name of dataset, number of draws
# draws random pairs of sample points that are either in the same group or in different group
# computes their absolute distance
# computes the mean distances in the same group and the mean of distances in different groups
# returns the difference between those mean distances
compare_pairs <- function(dataset,ndraws){
  same_group <- {}
  between_group <- {}
  for (i in 1:ndraws){
    # determine condition (colour) to sample from
    list_colours <- unique(all_experiments$colour)
    colours <- sample(list_colours,2,replace=FALSE)
    # Make spearate lists for each group
    # (not strictly necessary, but helpful)
    firstgroup <- all_experiments[all_experiments$colour==colours[1],
                                  "score"]
    secondgroup <- all_experiments[all_experiments$colour==colours[1],
                                   "score"]
    # draw samples
    s1s2 <- sample(firstgroup,2,replace=FALSE)
    s3 <- sample(secondgroup,1)
    # compute same group and between group differences, add to list
    same_group <- c(same_group, abs(s1s2[1]-s1s2[2]))
    between_group <- c(between_group, abs(s1s2[2]-s3))
  }
  #compute means of same-group and between-group differences
  mean_same <- mean(same_group)
  mean_between <- mean(between_group)
  # compute absolute difference betwee those means
  diffmeans = abs(mean_same-mean_between)
  return(diffmeans)
}
our_experiment <- compare_pairs(all_experiments, 1000)
our_experiment
null_distribution <- {}
for (j in 1:1000){
  # make new dataframe called random_experiment
  # and shufffle colour column
  random_experiment <- all_experiments
  random_experiment$colour <- sample(all_experiments$colour,
                                     nrow(all_experiments),replace=FALSE)
  # use compare_pairs function on random_experiment
  random_meandiff <- compare_pairs(random_experiment,1000)
  # add the result to the Null distribution
  null_distribution <- c(null_distribution, random_meandiff)
}
hist(null_distribution)
