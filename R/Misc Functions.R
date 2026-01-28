################################################################################
# Get File Path Start
#
# Name: get_filepath_start
# Purpose: For grabbing the start of file paths to make code transferrable to
#          moving between PCs.
#
################################################################################

get_filepath_start = function(){
  starter = paste0("C:/Users/", Sys.info()["user"], "/")
}

################################################################################
# Sample Size Adjusted for Site-Level Correlation
#
# Name: ss_site_cor_adj
# Purpose: Calculating sample size adjusted for site-level correlation.
#
# Arguments:
#   n - the unadjusted sample size
#   nsites - the number of sites
#   icc - Intraclass Correlation Coefficient (ICC)
################################################################################

ss_site_cor_adj = function(n, nsites, icc){
  return((1-icc)/((nsites/n)-icc))
}

################################################################################
# Difference in Median
#
# Name: difference_in_median
# Purpose: Bootstrap Method to produce 95% CI of difference in medians of
#          samples from two groups
#
# Arguments:
#   group - the group (only designed for a variable with two levels/categories)
#   sample - the numeric variable
#   nits - the number of iterations
#   decimal - the number of decimal points to present
################################################################################

difference_in_median = function(group, sample, nits = 500, decimal = 0){

  value1 = levels(group)[1]
  value2 = levels(group)[2]
  sample1 = sample[which(group==value1)]
  sample2 = sample[which(group==value2)]

  #true_diff = median(sample1) - median(sample2)

  diff_in_medians = vector(mode = "numeric", length = nits)

  for(i in 1:nits){
    sample1_bs = sample(sample1, size = length(sample1), replace = TRUE)
    sample2_bs = sample(sample2, size = length(sample2), replace = TRUE)

    diff_in_medians[i] = median(sample1_bs, na.rm = TRUE) - median(sample2_bs, na.rm = TRUE)
  }

  diff_in_medians = sort(diff_in_medians)
  percentiles = quantile(diff_in_medians,c(0.025, 0.975))

  result = paste0("[",round(percentiles[1], decimal), ", ", round(percentiles[2], decimal), "]")
  return(result)
}

################################################################################
# Combining datasets (column bind)
#
# Name: cbind_fill
# Purpose: Special column bind for combining columns when heach dataset does not
#          have all the same rows
#
# Arguments:
#   data_left - the first set of columns
#   data_right - the second set of columns
################################################################################

library(dplyr)
library(stringr)
library(crayon)
library(qdapTools)

cbind_fill = function(data_left, data_right){


  result = NA

  fill_left = !all(rownames(data_right) %in% rownames(data_left))
  fill_right = !all(rownames(data_left) %in% rownames(data_right))

  if(fill_right & fill_left){
    cat(bold(red("Both matrices are missing rows. Use different function.")))
  }

  if(!fill_right & !fill_left){
    cat(bold(green("Do not need cbind_fill().")))
    result = cbind(data_left, data_right)
  }

  if(fill_right & !fill_left){
    data_filled = as.data.frame(matrix(data = NA, nrow = nrow(data_left), ncol = ncol(data_right), dimnames = list(rownames(data_left), colnames(data_right))))

    for(r in 1:nrow(data_filled)){
      if(rownames(data_filled)[r] %in% rownames(data_right)){
        data_filled[r,] = data_right[rownames(data_filled)[r],]
      }
    }

    result = cbind(data_left, data_filled)

  }

  if(fill_left & !fill_right){
    data_filled = as.data.frame(matrix(data = NA, nrow = nrow(data_right), ncol = ncol(data_left), dimnames = list(rownames(data_right), colnames(data_left))))

    for(r in 1:nrow(data_filled)){
      if(rownames(data_filled)[r] %in% rownames(data_left)){
        data_filled[r,] = data_left[rownames(data_filled)[r],]
      }
    }

    result = cbind(data_filled, data_right)

  }

  return(result)

}
