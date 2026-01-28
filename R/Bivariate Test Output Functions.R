################################################################################
# Chi Squared Test
#
# Name: chisq_result
# Purpose: Organize output from chi-squared test for publication.
#
# Arguments:
#   group - the grouping variable
#   variable - the other variable
#   decimal - the number of decimal places to be used in output
#   p_only - when FALSE (default), return statistic and p-value; when TRUE
#            return only the p-value
#   mcsim - when FALSE (default), montecarlo simulation is not used; when TRUE
#           monte carlo simulation is used; this is helpful when small sample
################################################################################

chisq_result = function(group, variable, decimal = 0, p_only = FALSE, mcsim = FALSE){

  table = table(variable, group)
  table = table[which(rowSums(table)!=0),]

  if(mcsim){
    test = chisq.test(table, simulate.p.value = TRUE, B = 10000)
  } else{
    test = chisq.test(table)
  }

  statistic = format(round(test$statistic, digits = decimal), nsmall = decimal)
  pvalue = ifelse(test$p.value<0.001, "<0.001", format(round(test$p.value, 3), nsmall=3))

  if(!p_only){
    result = paste0(statistic, " (", pvalue, ")")
  } else {
    result = pvalue
  }

  return(result)
}

################################################################################
# Chi Squared Test (specifically for variable operationalized as a numeric flag
#                   variable with 0s and 1s)
#
# Name: chisq_result_from_flag
# Purpose: Organize output from chi-squared test for publication.
#
# Arguments:
#   group - the grouping variable
#   variable - the other variable
#   decimal - the number of decimal places to be used in output
#   p_only - when FALSE (default), return statistic and p-value; when TRUE
#            return only the p-value
#   mcsim - when FALSE (default), montecarlo simulation is not used; when TRUE
#           monte carlo simulation is used; this is helpful when small sample
################################################################################

chisq_result_from_flag = function(group, variable, decimal = 0, p_only = FALSE, mcsim = FALSE){

  variable = factor(variable, levels = c(1,0))

  if(mcsim){
    test = chisq.test(table(variable, group), simulate.p.value = TRUE, B = 10000)
  } else{
    test = chisq.test(table(variable, group))
  }

  statistic = format(round(test$statistic, digits = decimal), nsmall = decimal)
  pvalue = ifelse(test$p.value<0.001, "<0.001", format(round(test$p.value, 3), nsmall=3))

  if(!p_only){
    result = paste0(statistic, " (", pvalue, ")")
  } else {
    result = pvalue
  }

  return(result)
}

################################################################################
# ANOVA Test (for 3+ groups)
#
# Name: anova_result
# Purpose: Organize output from ANOVA test for publication.
#
# Arguments:
#   group - the grouping variable
#   variable - the other variable (numeric for ANOVA)
#   decimal - the number of decimal places to be used in output
#   p_only - when FALSE (default), return statistic and p-value; when TRUE
#            return only the p-value
################################################################################

anova_result = function(group, variable, decimal = 0, p_only = FALSE){
  summary = summary(aov(variable~group,data = data))[[1]]
  statistic = format(round(unlist(summary[1,"F value"]), digits = decimal), nsmall = decimal)

  pvalue = ifelse(unlist(summary[1,"Pr(>F)"])<0.001, "<0.001", format(round(unlist(summary[1,"Pr(>F)"]), 3), nsmall=3))

  if(!p_only){
    result = paste0(statistic, " (", pvalue, ")")
  } else {
    result = pvalue
  }

  return(result)
}

################################################################################
# WILCOXON TEST (non-parametric version of t-test)
#
# Name: wilcoxon_result
# Purpose: Organize output from wilcoxon test for publication.
#
# Arguments:
#   group - the grouping variable (two groups)
#   variable - the other variable (numeric)
#   decimal - the number of decimal places to be used in output
#   p_only - when FALSE (default), return statistic and p-value; when TRUE
#            return only the p-value
################################################################################

wilcoxon_result = function(group, variable, decimal = 0, p_only = FALSE){
  test = wilcox.test(variable~group)

  statistic = format(round(test$statistic, digits = decimal), nsmall = decimal)
  pvalue = ifelse(test$p.value<0.001, "<0.001", format(round(test$p.value, 3), nsmall=3))

  if(!p_only){
    result = paste0(statistic, " (", pvalue, ")")
  } else {
    result = pvalue
  }

  return(result)
}

################################################################################
# KRUSKAL WALLIS (non-parametric version of ANOVA)
#
# Name: kruskal_wallis_result
# Purpose: Organize output from kruskal wallis test for publication.
#
# Arguments:
#   group - the grouping variable
#   variable - the other variable (numeric)
#   decimal - the number of decimal places to be used in output
#   p_only - when FALSE (default), return statistic and p-value; when TRUE
#            return only the p-value
################################################################################


kruskal_wallis_result = function(group, variable, decimal = 0, p_only = FALSE){
  test = kruskal.test(variable~group)

  statistic = format(round(test$statistic, digits = decimal), nsmall = decimal)
  pvalue = ifelse(test$p.value<0.001, "<0.001", format(round(test$p.value, 3), nsmall=3))

  if(!p_only){
    result = paste0(statistic, " (", pvalue, ")")
  } else {
    result = pvalue
  }

  return(result)
}
