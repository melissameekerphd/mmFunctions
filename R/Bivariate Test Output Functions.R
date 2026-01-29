
#' Chi Squared Test
#'
#' `chisq_result` organizes output from chi-squared test for publication.
#'
#' @param group the grouping variable
#' @param variable the other variable
#' @param decimal the number of decimal places to be used in output
#' @param p_only when FALSE (default), return statistic and p-value; when TRUE
#' return only the p-value
#' @param mcsim when FALSE (default), montecarlo simulation is not used; when
#' TRUE monte carlo simulation is used; this is helpful when small sample
#' @export

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

#' Chi Squared Test (Flag Variable)
#'
#' `chisq_result_from_flag` organizes output from chi-squared test for publication,
#' specifically for variable operationalized as a numeric flag variable with 0s
#' and 1s.
#'
#' @param group the grouping variable
#' @param variable the other variable (numeric flag variable with 0s and 1s)
#' @param decimal the number of decimal places to be used in output
#' @param p_only when FALSE (default), return statistic and p-value; when TRUE
#' return only the p-value
#' @param mcsim when FALSE (default), montecarlo simulation is not used; when
#' TRUE monte carlo simulation is used; this is helpful when small sample
#' @export

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

#' ANOVA Test
#'
#' `anova_result` organizes output from ANOVA test (for 3+ groups) for
#' publication.
#'
#' @param group the grouping variable
#' @param variable the other variable (numeric for ANOVA)
#' @param decimal the number of decimal places to be used in output
#' @param p_only when FALSE (default), return statistic and p-value; when TRUE
#' return only the p-value
#' @export

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

#' WILCOXON TEST
#'
#' `wilcoxon_result` organizes output from wilcoxon test (non-parametric
#' version of t-test) for publication.
#'
#' @param group the grouping variable (two groups)
#' @param variable the other variable (numeric)
#' @param decimal the number of decimal places to be used in output
#' @param p_only when FALSE (default), return statistic and p-value; when TRUE
#' return only the p-value
#' @export

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

#' KRUSKAL WALLIS
#'
#' `kruskal_wallis_result` organizes output from kruskal wallis test (non-parametric
#' version of ANOVA) for publication.
#'
#' @param group the grouping variable
#' @param variable the other variable (numeric)
#' @param decimal the number of decimal places to be used in output
#' @param p_only when FALSE (default), return statistic and p-value; when TRUE
#' return only the p-value
#' @export

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
