#' Summarize Numeric Variable for Table 1
#'
#' `summary_continuous_variable` summarizes a continuous variable for
#' a descriptive table.
#'
#' @param x the continuous variable
#' @param decimal the number of decimal places to be used in output
#' @param measure can take value "mean" (default) or "median"
#' @param description the name of the variable (x); you can specify your own
#' @param range can be TRUE or FALSE; when type = "median", range = TRUE will
#' output the [Q1, Q3] and range = FALSE will output the interquartile range
#' value
#' @param separated TRUE if you want the mean/median separate from the
#' SD/IQR/[Q1, Q3] and FALSE (default) if you want them pasted into one
#' @export

summary_continuous_variable = function(x, measure = "mean", decimal = 0,
                                       description = deparse(substitute(x)),
                                       range = TRUE, separated = FALSE){

  # If user wants mean and standard deviation
  if(measure == "mean"){

    mean = mean(x, na.rm = TRUE)
    mean_formatted = format(round(mean,decimal), nsmall = decimal)

    sd = sd(x, na.rm = TRUE)
    sd_formatted = format(round(sd,decimal), nsmall = decimal)

    if(!separated){
      result = c(paste0(description, " - mean(sd)"), paste0(mean_formatted, " (", sd_formatted, ")"))
    } else {
      result = c(paste0(description, " - mean(sd)"), mean_formatted, sd_formatted)
    }


  } else if(measure == "median"){

    median = median(x, na.rm = TRUE)
    median_formatted = format(round(median,decimal), nsmall = decimal)

    IQR = paste0("(", format(round(IQR(x, na.rm = TRUE),decimal), nsmall = decimal), ")")

    q1 = quantile(x, 0.25, na.rm = TRUE)
    q3 = quantile(x, 0.75, na.rm = TRUE)
    quarts = paste0("[", format(round(q1,decimal), nsmall = decimal), ", ", format(round(q3,decimal), nsmall = decimal), "]")

    spread = ifelse(range, IQR, quarts)

    if(!separated){
      result = c(paste0(description, " - median(IQR)"), paste(median_formatted, spread))
    } else {
      result = c(paste0(description, " - median(IQR)"), median_formatted, spread)
    }

  } else {
    result = c(NA, "Measure must be 'median' or 'mean'.")
  }

  if(!separated){
    result = as.data.frame(matrix(result, nrow = 1, ncol = 2))
    colnames(result) = c("value", "result")
  } else {
    result = as.data.frame(matrix(result, nrow = 1, ncol = 3))
    colnames(result) = c("value", "part1", "part2")
  }


  return(result)
}

#' Summarize Numeric Variable By Group for Table 1
#'
#' `table_continuous_by_group` summarizes a continuous variable by group for
#' a descriptive table.
#'
#' @param group the grouping variable
#' @param x the continuous variable
#' @param decimal the number of decimal places to be used in output
#' @param measure can take value "mean" (default) or "median"
#' @param range can be TRUE or FALSE; when type = "median", range = TRUE will
#' output the [Q1, Q3] and range = FALSE will output the interquartile range
#' value
#' @param separated TRUE if you want the mean/median separate from the
#' SD/IQR/[Q1, Q3] and FALSE (default) if you want them pasted into one
#' @export

table_continuous_by_group = function(group, x, decimal = 2, measure = "mean", range = TRUE, separated = FALSE){

  # Number of Groups
  n_groups = length(unique(group))

  # The Groups
  groups = sort(unique(group))

  # Create container for results
  if(!separated){
    results = rep(NA, times = n_groups)
    names(results) = groups
  } else {
    results = rep(NA, times = n_groups*2)
    names(results) = paste0(rep(groups, each = 2), rep(c(" - part1", " - part2"), times = length(groups)))
  }

  # If user wants mean and standard deviation
  if(measure == "mean"){
    for(g in 1:n_groups){
      mean = mean(x[group==groups[g]], na.rm = TRUE)
      sd = sd(x[group==groups[g]], na.rm = TRUE)

      if(!separated){
        results[g] = paste0(format(round(mean,decimal), nsmall = decimal), " (", format(round(sd,decimal), nsmall = decimal), ")")
      } else {
        results[(g*2-1):(g*2)] = c(mean, sd)
      }
    }
  }

  #If user wants median and IQR
  if(measure == "median"){
    for(g in 1:n_groups){
      median = median(x[group==groups[g]], na.rm = TRUE)
      median_formatted = format(round(median,decimal), nsmall = decimal)
      IQR = paste0("(", format(round(IQR(x[group==groups[g]], na.rm = TRUE), decimal), nsmall = decimal), ")")

      q1 = quantile(x[group==groups[g]], 0.25, na.rm = TRUE)
      q3 = quantile(x[group==groups[g]], 0.75, na.rm = TRUE)
      quarts = paste0("[", format(round(q1,decimal), nsmall = decimal), ", ", format(round(q3,decimal), nsmall = decimal), "]")

      spread = ifelse(range, IQR, quarts)

      results[g] = paste(median_formatted, spread)

      if(!separated){
        results[g] = paste(median_formatted, spread)
      } else {
        results[(g*2-1):(g*2)] = c(median_formatted, spread)
      }
    }
  }

  return(results)
}

#' Spread of Numeric Variable
#'
#' `summary_continuous_spread` summarizes a continuous variable with minimum,
#' Q1, median, Q3, and max
#'
#' @param group the grouping variable
#' @param x the continuous variable
#' @param decimal the number of decimal places to be used in output
#' @export

summary_continuous_spread = function(x, decimal = 0, description = deparse(substitute(x))){
  median = median(x, na.rm = TRUE)
  IQR = IQR(x, na.rm = TRUE)
  result = c(paste0(description, " - quartiles"),
             round(min(x, na.rm = TRUE),decimal),
             round(quantile(x, prob = 0.25, na.rm = TRUE),decimal),
             paste0(round(median,decimal), " (", round(IQR,decimal), ")"),
             round(quantile(x, prob = 0.75, na.rm = TRUE),decimal),
             round(max(x, na.rm = TRUE),decimal))


  result = as.data.frame(matrix(result, nrow = 1, ncol = 6))
  colnames(result) = c("value", "min", "firstquart", "median_IQR", "thirdquart", "max")
  return(result)
}

#' Spread of Numeric Variable by Group
#'
#' `table_continuous_by_group_spread` summarizes a continuous variable by group
#' with minimum, Q1, median, Q3, and max
#'
#' @param group the grouping variable
#' @param x the continuous variable
#' @param decimal the number of decimal places to be used in output
#' @export

table_continuous_by_group_spread = function(group, x, decimal = 2){

  n_groups = length(unique(group))

  groups = sort(unique(group))

  results = matrix(NA, nrow = n_groups, ncol = 5)
  colnames(results) = c("min", "firstquart", "median_IQR", "thirdquart", "max")
  rownames(results) = groups

  for(g in 1:n_groups){
    results[g, "min"] = round(min(x[group==groups[g]], na.rm = TRUE), decimal)
    results[g, "firstquart"] = round(quantile(x[group==groups[g]], prob = 0.25, na.rm = TRUE), decimal)
    results[g, "thirdquart"] = round(quantile(x[group==groups[g]], prob = 0.75, na.rm = TRUE), decimal)
    results[g, "max"] = round(max(x[group==groups[g]], na.rm = TRUE), decimal)
    median = median(x[group==groups[g]], na.rm = TRUE)
    IQR = IQR(x[group==groups[g]], na.rm = TRUE)
    results[g, "median_IQR"] = paste0(round(median,decimal), " (", round(IQR,decimal), ")")
  }

  results = cbind(groups, results)
  colnames(results) = c("value", "min", "firstquart", "median_IQR", "thirdquart", "max")

  return(results)
}

#' Frequency of Value
#'
#' `frequency_of_value` returns the frequency and % of a specific category
#' (value) of a variable (x)
#'
#' @param x the categorical variable
#' @param value the category that you want to determine the frequency/percent of
#' @param decimal the number of decimal places to be used in output
#' @param separated TRUE if you want the frequency separate from the percent
#' and FALSE (default) if you want them pasted into one

frequency_of_value = function(x, value, decimal = 0, separated = FALSE){
  sub = which(x==value)
  n = length(sub)
  n_formatted = format(length(sub), big.mark = ",")
  pct = length(sub)/length(x)*100
  pct_formatted = format(round(pct, decimal), nsmall = decimal)

  if(!separated){
    result = paste0(n_formatted, " (", pct_formatted, "%)")
  } else {
    result = c(n,pct)
  }

  return(result)
}

#' Frequency of Categories of Categorical Variable
#'
#' `frequency_of_variable` returns the frequency and % of all categories of a
#' variable (x)
#'
#' @param x the categorical variable
#' @param decimal the number of decimal places to be used in output
#' @param use_na the default ("ifany") shows a row for missing data if there is
#' any; if set to anything else, it will not.
#' @param remove_empty when FALSE (default), this removes the row for a
#' category in which there are no occurrences; if TRUE, it keeps the row in the
#' output
#' @param separated TRUE if you want the frequency separate from the percent
#' and FALSE (default) if you want them pasted into one
#' @export

frequency_of_variable = function(x, decimal = 0, use_na = "ifany", remove_empty = FALSE, separated = FALSE){
  if(remove_empty){
    x = droplevels(x)
  }
  num_values = length(levels(x))

  if(!separated){
    results = as.data.frame(matrix(data = NA, nrow = num_values, ncol = 2))
    colnames(results )= c("value", "result")
  } else {
    results = as.data.frame(matrix(data = NA, nrow = num_values, ncol = 3))
    colnames(results )= c("value", "part1", "part2")
  }

  results[,1] = levels(x)

  for(v in 1:num_values){
    value = results[v,1]

    if(!separated){
      results[v,2] = frequency_of_value(x,value, decimal = decimal)
    } else {
      results[v,2:3] = frequency_of_value(x,value, decimal = decimal, separated = TRUE)
    }

  }

  if(sum(is.na(x))>0 & use_na == "ifany"){
    n = sum(is.na(x))
    n_formatted = format(n, big.mark = ",")
    pct = sum(is.na(x))/length(x)*100
    pct_formatted = format(round(pct, decimal), nsmall = decimal)
    missing_freq = paste0(n, " (", pct, "%)")

    if(!separated){
      missing_row = c("Missing", missing_freq)
    } else {
      missing_row = c("Missing", n, pct)
    }

    results = rbind(results, missing_row)
  }

  return(results)
}

#' Frequency of Categories of Categorical Variable
#'
#' `frequency_by_patient_group` returns the frequency and % of all categories
#' of a variable (variable) by group (patient_group)
#'
#' @param patient_group the grouping variable
#' @param variable the categorical variable
#' @param decimal the number of decimal places to be used in output
#' @param percent_type when "col" (default), it computes column percents; when
#' "row" it computes row percents
#' @param remove_empty when TRUE (default), it removes rows categories with zero
#                  frequency
#' @param separated TRUE if you want the frequency separate from the percent
#' and FALSE (default) if you want them pasted into one
#' @export

frequency_by_patient_group = function(patient_group, variable, decimal = 0, percent_type = "col", remove_empty = TRUE, use_na = "ifany", separated = FALSE){

  two_by_two_freq = as.matrix(table(variable, patient_group, useNA = use_na))
  if(remove_empty  & sum(two_by_two_freq[which(rownames(two_by_two_freq)!="Missing"),])!=0){
    two_by_two_freq = two_by_two_freq[which(rowSums(two_by_two_freq)!=0),]
  }

  two_by_two = NA

  if(percent_type=="col"){

    two_by_two_pct = t(apply(two_by_two_freq,1,function(x) x/colSums(two_by_two_freq)*100))
    two_by_two_pct_formatted = t(apply(two_by_two_freq,1,function(x) format(round(x/colSums(two_by_two_freq)*100,decimal), nsmall = decimal)))
    two_by_two_pct_formatted[which(two_by_two_pct_formatted=="NaN")] = "0"

    if(!separated){
      two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), " (", two_by_two_pct_formatted, "%)"),
                           nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
    } else {

      two_by_two = cbind(two_by_two_freq[,1], two_by_two_pct[,1])
      for(c in 2:ncol(two_by_two_pct)){
        two_by_two = cbind(two_by_two, two_by_two_freq[,c], two_by_two_pct[,c])
      }
      two_by_two = matrix( two_by_two,
                           nrow=nrow(two_by_two_freq),
                           dimnames = list(rownames(two_by_two_freq),
                                           paste0(rep(colnames(two_by_two_freq), each = 2), rep(c(" - part1", " - part2"), times = ncol(two_by_two_freq)))))
    }


  } else if(percent_type == "row"){

    two_by_two_pct = t(apply(two_by_two_freq,1,function(x) x/sum(x)*100))
    two_by_two_pct_formatted = format(round(two_by_two_pct,decimal), nsmall = decimal)
    two_by_two_pct_formatted[which(two_by_two_pct_formatted=="NaN")] = "0"

    if(!separated){
      two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), " (", two_by_two_pct_formatted, "%)"),
                           nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
    } else {
      two_by_two = cbind(two_by_two_freq[,1], two_by_two_pct[,1])
      for(c in 2:ncol(two_by_two_pct)){
        two_by_two = cbind(two_by_two, two_by_two_freq[,c], two_by_two_pct[,c])
      }
      two_by_two = matrix( two_by_two,
                           nrow=nrow(two_by_two_freq),
                           dimnames = list(rownames(two_by_two_freq),
                                           paste0(rep(colnames(two_by_two_freq), each = 2), rep(c(" - part1", " - part2"), times = ncol(two_by_two_freq)))))
    }


  } else {
    cat(bold(red("The parameter 'percent_type' must be specified as row or col.")))
  }

  return(two_by_two)
}

#' Frequency of Flag in Binary Variable
#'
#' `frequency_of_flag_variable` returns the frequency and % of the flag (1s) of
#' a variable (x) coded as 0s and 1s
#'
#' @param x the flag variable (must be coded as 0s and 1s)
#' @param decimal the number of decimal places to be used in output
#' @param show_denom when FALSE (default) the output does not include the
#' denominator; when TRUE the output does include the denominator; note - this
#' functionality is only built out for when separated = FALSE
#' @param separated TRUE if you want the frequency separate from the percent
#' and FALSE (default) if you want them pasted into one
#' @export

frequency_of_flag_variable = function(x, decimal = 0, show_denom = FALSE, separated = FALSE){

  #Setting up results container
  if(!separated){
    results = as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))
    colnames(results )= c("value", "result")
  } else {
    results = as.data.frame(matrix(data = NA, nrow = 1, ncol = 3))
    colnames(results )= c("value", "part1", "part2")
  }

  results[1,1] = paste(deparse(substitute(x)), collapse = "")

  #Evaluating frequency:
  n = sum(x, na.rm = TRUE)

  if(!separated){
    if(show_denom){
      results[1,2] = paste0(format(n, big.mark =","), "/", format(length(x), big.mark =","), " (", round(n/length(x)*100,decimal), "%)")
    } else {
      results[1,2] = paste0(format(n, big.mark =","), " (", round(n/length(x)*100,decimal), "%)")
    }
  } else {
    results[1,2] = n
    results[1,3] = n/length(x)*100
  }

  #If Missing Data:
  if(sum(is.na(x))>0){
    n = sum(is.na(x))
    n_formatted = format(n, big.mark = ",")
    pct = n/length(x)*100
    pct_formatted = round(sum(is.na(x))/length(x)*100, decimal)

    if(!separated){
      if(show_denom){
        missing_freq = paste0(n_formatted, "/", length(x), " (", pct_formatted, "%)")
      } else{
        missing_freq = paste0(n_formatted, " (", pct_formatted, "%)")
      }

      missing_row = c("Missing", missing_freq)

    } else {
      missing_row = c("Missing", n, pct)
    }

    results = rbind(results, missing_row)
  }

  return(results)
}

#' Frequency of Flag in Binary Variable by Group
#'
#' `frequency_flag_by_patient_group` returns the frequency and % of the flag (1s) of
#' a variable (x) coded as 0s and 1s by group
#'
#' @param patient_group the grouping variable
#' @param variable the flag variable (must be coded as 0s and 1s)
#' @param decimal the number of decimal places to be used in output
#' @param percent_type when "col" (default), it computes column percents; when
#' "row" it computes row percents
#' @param show_denom when FALSE (default) the output does not include the
#' denominator; when TRUE the output does include the denominator; note - this
#' functionality is only built out for when separated = FALSE
#' @param separated TRUE if you want the frequency separate from the percent
#' and FALSE (default) if you want them pasted into one
#' @export

frequency_flag_by_patient_group = function(patient_group, variable, decimal = 0, percent_type = "col", show_denom = FALSE, separated = FALSE){

  if(sum(variable)==0){
    if(!separated){
      zeros = as.data.frame(matrix(rep("0 (0%)", times = length(levels(patient_group))), nrow = 1))
      colnames(zeros) = levels(patient_group)

    } else {
      zeros = as.data.frame(matrix(rep(c(0, 0), times = length(levels(patient_group))), nrow = 1))
      colnames(zeros) = paste0(rep(levels(patient_group), each = 2),
                               rep(c(" - part1", " - part2"), times = length(levels(patient_group))))
    }
    rownames(zeros) = paste(deparse(substitute(x)), collapse = "")
    return(zeros)
  } else {
    two_by_two_freq = as.matrix(table(variable, patient_group, useNA = "ifany"))

    two_by_two = NA

    if(percent_type=="col"){

      two_by_two_pct = t(apply(two_by_two_freq,1,function(x) format(round(x/colSums(two_by_two_freq)*100,decimal), nsmall = decimal)))
      two_by_two_pct[which(two_by_two_pct=="NaN")] = "0"

      two_by_two_denom = rbind(t(colSums(two_by_two_freq)), t(colSums(two_by_two_freq)))

      if(!separated){
        if(show_denom){
          two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), "/",
                                      format(two_by_two_denom, big.mark = ","),
                                      " (", two_by_two_pct, "%)"),
                               nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
        } else {
          two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), " (", two_by_two_pct, "%)"),
                               nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
        }
      } else {
        two_by_two = cbind(two_by_two_freq[,1], two_by_two_pct[,1])
        for(c in 2:ncol(two_by_two_pct)){
          two_by_two = cbind(two_by_two, two_by_two_freq[,c], two_by_two_pct[,c])
        }
        two_by_two = matrix( two_by_two,
                             nrow=nrow(two_by_two_freq),
                             dimnames = list(rownames(two_by_two_freq),
                                             paste0(rep(colnames(two_by_two_freq), each = 2), rep(c(" - n", " - pct"), times = ncol(two_by_two_freq)))))

      }


    } else if(percent_type == "row"){

      two_by_two_pct = t(apply(two_by_two_freq,1,function(x) format(round(x/sum(x)*100,decimal), nsmall = decimal)))
      two_by_two_pct[which(two_by_two_pct=="NaN")] = "0"

      two_by_two_denom = matrix(rep(rowSums(two_by_two_freq), times = ncol(two_by_two_pct)),
                                ncol =ncol(two_by_two_pct))

      if(!separated){
        if(show_denom){
          two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), "/",
                                      format(two_by_two_denom, big.mark = ","),
                                      " (", two_by_two_pct, "%)"),
                               nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
        } else {
          two_by_two = matrix( paste0(format(two_by_two_freq, big.mark = ","), " (", two_by_two_pct, "%)"),
                               nrow=nrow(two_by_two_freq), dimnames=dimnames(two_by_two_freq) )
        }
      } else {
        two_by_two = cbind(two_by_two_freq[,1], two_by_two_pct[,1])
        for(c in 2:ncol(two_by_two_pct)){
          two_by_two = cbind(two_by_two, two_by_two_freq[,c], two_by_two_pct[,c])
        }
        two_by_two = matrix( two_by_two,
                             nrow=nrow(two_by_two_freq),
                             dimnames = list(rownames(two_by_two_freq),
                                             paste0(rep(colnames(two_by_two_freq), each = 2), rep(c(" - n", " - pct"), times = ncol(two_by_two_freq)))))
      }

    } else {
      cat(bold(red("The parameter 'percent_type' must be specified as row or col.")))
    }

    two_by_two = as.data.frame(two_by_two)
    rownames(two_by_two)[2] = paste(deparse(substitute(x)), collapse = "")

    return(two_by_two[paste(deparse(substitute(x)), collapse = ""),])
  }


}
