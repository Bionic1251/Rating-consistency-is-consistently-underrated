library(ordinal)
library(lme4)

# loading data
answers <- read.csv(file = 'transformed.csv')

# label codes: 0 - objective, 1 - misc, 2 - subjective, 3 - genre
fields = c("obj", "misc", "subj", "genre")

# comparing categories between each other in terms of rating consistency
runLogitRegression <- function(col_name1, col_name2, data){
  dataset = rbind(data[which(data[[col_name1]] == 1), ], data[which(data[[col_name2]] == 1), ])
  dataset$category = dataset[[col_name1]]
  glmerRes = glmer(consistency ~ category + (1|UID), data=dataset, family = binomial)
  print(summary(glmerRes))
}

for (i in 1:4) {
  for (j in (i + 1):4) {
    if (j > 4 || j == i) {
      next
    }
    print("")
    print(c(fields[i], fields[j]))
    runLogitRegression(fields[i], fields[j], answers)
  }
}

# subjective tags are more inconsistent than objective tags
# let's look at factors that might affect this difference
questions = c("familiar", "how_long_ago", "easy", "how_often")

runOrdinalRegression <- function(col_name1, col_name2, response, data){
  dataset = rbind(data[which(data[[col_name1]] == 1), ], data[which(data[[col_name2]] == 1), ])
  dataset$category = dataset[[col_name1]]
  dataset$response = factor(dataset[[response]])
  clmmRes = clmm(response ~ category + (1|UID), na.action=na.omit, data=dataset)
  print(summary(clmmRes))
}

runFactors <- function(field1, field2, questions, cleanAnswers){
  for (k in 1:4){
    resp = questions[k]
    print(resp)
    print(c(field1, field2))
    runOrdinalRegression(field1, field2, resp, cleanAnswers)
  }
}

runFactors(fields[1], fields[3], questions, answers)

# familiarity and ease of rating correlate with rating inconsistency between subjective and objective tags
# this could be the reason why subjective tags are more inconsistent than objective tags
# let's detect fators that affect rating consistency
runLogitRegressionWithFactors <- function(col_name1, col_name2, data){
  print(c(col_name1, col_name2))
  dataset = rbind(data[which(data[[col_name1]] == 1), ], data[which(data[[col_name2]] == 1), ])
  dataset$category = dataset[[col_name1]]
  glmerRes = glmer(consistency ~ category + familiar + how_long_ago + how_often + easy + (1|UID), data=dataset, family = binomial)
  print(summary(glmerRes))
}

runLogitRegressionWithFactors(fields[1], fields[3], answers)

# rating consistency is correlated with ease of rating