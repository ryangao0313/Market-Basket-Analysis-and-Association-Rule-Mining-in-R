#Association Rule Mining

#Import library
library(arules)
library(arulesViz)

#####   Data

#Import data (AdultUCI data set)
data("Groceries")

#Inspect the data
class(Groceries)

#The first two transactions and the items involved in each transaction
inspect(head(Groceries, 2))



#####   Generating Rules

#Generating Rules using apriori (Support, confidence(if you want stronger rules, increase conf), lift) (15 rules)
grocery_rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))
#Display the top 3 rules generated, LHS is the buy combos already exist, and RHS is what people tend to buy in this case
inspect(head(sort(grocery_rules, by="confidence"), 3))



#####   Limiting the number of rules generated

# This shows what products are bought before buying "whole milk" and will generate rules that lead to buying "whole milk". (3765 rules)
wholemilk_rules <- apriori(data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (rhs="whole milk"))
# This displays the top 3 rules about whole milk
inspect(head(sort(wholemilk_rules, by="confidence"), 3))

# This generates only one rule in the output. (changed support parameter)
grocery_rules_increased_support <- apriori(Groceries, parameter = list(support = 0.02, confidence = 0.5))
inspect(head(sort(grocery_rules_increased_support, by="confidence")))

#Remove the shorter rules that are subsets of the longer rules
#Sometimes you might be interested in finding the rules involving maximum number of items

###subsets <- which(colSums(is.subset(grocery_rules, grocery_rules)) > 1)
###grocery_rules <- grocery_rules[-subsets]


# This gives more than 1,500,000 rules (a lot of rules due to low support parameter)
###rules <- apriori(Groceries, parameter = list(supp = 0.0001, conf = 0.5))
# This gives 982,000 rules. (arem is auto remove less interesting rules to simplify the quality of the resulting rules)
###rules_chi2 <- apriori(Groceries, parameter = list(supp = 0.0001, conf = 0.5, arem = "chi2"))
#This gives 5668 rules
rules_2 <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))
subsets_rule_2 <- which(colSums(is.subset(rules_2, rules_2)) > 1)
rules_2 <- rules_2[-subsets_rule_2]
#After removal, it reduced to 1755 rules


















#####   Converting Data Frame into Transactional data

#Load dataset (AdultUCI)
data("AdultUCI")
class(AdultUCI)

str(AdultUCI)

#If the value of a column is numeric, it cannot be used as the column can take infinite values
#Convert the integer columns to factor columns

#AdultUCI <- lapply(AdultUCI, function(x){as.factor(x)})












###Discretization using the cut() function
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

AdultUCI$age  <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI$"capital-gain" <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf, 0, median(AdultUCI[[ "capital-gain"]][AdultUCI["capital-gain"] > 0 ]),Inf)), labels = c("None", "Low", "High"))
AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))

AdultUCI_mean <-AdultUCI 
AdultUCI_interval <-  AdultUCI
AdultUCI_freq <- AdultUCI 



AdultUCI$"hours-per-week" <- ordered(cut(AdultUCI[["hours-per-week"]],c(0,40,60,101)),labels=c("part-time","full-time", "overtime"))



AdultUCI_mean$"hours_mean" <- ordered(cut(AdultUCI_mean[[ "hours-per-week"]], c(-Inf, 5, mean(AdultUCI_mean[[ "hours-per-week"]][AdultUCI_mean["hours-per-week"] > 0 ]),Inf)), labels = c("parttime", "fulltime", "overtime"))
summary(AdultUCI_mean$hours_mean)


AdultUCI[["fnlwgt"]] <- NULL
AdultUCI_mean[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <-NULL
AdultUCI_mean[["education-num"]] <-NULL
AdultUCI_mean[["hours-per-week"]]<-NULL


###Discretization using the discretize() function
AdultUCI_interval[[ "hours-per-week"]] <- discretize(AdultUCI_interval$"hours-per-week", method ='interval', breaks = 2);
AdultUCI_freq [[ "hours-per-week"]] <- discretize(AdultUCI_freq$"hours-per-week", method ='frequency', breaks = 2);

#Deleting the variables which were disretizied 
AdultUCI_interval[["fnlwgt"]] <- NULL
AdultUCI_freq[["fnlwgt"]] <- NULL
AdultUCI_interval[["education-num"]] <-NULL
AdultUCI_freq[["education-num"]] <-NULL





adultTR <- as(AdultUCI, "transactions")
adultTR_mean <- as(AdultUCI_mean, "transactions")
adultTR_interval <-as(AdultUCI_interval, "transactions")
adultTR_freq <- as(AdultUCI_freq, "transactions")

inspect(head(adultTR))
summary(adultTR)
summary(adultTR_mean)
summary(adultTR_interval)

#Analysis of the transactional data
freqTbl  = itemFrequency(adultTR, type = "relative") #data discretized using the cut() function
str(freqTbl)
summary(freqTbl)
print(freqTbl)

freqTbl_mean  = itemFrequency(adultTR_mean, type = "relative") #data discretized using the mean
str(freqTbl_mean)

freqTbl_interval = itemFrequency(adultTR_interval, type = "relative") #data discretized using the intervals
str(freqTbl_interval)

freqTbl_freq = itemFrequency(adultTR_freq, type = "relative") #data discretized using the frequency
str(freqTbl_freq)

#Sorting elements by the support
freqTbl = sort(freqTbl, decreasing= TRUE)
print(freqTbl)

freqTbl_mean = sort(freqTbl_mean, decreasing= TRUE)
print(freqTbl_mean)


freqTbl_interval = sort(freqTbl_interval, decreasing = TRUE)
print(freqTbl_interval)

freqTbl_freq = sort(freqTbl_freq, decreasing = TRUE)
print(freqTbl_freq)


#Plotting the results
itemFrequencyPlot(adultTR, type ="relative", support= 0.2)
itemFrequencyPlot(adultTR_mean, type ="relative", support= 0.2)
itemFrequencyPlot(adultTR_interval, type ="relative", support= 0.2)
itemFrequencyPlot(adultTR_freq, type ="relative", support= 0.2)



#Mining the frequent itemsets
#In this step we will use the apriori algorithm to compare the mining of the frequent itemsets for the different types of discretization.
#Setting the parameters
aParam  = new("APparameter", "confidence" = 0.6, "support" =0.5, "minlen"= 1, maxtime = 20) 
print(aParam)
aParam@support <- 0.3
aParam@confidence <-0.8
aParam@target ="frequent itemsets"

#Apriori algorithm
asets_user <-apriori(adultTR,aParam) #data discretized using the cut() function
asets_mean <-apriori(adultTR_mean,aParam) #data discretized using the mean
str(asets_user)
str(asets_mean)

asets_interval <-apriori(adultTR_interval,aParam)#data discretized using the interval
asets_freq <-apriori(adultTR_freq,aParam) #data discretized using the frequency
str(asets_interval)
str(asets_freq)

#Comparing the lenght of frequent itemsets
length(asets_user)
length(asets_mean)
length(asets_interval)
length(asets_freq)


#Summary of the results
summary(asets_user)
summary(asets_mean)
summary(asets_interval)
summary(asets_freq)

#Inspecting first 10 rules by support
inspect(head(sort(asets_user, by="support"),10))
inspect(head(sort(asets_mean, by="support"),10))
inspect(head(sort(asets_interval, by="support"),10))
inspect(head(sort(asets_freq, by="support"),10))



#Itemset frequency analysis using the key word
#In this step we will compare the frequent itemsets using 4 different key words - name of columns - that were discretized using a different method
#Hours per week - cut() function 
sets_hours_per_week_user <- subset(asets_user, subset = items %pin% "hours-per-week")
print(sets_hours_per_week_user)
inspect(sort(sets_hours_per_week_user, by="support"))

#Hours mean - mean method
sets_hours_per_week_mean <- subset(asets_mean, subset = items %pin% "hours_mean")
print(sets_hours_per_week_mean)
inspect(sort(sets_hours_per_week_mean, by="support"))

#Hours per week - interval method
sets_hours_per_week_interval <- subset(asets_interval, subset = items %pin% "hours-per-week")
print(sets_hours_per_week_interval)
inspect(sort(sets_hours_per_week_interval, by="support"))

#Hours per week - frequency method
sets_hours_per_week_freq <- subset(asets_freq, subset = items %pin% "hours-per-week")
print(sets_hours_per_week_freq)
inspect(sort(sets_hours_per_week_freq, by="support"))

#plot(asets[1:10])
plot(asets_user[size(asets_user)>3], method = "graph")
plot(asets_user[size(asets_user)>2], method = "paracoord", control = list(reorder = TRUE))

plot(asets_mean[size(asets_mean)>2], method = "graph")
plot(asets_mean[size(asets_mean)>2], method = "paracoord", control = list(reorder = TRUE))

plot(asets_interval[size(asets_interval)>2], method = "graph")
plot(asets_interval[size(asets_interval)>2], method = "paracoord", control = list(reorder = TRUE))

plot(asets_freq[size(asets_freq)>2], method = "graph")
plot(asets_freq[size(asets_freq)>2], method = "paracoord", control = list(reorder = TRUE))



#Rules mining
#Setting the parameters
aParam@target ="rules"
aParam@minlen = 2L
aParam@confidence =0.8

print(aParam)

#Apriori function
aRules_user <-apriori(adultTR,aParam)
aRules_mean <-apriori(adultTR_mean,aParam)
aRules_interval <-apriori(adultTR_interval,aParam)
aRules_freq <-apriori(adultTR_freq,aParam)

###Rules statistics
#cut() function
summary(aRules_user)
length(aRules_user)
str(aRules_user)
inspect(head(sort(aRules_user, by="confidence")))

#Mean method
summary(aRules_mean)
length(aRules_mean)
str(aRules_mean)
inspect(head(sort(aRules_mean, by="confidence")))


#Interval method
summary(aRules_interval)
length(aRules_interval)
str(aRules_interval)
inspect(head(sort(aRules_interval, by="confidence")))


#Frequency method
summary(aRules_freq)
length(aRules_freq)
str(aRules_freq)

inspect(head(sort(aRules_freq, by="confidence")))