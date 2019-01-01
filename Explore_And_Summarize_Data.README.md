# udacity
p 6

title: "Project Explore and Summarize Data"
output:
  html_document: default
  html_notebook: default
  pdf_document: default
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(cache=TRUE)
```

#Loan Data from [Prosper](https://www.prosper.com/)

The data set examined contains data from 113,937 loans with 81 variables on each loan, including loan amount, borrower rate (or interest rate), current loan status, borrower income, borrower employment status, borrower credit history, and the latest payment information.

```{r echo=FALSE, message=FALSE, warning=FALSE, loadingModules}
# Plotting
library(ggplot2)
# Scaling for plots 
library(scales)
# Filtering and grouping
library(dplyr)
# Scatterplot matrices
library(GGally)
# Date manipulation
library(lubridate)
# Tabulate modules
library(memisc)
# Model training
library(caret)
# glmStepAIC Model
library(MASS)
```
```{r echo=FALSE, message=FALSE, warning=FALSE, helperFunctions}
# Functions prints out data frame with two columns, ColumnName and 
# NumberMissingValues with a row for each
# column in df with missing entries.
check_cols_with_na <- function(df) {
  missing_vals <- sapply(df, function(x) sum(is.na(x)))
  missing_vals <- data.frame(ColumnName=names(missing_vals), 
                             NumberMissingValues=missing_vals, row.names = c())
  subset(missing_vals, NumberMissingValues>0)
}
# Function to plot a stacked histogram for LoanOriginationDate 
# with stacked entries for missing and
# non missing entries for the variable given by fieldname.
plot_missing_vs_loanOrigDate <- function(fieldname) {
  ggplot(aes_string("year(LoanOriginationDate)", 
                    fill= paste("is.na(", fieldname,")")), data=loans) +
  geom_histogram(binwidth = 1) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(paste("Dates with missing",fieldname)) +
  xlab("Year of loan origination") +
  scale_x_continuous(breaks=seq(2002,2015,1))
}
# Funtion to calculate ROC curve for probabilistic predictors
# taken form https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/calculate_roc.R
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$DefaultFlag == 1) / sum(df$DefaultFlag == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$DefaultFlag == 0) / sum(df$DefaultFlag == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$DefaultFlag == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$DefaultFlag == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, 
                     function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}
# Function to plot ROC curve from data frame produced by 
# calculate_roc function.
# Taken from https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/plot_roc.R
plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  library(grid)
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point() +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], 
               alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], 
               alpha=0.5, linetype="dashed")
  
}
```

```{r echo=FALSE, message=FALSE, warning=FALSE, loadingdata}
loans <- read.csv("prosperLoanData.csv")
loans_orig <- loans
head(loans)
#str(loans)
```
In this analysis I will try to find variables in the data set that drive the risk of a loan. Therefore I will concentrate on data collected before the origination of a loan.

# Data wrangling

Dates are imported as factors, but are converted to numeric dates. 
```{r echo=FALSE, message=FALSE}
loans$ListingCreationDate <- as.Date(loans$ListingCreationDate)
loans$ClosedDate <- as.Date(loans$ClosedDate)
loans$DateCreditPulled <- as.Date(loans$DateCreditPulled)
loans$FirstRecordedCreditLine <- as.Date(loans$FirstRecordedCreditLine)
loans$LoanOriginationDate <- as.Date(loans$LoanOriginationDate)
str(loans)
```

Ratings are reorganized so, that the grades are ordered from AA (low risk) to HR (high risk). The levels of `LoanStatus` are also reorganized so that the order is more meaningful (but still subjective) for further analyses. The variables `IncomeVerifiable`, `IsBorrowerHomeowner` and `CurrentlyInGroup` are converted to a logical variables.
```{r echo=FALSE, message=FALSE, warning=FALSE}
loans$ProsperRating..Alpha. <- factor(loans$ProsperRating..Alpha., 
                                      c("AA", "A", "B", "C", "D", "E", "HR"))
loans$CreditGrade <- factor(loans$CreditGrade, 
                            c("AA", "A", "B", "C", "D", "E", "HR", "NC"))
loans$LoanStatus <- factor(loans$LoanStatus, 
                           levels=c("Completed", 
                                    "Current", 
                                    "Chargedoff", 
                                    "Defaulted",  
                                    "FinalPaymentInProgress", 
                                    "Past Due (>120 days)", 
                                    "Past Due (1-15 days)", 
                                    "Past Due (16-30 days)", 
                                    "Past Due (31-60 days)", 
                                    "Past Due (61-90 days)", 
                                    "Past Due (91-120 days)",
                                    "Cancelled"))
loans$IncomeVerifiable <- as.logical(loans$IncomeVerifiable)
loans$IsBorrowerHomeowner <- as.logical(loans$IsBorrowerHomeowner)
loans$CurrentlyInGroup <- as.logical(loans$CurrentlyInGroup)
```

The following columns include missing values.
```{r echo=FALSE, message=FALSE, warning=FALSE}
check_cols_with_na(loans)
```

`ClosedDate` is often missing, because loans had not been closed, when the data was recorded.
```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(LoanStatus), data=subset(loans, is.na(ClosedDate))) +
  geom_bar(fill="#4169e1") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Loan status for loans with missing closed date")
```

Many other missing values are connected to the date of origination, because the amount of data collected changed during time. 

`EmploymentStatusDuration` is missing in before 2008.
```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("EmploymentStatusDuration")
```


`CreditScoreRangeLower` is missing for few loans originated prior to 2007.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("CreditScoreRangeLower")
```

`FirstRecordedCreditLine` is missing for some loans originated in 2006. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("FirstRecordedCreditLine")
```

`CurrentCreditLines` is missing for loans originated in 2006 and for some originated in 2007.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("CurrentCreditLines")
```

`TotalInquiries` is missing for some loans originated in 2006.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("TotalInquiries")
```

`AmountDelinquent` is missing for loans originated in 2006 and for some originated in 2007.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("AmountDelinquent")
```

`TotalProsperLoans` is missing for loans for which the borrower does not have previous Prosper loans.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("TotalProsperLoans")
```

`FirstRecordedCreditLine` is missing for some loans originated in 2006.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("FirstRecordedCreditLine")
```

`BankcardUtilization` is missing for loans in 2006 and for few loans in 2007. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_missing_vs_loanOrigDate("BankcardUtilization")
```


To obtain a clean subset of data, entries with missing values for `EmploymentStatusDuration`, `ClosedDate`, `FirstRecordedCreditLine` and `BankcardUtilization` are removed. Because a closing date is required there will be only closed loans in the resulting data set.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Remove entries with missing values for the given fields for 
# the cleaned loans data set.
loans_clean <- subset(loans, 
                      !is.na(EmploymentStatusDuration) &
                        !is.na(ClosedDate) & 
                        !is.na(FirstRecordedCreditLine) &
                        !is.na(BankcardUtilization))
```

The following table shows fields that still include missing values. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
check_cols_with_na(loans_clean)
```

Fields related to previous Prosper loans are missing, if the borrower has no previous Prosper loans, according to the [documentation](https://www.google.com/url?q=https://docs.google.com/spreadsheets/d/1gDyi_L4UvIrLTEC6Wri5nbaMmkGmLQBk-Yx3z0XDEtI/edit?usp%3Dsharing&sa=D&ust=1496705059891000&usg=AFQjCNHMPfI22Q3BdA7io-Z1-maqjiIT7Q). These fields are set to 0 in cases where they are missing for further analyses. Other columns with missing values will be ignored through the rest of this project. The are no further columns with missing entries:

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Function to replace missing values with 0.
replace.na <- function(data) {
  eval.parent(substitute(data[is.na(data)] <- 0))
}
# Set missing values for previous Prosper loans to 0 for further analyses.
replace.na(loans_clean$ProsperPaymentsLessThanOneMonthLate)
replace.na(loans_clean$ProsperPaymentsOneMonthPlusLate)
replace.na(loans_clean$ProsperPrincipalBorrowed)
replace.na(loans_clean$ProsperPrincipalOutstanding)
replace.na(loans_clean$TotalProsperLoans)
replace.na(loans_clean$TotalProsperPaymentsBilled)
replace.na(loans_clean$OnTimeProsperPayments)
replace.na(loans_clean$DebtToIncomeRatio)
# Remove not further needed cols.
loans_clean <- loans_clean[,-which(names(loans_clean) %in% c("CreditGrade",			
        "EstimatedEffectiveYield",		
        "EstimatedLoss",		
        "EstimatedReturn",		
        "ProsperRating..numeric.",		
        "ProsperRating..Alpha.",		
        "ProsperScore",		
        "ScorexChangeAtTimeOfListing",		
        "LoanFirstDefaultedCycleNumber"))]
check_cols_with_na(loans_clean)
```

# Univariate analysis

```{r echo=FALSE, message=FALSE, warning=FALSE}
borrower_fields <- c("BorrowerState", 
                     "Occupation", 
                     "EmploymentStatus", 
                     "EmploymentStatusDuration", 
                     "IsBorrowerHomeowner", 
                     "CreditScoreRangeLower", 
                     "CreditScoreRangeUpper", 
                     "FirstRecordedCreditLine", 
                     "CurrentCreditLines", 
                     "TotalCreditLinespast7years", 
                     "OpenRevolvingAccounts", 
                     "OpenRevolvingMonthlyPayment", 
                     "InquiriesLast6Months", 
                     "TotalInquiries", 
                     "CurrentDelinquencies", 
                     "AmountDelinquent", 
                     "DelinquenciesLast7Years", 
                     "PublicRecordsLast10Years", 
                     "PublicRecordsLast12Months", 
                     "RevolvingCreditBalance", 
                     "BankcardUtilization", 
                     "AvailableBankcardCredit", 
                     "TotalTrades", 
                     "TradesNeverDelinquent..percentage.", 
                     "TradesOpenedLast6Months", 
                     "DebtToIncomeRatio", 
                     "IncomeVerifiable", 
                     "StatedMonthlyIncome", 
                     "TotalProsperLoans", 
                     "TotalProsperPaymentsBilled", 
                     "OnTimeProsperPayments", 
                     "ProsperPaymentsLessThanOneMonthLate", 
                     "ProsperPaymentsOneMonthPlusLate", 
                     "ProsperPrincipalBorrowed", 
                     "ProsperPrincipalOutstanding")
```
Most of the loans are completed. Some a charged off or defaulted. Since both states result in a loss for Prosper I will take flag loans as bad, if they either defaulted or have been charged off. The single cancelled loan is removed from the data set. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(x=LoanStatus, group=LoanStatus), data=loans_clean) +
  geom_bar(fill="#4169e1") +
  geom_text(aes(label = scales::percent(..count../sum(..count..)), 
                y= ..count..),
            stat= "count", vjust = -.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(limits = c(0,60000))
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Remove cancelles loans for the final cleaned loans data set.
loans_clean_fin <- subset(loans_clean, LoanStatus!="Cancelled")
```

There is a gap in loan origination dates. 

```{r echo=FALSE, message=FALSE, warning=FALSE, plot_loanoriginationdate}
ggplot(aes(LoanOriginationDate), data=loans_clean_fin) +
  geom_histogram(binwidth=90, fill="#4169e1") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Loan Origination Date") +
  xlab("Date of loan origination") +
  scale_x_date(date_breaks = "3 month")
```

#Uni- and Multivariate
Now, uni-variate and multi-variate distributions and correlations are analyzed for changing subsets of the borrower related variables, since it is not practical to put all 35 variables in a scatter plot matrix. Variables in a subsets are related to specific properties of borrowers.

## Borrower's life circumstances
`EmploymentStatus`, `EmploymentStatusDuration`, `IsBorrowerHomeowner`,`IncomeVerifiable`, `StatedMonthlyIncome`, `DebtToIncomeRatio`,  `Occupotion` and `BorrowerState` are regarded as variables connected to the life circumstances of the respective borrower.
 `Occupotion` and `BorrowerState` are omitted since these categorical features have too many levels for an interpretable two-variable analysis. None of the investigated variables for the borrower's life circumstances show strong correlations. The plot shows in red completed loans, defaulted loans in blue and charged off loans in green.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), columns = c(
                     "EmploymentStatus", 
                     "EmploymentStatusDuration",
                     "IsBorrowerHomeowner",
                     "IncomeVerifiable", 
                     "StatedMonthlyIncome",
                     "DebtToIncomeRatio"), 
                     lower=list(combo="dot"),
        diag = list(continuous="densityDiag")) 
```

The median stated monthly income is $4250. There are many loans where borrower's stated monthly incomes of 0 that are not verifiable. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = loans_clean_fin, 
       aes(x=StatedMonthlyIncome, fill=IncomeVerifiable)) +
  geom_histogram(binwidth = 500, boundary = 0) +
  scale_x_continuous(limits = c(0,15000), breaks = seq(0,15000,2000)) +
  ggtitle("Distribution of stated monthly income")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(loans_clean_fin$StatedMonthlyIncome)
```

Investors have the highest verifiable mean monthly income (green dashed line).

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.height=30, fig.width=10}
ggplot(data = loans_clean_fin, 
       aes(x=StatedMonthlyIncome, fill=IncomeVerifiable)) +
  geom_histogram(binwidth = 500, boundary = 0) +
  scale_x_continuous(limits = c(0,15000), breaks = seq(0,15000,2000)) +
  ggtitle("Distribution of stated monthly income by occupation") +
  geom_vline(data = loans_clean_fin %>% 
               filter(IncomeVerifiable == FALSE) %>% 
               group_by(Occupation) %>% 
               summarise(mean = mean(StatedMonthlyIncome)),
    aes(xintercept=mean),
    color="red", linetype="dashed", size=1) +
  geom_vline(data = loans_clean_fin %>% 
               filter(IncomeVerifiable == TRUE) %>% 
               group_by(Occupation) %>% 
               summarise(mean = mean(StatedMonthlyIncome)),
    aes(xintercept=mean),
    color="green", linetype="dashed", size=1) +
  facet_wrap(~Occupation, ncol=2, scales = "free")
```

## Credit score
`CreditScoreRangeLower` and `CreditScoreRangeUpper` can be derived from each other. Therefore, `CreditScoreRangeUpper` is removed from the data set, since is does not provide additional information over `CreditScoreRangeLower`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=loans_clean_fin, 
       aes(x=CreditScoreRangeLower, y=CreditScoreRangeUpper)) +
  geom_point(color="#4169e1") +
  ggtitle("Lower vs. Upper Credit Score Range")
```

## Credit lines
Credit line related features are `FirstRecordedCreditLine`, `CurrentCreditLines` and `TotalCreditLinespast7years`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("FirstRecordedCreditLine", 
                    "CurrentCreditLines", 
                    "TotalCreditLinespast7years"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

First recorded credit line is anti-correlated to the other two features. The earlier a customer obtained a credit line the more credit lines he has or had.
Naturally, current credit line and total credit lines in past seven years are correlated. The number of total credit lines in the past seven years  are at least the number of current credit lines. A new variable, `TotalClosedCreditLinesPast7years` is calculated as the difference between the number of total credit lines in the past seven years, `TotalCreditLinespast7years` and the number of current credit lines, `CurrentCreditLines`. The correlation of `CurrentCreditLines` and `TotalClosedCreditLinesPast7years` is half the correlation of `CurrentCreditLines` and `TotalCreditLinespast7years`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
loans_clean_fin$TotalClosedCreditLinesPast7years <- 
  with(loans_clean_fin, TotalCreditLinespast7years-CurrentCreditLines)
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("FirstRecordedCreditLine", 
                    "CurrentCreditLines", 
                    "TotalClosedCreditLinesPast7years"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

## Revolving accounts
`OpenRevolvingAccounts`, `OpenRevolvingMonthlyPayment`, `RevolvingCreditBalance` `BankcardUtilization` and `AvailableBankcardCredit` are variables related to credit card accounts.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("OpenRevolvingAccounts", 
                    "OpenRevolvingMonthlyPayment",
                    "RevolvingCreditBalance",
                    "BankcardUtilization", 
                    "AvailableBankcardCredit"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

Defaulted loans tend to have higher monthly payments for open revolving credits.

```{r  echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=subset(loans_clean_fin,OpenRevolvingMonthlyPayment>0),
       aes(x=OpenRevolvingMonthlyPayment, color=LoanStatus)) +
  geom_density() +
  scale_x_log10() +
  ggtitle("Density of OpenRevolvingMonthlyPayment for defaulted, \ncharged off and completed loans")
```

The correlation between `RevolvingCreditBalance` and `OpenRevolvingMonthlyPayment` becomes obvious when switching to a logarithmic scale for both variables and plotting the binned median revolving credit balance versus the open monthly payments for revolving credits. The correlation on the logarithmic scale is .86.

```{r  echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=subset(loans_clean_fin, 
                   RevolvingCreditBalance>0 & OpenRevolvingMonthlyPayment>0), 
       aes(x=OpenRevolvingMonthlyPayment, 
           y=RevolvingCreditBalance, color=LoanStatus)) +
  scale_x_log10() +
  scale_y_log10() +
  stat_summary_bin(geom="line", fun.y="median") +
  ggtitle("Binned median revolving credit balance vs open monthly payments")
with(subset(loans_clean_fin, 
            RevolvingCreditBalance>0 & OpenRevolvingMonthlyPayment>0),
     cor(log(OpenRevolvingMonthlyPayment), log(RevolvingCreditBalance)))
```

The utilization of bank card revolving credits is bi-modal with peaks at 0 and 100%. In cases of defaulted and charged-off loans the peak at 0 is smaller and higher at 100%.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=subset(loans_clean_fin), 
       aes(x=BankcardUtilization, color=LoanStatus)) +
  geom_density() +
  scale_x_continuous(limits=c(0,2)) +
  ggtitle("Bankcard Utilization in percent")
```

Borrower that completed the repayment of the loan have slightly higher bank card credits.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=loans_clean_fin, 
       aes(x=LoanStatus, y=AvailableBankcardCredit, fill=LoanStatus)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,3000)) +
  ggtitle("Available Bankcard Credit")
```

## Inquiries related features.

`InquiriesLast6Months` and `TotalInquiries` are inquiries related features. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("InquiriesLast6Months",
                    "TotalInquiries"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

To decorrelate `InquiriesLast6Months` and `TotalInquiries` a new variable `InquiriesBefore6Months` is calculated as the difference between `TotalInquiries` and `InquiriesLast6Months`. The correlation decreases from .74 to .47.

```{r echo=FALSE, message=FALSE, warning=FALSE}
loans_clean_fin$InquiriesBefore6Months <- 
  with(loans_clean_fin, TotalInquiries-InquiriesLast6Months)
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("InquiriesLast6Months",
                    "InquiriesBefore6Months"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

Defaulted loans tend to have  more inquiries in the last 6 month.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=loans_clean_fin, 
       aes(x=LoanStatus, y=InquiriesLast6Months, fill=LoanStatus)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,10)) +
  ggtitle("Inquiries in the last 6 month")
```

## Delinquencies related features
Delinquencies related features are `AmountDelinquent`, `CurrentDelinquencies` and `DelinquenciesLast7Years`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(subset(loans_clean_fin, 
               AmountDelinquent>0 | CurrentDelinquencies>0 | 
                 DelinquenciesLast7Years>0),
        mapping = aes(color=LoanStatus), 
        columns = c("AmountDelinquent",
                    "CurrentDelinquencies",
                    "DelinquenciesLast7Years"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

Most of the loans have zero amount delinquent, 84% for completed loans, 71% for charged off loans and 74% for defaulted loans. The distribution of delinquent amounts is comparable for all three investigated loan status.  

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=subset(loans_clean_fin, AmountDelinquent>0), 
       aes(x=LoanStatus, y=AmountDelinquent, fill=LoanStatus)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,100)) +
  ggtitle("Amount delinquent")
loans_clean_fin %>% group_by(LoanStatus) %>% 
  summarise(mean=mean(AmountDelinquent), 
            Percent_Zero_Amount=sum(AmountDelinquent==0)/n())
```

The median number of current delinquencies is a higher for charged off and defaulted loans.

```{r echo=FALSE, message=FALSE, warning=FALSE}
means <- aggregate(CurrentDelinquencies ~  LoanStatus, loans_clean_fin, mean)
ggplot(data=subset(loans_clean_fin, CurrentDelinquencies>0), 
       aes(x=LoanStatus, y=CurrentDelinquencies, fill=LoanStatus)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,12), breaks=seq(1,12)) +
  geom_point(data=means, 
             aes(x=LoanStatus, y=CurrentDelinquencies), color="red") +
  geom_text(data = means, 
            aes(label = sprintf("%0.2f", CurrentDelinquencies), 
                y = CurrentDelinquencies + 0.5)) +
  ggtitle("Current delinquencies")
```

The distributions significantly differ as ANOVA reveals.

```{r  echo=FALSE, message=FALSE, warning=FALSE}
aov_delinq_status <- aov(CurrentDelinquencies ~  LoanStatus, 
                         data=loans_clean_fin)
summary(aov_delinq_status)
confint(aov_delinq_status)
```



# Trade related variables

Trade related variables are `TotalTrades`, `TradesNeverDelinquent..percentage.` and `TradesOpenedLast6Months`.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("TotalTrades", 
                     "TradesNeverDelinquent..percentage.", 
                     "TradesOpenedLast6Months"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

# Prosper loan related features
Prosper loan related features are `TotalProsperLoans`, `TotalProsperPaymentsBilled`, `OnTimeProsperPayments`, `ProsperPaymentsLessThanOneMonthLate`, `ProsperPaymentsOneMonthPlusLate`, `ProsperPrincipalBorrowed` and `ProsperPrincipalOutstanding`. Since the number of on time Prosper payments is limited to the number of billed Prosper payments, the variables `TotalProsperPaymentsBilled` and `OnTimeProsperPayments` are strongly correlated.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(loans_clean_fin, mapping = aes(color=LoanStatus), 
        columns = c("TotalProsperLoans", 
                     "TotalProsperPaymentsBilled", 
                     "OnTimeProsperPayments", 
                     "ProsperPaymentsLessThanOneMonthLate", 
                     "ProsperPaymentsOneMonthPlusLate", 
                     "ProsperPrincipalBorrowed", 
                     "ProsperPrincipalOutstanding"), 
        lower=list(combo="dot"),
        diag = list(continuous="densityDiag"))
```

A new variable `OverdueProsperPaymentsPercent` is calculated as the difference of `TotalProsperPaymentsBilled` and `OnTimeProsperPayments` relative to `TotalProsperPaymentsBilled`. The correlation can be reduced to .07 performing this transformation.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Set OverdueProsperPaymentsPercent to 0 if no payments are billed, otherwise set it the percentage of overdue payments
# wrt. the total number of billed payments.
loans_clean_fin$OverdueProsperPaymentsPercent <- 
  with(loans_clean_fin, 
       ifelse(TotalProsperPaymentsBilled==0, 0,
              (TotalProsperPaymentsBilled-OnTimeProsperPayments) /
                TotalProsperPaymentsBilled))
ggplot(data = loans_clean_fin, 
       aes(x=TotalProsperPaymentsBilled, y=OverdueProsperPaymentsPercent,
           color=LoanStatus)) +
  geom_point() +
  ggtitle("OverdueProsperPaymentsPercent vs TotalProsperPaymentsBilled")
# Calculate correlation of new varibale with TotalProsperPaymentsBilled.
with(subset(loans_clean_fin,TotalProsperPaymentsBilled>0) , 
     cov(TotalProsperPaymentsBilled, OverdueProsperPaymentsPercent))
```

# Logistic Regression

Based in the decorrelated borrower related variables a step-wise logistic regression is performed.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Define target variable
loans_clean_fin$DefaultFlag <- factor(with(loans_clean_fin, as.integer(LoanStatus %in% c("Defaulted", "Chargedoff"))))
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Select numerical and categorical variables for the regression
numerics <- c(
            "InquiriesBefore6Months",
            "InquiriesLast6Months",
            "FirstRecordedCreditLine", 
            "CurrentCreditLines", 
            "TotalClosedCreditLinesPast7years",
            "BankcardUtilization", 
            "AvailableBankcardCredit",
            "TotalTrades", 
            "TradesNeverDelinquent..percentage.", 
            "TradesOpenedLast6Months",
            "EmploymentStatusDuration", 
            "StatedMonthlyIncome", 
            "DebtToIncomeRatio",
            "OpenRevolvingAccounts", 
            "OpenRevolvingMonthlyPayment", 
            "RevolvingCreditBalance", 
            "TotalProsperLoans", 
            "TotalProsperPaymentsBilled", 
            "OverdueProsperPaymentsPercent", 
            "ProsperPaymentsLessThanOneMonthLate", 
            "ProsperPaymentsOneMonthPlusLate", 
            "ProsperPrincipalBorrowed", 
            "ProsperPrincipalOutstanding",
            "LoanOriginalAmount",
            "CreditScoreRangeLower"
)
categorical <- c(
  "BorrowerState", 
  "Occupation", 
  "EmploymentStatus", 
  "IsBorrowerHomeowner", 
  "IncomeVerifiable")
# Define variables for exogene, endogen and all variables.
exogene <- c(numerics, categorical)
endogen <- "DefaultFlag"
allVars <- c(endogen, exogene)
```
```{r echo=FALSE, message=FALSE, warning=FALSE}
# Split the data set in data sets for training (75%) and testing (25%)
set.seed(1)
train_ind <- createDataPartition(loans_clean_fin$DefaultFlag, 
                                 p=0.75, list=FALSE)
train <- loans_clean_fin[train_ind, allVars]
test <- loans_clean_fin[-train_ind, allVars]
```
```{r echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE}
# perform the regression
# During the preprocessing, remove variables with near zero variance, center and scale any numeric variables. 
control <- trainControl(verboseIter = F, number = 1 )
glmModel <- train(DefaultFlag ~ ., 
                  data = train,
                  preProcess = c("center", "scale", "nzv"),
                  method = "glmStepAIC")
saveRDS(glmModel, "glmModel.rds")
```
```{r echo=FALSE, message=FALSE, warning=FALSE}
glmModel <- readRDS("glmModel.rds")
```

The final logistic model is significantly better then randomly selecting with an accuracy of 73%.

```{r echo=FALSE, message=FALSE, warning=FALSE}
test$pred <- predict(glmModel, test)
confusionMatrix(data = test$pred, reference = test$DefaultFlag, positive = "0")
```

The following plot shows the ROC curve on the training data set for the logistic model. The blue line indicates the performance of pure guessing. With a threshold of 0.2 the false positive rate would be at 60%, but the true positive rate would be at 82%.

```{r echo=FALSE, message=FALSE, warning=FALSE}
roc <- calculate_roc(
  data.frame(pred = predict(glmModel, train, type = "prob")[,"1"], 
             DefaultFlag=train$DefaultFlag),
  3, 100, n = 100)
plot_roc(roc, 0.2, 3, 100)
```

The most important variable indicating high risk according to the model is the Credit score followed by the stated income and the original amount of the loan. The higher the score, the lesser the risk, the higher the income, the lesser the risk and the higher the loan amount, the higher the risk, that the borrower is not able to repay the loan.

```{r echo=FALSE, message=FALSE, warning=FALSE}
sort(summary(glmModel)$coefficients[,"Estimate"],decreasing = T)
```

Compared to the Prosper Rating the predicted default probability increases with lower rating grades and the median predicted probabilities for defaulted loans are higher than the median predicted probabilities for completed loans.

```{r echo=FALSE, message=FALSE, warning=FALSE}
# Data frame with prediction from model ans Pospers ratings
cmp_model_rating <- subset(
  data.frame(pred = predict(glmModel, train, type = "prob")[,"1"],
             ProsperRating..Alpha.=loans[rownames(train),]$ProsperRating..Alpha.,
             DefaultFlag=train$DefaultFlag),
  !is.na(ProsperRating..Alpha.))
ggplot(data = cmp_model_rating,
       aes(y=pred, x=ProsperRating..Alpha., 
           color=DefaultFlag, group=DefaultFlag)) +
  geom_jitter(alpha=0.1) +
  geom_line(data = cmp_model_rating %>%
              group_by(ProsperRating..Alpha., DefaultFlag) %>%
              summarise(median=median(pred)),
            aes(y=median, x=ProsperRating..Alpha., color=DefaultFlag))
```

#Final Plots and Summary

There is a gap in loan origination dates for over one year. Prosper had been [shut down by SEC](https://techcrunch.com/2008/11/26/sec-outlines-its-reasoning-for-shutting-down-p2p-lender-prosper/) during this time.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(LoanOriginationDate), data=loans_clean_fin) +
  geom_histogram(binwidth=90, fill="#4169e1") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Loan Origination Date") +
  xlab("Date of loan origination") +
  scale_x_date(date_breaks = "3 month")
```

The data set contains some variables, that strongly correlate. An example is the correlation between the variables `OpenRevolvingMonthlyPayment` and `RevolvingCreditBalance`.

```{r  echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data=subset(loans_clean_fin, 
                   RevolvingCreditBalance>0 & OpenRevolvingMonthlyPayment>0), 
       aes(x=OpenRevolvingMonthlyPayment, 
           y=RevolvingCreditBalance, color=LoanStatus)) +
  scale_x_log10() +
  scale_y_log10() +
  stat_summary_bin(geom="line", fun.y="median") +
  ggtitle("Binned median revolving credit balance vs open monthly payments")
```

A simple logistic model can be derived form the data to discriminate between high and low risk loans. With a threshold of 0.2 the false positive rate would be at 60%, but the true positive rate would be at 82%.

```{r echo=FALSE, message=FALSE, warning=FALSE}
plot_roc(roc, 0.2, 3, 100)
```

# Reflection

The data set contains data about loans originated on the peer-to-peer landing platform Prosper. The observed default rates in the data set are quite high. Prosper provides ratings for each borrower to indicate the chance that the borrower will repay the complete loan. In this analyses is shown, that it is possible to fit a simple logistic model to data to predict defaults of loans. Although the model does perform only fairly it can give additional hints on the risk of each loan in conjunction with Prospers own rating.

To improve the model one could also try to use more sophisticated models like boosted decision trees, neural nets or support vector machines, since simple logistic models to not include cross-effects between variables which these methods can include.
