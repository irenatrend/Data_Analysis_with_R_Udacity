Exploratory Data Analysis by Irena Trendafilova
========================================================

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using
# in your analysis in this code chunk.

# Notice that the parameter "echo" was set to FALSE for this code chunk.
# This prevents the code from displaying in the knitted HTML output.
# You should set echo=FALSE for all code chunks in your file.

library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(lubridate)

```

----------------------------------------------------

```{r echo=FALSE, Load_the_Data}
# Load the Data
ploans <- read.csv('prosperLoanData.csv', sep = ',', stringsAsFactors=FALSE)
```


# Intro
Prosper is the market leader in peer-to-peer lending (a popular alternative to 
traditional loans and investing options). 

Here's how it works: (www.prosper.com)

* **Borrowers** choose a loan amount, purpose and post a loan listing.
* **Investors** review loan listings and invest in loans that meet their criteria.
* **Once the process is complete**, borrowers make fixed monthly payments and 
investors receive a portion of those payments directly to their Prosper account.


**Analysis Process Overview**

**First**, I spent some time to understand the Prosper business process. What 
helped me the most here, was the Prosper website.

**Second**, I focused on clarifying the meaning of each variable. I spent a 
lot of time on understanding variables and values stored in each variable. 

My main idea was to classify them, then eliminate some of the variables that 
will not give me useful info about this data set and  to create new variables. 
Classification and elimination helped me to reduce the number of available 
variables and focus on main features of interest in my analysis.

**Third**, I've analyzed the variables, by asking my self simple questions, 
plotting the variables and understanding how these variables are related.

Here is the summary of my analysis.

----------------------------------------------------

# Summary of the Data Set

This data set contains 113937 loans with 81 variables on each loan:

```{r echo=FALSE, "Data set dimensions"}
dim(ploans) 
```

Here are the names of all the 81 variables:

```{r echo=FALSE, "Variables names"}
names(ploans)  
```

Variables Overview:

```{r echo=FALSE, "Variables Overview"}
str(ploans)  

# Variables Summary Overview:
# summary(ploans)
```

----------------------------------------------------

# Univariate Plots Section

**What's the time interval for loans available in this data set?**
 
Since the company was founded in 2005 and the last updated of this data set was 
on March 11th 2014, I just want to confirm that the range of the Listing 
Creation Dates is inside that time-frame interval.

```{r echo=FALSE, "Range of the Listing Creation Date"}
range(ploans$ListingCreationDate)
```

As we can see the first created listing is on 2005-11-09 and the last one is 
on 2014-03-10, which is the day before this data set was last updated.

During the variables understanding process, I've realized that some of the 
variables are applicable only for loans created after July 2009. So, I was 
curious to know what's the number or percentage of those loans.

**How many loans were created before July 2009?**

```{r echo=FALSE,  "How many loans were created before July 2009"}

date_07012009 <- "2009-07-01"

table(ploans$ListingCreationDate < date_07012009,
      dnn = "Number of loans created prior to July 1st 2009")

table(ploans$ListingCreationDate < date_07012009,
      dnn = "Percentage of loans created prior to July 1st 2009") /
    length(ploans$ListingCreationDate) * 100
```

This means that for 29084 loans or 25.52% of loans records we don't have values 
for some variables (example: ProsperScore, ProsperRating (numeric), 
ProsperRating (Alpha), EstimatedEffectiveYield, EstimatedLoss, EstimatedReturn).

**New variables and data frames**

Here are the  additional variables and helper data frames I've created:

* Variables (ListingCreationYear, MonthlyDebt, EmploymentInfo, 
ListingCategoryName)
* Data frames (loansAfter20090701, loansBefore20090701)

```{r echo=FALSE, "New variables and data frames"}
# Create new variable named Listing Creation Year 
ploans$ListingCreationYear <- year(ploans$ListingCreationDate)
ploans$ListingCreationYear <- as.factor(ploans$ListingCreationYear)

# Create new variable named MonthlyDebt  
ploans$MonthlyDebt <- ploans$DebtToIncomeRatio * ploans$StatedMonthlyIncome


# Create new variable named Listing Category Name 
indexListingCategory <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
valuesListingCategory <- c("Not Available", "Debt Consolidation", 
                           "Home Improvement", "Business", "Personal Loan", 
                           "Student Use", "Auto", "Other", "Baby&Adoption", 
                           "Boat", "Cosmetic Procedure",  "Engagement Ring", 
                           "Green Loans", "Household Expenses",  
                           "Large Purchases", "Medical/Dental", "Motorcycle",
                           "RV", "Taxes", "Vacation", "Wedding Loans")

ploans$ListingCategoryName <- 
  valuesListingCategory[match(ploans$ListingCategory..numeric., 
                              indexListingCategory)]

# unique(ploans$IncomeRange)

# Reorder Income Range values
incomeRanges <- c('Not displayed', 'Not employed',
                  '$0','$1-24,999', '$25,000-49,999',
                  '$50,000-74,999', '$75,000-99,999', 
                  '$100,000+')

ploans$IncomeRange <- factor(ploans$IncomeRange, levels = incomeRanges)


# Create sub sets of data
loansAfter20090701 <- subset(ploans, 
                             ploans$ListingCreationDate > date_07012009)

loansBefore20090701 <- subset(ploans, 
                              ploans$ListingCreationDate < date_07012009)
```

**What's the distribution of loans by year or by quarter?**

The first thing I analyzed was the financial stability of Prosper business. 
How their business was growing and were there any ups and downs between 2005 and 
2014. Let's see how are loans distributed over the years.

```{r echo=FALSE, "Listing Creation Year"}
ggplot(aes(x = ListingCreationYear), data = ploans) + 
  geom_histogram() +  
  ggtitle("Loan Origination by Year") +
  xlab("Loan Creation Year")
```

The number of loans created almost doubled from 2006 to 2007, then dropped a 
little in 2008 and decreased seriously in 2009. Definitely the main reason 
behind this number is the global financial crisis. 
Since 2009 the business started to recover every year. The business prospered at 
2013. I want to mention that we don't have all records for 2014.

**Let's see how loans are distributed by quarters?**

```{r echo=FALSE, "Listing Creation Quarter"}
quarters = unique(ploans$LoanOriginationQuarter) 
quarters = quarters[order(substring(quarters,4,7), substring(quarters,1,2))]

ggplot(aes(x = LoanOriginationQuarter), data = ploans) + 
  geom_histogram() +
  scale_x_discrete(limits= quarters) +
  ggtitle("Loan Origination Quarter") +
  xlab("Loan Origination Quarter") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))  
```

Definitely 2009 was the worst year for the Prosper business. There were no loans 
created in Q1 2009 and very few in Q2 2009. Again, my guess is that the global 
financial crisis was the main reason for those numbers. Q4 2013 was the best 
quarter ever. Even though we don't have all data for Q1 2014 we can see that the 
business is still growing in positive direction.

Here is the first group of questions I asked my self. How much people are 
borrowing? What's the most frequent term? What people borrow money for?

**How much people are borrowing?**

```{r echo=FALSE, "How much people are borrowing"}
ggplot(aes(x = LoanOriginalAmount), data = ploans) + 
  geom_histogram(binwidth=500, color='black') +
  ggtitle("Loan Original Amount") +
  xlab("Loan Original Amount")

summary(ploans$LoanOriginalAmount)
```

The minimum amount people are borrowing is $1000, maximum is $35000. Mean value 
is $8337 and median is $6500. We can see picks on amounts like $5000, $10000, 
$15000, $20000 and $25000 which is expected. Not much people borrowed $30000 or 
$35000 but again we have small picks there too. 

If we change the binwidth to 5000, it clearly appears that the frequency 
globally decreases with the Amount increase. 

```{r echo=FALSE, "How much people are borrowing Trend"}
ggplot(aes(x = LoanOriginalAmount), data = ploans) + 
  geom_histogram(binwidth=5000, color='black') +
  ggtitle("Loan Original Amount") +
  xlab("Loan Original Amount") 
```

**What is the most frequent Term of loan?**

```{r echo=FALSE, "What is the most frequent Term of loan"}
ggplot(aes(x = Term), data = ploans) + 
  geom_histogram() +  
  ggtitle("Loans by Term") +
  xlab("Term") +
  scale_x_discrete(limits= c(unique(ploans$Term)))

table(ploans$Term)

summary(ploans$Term)
```

The most frequent term is 36 months (3years). Only 1614 of the borrowers have 
chosen 1 year and 24545 have chosen 5 years. With 87778 of loans with 3 years 
term, surely that's the most frequent choice.

**Let'��s see what people borrow money for?**

```{r echo=FALSE, "The Purpose of the Loans"}
ggplot(aes(x = ListingCategoryName), data = ploans) + 
  geom_histogram(color='black') +
  ggtitle("What people borrow money for?") +
  xlab("The Purpose of the Loans") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1)) 

```

The leading loan category is Debt consolidation.

Again I want to check is there any relationship between the loan creation date 
and category being empty.

```{r echo=FALSE, "Loans without ListingCategory" }
loansWithoutListingCategory <- 
  subset(ploans, 
         ListingCategory..numeric. == '0')[c('ListingCreationDate', 
                                             'ListingCreationYear', 
                                             'ListingCategory..numeric.')]

table(loansWithoutListingCategory$ListingCreationYear)
```

Most of the loans without Listing category are created in 2005, 2006 and 2007, 
only 20 loans are created from 2011 to 2014.


Next group of questions I asked my self are: How much borrowers earn? What's the 
Monthly Loan Payment?How the Debt to Income is distributed? How is the Monthly 
Debt distributed? Whats the number of homeowners?


**How much borrowers earn annually?**

```{r echo=FALSE, "Annually income ranges of the borrowers"}
ggplot(aes(x = IncomeRange), data = ploans) + 
  geom_histogram(fill = "#ABADA6") +
  geom_text(stat="bin", aes(label = ..count..)) +
  ggtitle("Annually income ranges of the borrowers")  +
  xlab("Income Range") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))
```

Most of the borrowers have income somewhere between $25,000 and $74,999. 
The number of borrowers who doesn't belong to any income range is low 
(7741 without info) and only 806 are not employed.


**Monthly Loan Payment**

```{r echo=FALSE, "How Monthly Loan Payment is distributed"}
ggplot(aes(x = MonthlyLoanPayment), data = ploans) + 
  geom_histogram(fill = "#ABADA6", color='black', binwidth = 10) +
  ggtitle("Monthly Loan Payment of the borrowers") +
  xlab("Monthly Loan Payment")

summary(ploans$MonthlyLoanPayment)

ggplot(aes(x = sqrt(MonthlyLoanPayment)), data = ploans) + 
  geom_histogram(fill = "#ABADA6", color='black', binwidth = 1) +
  ggtitle("Monthly Loan Payment of the borrowers") +
  xlab("sqrt(Monthly Loan Payment)")
```

Borrowers mean monthly loan payment is $272.5. Median value is $217.7. 
Monthly Loan Payment had long-tailed distributions and so I've ploted the 
square root and the distribution looked more normal.

**How the Debt to Income is distributed?**

Let'��s draw the histogram to get a better understanding of DIT(Debt to Income
) Ratio?.

```{r echo=FALSE, "How the Debt to Income is distributed"}
ggplot(aes(x = DebtToIncomeRatio), 
       data = subset(ploans, DebtToIncomeRatio < 1)) + 
  geom_histogram(fill = "#ABADA6", color='black', binwidth = 0.01) +
  ggtitle("Debt To Income Ratio of the borrowers") +
  xlab("Debt To Income Ratio")

summary(ploans$DebtToIncomeRatio)
```

For better visualization I've plotted only loans with DebtToIncomeRatio lower 
than 1. Most of the borrowers are trying to keep their Debt to Income Ratio 
bellow 0.32 (3rd quarter value).


**How is the Monthly Debt distributed?**

I've created new variable named Monthly Debt by multiplying the 
DebtToIncomeRatio and StatedMonthlyIncome.

```{r echo=FALSE, "How is the Monthly Debt distributed"}
ggplot(aes(x = MonthlyDebt), data = subset(ploans, MonthlyDebt < 5000)) +
  geom_histogram(fill = "#ABADA6", color='black', binwidth = 70) +  
  ggtitle("Monthly Debt") +
  xlab("Monthly Debt") +
  scale_x_continuous(breaks=seq(0, 5000, 500))

summary(ploans$MonthlyDebt)
summary(subset(ploans, MonthlyDebt < 5000)$MonthlyDebt)

```

Mean monthly debt for all record in this data set is 1225. It's obvious that 
the max value of 171000 is outlier.The resulting distribution for loans with 
MonthlyDebt < 5000 is positively skewed with mean monthly debt of 1205. 


**Whats the number of homeowners vs one that don't own home?**

```{r echo=FALSE, "Whats the number of home owner vs one that don't own home" }
table(ploans$IsBorrowerHomeowner,
      dnn = "Is Borrower Homeowner")
```

More than half are homeowners.57478 are owning home and 56459 are not home 
owners.


Next, I focused on borrower's employment status and occupation. How borrower 
employment duration and employment status are related with the loans 
applications? What'��s the most frequent Occupation of borrowers? Whic
h are the top 10 Occupations between Prosper loans borrowers?

**When does a person apply for a loan in terms of the length of their 
working status?**

```{r echo=FALSE, "Employment Status Duration overview" }
ggplot(aes(x = EmploymentStatusDuration/12), data = ploans) + 
  geom_histogram(binwidth=1, fill = "#ABADA6", color='black') +
  ggtitle("Employment Duration of the borrowers") +
  xlab("Employment duration (years)") +
  theme(text = element_text(size=12))
```

With the increase of the length of employment there is a very clear descending 
trend of the number of people who borrow loans. Probably the longer one works, 
the more one earns and is less likely to borrow loan.

**What's the Employment status of the borrowers?**

```{r echo=FALSE, "What is the Employment status of the borrowers" }
ggplot(aes(x = EmploymentStatus), data = ploans) + 
  geom_histogram(fill = "#ABADA6") +
  geom_text(stat="bin", aes(label = ..count..)) +
  ggtitle("Employment Status of the borrowers")  +
  xlab("Employment Status") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))
```

I've decided to create new variable to store whether one is employed or not. 
So everyone who has Employment Status as Employed, Full-time, Part-time or 
Self-employed I marked as employed, and all the other as unemployed.

```{r echo=FALSE, "Employment Info Variable"}
ploans$EmploymentInfo <- ifelse(ploans$EmploymentStatus == 'Employed' |  
                                  ploans$EmploymentStatus == 'Full-time' | 
                                  ploans$EmploymentStatus == 'Part-time' | 
                                  ploans$EmploymentStatus == 'Self-employed', 
                                c("employed"), 
                                c("unemployed")) 

table(ploans$EmploymentInfo)
```

100899 of the loans have employed borrowers and only 13038 have not employed or 
no info for the employment status. This indicates that usually employed people 
are borrowing money.

**What'��s the Occupation of borrowers in this dataset?**

For this purpose I've created new data frame named top10Occupations. This data 
set contains all loans where the occupation of the borrower is one of the top 
10 occupations (Professional,  Computer Programmer, Executive, Teacher, 
Administrative Assistant, 
Analyst, Sales - Commission, Accountant/CPA, Clerical and Sales - Retail).

```{r echo=FALSE, "Top 10 Occupation" }
groupByOccupation <- group_by(ploans, Occupation)
borrowersByOccupation <- summarise(groupByOccupation, count = n())
borrowersByOccupation <- 
  borrowersByOccupation[order(-borrowersByOccupation$count),]

# Get the Top 10 Values for Occupation
head(borrowersByOccupation, 10)

# Ignore emoty and Other Occupations
borrowersByOccupation <- subset(borrowersByOccupation, 
                                Occupation != "Other" 
                                & Occupation != "")

# Get the Top 10 Values for Occupation
head(borrowersByOccupation, 10)

occupations <- unique((head(borrowersByOccupation, 10))$Occupation)

top10Occupations <- subset(ploans, 
                           (Occupation %in% occupations))
```

It looks like the data is incomplete for 28617 loans. 28617 loans have Other as 
value for occupation. 

I want to check is there any relationship between the listing creatin year, 
employment status and occupation being empty.

```{r echo=FALSE, "Loans without Occupation" }
loansWithoutOccupation <- 
  subset(ploans, 
         Occupation == '')[c('Occupation',
                       'EmploymentStatus',
                       'ListingCreationYear')]

table(loansWithoutOccupation$ListingCreationYear)

table(loansWithoutOccupation$EmploymentStatus)
```

Based on the numbers occupation is empty for loans created between 2005 and 2007 
where emploment status is empty, and for loans created in 2013 and 2014 when the 
employment status is Other.


I've excluded empty occupations and Other occupation option.

**Let's plot the top 10 occupation.**

```{r echo=FALSE, "Occupation of the borrowers"}
ggplot(aes(x = Occupation), data = top10Occupations) + 
  geom_histogram(fill = "#ABADA6") +
  geom_text(stat="bin", aes(label = ..count..)) +
  ggtitle("Top 10 Occupations of the borrowers") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))
```

Occupation named Professional is the leader here.


Next, I've analyzed in which states borrowers live and which are the top 10 
States?

**In which states borrowers live?**

I was very curious to analyse in which states borrowers live? Which are the top 
10 States? 

```{r echo=FALSE, "Top 10 States" }
groupByBorrowerState <- group_by(ploans, BorrowerState)
borrowersByState <- summarise(groupByBorrowerState, count = n())
borrowersByState <- borrowersByState[order(-borrowersByState$count),]

states <- unique((head(borrowersByState, 11))$BorrowerState)

top10States <- subset(ploans,  (BorrowerState %in% states))
```

Based on the numbers in this table we can see that the TOP 5 states with 
the largest numbers of borrowers are California, Texas, New York, Florida and 
Ilinois.

There are 5515 loans without state. I had some feeling that probably there is 
correlation between Listing Creation Date and State being empty. Let's explore 
that. Let's see when those 5515 loans are created. For that purpose I've created 
new data frame called loansWithoutState which contains the loans where the state 
is empty. 

```{r echo=FALSE, "Loans without State" }
loansWithoutState <- subset(ploans, 
                            BorrowerState == '')[c('ListingCreationDate', 
                                                   'ListingCreationYear', 
                                                   'BorrowerState')]

loansWithoutState$ListingCreationDate <- 
  as.Date(loansWithoutState$ListingCreationDate)

loansWithoutState <- 
  loansWithoutState[rev(order(loansWithoutState$ListingCreationDate)),]

table(loansWithoutState$ListingCreationYear)
```

All of the loans without borrower state info are created before 2009. It looks 
like the Borrower State info had become required field value after 2009.

**Let's plot the top 10 states.**

```{r echo=FALSE, "Top 10 States Plot" }
ggplot(aes(x = BorrowerState), data = subset(top10States, BorrowerState != '')) + 
  geom_histogram() +
  ggtitle("TOP 10 States") +
  theme(text = element_text(size=10)) +
  xlab("Borrower State")
```

On this plot we can see that California is leader followed by Texas, New York, 
Florida, Illinois and Georgia with more than 5000 Prosper loans.

Next I've analyzed some Prosper values and other values that I was curious to 
learn about.

**Prosper Ratings**

Every loan application is assigned a Prosper Rating, which allows to maintain 
consistency in the evaluation process. Prosper Ratings allow potential investors 
to easily consider a loan application's level of risk because the rating 
represents an estimated average annualized loss rate range to the 
investor. (www.prosper.com)


Prosper Rating - Estimated Avg. Annual Loss Rate:

* AA - 0.00 - 1.99% 
* A  - 2.00 - ��3.99% 
* B  - 4.00 - ��5.99% 
* C  - 6.00 - ��8.99% 
* D  - 9.00 - ��11.99% 
* E  - 12.00 - ��14.99% 
* HR >= 15.00% 


**What'��s the Prosper Rating for the borrowers in this dataset?**

```{r echo=FALSE, "Prosper Rating Alpha of the borrowers"}
ggplot(aes(x = ProsperRating..Alpha.), data = ploans) + 
  geom_histogram(fill = "#ABADA6") +
  geom_text(stat="bin", aes(label = ..count..)) +
  ggtitle("Prosper Rating of the borrowers")  +
  xlab("Prosper Rating") +
  scale_x_discrete(limits= c('', 'AA', 'A', 'B','C', 'D', 'E', 'HR'))
```

Most of the borrowers have prosper rating between A and D. AA is the most rare 
Prosper Rating 

All 29084 loans that have no Prosper Rating are the one created before 
July 1st 2009.

**Let's explore the Prosper Score of borrowers in this dataset?**

```{r echo=FALSE, "Prosper Score of the borrowers"}
ggplot(aes(x = ProsperScore), data = ploans) + 
  geom_histogram() +  
  ggtitle("Prosper Score of the borrowers") + 
  scale_x_continuous(breaks=seq(0, 12, 1)) +
  xlab("Prosper Score")

summary(ploans$ProsperScore)
```

Prosper scores are normally distributed mean=5.95 and median 6.

**What is the min, max, median and mean of the other Prosper Loans 
borrowers have?**

```{r echo=FALSE, "What is the min, max, median and mean"}
ggplot(aes(x = TotalProsperLoans), data = ploans) + 
  geom_histogram() +
  ggtitle("Total Number of existing Prosper Loans of the Borrowers") +
  xlab("Existing Prosper Loans") +
  theme(text = element_text(size=12)) +
  scale_x_discrete(breaks=seq(0, 8, 1))



summary(ploans$TotalProsperLoans)
```

As we can see borrowers have max 8 other additional Prosper loans, and mostly 
less than 2 additional prosper loans.


**How many lenders usually are sharing the risk together?**

```{r echo=FALSE, "Investors"}
summary(ploans$Investors)
```

We can notice that there is huge difference between 3rd Qu. and Max. value, so 
it looks like the Max value is outlier. Mean number of investors is around 
80.48 and median 44. 


**How many loans have recommendations?**

```{r echo=FALSE, "How many loans have recommendations"}
table(ploans$Recommendations)
```

109678 loans don't have recommendations. Recommendations are not so popular in 
the loans applications.


**How lenders benefit from investing in loans?**

```{r echo=FALSE, "The Lender Yield on the Loan"}
summary(ploans$LenderYield)

ggplot(aes(x=LenderYield), data=ploans) + 
    geom_histogram(binwidth=0.005, fill = "#ABADA6", color='black') +
    scale_x_continuous(breaks=seq(0, 0.5, 0.05)) + 
    labs(title="The Lender Yield on the Loan") +
    xlab("Lender Yield")
```

Most yields is between 0.05 and 0.35. The highest peak in the graphic is 
around 0.31.

----------------------------------------------------

# Univariate Analysis


### What is the structure of your dataset?

For the purpose of this project I am using the Prosper data set provided as part 
of this course, which contains all Prosper loans created until March 11th, 2014. 
It's tidy data sets, since each variable is a column and each row is an 
observation. There are discrete and continuous variables.

```{r echo=FALSE, "Browse Data"}
dim(ploans)
```

This data set contains 113937 loans with 81 variables on each loan.


### What is/are the main feature(s) of interest in your dataset?

In my analysis the main features of interest are:

*  **Number of investors** which can help me to determinate which loans are 
attractive for investing. What are the main characteristics of those loans?
*  **Borrower rate** is another variable that is my feature of interest. I 
really want to learn when borrower rate is lower and when higher.
*  **Listing Creation Date** which can help me to understand more about the 
Prosper business and how loans are distributed over the years.

Prosper Score, Income Range, Loan amount, Loan Category and Term  are also 
part of my main features group.


### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

Other variables that can help me to support my investigation are Credist Score, 
Debt To Income Ratio, Prosper Rating, Occupation, Employment Status and 
Employment Duration.


### Did you create any new variables from existing variables in the dataset?

I created following new variables during the analysis: ListingCretionYear, 
MonthlyDebt, EmploymentInfo, ListingCategoryName.

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?


If I was not aware of the global financial crisis in 2009 for me the high 
decrease of the number of the loans would be unusual distribution. I didn't 
face with any surprising about the distributions during the initial analysis.
  
I set ListingCreationYear variable as a factor so when I plotted it would look 
discrete. I've alose taked care of ranked variables order and variables types.

----------------------------------------------------

# Bivariate Plots Section

I was very curious to learn which loans are attractive for investors? When more 
people are sharing the risk for one loan? So I've decided to plot couple of 
values vs Number of investors.

## Number of Investors Analysis

**Are Number of Investors and Prosper Score related. Do we have more investors if Prosper Score is higher?**

```{r echo=FALSE, "Prosper Score vs Number of Investors"}
table(loansAfter20090701$ProsperScore,
      dnn="Number of loans per prospre score")

ggplot(aes(x = as.factor(ProsperScore), y = Investors), 
       data = loansAfter20090701) + 
  geom_boxplot() +
  ggtitle("Prosper Score vs Number of Investors") +
  xlab("Prosper Score")

```

Let's try to plot mean investors value for each prosper score and see how 
it looks like.

```{r echo=FALSE, "Prosper Score vs Mean(Number of Investors)"}
prosperscore_groups <- group_by(loansAfter20090701, ProsperScore)

loansAfter20090701.investors_by_prosperscore = summarise(prosperscore_groups,
          investors_mean = mean(Investors),
          investors_median = median(Investors),
          n = n())

ggplot(aes(x = ProsperScore, y = investors_mean), 
       data = loansAfter20090701.investors_by_prosperscore) + 
  geom_line() + 
  scale_x_discrete(breaks=seq(0, 12, 1)) +
  ggtitle("Prosper Score vs Mean (Number of Investors)") +
  xlab("Prosper Score") +  
  ylab("Mean (Investors)")
```

The number of investors is growing when prosper score is higher. The peak is 
reached for prosper score equal to 10. In this case I've used the subset of all 
loans created after July 1st 2009 (loansAfter20090701) since this property was 
not available for loans created before that date.


**Is there any relationship between Loan Original Amount and Number of 
Investors**

```{r echo=FALSE, "Loan Amount vs Number of Investors plot"}
ggplot(aes(x = LoanOriginalAmount, y = Investors), data = ploans) + 
  geom_point(alpha = 1/20) +
  geom_smooth(method = 'lm', color = 'red') +
  ggtitle("Loan Original Amount vs Number of Investors") +
  xlab("Loan Original Amount")
```  

Larger loans have more investors. Again we can see picks for amount values of 
5000, 10000, 15000, etc...


**Are Number of Investors and Number of Prosper Loans related? Do we have more 
investors if the Borrower already have Prosper Loans**

```{r echo=FALSE, "TotalProsperLoans vs Number fo Investors" }
ggplot(aes(x = as.factor(TotalProsperLoans), y = Investors), 
       data = subset(ploans, !is.na(TotalProsperLoans))) + 
  geom_boxplot() +
  ggtitle("Total Number of existing Prosper Loans vs Number of Investors") +
  xlab("Total Number of existing Prosper loans")

summary(ploans$TotalProsperLoans)
```

The number of investors on one loan is higher if the borrower have less existing 
prosper loans. 

I would love to create one more plot, with the mean values of the Total prosper 
loan values.

```{r echo=FALSE, "TotalProsperLoans vs Number fo Investors - v2" }
totalprosperloans_groups <- group_by(subset(ploans, !is.na(TotalProsperLoans)), 
                                     TotalProsperLoans)

ploans.investors_by_totalprosperloans = summarise(totalprosperloans_groups,
          investors_mean = mean(as.numeric(Investors)),
          investors_median = median(as.numeric(Investors)),
          n = n())

head(ploans.investors_by_totalprosperloans, 11)

ggplot(aes(x = TotalProsperLoans, y = investors_mean), 
       data = ploans.investors_by_totalprosperloans) + 
  geom_line() + 
  geom_smooth(method = 'lm', color = 'red')  +
  ggtitle("Total Prosper Loans vs Mean (Number of Investors)") +
  xlab("Total Prosper Loans") +
  ylab("Mean (Investors)") +
  scale_x_discrete(breaks=seq(0, 9, 1))  
```

On this plot we can see the same trend as on the previous plot, where more 
existing prosper loans leads to lower mean value of number of investors.

```{r echo=FALSE, "Estimated Loss vs Investors" }
ggplot(aes(x = EstimatedLoss, y = Investors), data = loansAfter20090701) + 
  geom_point(alpha = 1/20) +
  ggtitle("Estimated Loss vs Number of Investors") +
  xlab("Estimated Loss") 

cor.test(ploans$EstimatedLoss, ploans$Investors, method = "pearson")

```

Smaller estimated loss is always more attractive for investing. That's why 
smaller estimated loss values usually have more investors.


## Laon Amount Analysis

Next, I was curious to learn is there any relationship between the loan amount 
and other values in this data set. Let's see couple of them.


**How borrowed loan amount vary trought the years?**

```{r echo=FALSE, "Year/Amount plot" }
ggplot(aes(x=as.character(ListingCreationYear), y=LoanOriginalAmount), 
       data=ploans) +
    geom_boxplot() +
  ggtitle("Listing Creation Year vs Loan Original Amount") +
  xlab("Listing Creation Year") +
  ylab("Loan Original Amount")

by(ploans$LoanOriginalAmount, ploans$ListingCreationYear, summary)

```

Mean loans amounts decreased in 2009 to 4393 and reached the top value in 
2013/2014 (10610/11920).

**How loan amount and term are related? Let's plot them.**

```{r echo=FALSE, "Term/Amount plot"}
ggplot(aes(x = as.character(Term), y = LoanOriginalAmount), 
       data = ploans) + 
  geom_boxplot() +
  ggtitle("Term vs Loan Original Amount") +
  xlab("Term") +
  ylab("Loan Original Amount")

by(ploans$LoanOriginalAmount, ploans$Term, summary)
```

When the term is longer then the loan amounts are larger. Median and mean values 
are growing as the term is growing. 

**Let's see how loan amount is related to the IncomeRange?**

```{r echo=FALSE, "Income Range vs Loan Amount"}
ggplot(aes(x = IncomeRange, y = LoanOriginalAmount), data=ploans) +
    geom_boxplot() +
  ggtitle("Income Range  vs Loan Amount") +
  xlab("Income Range") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1)) +
  ylab("Loan Original Amount")
```

Larger loans on average are related to larger incomes.


**Let's see how the Employment Status is related to the loan amount.**

For this purpose we are going the use the newly created variable EmploymentInfo.

```{r echo=FALSE, "Employment Status vs Loan Amount"}
ggplot(aes(x = EmploymentInfo, y = LoanOriginalAmount), data=ploans) +
    geom_boxplot() +
  ggtitle("Employment Status  vs Loan Original Amount") +
  xlab("Employment Info") +
  ylab("Loan Original Amount") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))
```

Employed people are borrowing more money then unemployed.


## Other Variables Analysis

**Monthly Debt vs Current Credit Lines**

Let's see how number of credit lines and monthly debt are related. 

Current Credit Lines

```{r echo=FALSE, "CurrentCreditLines Summary"}
summary(ploans$CurrentCreditLines)
```

MonthlyDebt

```{r echo=FALSE, "MonthlyDebt Summary"}
summary(ploans$MonthlyDebt)
```


```{r echo=FALSE, "CurrentCreditLines"}
ggplot(aes(x = CurrentCreditLines, y = MonthlyDebt), 
       data = subset(ploans, MonthlyDebt != 171000)) + 
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420')) +
  geom_smooth(method = 'lm', color = 'red') +
  xlim(0,60) +
  ylim(0,8000) +
  ggtitle("Monthly Debt vs Current Credit Lines") +
  xlab("Monthly Debt") +
  ylab("Current Credit Lines")

cor.test(ploans$CurrentCreditLines, ploans$MonthlyDebt, method = "pearson")
```

As the number of credit lines is increased, debt is growing. We have positive 
correlation of 0.47. 

----------------------------------------------------

# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?  

The number of investors is growing when the prosper score is higher, loan amount 
is bigger, borrowers have less existing prosper loans, estimated loss is lower.

The mean loan amount vary trough years. The lowest mean was $4393 in 2009 and 
the top mean value is in 2013/2014 ($10610/$11920)
Higher loan amounts have longer term, and they are related to employed borrowers 
with larger incomes.


### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?

I've observed that employed people are borrowing more money and as the number of 
credit lines is increasing, debt is growing.


### What was the strongest relationship you found?
The strongest relationship I found was between number of credit lines and the 
newly created variable MonthlyDebt (r^2 = 0.47,  positive correlation of 0.47).  

----------------------------------------------------

# Multivariate Plots Section

In this section, I want to learn more about the Borrower Rate. When we have 
lower or higher borrower values?


**Borrower Rate / Loan Amount / Prosper Rating**

```{r echo=FALSE, "Borrower Rate vs Loan Amount vs Prosper Rating"}
ggplot(aes(x=BorrowerRate, y = LoanOriginalAmount), data = loansAfter20090701) +
  geom_point(alpha = 1/20) +
  ggtitle("Borrower Rate vs Loan Amount (by Prosper Ratings)") +
  ylab("Loan Amount") +
  xlab("Borrower Rate") +
  facet_wrap(~ProsperRating..numeric., ncol=3,  scales = "free") +
  ylim(0, 35000)

# table(loansAfter20090701$ProsperRating..numeric.)
```

Loans with higher prosper rating have lower Borrower Rate. Borrowers with lower 
Prosper Rating usually are borrowing smaller amounts with higher rate. For 
better visualization I've used only loans created after 7/1/2009 because Prosper 
Rating was not available for loans before that date.


**Borrower Rate / Loan Amount / Prosper Rating by Income Range**

```{r echo=FALSE, "L.Amount vs B.Rate vs Prosper Rating by Income Range" }
ggplot(aes(x = BorrowerRate, y = LoanOriginalAmount), 
       data = loansAfter20090701) +
  geom_point(aes(color=ProsperRating..numeric.)) +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("B.Rate vs L.Amount vs Prosper Rating (by Income Range)") +
  xlab("Borrower Rate") +
  ylab("Loan Original Amount") +
  facet_wrap(~IncomeRange, ncol=3,  scales = "free") +
  xlim(0, 0.4) +
  ylim(0, 35000)
```

On these plots we can see how higher income range and higher Prosper rating 
leads to lower borrower rate. Bigger income range mean more loans with higher 
amount We can observe that as risk is lower the rate is smaller. For better 
visualization I've used only loans created after 7/1/2009 because 
Prosper score was not available for loans before that date.


**Borrower Rate / Income Range by Is Borrower Homeowner**

```{r echo=FALSE, "Income Range  vs Borrower Rate vs Homeowners"}
ggplot(aes(x = as.factor(IncomeRange), y = BorrowerRate), 
       data = subset(ploans, 
                     IncomeRange != 'Not employed' & 
                       IncomeRange != 'Not displayed'& 
                       IncomeRange != '$0')) +
  geom_boxplot(aes(fill = IsBorrowerHomeowner)) +
  ggtitle("Income Range vs Borrower Rate (by Home Owner)") +
  guides(fill = guide_legend(title = "Is Borrower Homeowner")) +
  xlab("Income Range") +
  ylab("Borrower Rate")  +
   theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))

```

What we can observer from this graph is that borrowers with higher income 
who are home owners have lower borrower rates. For better visualization I've 
plotted the subset of data of all records that belonge to any valid 
income range (range > $0). 

**Borrower Rate / Loan Amount / Prosper Score**

```{r echo=FALSE, "Borrower Rate vs Loan Amount by Prosper Score"}
ggplot(aes(x=BorrowerRate, y = LoanOriginalAmount), data = loansAfter20090701) +
  geom_point(alpha = 1/20) +
  ggtitle("Borrower Rate vs Loan Amount vs  for different Prosper Scores") +
  ylab("Loan Amount") +
  xlab("Borrower Rate")  +
  facet_wrap(~ProsperScore, ncol=3,  scales = "free")  +
  xlim(0, 0.4) +
  ylim(0, 35000)

summary(ploans$ProsperScore)
```

On this plot we can observe that higher Prosper Score leads to lower Borrower 
rate. For better visualization I've plotted the subset of loans created 
after 7/1/2009 because Prosper score was not available for loans before 
that date. 


----------------------------------------------------

# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

* Lower borrower rates is usually related to higher credit score, higher prosper 
score, higher prosper rating and owning home.
* Borrowers with lower Prosper Rating usually are borrowing small amounts with 
very high rate. 
* Most of the borrowers have credit scores around 700.
* Higher income range and higher Prosper Rating leading to lower Borrower Rate.


### Were there any interesting or surprising interactions between features?

I think that most of the resualts I've observd were expected. Lower risk leads 
to lower borrower rate and higer interest for investig.   

### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.

------

# Final Plots and Summary

### Plot One

```{r echo=FALSE, "Plot_One"}
ggplot(aes(x = ListingCategoryName), data = ploans) + 
  geom_histogram(color='black') +
  ggtitle("What people borrow money for?") +
  xlab("The Purpose of the Loans") +
  ylab("Number of loans") +
  theme(text = element_text(size=12), 
        axis.text.x = element_text(angle=90, vjust=1))  

# table(ploans$ListingCategoryName)
```

### Description One

On this plot we can see that the larger part of the loans are classified under 
debt consolidation. 58308 loans or around 51% are loans under debt consolidation 
category. 

I've invastigated why this percent is so big, and I found that there are couple 
of reasons why people are borrowing money on Prosper for debt consolidation:

* Consolidating other high interest loans into one Prosper debt consolidation 
loan with a great rate, which leads to saving on amount of interest charged on 
other debts each month
* Prosper debt consolidation loans are so popular since they have a fixed 
interest rate
* Prosper offers only unsecured loans, so borrowers have no need to own home for 
debt consolidation
* To avoid bad credit, and maintain good credit score


### Plot Two

```{r echo=FALSE, Plot_Two}
ggplot(aes(x = as.factor(ProsperScore), y = Investors), 
       data = loansAfter20090701) + 
  geom_boxplot() +
  ggtitle("Prosper Score vs Number of Investors") +
  xlab("Prosper Score") +
  ylab("Number of Investors")

# Used to analyse numbers for each Prosper Score
#by(ploans$Investors, ploans$ProsperScore, summary)
```

### Description Two

On this plot we can see trend of how higher prosper score attract more 
investors to invest in one loan. We can also observer how the mean and median 
values are growing as Prosper score is growing and the max is achived for 
Prosper score of 10. The 3rd quartile number for each Prosper score is less 
then 250 investors. We can notice four outliers which are loans with number 
of investors bigger than 1000. The highest difference between number of 
investors between 1st and 3rd quartile is for credit score of 10 where we have 
value of 1 investor for the 1st quartile and value of  206 for 
3rd quartile. In the future if I deceide to invest some money using Prosper 
website The information that more people are investing in loans with higher 
Prosper Score, can make influance on my decision  to deceide which loans should 
I consider as good for investing. On this plot I've plotted subset of all loans 
created after July 1st 2009 (loansAfter20090701).


### Plot Three
```{r echo=FALSE, Plot_Three}
ggplot(aes(x = as.factor(ProsperScore), y = BorrowerRate), 
       data=loansAfter20090701) +
  geom_boxplot(aes(fill = IsBorrowerHomeowner)) +
  ggtitle("Prosper Score vs Borrower Rate by Home Owner") +
  guides(fill = guide_legend(title = "Is Borrower Homeowner")) +
  xlab("Prosper Score") +
  ylab("Borrower Rate") 


# Plot Three Option 2
# prosperscore_br_ho <- group_by(ploans, ProsperScore, IsBorrowerHomeowner)

#borrower_rate_by_ps_ho = summarise(prosperscore_br_ho,
#          rate_mean = mean(BorrowerRate),
#          rate_median = median(BorrowerRate),
#          n = n())

#ggplot(aes(x = ProsperScore, y = rate_mean), 
#       data = borrower_rate_by_ps_ho) + 
#  geom_line(aes(colour = IsBorrowerHomeowner)) + 
#  ggtitle("Prosper Score vs Mean (Borrower Rate)") +
#  xlab("Prosper Score") +
#  ylab("Mean (Borrower Rate)") +
#  scale_x_discrete(breaks=seq(0, 11, 1))
```

### Description Three
On this plot we can see trend of how  higher Prosper scores and owning home
leads to lower borrower rate. It's good to have this information in case I need 
to borrow money using Prosper web site in the future. Owning home will provide 
me better borrower rate. For better visualization I've used the data subset 
loansAfter20090701, since Prosper Score was not available for loans before that 
date.


------------------------------------------------------------------

# Reflection

Before this project I didn't know anything about Prosper and I was lightly 
knowledgeable about the peer-to-peer lending business process. That's why when I 
was making my decision, which data set to analyse, white/red wine or Prosper 
one, I've decided to go with the Prosper data set and learn something more about 
it. My first reaction was "Well how can I analyse something that I have no idea 
how it works", but after spending a lot of time on Prosper web site, and going 
over the variables in my excel sheet I've reached the feeling of being more and 
more knowledgeable and more confident in my analysis.

At the beginning I was struggling with the ggplot2 syntax, bat after a lot of 
practice, searching and reading on Google about the ggplot2 syntax I've become 
very comfortable with the basic ggplot2 syntax which helped me to create all 
these plots.  

As a next step, I am planing  to analyse some of the other variables that I 
didn't covered in this analysis. I'm sure that I can learn more about this 
data set from the rest of the variables as well. Since my main focus on this 
project was exploratory analysis as a next step I would like to build 
predictive model. I would also like to contact Prosper and ask if they can 
provide me with the latest dataset (if it's available for public), so I can 
compare my current observed trends vs the latest year trends. Were there any 
other major changes since march 2014, or the trends are the same. 

To conclude, now after getting all this knowledge it's much more easy to start 
any other analysis, do it much better and reach the next level. After my work 
in this project, what I really become more interested in is risk analysis.

