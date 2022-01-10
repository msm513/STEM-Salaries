#----------------------------------------------------------------
# Stem Salaries
#
# Created:  11/13/2021
#
# Last Modified:  01/05/2022  
#
# Authors:  Miles S. Marimbire, University of Cincinnati
#----------------------------------------------------------------

#----------------------------------------------------------------
# This code is open source feel free to add any modification you wish
#----------------------------------------------------------------
# Set up R 
#----------------------------------------------------------------
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())
# To clear just the console window, type "Ctrl+L" or use Edit pull down menu

# Specify a display option
#options("scipen"=999, digits=2)

# === Set the working directory.  Note that R uses "/" not "\"
# === So the command is setwd("your directory path") or use the Session pull down menu
setwd("~/R/")
# === NOTE:  If using a MAC computer, you might need to replace the above command with something like
# === setwd("Desktop/R/")
#----------------------------------------------------------------

#----------------------------------------------------------------
# List of packages 
packages <- c("AER","car","ggplot2","gmodels", "haven", "jtools", "pastecs", "psych", "skedastic",
              "stargazer", "summarytools","tidyverse", "corrplot", "olsrr", "RNHANES", "dplyr", 
              "gridExtra", "ggthemes","shiny","shinydashboard", "dlookr", "gtools", "scales", "caret", "rvest","xml2","readr")

# Install the packages
# Run this code to install packages you do not have installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))
search()

# Set the number of digits to display when using the jtools commands, like summ
options("jtools-digits"=4) 
options(scipen = 999)

# You can use these methods if you need a example of how to use a package
browseVignettes("AER")  # Short documents on how to use the packages
?mean # Opens the help page for the mean function
?"+" #  Opens the help page for addition
?"if" # Opens the help page for if, used for branching code
??plotting #  Searches for topics containing words like "plotting"
??"regression model" #  Searches for topics containing phrases like this
#--------------------------------------------------------

#--------------------------------------------------------
# Stem Salaries
#--------------------------------------------------------
# Load in the data
salary <- read_csv("Data/Levels_Fyi_Salary_Data.csv")
View(salary)
#--------------------------------------------------------

#--------------------------------------------------------
# Data Exploration 
#--------------------------------------------------------
# Summarize the data
summary(salary)

# Descriptive Stats of numeric values
describe(salary)

# Check how many NA are in data set
sum(is.na(salary))# We have 115,356 NA'S in our data set

# Lets remove all NA from data set and clean it up!
??"is.na" 
saa <- salary %>% drop_na()# Now data has removed all NA 
sum(is.na(saa))# Check to see if data has been sub setted correctly 
sal <- saa %>%
  select(-timestamp)# Delete time stamp row, its pointless to keep
view(sal)

# Now that data is clean lets look for the strongest correlations
correlate(sal)
plot_correlate(sal)# Lets plot correlation

# Correlation between selected variables
sal %>%
  select(totalyearlycompensation, yearsofexperience, yearsatcompany,
         basesalary, bonus, stockgrantvalue, 
         Highschool,Doctorate_Degree, Bachelors_Degree, 
         Masters_Degree) %>%
  cor() 

# Normality test
normality(sal)
plot_normality(sal)

# Now let's view the frequency of particular values in the data set
stat_mode(sal$company)
stat_mode(sal$level)
stat_mode(sal$title)
stat_mode(sal$location)
stat_mode(sal$yearsofexperience)
stat_mode(sal$yearsatcompany)
stat_mode(sal$tag)
stat_mode(sal$gender)
stat_mode(sal$otherdetails)

# Frequency of each occurrence
freq(sal$Race, count=TRUE,pct=TRUE,cumul.count=TRUE,cumul.pct=TRUE,total.name="Total" )
freq(sal$gender, count=TRUE,pct=TRUE,cumul.count=TRUE,cumul.pct=TRUE,total.name="Total" )
freq(sal$title, count=TRUE,pct=TRUE,cumul.count=TRUE,cumul.pct=TRUE,total.name="Total" )
freq(sal$yearsofexperience, count=TRUE,pct=TRUE,cumul.count=TRUE,cumul.pct=TRUE,total.name="Total" )
freq(sal$yearsatcompany, count=TRUE,pct=TRUE,cumul.count=TRUE,cumul.pct=TRUE,total.name="Total" )


# EDA Reports
eda_paged_report(sal)
eda_web_report(sal)

#--------------------------------------------------------
# Data Visualization
#--------------------------------------------------------
#  Histogram of Total Yearly Compensation
sal %>%
  ggplot() + 
  geom_histogram(mapping = aes(totalyearlycompensation), bins=100, fill="lightblue", color="black") +
  labs(title = "Total Compensation Distribution", x="Total Yearly Compensation", y="Count")

# Salary by years of experience 
sal %>%
  ggplot() +
  geom_point(mapping = aes(x=yearsofexperience, y=totalyearlycompensation), size=1) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Compensation Based on Years", x="Years of Experience", y="Total Yearly Compensation")

# Salary by years at company
sal%>%
  ggplot()+
  geom_point(mapping=aes(y=totalyearlycompensation,x=yearsatcompany),color="blue") +
  scale_y_continuous(labels = scales::dollar_format())+
  labs(title = "Compensation Based on Years", x="Years at Company", y="Total Yearly Compensation")

# Top Companies For Data Scientists
sal %>% 
  filter(title== "Data Scientist")%>% 
  count(company, sort= T) %>%
  head(n=15) %>% 
  ggplot() + aes(x= reorder(company, n), y=n) + geom_col(fill='#0E5E8E') + coord_flip() + 
  theme(plot.title= element_text(hjust = .5), axis.title = element_text(size=15, face='bold'),
                                              axis.text= element_text(size=12)) + 
  ylab("\n Count") + xlab("Company\n") + ggtitle("Top Companies For Data Scientists")

# Top Locations for Data Scientist 
sal %>% 
  filter(title== "Data Scientist")%>% 
  count(location, sort= T) %>%
  head(n=15) %>% 
  ggplot() + aes(x= reorder(location, n), y=n) + geom_col(fill='#0E5E8E') + coord_flip() + 
  theme(plot.title= element_text(hjust = .5), axis.title = element_text(size=15, face='bold'),
        axis.text= element_text(size=12)) + 
  ylab("\n Count") + xlab("Location\n") + ggtitle("Top Locations For Data Scientists")


# Top companies for software engineer  
sal %>% 
  filter(title== "Software Engineer")%>% 
  count(company, sort= T) %>%
  head(n=15) %>% 
  ggplot() + aes(x= reorder(company, n), y=n) + geom_col(fill='#0E5E8E') + coord_flip() + 
  theme(plot.title= element_text(hjust = .5), axis.title = element_text(size=15, face='bold'),
        axis.text= element_text(size=12)) + 
  ylab("\n Count") + xlab("Company\n") + ggtitle("Top Companies For Software Engineer")

# Top Locations for Software Engineer 
sal %>% 
  filter(title== "Software Engineer")%>% 
  count(location, sort= T) %>%
  head(n=15) %>% 
  ggplot() + aes(x= reorder(location, n), y=n) + geom_col(fill='#0E5E8E') + coord_flip() + 
  theme(plot.title= element_text(hjust = .5), axis.title = element_text(size=15, face='bold'),
        axis.text= element_text(size=12)) + 
  ylab("\n Count") + xlab("Location\n") + ggtitle("Top Locations For Software Engineer")

#----------------------------------------------------------------
# Modeling The Data
#----------------------------------------------------------------
# Subset data for top companies only
new_data <- sal %>%
  mutate(amazon =as.numeric(company=="Amazon")) %>%
  mutate(microsoft =as.numeric(company=="Microsoft")) %>%
  mutate(facebook =as.numeric(company=="Facebook")) %>%
  mutate(google =as.numeric(company=="Google")) %>%
  mutate(apple =as.numeric(company=="Apple")) %>%
  mutate(ibm =as.numeric(company=="IBM")) %>%
  mutate(uber =as.numeric(company=="Uber")) %>%
  mutate(capital_one =as.numeric(company=="Capital One")) %>%
  mutate(walmart_labs =as.numeric(company=="Walmart Labs")) %>%
  mutate(linkedin =as.numeric(company=="LinkedIn")) %>%
  mutate(jpmorgan =as.numeric(company=="JPMorgan Chase")) %>%
  mutate(netflix =as.numeric(company=="Netflix")) %>%
  mutate(intuit =as.numeric(company=="Intuit")) %>%
  mutate(paypal =as.numeric(company=="PayPal")) %>%
  mutate(booz =as.numeric(company=="Booz Allen Hamilton")) %>%
  mutate(data_science =as.numeric(title=="Data Scientist"))
  
# Relation of how salary, stocks and bonus affect the total compensation
reg <- lm(totalyearlycompensation~basesalary+stockgrantvalue+bonus, data = new_data)
AIC(reg)
BIC(reg)
summ(reg) # Output of regression model

# Relation of how salary, stocks, bonus and company affect the total compensation
reg_2 <- lm(totalyearlycompensation~basesalary+stockgrantvalue+bonus+amazon+microsoft+facebook+google+apple+ibm+uber+capital_one+walmart_labs+linkedin+jpmorgan+netflix+intuit+paypal+booz+data_science, data=new_data)
AIC(reg_2)
BIC(reg_2)
summ(reg_2) # Output of regression model

# Output to compare the Multi-linear regression models
stargazer(reg,reg_2,type="text",align=TRUE,
          title="Total Compensation Factors Models",
          dep.var.labels = "Compensation Factors",
          keep.stat=c("n","rsq","f","aic","bic"), no.space=FALSE,df=FALSE,
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

#----------------------------------------------------------------
# How does Degree Held and Race Affect Total Yearly Compensation?
#----------------------------------------------------------------
Reg_3 <- lm(totalyearlycompensation~Race_Hispanic+Race_Black+Race_Two_Or_More+Race_White+Race_Asian+Race, sal)
AIC(Reg_3)
BIC(Reg_3)
summ(Reg_3)

Reg_4 <- lm(totalyearlycompensation~Masters_Degree+Bachelors_Degree+Doctorate_Degree+Highschool+yearsofexperience+yearsatcompany, sal)
AIC(Reg_4)
BIC(Reg_4)
summ(Reg_4)

stargazer(Reg_3,Reg_4,type="text",align=TRUE,
          title="Total Compensation Factors Models",
          dep.var.labels = "Race and Education",
          keep.stat=c("n","rsq","f","aic","bic"), no.space=FALSE,df=FALSE,
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)

#--------------------------------------------------------
# Export Data
#--------------------------------------------------------
# Convert Clean data to csv file so we can make dashboard!
write.csv(sal,"C:\\Users\\miles\\OneDrive\\Desktop\\R Scripts\\R Projects\\sal.csv", row.names = FALSE)


