#########################################
### RTI Aviation Accidents Exercise #####
### Structured data processing ##########
### Sandy Preiss ########################
#########################################

#######
# Reading XML and cleaning data from the NTSB Incident database
# Code base from Todd Curtis of AirSafe.com.
#######

# Step 0:  Load the NTSB output file:

require(XML)
if (!require("XML")){ 
  install.packages("XML") 
} 
library(XML) 

data <- xmlParse("C:\\Users\\Sandy\\Documents\\GitHub\\data-scientist-exercise02\\data\\AviationData.xml")

# The XML file is converted to a list format where each row contains both 
# the name of the variable and the value of that variable
ntsb.data <- xmlToList(data)

# Converts to data frame but transposes values, reverse the transposal and remove 
# unneeded row names
ntsb.data = as.data.frame(ntsb.data) 
ntsb.data = as.data.frame(t(ntsb.data))
rownames(ntsb.data) = NULL

ntsb.data.raw = ntsb.data # Raw data on standby in case of a later problem


# DATA CLEANING

# Step 1: Eliminate any columns (variables) added as a consequence of Step 0,
# columns which have no data (all are NA) 
elim.var = which(apply(ntsb.data, 2, function(x) all(is.na(x))))
elim.var = as.data.frame(elim.var) # Column name and column number of this data frame are the columns to be cut

# Eliminates all columns with only NA values
if (nrow(elim.var)>0) ntsb.data = ntsb.data[,-elim.var[,1]] 

# *Update: Step 1A (XML only): Ensure that the column (variable) names and data types fit the standard set 
# by the process used to convert the text file data into a CSV file

colnames(ntsb.data) = c("Event.Id", "Investigation.Type", "Accident.Number",
                        "Event.Date", "Location",  "Country", "Latitude", "Longitude",
                        "Airport.Code", "Airport.Name","Injury.Severity", "Aircraft.Damage",
                        "Aircraft.Category", "Registration.Number", "Make", "Model",
                        "Amateur.Built","Number.of.Engines", "Engine.Type", "FAR.Description",
                        "Schedule","Purpose.of.Flight", "Air.Carrier", "Total.Fatal.Injuries",
                        "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured",
                        "Weather.Condition", "Broad.Phase.of.Flight", "Report.Status",
                        "Publication.Date")

# Step 2: Ensure character variables are of type character

# As part of the process, need to ensure that there are not extra spaces
# at the beginning or end of each character value

# ================================
# Use function 'stripper', which works like the str_trim() function in the strigr package  
# Using this function to use only base R package.

stripper <- function(x){
  # This function removes leading and trailing spaces from a vector.
  # Space characters include tab, newline, vertical tab, form feed, 
  # carriage return, space.
  # Equivalent to the str_trim() function in the strigr package   
  x = as.character(x)
  x = sub("[[:space:]]+$", "", x) # Remove leading space characters
  x = sub("^[[:space:]]+", "", x) # Remove trailing space characters
  return(x)
}
# ================================

# Find which columns match the following known character vectors
char.vecs = c("Event.Id","Investigation.Type","Accident.Number",       
              "Location", "Country", "Airport.Code", "Airport.Name", "Injury.Severity",
              "Aircraft.Damage", "Aircraft.Category", "Registration.Number", "Make",
              "Model", "Amateur.Built", "Engine.Type", "FAR.Description","Schedule",              
              "Purpose.of.Flight", "Air.Carrier", "Weather.Condition",
              "Broad.Phase.of.Flight", "Report.Status")

char.vecs.ndx = which(colnames(ntsb.data) %in% char.vecs)

# Ensure that the character variable is of type character, then remove extra spaces
for (i in 1:length(char.vecs.ndx)) {
  ntsb.data[,char.vecs.ndx[i]] = as.character(ntsb.data[,char.vecs.ndx[i]])
  ntsb.data[,char.vecs.ndx[i]] = stripper(ntsb.data[,char.vecs.ndx[i]])
}

# Step 3: Ensure numerical variables are of type numeric

# Find which columns match the following known numerical vectors
num.vecs = c("Latitude", "Longitude", "Number.of.Engines", "Total.Fatal.Injuries",  
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured")

# Creates a vector for the column numbers for numeric variables
num.vecs.ndx = which((colnames(ntsb.data)) %in% num.vecs)

# Note: This step appears to replace missing numeric values with NA
for (i in 1:length(num.vecs.ndx)) {
  ntsb.data[,num.vecs.ndx[i]] = as.numeric(as.character(ntsb.data[,num.vecs.ndx[i]]))
}

# Step 4: Change date variables into a format suitable for R

# Dates are in form mm/dd/YYYY, must convert to a date format of YYYY-mm-dd
# Two date variables, Event.Date and Pulication Date
ntsb.data$Event.Date = as.Date(ntsb.data$Event.Date, "%m/%d/%Y")
ntsb.data$Publication.Date = as.Date(ntsb.data$Publication.Date, "%m/%d/%Y")
# Note: This step appears to replace missing date values with NA

# Now will have separate columns for Year, Month, Day, and Weekday for Event.Date
ntsb.data$Year = as.numeric(format(ntsb.data$Event.Date, "%Y")) # Ensure it is a numeric variable
ntsb.data$Month = months(ntsb.data$Event.Date, abbreviate = TRUE)
ntsb.data$Day = format(ntsb.data$Event.Date, "%d")
ntsb.data$Weekday = weekdays(ntsb.data$Event.Date, abbreviate = TRUE)

# DATA CONVERSION: Ordering days of the week and months of the year 

# Make months and days of the week factors and order them as they are in a calendar
ntsb.data$Month = factor(ntsb.data$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                                                  "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)

ntsb.data$Weekday = factor(ntsb.data$Weekday,levels=c("Sun","Mon","Tue",
                                                      "Wed","Thu","Fri","Sat"), ordered=TRUE)

# Step 5 Eliminate any rows (records) which has no date (Event.Date is NA)
elim.row = which(is.na(ntsb.data$Event.Date))
if (length(elim.row)>0) ntsb.data = ntsb.data[-elim.row,] # Eliminates all rows with only NA value for Event.Date

# Step 6: Change blank, "N/A","Unknown", and similar responses to NA

# Must first put airport names and make in title case, first by using tolower() followed by toTitleCase()
# This has to be done before any effort to replace words like "UNK", and "NA" with NA
# Will install {tools} package for title case function

# The first step is to install new packages that will be needed for the analysis.
# In this case, the function needed is toTitleCase(text)
options(repos = c(CRAN = "http://cran.rstudio.com"))
if("tools" %in% rownames(installed.packages()) == FALSE) 
{install.packages("tools")}
library(tools)

ntsb.data$Airport.Name = toTitleCase(tolower(ntsb.data$Airport.Name))
ntsb.data$Air.Carrier = toTitleCase(tolower(ntsb.data$Air.Carrier))
ntsb.data$Make = toTitleCase(tolower(ntsb.data$Make))
# NOTE: Not a perfect solution, does not take into account names like McFarland, will end up as Mcfarland
#       Also, for some reason, umlats that were in a word in an XML file don't respond to the toTitleCase commmand
# First, define  a vector of words or phrases that are to be replaced with NA
repl.words = c("", ".", "N/A", "n/a", "N/a","NA","na","Na", "none", "None", "UNK","Unknown","Unavailable", "UNKNOWN", "unknown", "Unk", "unk")
# Note that the following checks all columns, even non-character one, but in the end,
# only character columns will have any non-zero values for replacement words
repl.list = apply(ntsb.data,2, function(x) which(x %in% repl.words))

# The number of times replacement words occur for each database variable (column), 
# is placed in a one-column data frame where the row names correspond to the 
# variable names the column has the number of times the replacement words occur
with.missing = as.data.frame(sapply(repl.list,length)) 
colnames(with.missing) = "Replacement.Words"

# Identify columns corresponding with character-based
# variables with at least one replacement word
with.missing.ndx = which(with.missing[,1]>0) 

# Replace replacement words with NAs in those columns containing
# one or more replacement words

for(i in 1:length(with.missing.ndx)){
  repl.vector=ntsb.data[,with.missing.ndx[i]]%in%repl.words 
  ntsb.data[repl.vector,with.missing.ndx[i]]=NA
} 

# Step 7: Specify city name, state abbreviation, and full state name
# for any location in the United states. 

# This step will eliminate rows that have NA as the location AND NA as the country
# Will not add city or state if not one of the identifiable US abbreviations

# First, get the index of records with non-NA locations
#       which(!is.na(ntsb.data$Location))
# Second, get the index of records with non-NA countries 
#       which(!is.na(ntsb.data$Country))
# Now get a vector that is the union of these two indices
# where there is either a country or a location that is not NA
city.locs.ndx = union(which(!is.na(ntsb.data$Location)), which(!is.na(ntsb.data$Country)) )
ntsb.data = ntsb.data[city.locs.ndx,]

# STATE CODES: Before adding full state names to data frame, must match
# two-letter state codes with full state names.

# First, create a vector of full state names by agmenting the built-in R 
# lists of names and abbreviations of the 50 states (state.name and state.abb) 
# with all other names and two-letter abbreviations used by the US Postal Service (USPS)

# Source is USPS Publication 28 - postal addressing standards 
# Located at http://pe.usps.gov/text/pub28/welcome.htm accessed 9 December 2015

extra.abb = c("AS", "DC", "FM","GU","MH", "MP", "PW", "PR", "VI", "GM", "AO","PO")
extra.name = c("American Samoa", "District of Columbia", "Federated States of Micronesia",
               "Guam", "Marshall Islands", "Northern Mariana Islands", "Palau",
               "Puerto Rico", "Virgin Islands", "Gulf of Mexico", "Atlantic Ocean",
               "Pacific Ocean")

# Now append them to the R-provided list of 50 state names and abbreviations
usps.abb = c(state.abb,extra.abb)
usps.state = c(state.name,extra.name)

# Next is to identify the number of commas in city identifiers
# This is because the NTSB output did not have separate variable for state
# States in NTSB output are two letter codes preceeded by a comma and a space

# These three vectors each initialized with a number of NA values equal to the
# number of rows to ensure that the final vector will be compatible with ntsb.data
comma.pos = rep(NA,nrow(ntsb.data)) # start with NA for number of commas for all Locations
city.vec = rep(NA,nrow(ntsb.data))
state.vec = rep(NA,nrow(ntsb.data))

for(x in 1:length(city.locs.ndx)){
  # Create a list that contains vector of comma positions
  comma.pos.list = gregexpr(",", ntsb.data$Location[city.locs.ndx[x]], fixed=TRUE)
  comma.pos.vec = comma.pos.list[[1]] # Vector of comma positinos
  comma.pos[x] = comma.pos.vec[length(comma.pos.vec)]
  
  # Get the length of the Location field character string
  num.chars = nchar(ntsb.data$Location[city.locs.ndx[x]]) 
  
  # Determine state code if location has enough characters for comma, space, and code
  if(comma.pos[x] >= 1 & num.chars >= 4){
    # Use last comma position to determine where split character string and find state code
    city.vec[city.locs.ndx[x]] =  substr(ntsb.data$Location[city.locs.ndx[x]], 1,(comma.pos[x]-1)) 
    state.vec[city.locs.ndx[x]] =  substr(ntsb.data$Location[city.locs.ndx[x]], (comma.pos[x]+2),num.chars)       
  } # End of if statement for creating city name and state abbreviation
  
} # End of process for finding US city names and state abbreviations     

# Initialize the full state name vector with a number of NA values equal to the
# number of rows to ensure that the final vector will be compatible with ntsb.data
state.full=rep(NA,nrow(ntsb.data))

for (i in 1:length(city.locs.ndx)){
  if(state.vec[i] %in% usps.abb) {
    state.full[i]=usps.state[grep(state.vec[i], usps.abb)]
  }
  # Erase city and two-letter state code if state code is not in usps.abb list
  if(!(state.vec[i] %in% usps.abb)) {
    city.vec[i]=NA
    state.vec[i]=NA
  }
}

# Can now add city and state abbrevications, and full state names to data frame
ntsb.data$City=city.vec
ntsb.data$City = toTitleCase(tolower(ntsb.data$City)) # Ensure city names are in title case
ntsb.data$State.code=state.vec
ntsb.data$State=state.full

# Step 8: Add a new variable that has the maximum injury outcome (Fatal, Injury, or None) for
#       every record with at least one non-NA value for the number of fatalities,
#       injuries (serious, minor, or none). Total number of fatalities already given by 
#       variable Total.Fatal.Injuries, so just need the non-NA  values greater than zero for fatal Max.Injury value

# First, the Max.Injury fatals
ntsb.data$Max.Injury = rep(NA,nrow(ntsb.data)) # start with NA for maximum injury
fatal.ndx = which(ntsb.data$Total.Fatal.Injuries > 0)
ntsb.data$Max.Injury[fatal.ndx] = "Fatal"

# Second, the Max.Injury injuries. For Max.Injury to be equal to Injury, there must be at 
#       least one minor or serious injury, and no fatalities
#       Get the index of fatals and the index of injuries,
#       find the intersection, then take away the intersection from the
#       index of injuries.

injury.ndx = which(ntsb.data$Total.Serious.Injuries > 0 | ntsb.data$Total.Minor.Injuries>0)
injury.only.ndx = setdiff(injury.ndx,intersect(injury.ndx,fatal.ndx)) # Injuries, but not fatalitiesf
ntsb.data$Max.Injury[injury.only.ndx] = "Injury"

# Lastly, the no injury events, which would be the no injury events, 
#       excluding the Max.Injury injury events previously marked as "Fatal" or "Injury"
no.injury.ndx = which(ntsb.data$Total.Uninjured > 0)
no.injury.ndx = setdiff(no.injury.ndx,union(fatal.ndx,injury.only.ndx))
ntsb.data$Max.Injury[no.injury.ndx] = "None"

# Also adding a variable for total total injuries, and total number of people involved 
#       for records with non-NA data

ntsb.data$Total.Injured = rep(NA,nrow(ntsb.data))
ntsb.data$Total.Injured[injury.ndx] = apply(ntsb.data[injury.ndx, c("Total.Serious.Injuries","Total.Minor.Injuries")],1,sum,na.rm=TRUE)

# For total number of people involved, need index of every row with at least one non-zero value for
#       fatals, minor injuries, serious injuries, or no injuries
occupied.ndx = unique(sort(c(fatal.ndx,injury.ndx,no.injury.ndx))) # non-duplicated

ntsb.data$Total.Involved = rep(NA,nrow(ntsb.data))
ntsb.data$Total.Involved[occupied.ndx] = apply(ntsb.data[occupied.ndx, c("Total.Fatal.Injuries","Total.Serious.Injuries","Total.Minor.Injuries","Total.Uninjured")],1,sum,na.rm=TRUE)

# Step 9: Arrange the new columns in logical groupings

new.cols = c("Event.Id", "Investigation.Type", "Accident.Number",
             "Event.Date", "Year", "Month", "Day", "Weekday",
             "Location", "City", "State.code", "State", "Country", 
             "Airport.Code", "Airport.Name", "Latitude", "Longitude",
             "Injury.Severity", "Aircraft.Damage", "Aircraft.Category",
             "Registration.Number", "Make", "Model", "Amateur.Built",
             "Number.of.Engines", "Engine.Type", "FAR.Description", "Schedule",
             "Purpose.of.Flight", "Air.Carrier", "Total.Fatal.Injuries",
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Injured", "Total.Uninjured", "Total.Involved",
             "Max.Injury",
             "Weather.Condition", "Broad.Phase.of.Flight", "Report.Status",
             "Publication.Date")

ntsb.data = ntsb.data[,new.cols]

# Step 10 (Final step): Save the processed data frame as a CSV and Text file

# First output file is a CSV file direct from the data frame using default options for write.csv function
write.csv(ntsb.data, file = "ntsb_data.csv", row.names = FALSE)

# Second output file replaces all NA values with a null-type character before export, 
#       removes any row numbers, and adds a seperator consisting of a vertical bar with a space on each side.

# This makes the output consistent with the NTSB text file output format, and
#       also makes it easier to do a side-by-side comparison with NTSB output using data visualization
#       software like Tableau or data cleaning software like OpenRefine
ntsb.data.text = ntsb.data
ntsb.data.text = sapply(ntsb.data.text, as.character)
ntsb.data.text[is.na(ntsb.data.text)] = "" 
ntsb.data.text = as.data.frame(ntsb.data.text)
write.table(ntsb.data.text, file = "ntsb_data.txt", sep = " | ", quote = FALSE, row.names = FALSE)


# EDA

print("Summary statistics")

paste("Total number of records - ", format(nrow(ntsb.data.raw), big.mark=","), sep="") 

paste("Number of records excluded - ", nrow(ntsb.data.raw)-nrow(ntsb.data)) 

paste("Number of records processed - ", format(nrow(ntsb.data), big.mark=","), sep="") 

paste("Number of events with an indentifiable number of occupants - ", format(length(occupied.ndx), big.mark=","), sep="") 

paste("Number of events with either fatalities or injuries - ", format(length(union(fatal.ndx,injury.only.ndx)), big.mark=","), sep="") 

paste("Number of records with a US location - ", format(nrow(ntsb.data[which(ntsb.data$Country=="United States"),]), big.mark=","),".", sep="")

paste("Total number of records involving fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries>=1, na.rm=TRUE), big.mark=","),".", sep="") 

paste("Total fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries, na.rm=TRUE), big.mark=","),".", sep="") 

print("Table of reports by state")
table(ntsb.data$State.code)

print("Table of top 15 states by number of events")
sort(table(ntsb.data$State.code), decreasing = TRUE)[1:15]


# Vector of fatal events
fatal.vec = which(ntsb.data$Total.Fatal.Injuries>=1 & ntsb.data$Year>=2005)

# Data frame of all fatals from 2005 and beyond
fatal.df = ntsb.data[fatal.vec,]

# Histogram of top 15 states by number of events
barplot(sort(table(ntsb.data[ntsb.data$Year>=2005,]$State.code),
             decreasing = TRUE)[1:15], col="dodgerblue",xlab="State", ylab="Events", 
        cex.names = 0.7, main="Top 15 states by number of events")


print("Table of top 15 states by number of fatal events")
# Vector of all fatals for events with a US state code
sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15]

# Top 15 states by total fatalities
print("Table of top 15 states by number of fatalities")
sort(as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum)), decreasing = TRUE)[1:15]

# Histogram of top 15 states by number of fatal events 
barplot(sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15],
        col="dodgerblue",xlab="State", ylab="Fatal events",
        cex.names = 0.7, main="Top 15 states by number of fatal events")


# Table of events by day of the week 
print("Events by day of the week")
table(ntsb.data[ntsb.data$Year>=2005,]$Weekday)

# Histogram events by day of the week
barplot(table(ntsb.data[ntsb.data$Year>=2005,]$Weekday), col="dodgerblue",
        xlab="Day", ylab="Events", main="Events by day of week")

# Table of fatal events by day of the week 
print("Fatal events by day of the week")
table(fatal.df$Weekday)

# Histogram of fatal events by day of the week
barplot(table(fatal.df$Weekday), col="dodgerblue", cex.names = 1.2,
        xlab="Day", ylab="Events", main="Fatal events by day of week")

# Table of events by month of the year
print("Events by month of the year")
table(ntsb.data[ntsb.data$Year>=2005,]$Month)

# Histogram of events by month of the year
barplot(table(ntsb.data[which(ntsb.data$Year>=2005),]$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Month", ylab="Events", main="Events by month")

# Table of fatal events by month of the year
print("Fatal events by month of the year")
table(fatal.df$Month)

# Histogram of fatal events by month of the year
barplot(table(fatal.df$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Day", ylab="Fatal events", main="Fatal events by month")



# Plot of events by year
barplot(table(ntsb.data[ntsb.data$Year>=2005,]$Year), col="dodgerblue",
        xlab="Year", ylab="Events", main="Events by year")


# Histogram of fatal events by year
barplot(table(ntsb.data[fatal.vec,]$Year), col="dodgerblue",
        xlab="Year", ylab="Fatal events", main="Fatal events by year")

# -------
# Fatalites by year

# Do a tapply for sums by category then ensure it is table
death.table = as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$Year, sum))

barplot(death.table, col="dodgerblue",
        xlab="Year", ylab="Fatalities", main="Fatalities by year")
# -------

# Total fatalities by US state and territory
as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum))

# Total fatal events  by US state and territory
table(fatal.df$State.code)

# Total fatal events by US state and territory sorted
sort(table(fatal.df$State.code), decreasing=TRUE)




############
#### Clustering
############

library(dplyr)
library(tidyr)

# using h2o for modeling - open and initiate
require(h2o)
h2o.removeAll()
h2o.init()

str(ntsb.data)

# fill numerical NA with 0 and categorical with missing level
ntsb.data <- ntsb.data %>% mutate_if(is.numeric , replace_na, replace = 0)
ntsb.data <- ntsb.data %>% mutate_if(is.character , replace_na, replace = 'Missing')

# convert strings to factors
ntsb.data <- as.data.frame(unclass(ntsb.data))

# convert ordered factors to regular factors so they place nice with h2o
ntsb.data$Month <- factor(ntsb.data$Month, ordered=FALSE)
ntsb.data$Weekday <- factor(ntsb.data$Weekday, ordered=FALSE)

# df to h2o frame
ntsb_h2o <- as.h2o(ntsb.data)

# select columns for clustering
colnames(ntsb_h2o)
x <- c(2,4:40) # all but EventID, Accident number, pub date

# k means with randomization to avoid local optima
km <- h2o.kmeans(training_frame = ntsb_h2o, 
                 x = x,
                 k = 10,
                 estimate_k = TRUE,
                 max_iterations = 100,
                 standardize = TRUE,
                 seed = 33,
                 init = 'PlusPlus')

km@model$model_summary
km@model$training_metrics

# get labels and make dataset with clusters
score <- h2o.predict(km, ntsb_h2o)
ntsb_scored <- h2o.cbind(ntsb_h2o, score)
ntsb_scored_df <- as.data.frame(ntsb_scored)



###########
### Cluster exploration
###########

library(data.table)
library(janitor)
library(ggplot2)
library(RColorBrewer)
library(purrr)

# join text cluster
text_clust <- fread('C:\\Users\\Sandy\\Documents\\GitHub\\data-scientist-exercise02\\data\\text_cluster.csv')
ntsb_final <- left_join(ntsb_scored_df, text_clust, by='Event.Id')

ntsb_final <- rename(ntsb_final, struct_cluster = predict) 

# clusters crosstab
t1 <- ntsb_final %>%
  tabyl(struct_cluster, text_cluster)

t1 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
# struct_clust 0 and 1 appear to be picking up something other than text categories 

# clusters chart
ggplot(ntsb_final, aes(fill=factor(struct_cluster), x=factor(text_cluster))) +
  geom_bar(position='fill')

ggplot(ntsb_final, aes(fill=factor(text_cluster), x=factor(struct_cluster))) +
  geom_bar(position='fill') +
  labs(title="Distribution of Clusters", x="Structured Data Cluster", y="Percentage", fill='Text Data Cluster') +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_brewer(palette='Set2')


#### explore vars by cluster

# Efficiently plot the two cluster vars with each feature
# Creates vectors of variable names - clusters, categorical features, and numeric features
# Applies plotting functions to each element of variable vectors to avoid lots of copy/paste

# prep vectors of variable names
clusters <- c('text_cluster', 'struct_cluster')
clusters <- set_names(clusters) # uses purrr to name each item so the plot function can interpret

y_cat <- c('Month', 'Weekday', 'State', 'Injury.Severity', 'Aircraft.Damage', 'Aircraft.Category', 'Amateur.Built', 
           'Engine.Type', 'FAR.Description', 'Purpose.of.Flight', 'Max.Injury', 'Weather.Condition', 'Broad.Phase.of.Flight')
y_cat <- set_names(y_cat)

y_num <- c('Year', 'Latitude', 'Longitude', 'Number.of.Engines', 'Total.Fatal.Injuries', 'Total.Serious.Injuries', 'Total.Minor.Injuries',
           'Total.Injured', 'Total.Uninjured', 'Total.Involved')
y_num <- set_names(y_num)

# make function for stacked bar plotting categorical vars
bar_func = function(x,y){
  ggplot(ntsb_final, aes(fill=.data[[x]], x=.data[[y]])) +
            geom_bar(position='fill')
}

bar_func('Country', 'struct_cluster') # test function 

# map bar plot function to each element of var name vectors
map(clusters, ~map(y_cat, bar_func, y=.x))


# make function for plotting numerical vars
violin_func = function(x,y){
  ggplot(ntsb_final, aes(x=.data[[x]], y=.data[[y]])) +
    geom_violin(trim=TRUE)
}

# need to encode clusters as factors
ntsb_final$text_cluster <- factor(ntsb_final$text_cluster, ordered=FALSE)
ntsb_final$struct_cluster <- factor(ntsb_final$struct_cluster, ordered=FALSE)

violin_func('struct_cluster', 'Number.of.Engines') # test function

# map violin plot function to each element of var name vectors
map(y_num, ~map(clusters, violin_func, y=.x))

