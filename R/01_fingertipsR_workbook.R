


# Libraries ---------------------------------------------------------------

if (!require(dplyr)) install.packages("dplyr")

# Note, fingertipsR is not on CRAN and won't be for the foreseeable future
if (!require(fingertipsR)) install.packages("fingertipsR", 
                                            repos = "https://dev.ropensci.org",
                                            dependencies = "Suggests")


# Starting at the end: fingertips_data() ----------------------------------
# The minimum requirement for fingertips_data is IndicatorID and AreaTypeID

## Run the following lines
## (hint: you can move your cursor to the line of code you want to execute, and press Ctrl + Shift + Enter to run it)
df <- fingertips_data(IndicatorID = 219,
                      AreaTypeID = 221,
                      ParentAreaTypeID = 223)
View(df)

## Question: do the number of records make sense to you? (for info, there are 42 ICBs and 7 NHS regions)
# Note, it is always good to sense check the number of records if possible
table(df$Timeperiod, df$AreaType)


# There are 3 ways to help users find IndicatorID
# 1. Go to the indicator definition on Fingertips website: https://fingertips.phe.org.uk/search/record%20of%20a%20bp#page/6/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/90603/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
# 2. Use the select_indicators() function and choose it interactively
# 3. Use the indicators() function and choose it programmatically

## Go to the fingertips website and navigate to;
## Cardiovascular Disease --> Cardiovascular Disease -- > Start --> Topic = Risk factors -- > Data view = Definitions --> Select the indicator


## Execute the select_indicators() function and locate the  Hypertension: QOF prevalence indicator using the search feature (then click Done)
select_indicators()

## If confident with filtering data, use the indicators() function to subset the dataset for the IndicatorID you've just discovered
# Notice how the indicator appears in multiple different profiles
indicators() %>% 
  filter(IndicatorID == 219)



# DANGER! Not all indicators have all AreaTypeIDs!
# There is an area_types() function which provides the codes available for AreaTypeID

## Try the area_types() function and view the results
area_types() %>% 
  View()

# So how do you know what AreaTypeIDs are available for an indicator
# There are 2 ways:
# 1. Through the website
# 2. indicator_areatypes() function

## Go to the fingertips website, where you were for Hypertension: QOF prevalence and click on Trends: https://fingertips.phe.org.uk/profile-group/cardiovascular-disease-diabetes-kidney-disease/profile/cardiovascular/data#page/4/gid/1938133106/pat/159/par/K02000001/ati/15/are/E92000001/iid/219/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
## Try clicking on each of the different options in the Geography dropdown - notice, not all of them are available



## Now try using the indicator_areatypes() function and seeing what other area types are available for that indicator
# Notice there are many more AreaTypeIDs than the website offers. That's because this same indicator might be in a different profile/domain which has a different geographical focus
indicator_areatypes(IndicatorID = 219) %>% 
  left_join(area_types(), by = "AreaTypeID") %>%
  distinct(IndicatorID, AreaTypeID, AreaTypeName) %>% 
  View()

# You now have enough information to get some data
## Using the fingertips_data() function, export data for Hypertension: QOF prevalence for ICBs
df <- fingertips_data(IndicatorID = 219,
                      AreaTypeID = 221)

## What happens if you change the AreaTypeID to Upper tier local authorities (post 4/23) (use area_types() to find out the correct AreaTypeID)?
## Do you understand why you get the number of records that are returned?
df <- fingertips_data(IndicatorID = 219,
                      AreaTypeID = 502)
nrow(df)

# fingertips_data has a ParentAreaTypeID argument. This exercise helps you understand what this is.
## Go to this url: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework/data#page/4/gid/1000042/ati/502/iid/93584/age/1/sex/2/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/car-do-0_ovw-do-0
## Click the drop down for "Areas grouped by". See how that list changes when you select different "Area types"


## Now look at the outputs of the area_types() function again. You can see that each AreaTypeID maps to multiple ParentAreaTypeIDs
area_types() %>% 
  View()

## Execute this line of code and inspect the different AreaTypes that are in the data:
# Note, each indicator-area type combination has a default parent area type
df <- fingertips_data(IndicatorID = 91337,
                      AreaTypeID = 167) # CCGs (2021/22)
View(df)
table(df$AreaType)

## Now execute this code and compare the AreaTypes in what is returned:
df <- fingertips_data(IndicatorID = 91337,
                      AreaTypeID = 167, # CCGs (2021/22)
                      ParentAreaTypeID = 220) 
View(df)
table(df$AreaType)


# Users can also get data for multiple indicators in one go
## Assign the two indicators Life Expectancy at Birth (90366) and Healthy Life Expectancy at Birth (90362) to a vector
ind <- c(90366, 90362)


## Pass that vector into the IndicatorID argument of fingertips_data(), along with AreaTypeID = 502
df <- fingertips_data(IndicatorID = ind,
                      AreaTypeID = 502)

## Look at what time periods are available for each indicator
table(df$Timeperiod, df$IndicatorName)


# Users can also get data for a whole domain or profile by using the DomainID or ProfileID arguments
# You can't get these IDs via the website, only by using the select_indicators() or indicators() functions

## Work out the DomainID for MSK and comorbidity
indicators() %>% 
  filter(grepl("MSK and comorbidity", DomainName))

## Pass this DomainID to the DomainID argument in fingertips_data(), along with AreaTypeID = 502
## Are there the number of indicators you are expecting?
df <- fingertips_data(DomainID = 1938133218,
                      AreaTypeID = 502)

table(df$IndicatorName)

## If you have time find a profile on the fingertips website that you import the data for and carry out the same exercise as above
# Note, the more indicators you import, the longer it is likely to take

df <- fingertips_data(ProfileID = 86, # TB Strategy Monitoring Indicators
                      AreaTypeID = 502)

# Polarity; high numbers are good for some indicators, but they are bad for others
# This is important when it comes to interpretation
# This information isn't provided by default, but can be added by setting the argument "rank" to TRUE

## Repeat one of the successful imports you have done previously, but include rank = TRUE within the fingertips_data() function
## View the results and look at the last few columns; does the polarity make sense to you for that indicator?
df <- fingertips_data(IndicatorID = 90603,
                      AreaTypeID = 221,
                      rank = TRUE)
View(df)

# Sometimes you might get unexpected results
# Unexpected results can come in the form of an error message or no data coming back
# If there is no data, it is good to test whether there really is no data being offered by the API for the set of inputs provided
# This is where the url_only argument is useful

## Repeat the query above, but include the argument url_only = TRUE
df <- fingertips_data(IndicatorID = 90603,
                      AreaTypeID = 221,
                      url_only = TRUE)

## Copy and paste that url into a web browser to see if there is data provided by the API



# Other useful functions --------------------------------------------------

# deprivation_decile() applies the Indices of Multiple Deprivation (produced by the Department of Communities and Local Government) to each area code

## Look at the documentation for deprivation_decile
?deprivation_decile

## Extract a dataset of Upper tier local authorities (post 4/20) with their 2019 deprivation decile and view the data
df <- deprivation_decile(AreaTypeID = 502,
                         Year = 2019)
View(df)


# We have previously inspected the "Definitions" page for an indicator on the Fingertips website
# This information is also available from the API using the indicator_metadata() function

## Import the indicator metadata for IndicatorID = 91872 and view the results
indicator_metadata(IndicatorID = 91872) %>% 
  View()


# fingertips_redred() is an enhancement function
# It simply executes the fingertips_data() function, and then filters the results for areas that are trending in the wrong direction and are also significantly worse than the comparator

## Execute the fingertips_redred() function on ProfileID 135 (Cardiovascular Diseases), for AreaTypeID 221
## Do the records returned make sense to you?
df <- fingertips_redred(ProfileID = 135, # Cardiovascular Diseases
                        AreaTypeID = 221)
