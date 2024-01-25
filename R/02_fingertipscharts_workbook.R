
# Libraries ---------------------------------------------------------------

if (!require(fingertipscharts)) remotes::install_github("ukhsa-collaboration/fingertipscharts",
                                                        build_vignettes = TRUE)
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(stringr)) install.packages("stringr")

# Note, fingertipsR is not on CRAN and won't be for the foreseeable future
if (!require(fingertipsR)) install.packages("fingertipsR", repos = "https://dev.ropensci.org")

# For this part of the workshop we'll use the Risk Factors domain from the Cardiovascular Diseases profile
## Execute this code
df <- fingertips_data(DomainID = 1938133106,
                      AreaTypeID = 221)


# This workshop will work through the visualisations available on Fingertips
# All the function names available in fingertipscharts are identical to the name on the  Fingertips website

# To start with, we will use a function from fingertipsR to understand the order of the indicators in the overview and the area profiles visualisations
ind_order <- indicator_order(DomainID = 1938133106,
                             AreaTypeID = 221,
                             ParentAreaTypeID = 6) %>% 
  select(IndicatorID, Sequence)

indicator_levels <- df %>% 
  left_join(ind_order, by = "IndicatorID") %>% 
  distinct(IndicatorName, Sequence) %>% 
  arrange(Sequence) %>% # arrange in order of the sequence
  pull(IndicatorName)

# This is the order the indicators should be displayed
print(indicator_levels)

# Apply these levels to the IndicatorName field when turning it into a factor
df <- df %>% 
  mutate(IndicatorName = factor(IndicatorName,
                                levels = indicator_levels))

# Overview (or tartan rug) ------------------------------------------------

## Execute the following lines of code to filter the indicators for the latest available time period and the areas of interest
parent <- "South West NHS Region"
fixed_areas <- c("England", parent)
df_overview <- df %>% 
  group_by(IndicatorID) %>% 
  filter((ParentName == parent | # keep all areas whose parent is "South West NHS Region"
            AreaName %in% fixed_areas), # also keep "England" and "South West NHS Region"
         TimeperiodSortable == max(TimeperiodSortable)) %>% # keep the latest time period available for each indicator
  ungroup() %>% 
  mutate(Value = round(Value, 1))# round values to 1 decimal place

## Look at the documentation for the overview() function
?overview

# This example demonstrates the principle of how all of the fingertipscharts functions work
p <- overview(data = df_overview,
              area = AreaName,
              indicator = IndicatorName,
              value = Value,
              fill = ComparedtoEnglandvalueorpercentiles,
              timeperiod = Timeperiod,
              top_areas = fixed_areas,
              wrap_length = 35,
              legend_position = "bottom")
p
## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/0/gid/1938133263/pat/6/par/E12000009/ati/202/are/E06000022/cid/4/page-options/ovw-do-0

# Compare indicators ------------------------------------------------------
## Execute the following lines of code to filter and shape the data in the right way
plot_data <- df %>% 
  group_by(IndicatorID) %>% 
  filter(IndicatorID %in% c(219, 91280),
         TimeperiodSortable == max(TimeperiodSortable)) %>% 
  ungroup() %>% 
  select(IndicatorName, AreaCode, Value) %>% 
  pivot_wider(names_from = IndicatorName, # this function turns long data into wide data
              values_from = Value) %>% 
  rename(Ind1 = `Hypertension: QOF prevalence (all ages)`,
         Ind2 = `Smoking: QOF prevalence (15+ yrs)`)

View(plot_data) # this is the data structure required for the compare_areas function

## complete x, y and area in the code below so that Ind1 is on the x axis and Ind2 is on the y axis
## use the ?compare_areas documentation for help
p <- compare_indicators(data = plot_data,
                        x = Ind1,
                        y = Ind2,
                        xlab = "Hypertension: QOF prevalence (all ages)",
                        ylab = "Smoking: QOF prevalence (15+ yrs)",
                        highlight_area = c("E92000001", "nE54000039"),
                        area = AreaCode,
                        add_R2 = TRUE)
p

# Map ---------------------------------------------------------------------

map_data <- df %>% 
  group_by(IndicatorID) %>% 
  filter(IndicatorID == 93709,
         TimeperiodSortable == max(TimeperiodSortable)) %>% # keep the latest time period for the indicator
  ungroup() %>% 
  mutate(
    ComparedtoEnglandvalueorpercentiles = factor(ComparedtoEnglandvalueorpercentiles,
                                                 levels = c("Higher", "Similar", "Lower", "Not compared")), # make sure the order of the levels is correct
    AreaCode = gsub("n", "", AreaCode)
  )
         
# Shape files are freely available at the open geography portal in the "Boundaries" section
## Go to this url: https://geoportal.statistics.gov.uk/
## Click Boundaries --> Health boundaries --> Integrated Care Boards --> 2022 boundaries
## Have a read of the descriptions of the available boundaries. Note that the difference is the resolution (which impacts the size of the file)
## Select one of the layers --> Select View Full Details --> Select View API Resources (on the right) then copy the GEOjson url and paste it below. Compare it with the link already provided
ons_api <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/ICB_JUL_2022_EN_BGC_V3/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

## Complete area_code and fill in the map code below using the ?fingertipscharts::map documentation
# Note, I have written "fingertipscharts::" below because this forces the "map" function to come from the fingertipscharts package. There are a few common "map" functions available from other packages too
p <- fingertipscharts::map(data = map_data,
                           ons_api = ons_api,
                           area_code = AreaCode,
                           fill = ComparedtoEnglandvalueorpercentiles,
                           title = "Last BP reading of patients (<80 yrs, with hypertension), in the last 12 months is <= 140/90 mmHg (denominator incl. PCAs)",
                           subtitle = "ICBs",
                           copyright_year = 2024)

p

## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile-group/cardiovascular-disease-diabetes-kidney-disease/profile/cardiovascular/data#page/8/gid/1938133106/pat/223/par/E40000003/ati/221/are/nE54000028/iid/93709/age/253/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1

## Complete area_code, fill, value, name_for_label
p <- map(data = map_data,
         ons_api = ons_api,
         area_code = AreaCode,
         fill = ComparedtoEnglandvalueorpercentiles,
         type = "interactive",
         value = Value,
         name_for_label = AreaName,
         title = "Last BP reading of patients (<80 yrs, with hypertension), in the last 12 months is <= 140/90 mmHg (denominator incl. PCAs)")

p



# Trends ------------------------------------------------------------------

df_trend <- df %>%
  filter(IndicatorID == 91280)

## Complete timeperiod, value, area, fill, lowerci, upperci using the ?trends documentation
p <- trends(data = df_trend,
            timeperiod = Timeperiod,
            value = Value,
            area = AreaCode,
            comparator = "E92000001",
            area_name = "nE54000039",
            fill = ComparedtoEnglandvalueorpercentiles,
            lowerci = LowerCI95.0limit,
            upperci = UpperCI95.0limit,
            title = "Smoking (QOF Prevalence)",
            subtitle = "BNSSG",
            xlab = "Year",
            ylab = "Percent (%)")
p

## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile-group/cardiovascular-disease-diabetes-kidney-disease/profile/cardiovascular/data#page/4/gid/1938133106/pat/223/par/E40000006/ati/221/are/nE54000039/iid/91280/age/188/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1


# Compare areas -----------------------------------------------------------

## Execute the following lines of code to get a dataset for one indicator, one region and one year
parent <- "South West NHS Region"
fixed_areas <- c("England", parent)
ordered_levels <- c("Better",
                    "Similar",
                    "Worse",
                    "Not compared")
df_ca <- df %>%
  group_by(IndicatorID) %>% 
  filter(IndicatorID == 91280,
         (AreaName %in% fixed_areas |
            ParentName == parent),
         TimeperiodSortable == max(TimeperiodSortable)) %>% 
  ungroup()

## Complete the arguments for area, value, fill, lowerci, upperci
## Use the ?compare_areas documentation
p <- compare_areas(data = df_ca, 
                   area = AreaName, 
                   value = Value,
                   fill = ComparedtoEnglandvalueorpercentiles,
                   lowerci = LowerCI95.0limit,
                   upperci = UpperCI95.0limit,
                   order = "desc",
                   top_areas = fixed_areas,
                   title = "Smoking: QOF prevalence (15+ yrs)
")
p
## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile-group/cardiovascular-disease-diabetes-kidney-disease/profile/cardiovascular/data#page/3/gid/1938133106/pat/223/par/E40000006/ati/221/are/nE54000039/iid/91280/age/188/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1


# Population --------------------------------------------------------------
## Execute the following lines of code to get 5 year age band population data from Fingertips
pop_data <- fingertips_data(IndicatorID = 92708,
                            AreaTypeID = 402)

pops <- pop_data %>% 
  filter(Age != "All ages",
         Sex %in% c("Male", "Female"),
         AreaName %in% c("England", "South East region", "Southampton"),
         Timeperiod == 2021) %>% 
  mutate(Age = factor(Age, 
                      levels = c("0-4 yrs", "5-9 yrs", "10-14 yrs", 
                                 "15-19 yrs", "20-24 yrs", "25-29 yrs",
                                 "30-34 yrs", "35-39 yrs", "40-44 yrs",
                                 "45-49 yrs", "50-54 yrs", "55-59 yrs",
                                 "60-64 yrs", "65-69 yrs", "70-74 yrs",
                                 "75-79 yrs", "80-84 yrs", "85-89 yrs", 
                                 "90+ yrs"))) # this orders the age bands correctly

## Complete the arguments for value, sex, age, area and area_name (try doing Southampton)
## Look at the ?population documentation for help
p <- population(data = pops,
                value = Value,
                sex = Sex,
                age = Age,
                area = AreaName,
                area_name = "Southampton",
                comparator_1 = "South East region",
                comparator_2 = "England",
                title = "Age Profile of Southampton compared to England and the South East region",
                subtitle = "2021",
                xlab = "% of total population")
p
## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile/child-health-profiles/data#page/12/gid/1938133263/pat/6/par/E12000008/ati/402/are/E06000045/iid/93563/age/34/sex/4/cid/4/page-options/ovw-do-0_cin-ci-4_car-do-0


# Boxplot -----------------------------------------------------------------

df_box <- df %>%
  filter(IndicatorID == 219,
         AreaType == "ICBs")

## Complete the arguments for timeperiod and value
## Use the ?box_plots documentation for help
p <- box_plots(df_box,
               timeperiod = Timeperiod,
               value = Value,
               title = "Hypertension: QOF prevalence (all ages)",
               subtitle = "In England",
               ylab = "Proportion (%)")

p

# Area profiles (spine chart)-----------------------------------------------------------
## Execute the following line to import data from the Cardiovascular profile
spine_data <- fingertips_data(DomainID = 1938133106,
                          AreaTypeID = 221,
                          ParentAreaTypeID = 223,
                          rank = TRUE)  # this is needed for polarity

# This gets the sequence of the indicators that is the same as on the website
ind_order <- indicator_order(DomainID = 1938133106,
                             AreaTypeID = 221,
                             ParentAreaTypeID = 223)

## Create a vector of the indicator names in the dislay order
indicator_levels <- spine_data %>% 
  left_join(ind_order, by = "IndicatorID") %>% 
  distinct(IndicatorName, Sequence) %>% 
  arrange(Sequence) %>% 
  pull(IndicatorName) %>% 
  rev() %>% 
  str_wrap(30)

## Apply these levels to the IndicatorName field, when turning it into a factor using the following code
spine_data <- spine_data %>% 
  mutate(IndicatorName = factor(str_wrap(IndicatorName, 30),
                                levels = indicator_levels))

## Filter the data for the latest time period for each indicator
df_spine <- spine_data %>% 
  inner_join(ind_order, by = c("IndicatorID", "Age", "Sex")) %>% # inner_join because there are some indicators that have multiple sex/ages
  group_by(IndicatorID) %>% 
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>% 
  ungroup()

# This exercise produces a spice chart with a trend arrow
## Complete value, count, area_code, indicator, timeperiod, trend, polarity, significance, area_type
## Use ?area_profiles documentation for support
p <- area_profiles(data = df_spine,
                   value = Value,
                   count = Count,
                   area_code = AreaCode,
                   local_area_code = "nE54000039",
                   indicator = IndicatorName,
                   timeperiod = Timeperiod,
                   trend = RecentTrend,
                   polarity = Polarity,
                   significance = ComparedtoEnglandvalueorpercentiles,
                   area_type = AreaType,
                   comparator_area_code = "E40000006")

p

## Compare your outputs to this one:
## https://fingertips.phe.org.uk/profile-group/cardiovascular-disease-diabetes-kidney-disease/profile/cardiovascular/data#page/1/gid/1938133106/pat/223/par/E40000006/ati/221/are/nE54000039/iid/91280/age/188/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1/page-options/ovw-do-0_cin-ci-4_car-do-NaN

# This exercise produces a spine chart without a trend arrow
## Try changing the some of the values for header_positions and seeing what effect this has on your plot when you print it to your plot window
p <- area_profiles(data = df_spine,
                   value = Value,
                   count = Count,
                   area_code = AreaCode,
                   local_area_code = "nE54000039",
                   indicator = IndicatorName,
                   timeperiod = Timeperiod,
                   # trend = RecentTrend,
                   polarity = Polarity,
                   significance = ComparedtoEnglandvalueorpercentiles,
                   area_type = AreaType,
                   header_positions = c(-1.83, -1, -1, -0.7, -0.5, -0.25, -0.05, 1.05),
                   header_labels = c("Indicator", "", "Time\nperiod", "Local\ncount",
                                     "Local\nvalue", "England\nvalue", "Worst/\nLowest", "Best/\nHighest"),
                   comparator_area_code = "E40000006")

p
