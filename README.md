# dc_crime_data
Statistics and Analysis Final Project on D.C. Crime Data

Everything was done in R Studio

Introduction
This project was prompted by this November 2020 article that I read, headlined, D.C.’s Homicide Numbers For 2020 Surpassed Last Year’s".

“There have been 167 homicides in D.C. this year, topping last year’s total of 166, the highest since 2008.”
“At the same time, violent crime in general has fallen 6% over the last year. Reports of sex abuse and robbery have dropped 18% and 13%, respectively, according to police data. Property crime has also sunk 20%. This parallels a national trend of overall crime quieting down after stay-at-home orders took effect in the early days of the pandemic.”

Questions
Is there a statistically significant decrease in crime that happened after the pandemic shut down the city?
Is there a relationship between the number and/or types of crimes and specific demographics of an area ? E.g. Are the “richer” parts of DC less prone to crime or less prone to more violent types of crime?
How are various types of crimes distributed around Washington, D.C.? E.g. Are auto thefts more common in certain parts of the city?

Summary of Results
Yes, there was a statistically significant decrease in crime after the pandemic shut down the city.
There seems to be a negative relationship between median age and reported violent crime.
There seems to be a positive relationship between % Asian and % Hawaiian/Pacific Islander within Census Tracts and reported non-violent crime.
Generally, however, I am hesitant to draw any conclusions because all the models I ran had low adjusted R2, the significance of the coefficients vary.

Datasets
I downloaded all my datasets from Open Data DC

ACS_2019_Median_Household_Income_Variables_Tract
ACS_Demographic_Characteristics_DC_Census_Tract
Crime_Incidents_in_2019
Crime_Incidents_in_2020
Crime_Incidents_in_2021

Dataset Notes
From the 2019 Median Household Income Data, I took the Census Tract, Median HH Income, and Total HH variables
From the DC Demographic Characteristics Data, I took the Census Tract, (calculated) Population Density, Sex Ratio (# of Males per 100 Females), Median Age, (calculated) % of each reported race (White, Black/African American, American Indian/Alaska Native, Asian, Hispanic/Latino, Hawaiian/Pacific Islander, Other Race, and 2 or More Races)
From the Crime Data, I took the Census Tract, Report Date, Report Time, Offense Type, Latitude and Longitude, and Ward.
