# Smartphone Price Prediction

This repository contains the codes for a academic project I did at Purdue from Nov to Dec'2021. Learn more about the project [here](https://asadhusain97.github.io/projects/smartphonepricer.html)

## App Link

Do checkout the live R-shiny app at https://asadman.shinyapps.io/Final_Project/

## Problem
In the smartphone industry, getting the price right has the maximum impact on profitability. 
This app aims to help mobile phone companies determine the optimal price range that is prevailing in the market based on the specifications of their phone.
The target variable was the price segment from 1 to 4. 


## What did we do?

**Step 1 - Understanding data**

We had 2000 device data with 20 specifications for each device including battery size, screen size, screen resolution, ram, 5G capability, etc.   

**Step 2 - Exploratory Data Analysis**

We made beautiful graphs using the ggplot library in R to see how each specification varied with different price ranges. We calculated correlation 
between all columns to reduce multicollinearity.

 **Step 3 - Modelling**
 
We tried multiple machine learning models and got the best results with random forest model. 

**Results**

Overall our app predicted the correct price range with 94% accuracy. 
