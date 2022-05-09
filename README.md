# Porfolio Optimization



## General Information
> This R code allows the user to input a list of companys' stock exchange tickers and will determine how much stock 
> of each company to purchase in order to create the most optimal, diversified portfolio.



## Setup
Requirements include access or knowledge of companys' stock exchange tickers. 
I suggest using Yahoo Finance



## Usage
Create a CSV file named BETAS and vertically list the companys' tickers. 
Be sure to have your directory set correctly

## Read the betas file.  It is read into a data frame
betasFile <- read.csv("betas.csv",header=TRUE)



## Acknowledgements
Many thanks to Professor Lewis of Wealth Management for making this code possible and having one of the most enjoyable classes of my college career.



