
## Objective 

The objective of this project is to analytically explore the relationship 
between water quality features(pH, temperature, conductivity, turbidity) and 
dissolved oxygen levels while also presenting a predictive model for potential 
DO levels.


## Data
Data consists of 500 rows (observations) and 6 columns (water quality features) 
but is manipulated (dropping Sample.ID) to fit this project. 

Data was downloaded as a .csv file and imported into R.
Data can be viewed via the link below:
https://www.kaggle.com/datasets/shreyanshverma27/water-quality-testing

## Dependencies:
library(ggplot2)

library(gridExtra)

library(scatterplot3d)

library(MASS)

library(marginaleffects)

library(GGally)

library(ggcorrplot)

library(mand)

library(marginaleffects)

library(tidyverse)

library(nnet)

require(graphics)

## Project recreation:
In order to recreate this project, the above dependencies must be loaded into R.
Then the code provided in the scripts folder titled analysis, should be run and
the analysis of the water quality data will take place.
