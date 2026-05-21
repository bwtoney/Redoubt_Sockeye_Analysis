# Redoubt_Sockeye_Analysis
This repository is designed to facilitate shared analysis of Redoubt Lake Sockeye Escapement and run timing between project partners. 
All data files are located in the data/raw folder and reflect untransformed files sourced from ADFG and FS via personal communication with area biologists
RunTiming.R is a script that explores modelling weir count data as a function of 'Day of Year' - via GLM and GAM frameworks (beware the GAMs) take a while
ImputationSimulation.R builds upon the aforementioned models to explore alternative approaches to imputing or modelling "missing days" 


The overall intent behind this repository is to explore run timing and compare alternative strategies for filling in the gaps of missing data.
The simulation is intended to allow managers to make decisions regarding the increase in uncertainty associated with missing more days of monitoring, as well as to test future scenarios for how the imputation simulation performs.
