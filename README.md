# BrokenBikes
This repository contains code for the implementation of the broken bikes detection method developed as part of the (ongoing) SmartDublin Active Travel Competition 2021. The method aims to attach a score, related to the probability the bike is unusable, for each bike in the Bleeper Bikes bike-sharing network in Dublin, Ireland. As the competition continues, this repository will be updated as work progresses with data, code and analysis of the method's strengths and weaknesses. 

Items currently in the repository:
- **bike_scrape.R** This script scrapes the Bleeper Bikes JSON file which updates at 5-minutes intervals and saves to .csv files. 
- **weather_scrape.R** This script scrapes weather information from Weather Underground and updates at 2-hour intervals. Scraped data is saved to a .csv. Note: The API key used to scrape from WeatherUnderground has been removed from the script and must be replaced for the code to work. 
- **grids.csv**  Our method divides the Dublin region into 100 grids, for which contextual environmental information such as elevation, light pollution etc. is captured. This file contains information for the associated grids. 
- **PrepData.R** This script reads in the Bike.csv, weather.csv and grids.csv files, cleans and augments them with features constructed to aid the detection of broken bikes. For each bike, the distance to the next nearest bike, the length of the previous trip and the time since the last trip are included. 
- **RF.R** This script implements V1 of the broken bike detection method. Using a 24hr period of bike data, a Random Forest regression model is trained to predict the time since the last trip based on environmental, weather and bike-specific features. This model is stored and used to produce a 'predicted stationary time' score for each bike, based on its last trip and location in the city. Bikes that have been stationary for significantly longer than was predicted by the model are flagged as potentially broken. A sample image of the scores is given below. 
- **Animate.R** This script can be used to create gifs of bikes moving around the city using the live-updating JSON files. 

<img src="bikes_129-153.gif" width = "400" /> <img src="bike_125_map.png" width = "400" />
