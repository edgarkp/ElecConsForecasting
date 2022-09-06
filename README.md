# Electricity Consumption Forecasting
The following study aims to forecast the electricity consumption of a particular building during the 18th February 2010. The forecasting will also assess the impact of the outdoor air temperature.


## Dataset
The dataset provided is found in an Excel file called *Elec-train* and gives the following features:  
*1) Timestamp:* A time value at which each quantity has been recorded. each timestamp is given every 15 minutes from 1/1/2010 1:15 to 2/18/2010 23:45     
*2) Power:* The electricity consumption in kW. It is measured every 15 minutes, from 1/1/2010 1:15 to 2/17/2010 23:45   
*3) Temp:* The outdoor air temperature in ?C. It is measured every 15 minutes from 1/1/2010 1:15 to 2/18/2010 23:45   


## Approach
Given the nature of the dataset, we will forecast using :   
**- an univariate approach** i.e. without the outdoor air temperature   
**- a covariate approach** i.e. with the outdoor air temperature    


## Forecasting Models
The goal is to get the best forecasts by navigating through various time-series models such as :   
- Holt Winters   
- ARIMA / SARIMA   
- NNAR   
- Dynamic regression models   


## To run the project
* Run the file *elec_cons_forecast.Rmd*   


## Closing
Feel free to comment and add any constructive critics that could help me better my solution :)    



