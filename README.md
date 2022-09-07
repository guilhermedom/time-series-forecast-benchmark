# Time Series Forecast Benchmark

Benchmarking app for time series forecasting methods.

---

### Usage

App currently deployed and runnable at: https://domdatascience.shinyapps.io/time-series-forecast-benchmark/

### App Features

Compare different time series forecasting methods using a single plot:

* The user can browse the file system and input any file having a monthly time series;
* A date range can be defined for the input time series with start and end date. This range is used to form the "Time" x-axis of the plot;
* The user can also input any number of months to be forecasted using 10 forecasting models:
    1. Naive Forecasting;
    2. Mean Forecast;
    3. Random Walk Forecast with Drift;
    4. Holt's Linear;
    5. Holt-Winters Additive;
    6. Holt-Winters Multiplicative;
    7. Holt-Winters Multiplicative with Drift;
    8. ARIMA;
    9. Linear Model;
    10. Neural Network.
* A plot is created with the original time series and the amount of months the user wants to forecast;
* The user can select models to make their forecasting curves visible on the plot;
* A small table is created for each one of the 10 forecasting models. Error metrics and other quality measures for each prediction method are shown.

### User Interface Sample

![ui_time-series-forecast-benchmark](https://user-images.githubusercontent.com/33037020/185526253-af93e7be-c143-4fe0-8090-a4106c596e14.png)

*[Shiny] is a framework that allows users to develop web apps using R and embedded web languages, such as CSS and HTML. Shiny apps focus on objectiveness and simplicity: only one or two R scripts have all the code for the app.*

*This app development started with knowledge and tools discussed during the course "Data Science Bootcamp" by Fernando Amaral. The app has been upgraded and personalized, adding new functionalities.*

[//]: #

[Shiny]: <https://www.shinyapps.io>
