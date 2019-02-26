# hask-can-weather
Average monthly high temperature for Ottawa, ON. Smoothed with a 4 year moving average.
<img src="Figure_1_cropped.png?raw=true" width="80%">

Cammand to retreive the data. This gets data from 1890 to 2018 from station 4333.
```bash
for year in `seq 1890 2018`;do wget --content-disposition "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=4333&Year=${year}&Day=14&timeframe=2&submit= Download+Data" ;done
```
## Installation

* Make sure you have the dependencies for [matplotlib in haskell](http://hackage.haskell.org/package/matplotlib).
* Build the repository with `satck build`
* Run the command either through ghci or install using `stack install`

## Ex
```bash
# Install dependencies for matplotlib

# Clone and build
git clone https://github.com/tvieregge/hask-can-weather.git
cd hask-can-weather
stack build

# Get the data, it can be put anywhere
cd data
for year in `seq 1960 2018`;do wget --content-disposition "http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=4333&Year=${year}&Day=14&timeframe=2&submit= Download+Data" ;done

# Run the project, '.' being the current directory (which has the data) and 4 being the size of the window for smothing
stack ghci
prelude> :main . -w 4
```
