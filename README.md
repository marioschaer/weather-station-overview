# Weather Station Data – Monthly Visualization

## Project Description

This R script provides a monthly overview and visualization of meteorological measurements from a weather station.

The analysis is based on an example dataset containing hourly measurements from a **Davis Vantage Pro 2 weather station** located in Reinach, Basel-Landschaft, Switzerland.

## Features

The script generates a monthly overview, including:

- **Temperature [°C]:** Hourly temperature, daily maximum and minimum temperatures, and monthly mean temperature.
- **Atmospheric Pressure [hPa]:** Hourly pressure measurements.
- **Precipitation [mm]:** Daily precipitation totals and monthly accumulated precipitation.
- **Wind [km/h]:** Mean wind speed, gusts and wind direction.
- **Wind Rose:** Graphical representation of wind direction frequencies.
- **Climate Comparison:** Comparison of monthly temperature and precipitation measurements with climatological reference values from Basel-Binningen (MeteoSwiss).

## Example Data

An included example dataset with hourly meteorological measurements demonstrates the functionality of the script.

The dataset contains the following variables (in this order):

- Date
- Time
- Mean Temperature
- Hourly maximum
- Hourly minimum temperature
- Relative humidity
- Dew point
- Atmospheric pressure 
- Mean wind speed
- Maximum wind gusts
- Wind direction
- Precipitation
- Solar Radiation

## Requirements

- R packages used: ggplot2, windRose, climaemet, grid, gridBase

## Example Output
<img src="example_output.png" width="60%">
