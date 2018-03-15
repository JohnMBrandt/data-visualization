# data-visualization

## Static Visualization

### Directory

Results are found in the `images` folder, with corresponding code in the `code` folder.

### iphone_data.R
Use dplyr to clean and reshape XML data downloaded from the iPhone's Health application. Visualize the number of steps taken at each hour of the day using ggplot2, broken down by month.
![](https://github.com/JohnMBrandt/data-visualization/blob/master/Static%20Visualizations/images/steps.png?raw=true)

### calendarmap.R
Use base R plotting functions to generate a daily heatmap of any variable (in this case, steps / day).
![](https://github.com/JohnMBrandt/data-visualization/blob/master/Static%20Visualizations/images/calendar1.png?raw=true)

### ozone.R
Use dplyr and ggplot2 to visualize ozone concentrations in different provinces of Spain.
![](https://github.com/JohnMBrandt/data-visualization/blob/master/Static%20Visualizations/images/Sample2.png?raw=true)

### science_giving.R
Using data from fivethirtyeight, visualize the political donations of scientists in the US based upon political party.
![](https://github.com/JohnMBrandt/data-visualization/blob/master/Static%20Visualizations/images/sample1.png?raw=true)

### ggplot_visualization 

Rmarkdown outlining the process of creating a publication-ready visualization.

## RShiny

Contains a RShiny application to interactively select columns to scatter plot and fit smoothed linear models to. Contains tooltips and color selections.

## Data Cleaning

Workshop on data cleaning with tidyr to convert scraped nested matrices to a dataframe that can be visualized.

