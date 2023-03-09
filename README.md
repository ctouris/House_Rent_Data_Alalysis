# House_Rent_Data_Alalysis
 R script that predicts rental price from a given data set
The code provided seems to be an exploratory analysis of the "House Rent Dataset". 
The dataset is loaded from a CSV file, NA values are removed, and then outliers are 
detected and removed from the Rent, Bathroom, Size, and BHK variables using the 
boxplot method. The ggplot2, lattice, caret, gapminder, tidyverse, and dplyr 
libraries are loaded, and some graphs are plotted using ggplot2.

The code performs a univariate and bivariate analysis of the variables in the dataset.
For instance, boxplots are plotted to explore the relationship between Rent and other 
variables like BHK, Size, Bathroom, City, Area.Type, Furnishing.Status, Tenant.Preferred, 
and Point.of.Contact. Also, histograms, density plots, scatter plots, and bar plots are 
used to understand the distribution of variables and their relationship with Rent.
