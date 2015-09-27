# DevDataProductProject
This Shiny project access the online NOAA information available for hurricanes and
tropical storms since the year 1851. The data is divided into Pacific and Atlantic data
sets. 

The UI allows the user to select any year of data associated with Atlantic hurrricane
season. The information for the selected year is retrieved from the NOAA weather site, 
cleaned and transformed for visualization. As part of the transforming of the data, the
Names are split into stormType and Name, the Date is transformed into start and end R
Date objected and the period for each storm is calculated for presentation to the user.

In addition to the plotted storm information indicating 'Category', 'Name', and 'Duration',
the summary of the storm informaiton is provided along with a data frame showing the
observations from the selected year. Selecting a new year will cause the information in
the plot and table to be updated.
