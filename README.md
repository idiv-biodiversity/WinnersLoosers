# WinnersLoosers

## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Description and Rationale](#description-and-rationale)
* [Functional example](#functional-xample)

## General info
This is the code for an App creating Figures (and providing the underlying data) for the balance between winners and loosers in occurrence change for aprox 2200 plant taxa in Germany between 1960 and 2017.
This App is a companion to the Scientific publication of Eichenber et al (2020): "Widespread decline in Central European plant diversity across six decades" published in Global Change Biology.
	
## Technologies
It is written in R, using - inter alia - the extensions and functions available from 
 - rshiny, 
 - plotly 
 - ggplot2

The files necessary to run the app are not available here, but can be downloaed from Electronic appendix of the above mentioned publication.

## Description and Rationale
The App allows the user to zoom into the plot, so that the plethora of information can be investigated by the user in detail.
It offers three different Tabs, each with different pieces of information.
One tab shows the relative values of changes in occurrence in %, the second one shows the same information in absolute values.
The thrid tab iges a brief introduction into the underlying methodology.
For the ease of interpretation, the plot in the upper half of the app is connected via a mouse-hover function to the table shown in the lower part. 
It contains a selection function, so that the user can hover over the plot and the alogrithm will automatically filter the table and provide the tabular information of the selected species.

NOTE: the files are sorted in decreasing order of changes.

## Functional example
To view the full functionality, please visit the website https://shiny.idiv.de/de25geka/WinnersLosers/. 

