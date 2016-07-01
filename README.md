# Finance scripts for R

This repository contains several finance scripts for r

 - beta.r is a simple script to calculate the beta for a stock on the ASX using yahoo finance data.
 - beta - asx200.r is similar to above but contains a function that scrapes the current asx200 constituents from the web and calculates the beta for all.
 - capm.r This using the beta script as a base but calculates the capm as well.
 - correlation.r This script will get a list of stocks and create a correlation matrix and plot from the list.

All of the scripts use Quandl data and are use ASX listed stocks but that can be easily changed.
