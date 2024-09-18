# Assignment 1 for DATA2902

This repository contains the code and resources for **DATA2902** individual assignment. This assignment involves creating an interactive web application using `R Shiny`. This assignment took a lot of time and effort, but the end result paid off immensely.

## Data Set

The data used in this project was collected through a **Google Forms** survey. The survey was sent to students in **DATA2X02**, and it mainly gathered demographic and other relevant information. This data was then cleaned and analyzed using `R`, specifically by the `datacleaning.rmd` file.

The cleaned data file can be found in the repository as `df_cleaned.csv`.

## User Guide

The User Guide on the App, how to use it, how many tabs it has, and the structure of the code could be found in the `DATA2902_User_Guide` PDF.

## Shiny App

The **Shiny App** for this project is hosted online and can be found [here](https://oscarpham2090.shinyapps.io/shiny-app/).

Some key features of the app:
- Interactive visualizations and hypothesis tests that allow users to explore the survey data.
- Dynamic filters and controls for better user experience.
- A clean and user-friendly UI.

All the Shiny code is structured as follows:
- **server.R**: Contains the backend logic and server-side functions for the app.
- **ui.R**: Handles the user interface, layout, and input-output relationships in the app.

The code for both can be found in the repository under `shiny-app` folder.
