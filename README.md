## LiMMCov Shiny application   
*A research tool to guide users in the process of selecting covariance structures in linear mixed models.*  

LiMMCov uses time series insights for selecting complex covariance structures. The tool offers visualisations to assess the residuals of fixed effects models and offer suggestions for the correct structure based on the data. The application is based on the Shiny package and can be run locally or on a server. The user-friendly interface enables the user to navigate the workflow. A report can be downloaded with the results. 

The application is freely accessible at <https://zq9mvv-vub0square.shinyapps.io/LiMMCov-research-tool/>


### User-interface
The web application provides a user-interface (UI) which comprises of main tabs: Home, Analysis, Manual and Contact.   

* The Analysis tab has the following tabs to manage the workflow:
  +	Dataset - data upload, view, and manipulation.
  +	General Linear Model (GLM) - for the initial fixed effects model.
  +	Covariance Analysis - to visualize the residuals extracted from the fixed effects model.
  +	Linear Mixed Model (LMM) - to fit the mixed model based on the selected covariance structure.
  +	Reports - to download reports.

Each tab is associated with an R code to perform analyses. These outputs are conditional on the sidebar panel inputs, which is achieved through a dynamic UI.

### Code Structure
The 'app.R' file is the main R file; all the other R files are called here.

* There are two main functions defined: ui and server:
  1. The UI part of Shiny, written in HTML and CSS, contains the main ui functions that call the other ui functions and set up the ui structure (sidebar panel/main panel). It handles user input, server output and UI display.

  2. The server part of Shiny, written in R, contains functions which handles the input from ui and process the output within a reactive value. These functions return a reactive value output. It processes the ui input to calculate output, communicates via keywords associated to each input and output functions declared in the ui function.  

            
Version 19-Dec-2024
