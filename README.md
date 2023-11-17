# BactEXTRACT

BactEXTRACT is a novel R Shiny application designed to streamline the analysis and visualization of bacterial growth data, addressing the increasing demands for straightforward data processing tools. It simplifies the transition from raw optical density measurements to publication-ready plots, offering a user-friendly interface that diminishes the complexity involved in growth curve analysis. 


## Installation

The application can be used as a local R Shiny app, by downloading the latest version on github:
``` {r}
install.packages("devtools")
library(devtools)
install_github("JulDenereaz/BactEXTRACT")
```

In addition, BacTEXTRACT is available at https://veeninglab.shinyapps.io/BactEXTRACT/

## Workflow

<img src="https://i.imgur.com/UOBAfZE.png" alt="BactEXTRACT Workflow">

<b>A:</b> Raw data from common plate readers and simple text files can be input into the software. Multiple files can be uploaded at the same time. <b>B:</b> The web interface of BactEXTRACT allows for the quick editing of any parameters and customized theming. <b>C:</b> BactEXTRACT fits a logistic curve to every sample. Different graphical layers such as color, shape, linetype, and facet can be chosen to allow for clear separation of all groups present in the data. <b>D:</b> Standard OD growth plot, on logarithmic or linear axis can be plotted. <b>E:</b> Luminescence and fluorescence sub-tables (if any) can be selected and plotted as well, either normalized by OD, or as a standalone plot. <b>F-G:</b> Each growth parameters output from the logistic fitting can be plotted, either as bar plot (<b>F</b>) or as checkerboard plot which is particularly handy in case combinations of antimicrobial compounds are being tested (<b>G</b>).

