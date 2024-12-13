# ShinyEcotox
"ShinyEcotox" is a freely available tool for analysis of standradized ecotoxicity tests. "ShinyEcotox" is based on R and the "drc", "multcomp", and "shiny" R packages.
  

    
# How to use 1: on your local PC
Install and load the Shiny and other R packages in your R session and perform the following command:
```r
# If necessary, install the following packages.
install.packages("shiny")
install.packages("drc")
install.packages("knitr")
install.packages("multcomp")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("here")
install.packages("shinyjs")
install.packages("shinycssloaders")
install.packages("shiny.i18n")
install.packages("DT")
install.packages("mvtnorm")

# If you have already installed packages, please enter the following command.
library(shiny)
runGitHub("KyoHiki/drc_ecotox")
```

    
# How to use 2: through Cloud server
Access: https://nies-ecotox.shinyapps.io/Shiny_io/
