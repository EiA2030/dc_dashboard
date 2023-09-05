install.packages(pkgs=c('devtools', 'remotes'), dependencies=TRUE, repos='https://cran.rstudio.com/')

install.packages(pkgs=c('shiny', 'shinyauthr', 'shinydashboard', 'tidyr', 'ggplot2', 'sf', 'lubridate', 'stringr', 'plotly',
	'shinyBS', 'shinyjs', 'leaflet', 'shinyalert', 'magrittr', 'shinycssloaders', 'reactable',
	'tippy', 'shinyWidgets', 'auth0', 'data.table', 'dplyr', 'sp', 'future', 'future.apply', 'foreach',
	'doParallel', 'wrapr'), dependencies=TRUE, repos='https://cran.rstudio.com/')

shinytest::installDependencies()

devtools::install_github('onaio/ona.R')
remotes::install_github('rapidsurveys/odktools')
install.packages("cronR", repos = "http://cran.us.r-project.org")

install.packages('shinythemes', repos='https://cran.rstudio.com/')
install.packages('rmarkdown', repos='https://cran.rstudio.com/')
tinytex::install_tinytex()