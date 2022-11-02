install.packages('shiny', dependencies=TRUE, repos='https://cran.rstudio.com/')
install.packages('shinyauthr', dependencies=TRUE, repos='https://cran.rstudio.com/')
shinytest::installDependencies()

install.packages(pkgs=c('shinydashboard', 'tidyr', 'ggplot2', 'sf', 'lubridate', 'stringr', 'plotly',
	'shinyBS', 'shinyjs', 'leaflet', 'shinyalert', 'magrittr', 'shinycssloaders', 'reactable',
	'tippy', 'shinyWidgets', 'auth0', 'data.table', 'dplyr', 'sp', 'future', 'future.apply', 'foreach',
	'doParallel', 'wrapr'), dependencies=TRUE, repos='https://cran.rstudio.com/')

install.packages('devtools', dependencies=TRUE, repos='https://cran.rstudio.com/')
devtools::install_github('onaio/ona.R')
