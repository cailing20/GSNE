fluidPage(
  radioButtons(inline = T,'tutorial-choose',label = "Functionality:",
               choices = c("Heatmap"="1","Cross-study Summary"="2","Pathway Analyses"="3","References"="4")),
  fluidRow(column(1),column(10,shinycssloaders::withSpinner(slickROutput("tutorial-slickr"))),column(1)))