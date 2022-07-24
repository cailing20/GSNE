fluidPage(
  sidebarPanel(
    useShinyjs(),
    div(
      h4('Select NE - transcriptome correlation result to analyze'),
      div(shinyTree("sclc-tree-pa", multiple = F,checkbox = F, theme="proton", themeIcons = FALSE, themeDots = FALSE)),
      radioButtons(inputId = "sclc-direction-pa",inline = T,label = "Choose genes by direction of NE correlation",choices = c('positive'='positive','negative'='negative')),
      numericInput(inputId = "sclc-num-pa", label = "Number of top genes for enrichment analysis",  min = 10, max = 5000, value = 100),
      selectInput(inputId = "sclc-gslib-pa", label = "Select a gene set library", choices = names(GS.libraries)[-c(1:3)])
    ),
    hr(),
    actionButton('sclc-submit-pa',"Submit")
    
  ),
  mainPanel(
    h5("Pathway Analysis Results"),
    br(),
    DT::dataTableOutput('sclc-results-pa')%>%withSpinner(color="#000080")
  )
  
)
