fluidPage(
  sidebarPanel(
    width=3,
    h4("Select summary source"),
    div(shinyTree("sclc-tree-stat", multiple = T,checkbox = T, theme="proton", themeIcons = FALSE, themeDots = FALSE)),
    div(radioButtons(inputId = 'sclc-stat-choice',label = "Display",choices = c('full table'='full','pre-defined set'='set'),inline = T,selected = 'full')),
    useShinyjs(),
    div(hidden(
      selectInput(inputId = "sclc-gslib-stat", label = "Select a gene set library", choices = names(GS.libraries)),
      selectInput(inputId = "sclc-gs-stat", label = "Select a gene set", choices = NULL)
    )),
    useShinyjs(),
    div(hidden(
      selectInput(inputId = 'sclc-gs_subgroup-stat',label = "Select a subset",choices=NULL)
    )),
    actionButton('sclc-summary-submit',label = 'Submit')
  ),
  mainPanel(width=9,
            h5("Cross-study Summary"),
            br(),
            DT::dataTableOutput('sclc-overall_stat')%>%withSpinner(color="#000080")
  )
  
)
