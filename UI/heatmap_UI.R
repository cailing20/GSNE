fluidPage(
  
  sidebarPanel(
    useShinyjs(),
    radioButtons('sclc-source-hm',label = "Select source",choices = c('SCLC','NBL'),selected = 'SCLC'),
    radioButtons('sclc-species-hm',label = "Select species",choices = c('human','mouse'),selected = 'human'),
    radioButtons('sclc-material-hm',label = "Select sample",choices = unique(study.summary$material),selected = 'unsorted tumor'),
    selectInput(inputId = "sclc-ds-hm", label = "Select a dataset", choices = with(study.summary[source=='SCLC'&species=='human'&material=='unsorted tumor'],structure(study,names=study.name)),selected = "George_2015"),
    hr(),
    radioButtons(inputId = "sclc-gs_choice-hm",label = "Select genes",choices = c('use predefined genes'='existing_gs','enter genes'="user_gs")),
    useShinyjs(),
    div(
      selectInput(inputId = "sclc-gslib-hm", label = "Select a gene set library", choices = names(GS.libraries)),
      selectizeInput(inputId = "sclc-gs-hm", label = "Select a gene set", choices = NULL)
    ),
    div(hidden(
      selectInput(inputId = 'sclc-gs_subgroup-hm',label = "Select a subset",choices=NULL)
    )),
    div(hidden(
      selectizeInput(inputId = "sclc-user-hm",label = "Paste or enter comma delimited genes",multiple = TRUE,
                     choices = 'x',
                     options = list(
                       delimiter = ',',
                       create = I("function(input, callback){
          return {
            value: input,
            label: input
           };
        }")
                     )
                     )
    )),
    div(hidden(checkboxInput(inputId = 'sclc-user-human_gene',label = "Use human gene symbol as input",value = F))),
    hr(),
    checkboxGroupInput(inputId = "sclc-hm_annot", label="Heatmap Annotation (choose at least one)", 
                       choices = c('neuroendocrine score'='NE','4 transcription factors'='TF'), selected = 'NE'),
    radioButtons(inputId = 'sclc-hm_order',label = 'Heatmap Sample Order',
                 choices = c('neuroendocrine score'='NE','4 transcription factors'='TF','data'='data')),
    radioButtons(inputId = 'sclc-hm-scaling','label'='z-transform expression data by gene',choices = c('yes','no')),
    actionButton('sclc-submit-hm',"Submit")
    
  ),
  mainPanel(
    h5("Heatmap"),
    br(),
    plotOutput('sclc-hm',height = '800px')%>%withSpinner(color="#000080")
  )
)



  
  
