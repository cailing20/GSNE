# setwd('/project/CRI/DeBerardinis_lab/lcai/SCLC/app/')

source('SCLC_prep.R')
ui <- navbarPage(title = "Gazdar Small Cell Lung Cancer Neuroendocrine Explorer",   
                 theme = shinytheme("spacelab")
                 ,tabPanel(title = "Tutorial",
                           source(file.path("UI/tutorial_UI.R"),  local = TRUE)$value
                 )
                 ,tabPanel(title = "Heatmap",
                          source(file.path("UI/heatmap_UI.R"),  local = TRUE)$value
                 )
                 ,tabPanel(title = "Cross-study Summary",
                           source(file.path("UI/cross_study_UI.R"),  local = TRUE)$value
                 )
                 ,tabPanel(title = "Pathway Analyses",
                           source(file.path("UI/pathway_analysis_UI.R"),  local = TRUE)$value
                 )
                 ,tabPanel(title = "References",
                           fluidPage(mainPanel("References",DT::dataTableOutput('sclc-reference'),width = 12))
                 ),
                 tabPanel(title = "About",
                          div(
                            h4('This web app is named after Dr. Adi F. Gazdar (1937-2018) in loving memory of his contribution to SCLC research and mentorship.',br(),br(),
                               'In pre-defined geneset libraries, "Gazdar Gene Family Colection" and "Gazdar Gene Family Collection (with subgroup)" are genes carefully curated by Dr. Gazdar.',br(),br(),
                               'Contact Ling[dot]Cai[at]UTSouthwestern[dot]edu for questions.')
                          )
                 )
                 ,tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }",
                             ".navbar-nav li a {font-size: 18px; }"
                 )
)
server<- function(input, output, session) {
  output$`sclc-reference`<-DT::renderDataTable(datatable(study.summary[,c('study','source','approach','species','material','dim','PMID','accession','Reference'),with=F], rownames = F,filter = 'top',options = list(pageLength = 15),escape = F))
  source(file.path("Server/tutorial_Server.R"),  local = TRUE)$value
  source(file.path("Server/heatmap_Server.R"),  local = TRUE)$value
  source(file.path("Server/cross_study_Server.R"),  local = TRUE)$value
  source(file.path("Server/pathway_analysis_Server.R"),  local = TRUE)$value
}


shinyApp(ui = ui, server = server)