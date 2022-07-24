# server<- function(input, output, session) {
# if user input is chosen, hide gene set library options
output$`sclc-hm`<-NULL
hm.dat<-reactive({readRDS(paste0('data/expr-',input$`sclc-ds-hm`,'.rds'))})
hm.mmu2hsa<-reactive({as.data.table(merge(data.frame(gene=colnames(hm.dat())),ref.cbn,by.x='gene',by.y='mouse'))})
hm.geneOptions<-reactive({
  if(input$`sclc-species-hm`=='mouse' & input$`sclc-user-human_gene`) hm.mmu2hsa()$human else colnames(hm.dat())
})
observeEvent(input$`sclc-source-hm`,{
  updateRadioButtons(session = session,'sclc-species-hm',choices = unique(study.summary[source==input$`sclc-source-hm` & material==input$`sclc-material-hm` & expr][['species']]),selected = input$`sclc-species-hm`)
  updateRadioButtons(session = session,'sclc-material-hm',choices = unique(study.summary[source==input$`sclc-source-hm` & species==input$`sclc-species-hm` & expr][['material']]),selected = input$`sclc-material-hm`)
  updateSelectInput(session = session,'sclc-ds-hm',choices = with(study.summary[source==input$`sclc-source-hm` & species==input$`sclc-species-hm` & material==input$`sclc-material-hm` & expr],structure(study,names=study.name)))
})
observeEvent(input$`sclc-species-hm`,{
  updateRadioButtons(session = session,'sclc-source-hm',choices = unique(study.summary[species==input$`sclc-species-hm` & material==input$`sclc-material-hm` & expr][['source']]),selected = input$`sclc-source-hm`)
  updateRadioButtons(session = session,'sclc-material-hm',choices = unique(study.summary[species==input$`sclc-species-hm` & source==input$`sclc-source-hm` & expr][['material']]),selected = input$`sclc-material-hm`)
  updateSelectInput(session = session,'sclc-ds-hm',choices = with(study.summary[source==input$`sclc-source-hm` & species==input$`sclc-species-hm` & material==input$`sclc-material-hm` & expr],structure(study,names=study.name)))
  if(input$`sclc-species-hm`=='mouse' & input$`sclc-gs_choice-hm`=='user_gs'){
    shinyjs::show('sclc-user-human_gene')
  }else{
    shinyjs::hide('sclc-user-human_gene')
    updateCheckboxInput(session = session,inputId = 'sclc-user-human_gene',value = F)
  }
})
observeEvent(input$`sclc-material-hm`,{
  updateRadioButtons(session = session,'sclc-source-hm',choices = unique(study.summary[material==input$`sclc-material-hm` & species==input$`sclc-species-hm` & expr][['source']]),selected = input$`sclc-source-hm`)
  updateRadioButtons(session = session,'sclc-species-hm',choices = unique(study.summary[material==input$`sclc-material-hm` & source==input$`sclc-source-hm` & expr][['species']]),selected = input$`sclc-species-hm`)
  updateSelectInput(session = session,'sclc-ds-hm',choices = with(study.summary[source==input$`sclc-source-hm` & species==input$`sclc-species-hm` & material==input$`sclc-material-hm` & expr],structure(study,names=study.name)))
})
observeEvent({input$`sclc-ds-hm`;input$`sclc-user-human_gene`},{
  updateSelectizeInput(session,inputId = 'sclc-user-hm',choices = data.frame(value=hm.geneOptions(),label=hm.geneOptions()),server = T,
                       options = list(
                         delimiter = ',',
                         create = I("function(input, callback){
          return {
            value: input,
            label: input
           };
        }")
                       ))
})

hm.NE<-reactive({readRDS(paste0('data/NE-',input$`sclc-ds-hm`,'.rds'))})
observeEvent(input$`sclc-gs_choice-hm`,{
  if(input$`sclc-gs_choice-hm`=='user_gs'){
    shinyjs::hide('sclc-gslib-hm')
    shinyjs::hide('sclc-gs-hm')
    shinyjs::hide('sclc-gs_subgroup-hm')
    if(input$`sclc-species-hm`=='mouse') shinyjs::show('sclc-user-human_gene')
    shinyjs::show('sclc-user-hm')
  }
  if(input$`sclc-gs_choice-hm`=='existing_gs'){
    shinyjs::show('sclc-gslib-hm')
    shinyjs::show('sclc-gs-hm')
    if(input$`sclc-gslib-hm`=="Gazdar Gene Family Collection (with subgroup)") shinyjs::show('sclc-gs_subgroup-hm')
    shinyjs::hide('sclc-user-hm')
    shinyjs::hide('sclc-user-human_gene')
    updateCheckboxInput(session = session,inputId = 'sclc-user-human_gene',value = F)
  }
})
observeEvent(input$`sclc-gslib-hm`,{
  shinyjs::hide('sclc-gs_subgroup-hm')
  updateSelectizeInput(session,inputId = "sclc-gs-hm",choices = names(GS.libraries[[input$`sclc-gslib-hm`]]),server = T)
  if(input$`sclc-gslib-hm`=="Gazdar Gene Family Collection (with subgroup)"){
    shinyjs::show('sclc-gs_subgroup-hm')
  }
})
observeEvent(input$`sclc-gs-hm`,{
  if(input$`sclc-gslib-hm`=="Gazdar Gene Family Collection (with subgroup)"){
    updateSelectInput(session,inputId = 'sclc-gs_subgroup-hm',choices = names(GS.libraries[["Gazdar Gene Family Collection (with subgroup)"]][[input$`sclc-gs-hm`]]))
  }
})
observeEvent(input$`sclc-hm_annot`,{
  updateRadioButtons(session,inputId = 'sclc-hm_order',
                     choices = c('neuroendocrine score'='NE','4 transcription factors'='TF','data'='data')[setdiff(c(ifelse('NE'%in%input$`sclc-hm_annot`,1,NA),ifelse('TF'%in%input$`sclc-hm_annot`,2,NA),3),NA)]
  )
})

observeEvent(input$`sclc-submit-hm`,{
  if(input$`sclc-gs_choice-hm`=='user_gs'){
    Genes<-input$`sclc-user-hm`
  }else{
    if(input$`sclc-gslib-hm`=="Gazdar Gene Family Collection (with subgroup)"){
      Genes<-isolate(GS.libraries[[input$`sclc-gslib-hm`]][[input$`sclc-gs-hm`]][[input$`sclc-gs_subgroup-hm`]])
    }else{
      Genes<-isolate(GS.libraries[[input$`sclc-gslib-hm`]][[input$`sclc-gs-hm`]])
    }
    if(input$`sclc-species-hm`=='mouse'){
      G.ref<-generate.ref(Genes)
      Genes<-with(G.ref,mouse[match(Genes,human)]);Genes<-Genes[!is.na(Genes)]
    }
  }
  if(input$`sclc-species-hm`=='mouse' & input$`sclc-user-human_gene`) Genes<-unique(hm.mmu2hsa()[match(Genes,human)][['gene']])
  genes<-intersect(colnames(hm.dat()),Genes)
  req(length(genes)>1,cancelOutput = T)
  req(length(input$`sclc-hm_annot`)>0,cancelOutput = T)

  # order=NULL,TF.mat=NULL,score=NULL,
  if('NE'%in%input$`sclc-hm_annot`){
    hm_score<-hm.NE()
  }else{hm_score<-NULL}
  if('TF'%in%input$`sclc-hm_annot`){
    TF.mat<-hm.dat()[,TF.pick[[input$`sclc-species-hm`]]]
  }else{TF.mat<-NULL}
  if(input$`sclc-hm_order`=='data'){
    hm_order<-NULL
  }else(hm_order<-input$`sclc-hm_order`)
  hm<-isolate(generate.hm(data=hm.dat()[,genes],scale.expr = input$`sclc-hm-scaling`=='yes',#NE.list[[input$`sclc-ds`]][genes,],
                          score=hm_score,TF.mat = TF.mat,order=hm_order,study=input$`sclc-ds-hm`))
  output$`sclc-hm`<-renderPlot({req(length(genes)>1,cancelOutput = T);hm})

})
# }