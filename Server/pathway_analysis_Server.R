output$`sclc-tree-pa` <- renderTree({cor.options})
output$`sclc-results-pa`<-NULL
pa.tree.node<-reactive({
  tree.selected<-intersect(names(unlist(get_selected(input$`sclc-tree-pa`, format = "slices"))),names(cor.options.vec))
  tree.df<-data.table(group=sapply(tree.selected, function(x) unlist(strsplit(x,split = '.',fixed = T))[1]),study.name=sapply(tree.selected, function(x) unlist(strsplit(x,split = '.',fixed = T))[2]))
  tree.df[,study:=cor.options.vec[tree.selected]][,species:=ifelse(grepl('human',study.name)|grepl('human',group),'human','mouse')]
})
pa.result<-reactive({
  if(!is.null(pa.tree.node())){
    stat.df<-as.data.table(readRDS(paste0('data/',ifelse(grepl('Meta',pa.tree.node()$group),'meta-','r-'),pa.tree.node()$study,'.rds')))
    colnames(stat.df)[1]<-pa.tree.node()$species
    if(pa.tree.node()$study=='H69') colnames(stat.df)[3]<-'r'
    m.df<-merge(ref.cbn,stat.df[,1:3,with=F],by=pa.tree.node()$species,all.y=T)
    setDT(m.df)
    m.df<-m.df[!is.na(r)][order(r,decreasing = input$`sclc-direction-pa`=='positive')]
    LC.hyper(genesets = GS.libraries[[input$`sclc-gslib-pa`]],genes = m.df$human,testset = m.df[1:input$`sclc-num-pa`][['human']])
  }
})
observeEvent(input$`sclc-submit-pa`,{
  if(nrow(pa.tree.node())!=0){
    output$`sclc-results-pa`<-DT::renderDataTable(
      isolate(datatable(pa.result(),rownames = F))
    )
  }else{showNotification("Please select a correlation result from the tree structure.")}
})