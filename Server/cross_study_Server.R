output$`sclc-overall_stat`<-NULL
output$`sclc-tree-stat` <- renderTree({cor.options})
stat.tree.node<-reactive({
  tree.selected<-intersect(names(unlist(get_selected(input$`sclc-tree-stat`, format = "slices"))),names(cor.options.vec))
  tree.df<-data.table(group=sapply(tree.selected, function(x) unlist(strsplit(x,split = '.',fixed = T))[1]),study.name=sapply(tree.selected, function(x) unlist(strsplit(x,split = '.',fixed = T))[2]))
  tree.df[,study:=cor.options.vec[tree.selected]][,species:=ifelse(grepl('human',study.name)|grepl('human',group),'human','mouse')]
})

observeEvent(input$`sclc-stat-choice`,{
  if(input$`sclc-stat-choice`=='full'){
    shinyjs::hide('sclc-gs_subgroup-stat')
    shinyjs::hide('sclc-gslib-stat')
    shinyjs::hide('sclc-gs-stat')
  }else{
    shinyjs::show('sclc-gslib-stat')
    shinyjs::show('sclc-gs-stat')
    updateSelectInput(session,inputId = "sclc-gs-stat",choices = names(GS.libraries[[input$`sclc-gslib-stat`]]))
    if(input$`sclc-gslib-stat`=="Gazdar Gene Family Collection (with subgroup)"){
      shinyjs::show('sclc-gs_subgroup-stat')
      updateSelectInput(session,inputId = 'sclc-gs_subgroup-stat',choices = names(GS.libraries[["Gazdar Gene Family Collection (with subgroup)"]][[input$`sclc-gs-stat`]]))
    }else{
      shinyjs::hide('sclc-gs_subgroup-stat')
    }
  }
})
observeEvent(input$`sclc-gslib-stat`,{
  shinyjs::hide('sclc-gs_subgroup-stat')
  updateSelectInput(session,inputId = "sclc-gs-stat",choices = names(GS.libraries[[input$`sclc-gslib-stat`]]))
  if(input$`sclc-gslib-stat`=="Gazdar Gene Family Collection (with subgroup)"){
    shinyjs::show('sclc-gs_subgroup-stat')
  }
})
observeEvent(input$`sclc-gs-stat`,{
  if(input$`sclc-gslib-stat`=="Gazdar Gene Family Collection (with subgroup)"){
    updateSelectInput(session,inputId = 'sclc-gs_subgroup-stat',choices = names(GS.libraries[["Gazdar Gene Family Collection (with subgroup)"]][[input$`sclc-gs-stat`]]))
  }
})
Genes<-reactive({
  if(input$`sclc-gslib-stat`=="Gazdar Gene Family Collection (with subgroup)"){
    GS.libraries[[input$`sclc-gslib-stat`]][[input$`sclc-gs-stat`]][[input$`sclc-gs_subgroup-stat`]]
  }else{
    GS.libraries[[input$`sclc-gslib-stat`]][[input$`sclc-gs-stat`]]
  }
})
tree.table<-reactive({
  if(!is.null(stat.tree.node())){
    stat.list<-lapply(1:nrow(stat.tree.node()),function(i){
      stat.df<-beautify.dt(as.data.table(readRDS(paste0('data/',ifelse(grepl('Meta',stat.tree.node()$group[i]),'meta-','r-'),stat.tree.node()$study[i],'.rds'))))
      colnames(stat.df)[1]<-stat.tree.node()$species[i]
      if(any(colnames(stat.df)=='r')) colnames(stat.df)[-1]<-paste0(stat.tree.node()$study[i],'_',colnames(stat.df)[-1])
      stat.df
    })
    m.df<-merge(ref.cbn,stat.list[[1]][,c(1:2,(if(grepl('Meta',stat.tree.node()$group[1])|stat.tree.node()$study[1]=='H69') 3)),with=F],by=stat.tree.node()$species[1],all=T)
    if(nrow(stat.tree.node())>1){ for(i in 2:nrow(stat.tree.node())){
      m.df<-merge(m.df,stat.list[[i]][,c(1:2,(if(grepl('Meta',stat.tree.node()$group[i])|stat.tree.node()$study[i]=='H69') 3)),with=F],by=stat.tree.node()$species[i],all=T)
    }}
    setDT(m.df)
    gene.desc<-hsa.entrez[match(m.df$human,Symbol)][['description']]
    gene.desc[is.na(gene.desc)]<-mmu.entrez[match(m.df$mouse[is.na(gene.desc)],Symbol)][['description']]
    m.df<-data.table(m.df[,c('human','mouse'),with=F],description=gene.desc,m.df[,-(1:2),with=F])
    m.df<-m.df[order(apply(as.matrix((m.df[,grep("_r$",colnames(m.df)),with=F])),1,function(x) sum(x,na.rm = T)))]
    m.df
  }
})

sketch.frame<-reactive({
  tree.freq<-stat.tree.node()[,.N,by=group]
  header.1<-as.character(c((if(any(grepl('Meta',stat.tree.node()$group))) paste0("th(colspan=",2*tree.freq[grep('Meta',group)][['N']],",rowspan=1,'Meta-analysis')")),
                           (if(any(!grepl('Meta',stat.tree.node()$group))) sapply(tree.freq[grep('Meta',group,invert = T)]$group,function(g) paste0("th(colspan=",tree.freq[group==g][['N']]+ifelse('H69'%in%stat.tree.node()[group==g][['study']],1,0),",rowspan=1,'",g,"')")))))
  header.2<-as.character(c((if(any(grepl('Meta',stat.tree.node()$group))) sapply(stat.tree.node()[grep('Meta',group)]$study.name,function(g) paste0("th(colspan=2,rowspan=1,'",g,"')"))),
                           (if(any(!grepl('Meta',stat.tree.node()$group))) sapply(stat.tree.node()[grep('Meta',group,invert = T)]$study,function(g) paste0("th(colspan=",ifelse(g=='H69',2,1),",rowspan=1,'",g,"')")))))
  header.3<-as.character(c((if(any(grepl('Meta',stat.tree.node()$group))) sapply(stat.tree.node()[grep('Meta',group)]$study.name,function(g) "th(colspan=1,rowspan=1,'r'),th(colspan=1,rowspan=1,'pv')")),
                           (if(any(!grepl('Meta',stat.tree.node()$group))) sapply(stat.tree.node()[grep('Meta',group,invert = T)]$study,function(g) ifelse(g=='H69',"th(colspan=1,rowspan=1,'NE - non-NE'),th(colspan=1,rowspan=1,'H69 - H69AD')","th(colspan=1,rowspan=1,'r')")))))
  
  htmltools::withTags(table(
    class = 'display',
    eval(parse(text=paste(c("thead(tr(th(colspan=3,rowspan = 2, 'Gene'),",
                            paste(header.1,collapse = ","),"),tr(",paste(header.2,collapse = ","),
                            "),tr(th(colspan=1,rowspan = 1, 'human'),th(colspan=1,rowspan = 1, 'mouse'),th(colspan=1,rowspan = 1, 'description'),",paste(header.3,collapse = ","),
                            "))"),collapse = "")))
  ))
})
dt.summary<-reactive({
  dt.summary<-tree.table()
  if(input$`sclc-stat-choice`!='full') dt.summary<-dt.summary[human%in%Genes()]
  dt.summary<-datatable(dt.summary, rownames = F,container = sketch.frame(),options = list(pageLength = 10,headerCallback = JS("function( thead, data, start, end, display ) {$(thead).closest('thead').find('th').css('border', '1px solid #e7e1ef');}")))
  # ,autoWidth = T,columnDefs = list(list(width = '150px', targets = c(1))),scrollX=T
  for(col.r in grep("_r$",colnames(tree.table()),value=T)){
    dt.summary<-dt.summary%>%formatStyle(col.r, col.r,backgroundColor = styleInterval(brks, clrs))
  }
  if('H69'%in%stat.tree.node()$study) dt.summary<-dt.summary%>% formatStyle(c('group_diff','h69_diff'),color = styleInterval(c(-0.00001,0.00001), c(BLUE,'gray', RED)))
  dt.summary
})

observeEvent({input$`sclc-summary-submit`},{
  if(!is.null(stat.tree.node())){
    output$`sclc-overall_stat`<-renderDataTable({isolate(dt.summary())})
  }else{
    showNotification("Please select at least one summary source.")
    }
})
