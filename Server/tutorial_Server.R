output$`tutorial-slickr` <- renderSlickR({
  width  <- session$clientData$`output_tutorial-slickr_width`
  imgs <- list.files(paste0("www/images/",input$`tutorial-choose`), pattern=".jpeg", full.names = TRUE)
  slickR(imgs,height=1200, width=1600,)
})