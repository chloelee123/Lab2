library(ggplot2)
library(tidyverse)
DRG <- read.csv("D:/sas/DRG_data.csv")
drg.data <- DRG %>%
  group_by(DRG.Definition) %>% mutate(DRG =strsplit(as.character(DRG.Definition),split ='')[[1]][1]) %>%
  ungroup()
#' DRG_Code_Boxplot
#'
#' @param vary
#'
#' @return plot
#' @export
#'
#' @examples
DRG_Code_Boxplot <- function(vary){
  if(vary=='Average.Total.Payments'){
    drg.data %>% ggplot(aes(x=DRG,y=Average.Total.Payments))+
      geom_boxplot()+
      scale_y_continuous(trans='log10')+
      theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1))+
      ylab('Log Average Total Payments')+xlab('DRG Code')+
      ggtitle('Average Total Payments for Hospitals by DRG Code')
  }
  if(vary=='Average.Medicare.Payments'){
    drg.data %>% ggplot(aes(x=DRG,y=Average.Medicare.Payments))+
      geom_boxplot()+
      scale_y_continuous(trans='log10')+
      theme(axis.text.x =element_text(angle = 90,size = 6,hjust = 1))+
      ylab('Log Average Medicare Payments')+xlab('DRG Code')+
      ggtitle('Average Medicare Payments for Hospitals by DRG Code')
  }
  else{
    paste("Wrong!")
  }
}

#DRG_Code_Boxplot('Average.Total.Payments')
