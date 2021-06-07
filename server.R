#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$AttPlot <- renderPlot({

       
        input_attr = input$attr
        
        if(input_attr == "FICO <= 680"){
            ybar= tmp38$pct_FICO_lt_680
            yintbar= tmp34$pct_FICO_lt_680
        } else if (input_attr == "LTV > 95") {
            ybar= tmp38$pct_LTV_gt_95_pur
            yintbar= tmp34$pct_LTV_gt_95_pur
        } else if (input_attr == "DTI >= 50") {
            ybar= tmp38$pct_DTI_gt_50
            yintbar= tmp34$pct_DTI_gt_50
        } else if (input_attr == "FIRST TIME HOME") {
            ybar= tmp38$pct_ft_home
            yintbar= tmp34$pct_ft_home
        } else if (input_attr == "SECOND HOME") {
            ybar= tmp38$pct_sec_home
            yintbar= tmp34$pct_sec_home
        } else if (input_attr == "INVESTOR HOME") {
            ybar= tmp38$pct_inv
            yintbar= tmp34$pct_inv
        } else if (input_attr == "CASH OUT REFI") {
            ybar= tmp38$pct_co_refi
            yintbar= tmp34$pct_co_refi
        }
        
        
        
        
        
        g = ggplot(data=tmp38, aes(x=CSA,y=ybar,fill=CSA))
        g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= tmp_attr[tmp_attr[,1]==input_attr,3],x="Top 20 Metros",y=tmp_attr[tmp_attr[,1]==input_attr,4]) + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue")) +geom_hline(yintercept = yintbar,linetype = 'dotted', col = 'red') + geom_text(aes(0,yintbar, label = "National  level",vjust=-1,hjust=-0.2),size=3)
        
    })

})
