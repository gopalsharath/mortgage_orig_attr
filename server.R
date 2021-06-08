#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a bar graph
shinyServer(function(input, output) {

    output$AttPlot <- renderPlot({
#User provides their choice of the variable to be graphed in the bar graph  
#by selecting from the drop down menu     
        input_attr = input$attr
        
        if(input_attr == "FICO <= 680"){
            ybar= df20$pct_FICO_lt_680
            yintbar= df17$pct_FICO_lt_680
        } else if (input_attr == "LTV > 95") {
            ybar= df20$pct_LTV_gt_95_pur
            yintbar= df17$pct_LTV_gt_95_pur
        } else if (input_attr == "DTI >= 50") {
            ybar= df20$pct_DTI_gt_50
            yintbar= df17$pct_DTI_gt_50
        } else if (input_attr == "FIRST TIME HOME") {
            ybar= df20$pct_ft_home
            yintbar= df17$pct_ft_home
        } else if (input_attr == "SECOND HOME") {
            ybar= df20$pct_sec_home
            yintbar= df17$pct_sec_home
        } else if (input_attr == "INVESTOR HOME") {
            ybar= df20$pct_inv
            yintbar= df17$pct_inv
        } else if (input_attr == "CASH OUT REFI") {
            ybar= df20$pct_co_refi
            yintbar= df17$pct_co_refi
        }
        
        
        
        
        #Bar graph of chosen variable across all 20 metros with national level shown as a dotted line      
        g = ggplot(data=df20, aes(x=CSA,y=ybar,fill=CSA))
        g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= tmp_attr[tmp_attr[,1]==input_attr,3],x="Top 20 Metros",y=tmp_attr[tmp_attr[,1]==input_attr,4]) + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue")) +geom_hline(yintercept = yintbar,linetype = 'dotted', col = 'red') + geom_text(aes(0,yintbar, label = "National  level",vjust=-1,hjust=-0.2),size=3)
        
    })

})
