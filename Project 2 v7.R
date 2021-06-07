library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
df1=read.table(file='sample_orig_2020.txt',sep='|',header=FALSE,quote="",nrows=60000,stringsAsFactors = FALSE)
df2= df1 %>% select (1,2,3,5,7,8,9,10,11,12,18,19,20,21,22)
colnames(df2)=c("FICO","first_pmt_dt","first_time_home","MSA","num_units","occup_stat","OCLTV","DTI","OUPB","OLTV","prop_type","ZIP","loan_id","purpose","term")
?read_xlsx
df_z1=read_xlsx(path='ZIP_CBSA_032021.xlsx',col_names=TRUE,n_max=50000)
df_z2=read_xlsx(path='ZIP_CBSA_DIV_032021.xlsx',col_names=TRUE,n_max=10000)
df_z3=read_xls(path='CBSA_list_2020.xls',col_names=TRUE,n_max=10000)
dfz1= df_z1 %>% select('ZIP','CBSA','USPS_ZIP_PREF_CITY','USPS_ZIP_PREF_STATE')
dfz2= df_z2 %>% select('ZIP','CBSA_DIV','USPS_ZIP_PREF_CITY','USPS_ZIP_PREF_STATE')
dfz3 = df_z3 %>% select(1:7)
dfz4=inner_join(dfz1,dfz2,by='ZIP')
dfz5= dfz3 %>% filter (!is.na(dfz3$`CSA Code`))
dfz6= dfz1 %>% mutate(paste(substr(ZIP,1,3), '00',sep=''))
colnames(dfz6)[5] = 'ZIP_3'
df3= df2 %>% mutate(ifelse (nchar(df2$ZIP)<5, ifelse(nchar(df2$ZIP)<4,paste('00',df2$ZIP,sep=''),
                                                     paste('0',df2$ZIP,sep='')), df2$ZIP))
colnames(df3)[16]='ZIP_C'
dfz7=inner_join(dfz6,dfz5,by=c('CBSA' = 'CBSA Code'))
dfz8=distinct(dfz7,dfz7$ZIP_3,dfz7$`CSA Code`, dfz7$`CSA Title`)
colnames(dfz8)= c("ZIP_3","CSA Code", "CSA Title")
dfz9=as.data.frame(table(dfz8[,1]))
df4= df3 %>% mutate(as.character(MSA))
colnames(df4)[17] ="MSA_C"
df5=left_join(df4,dfz3,by = c("MSA_C" = "CBSA Code"))
df6=distinct(df5)
df7=left_join(df6,dfz3,by = c("MSA_C" = "Metropolitan Division Code"), na_matches="never")
df8=distinct(df7)
df9=left_join(df8,dfz8,by = c("ZIP_C" = "ZIP_3"))
df10=df9 %>% mutate (ifelse (!is.na(df9$`CSA Code.x`),df9$`CSA Code.x`,df9$`CSA Code.y`))
colnames(df10)[32]='CSA Code tmp'
df11=df10 %>% mutate (ifelse (!is.na(df10$`CSA Code tmp`),df10$`CSA Code tmp`, ifelse(!is.na(df10$`CSA Code`),df10$`CSA Code`,df10$`CSA Code tmp` )))
colnames(df11)[33] = "CSA_Code_Final"
df12=distinct(df11,df11$loan_id,.keep_all=TRUE)

df13=df9 %>% mutate (ifelse (!is.na(df9$`CSA Title.x`),df9$`CSA Title.x`,df9$`CSA Title.y`))
colnames(df13)[32] = 'CSA Title tmp'
df14=df13 %>% mutate (ifelse (!is.na(df13$`CSA Title tmp`),df13$`CSA Title tmp`, ifelse(!is.na(df13$`CSA Title`),df13$`CSA Title`,df13$`CSA Title tmp` )))
colnames(df14)[33] = "CSA_Title_Final"
df15=distinct(df14,df14$loan_id,.keep_all=TRUE)
df16=df15 %>% group_by(CSA_Title_Final) %>% summarise(ave_FICO=round(mean(FICO)),ave_OLTV=round(mean(OLTV)), ave_DTI = round(mean(DTI)),pct_ft_home=round(sum(first_time_home =='Y')/sum(first_time_home =='Y' | first_time_home =='N')*100,1),pct_sec_home=round(sum(occup_stat == 'S' & purpose == 'P')/sum(purpose == 'P')*100,1),pct_inv=round(sum(occup_stat == 'I' & purpose == 'P')/sum(purpose == 'P')*100,1), pct_15_and_lower=round(sum(term <= 180)/n()*100,1),pct_refi=round(sum(purpose == 'C' | purpose == 'N' | purpose == 'R'  )/sum(purpose == 'C' | purpose == 'N' | purpose == 'R' | purpose == 'P' )*100,1),pct_co_refi=round(sum(purpose == 'C')/sum(purpose == 'C' | purpose == 'N' | purpose == 'R' )*100,1),pct_FICO_lt_680=round(sum(FICO <=680)/sum(FICO != 9999 )*100,1),pct_LTV_gt_95_pur=round(sum(OLTV >= 95 & purpose == 'P')/sum(OLTV != 999 & purpose == 'P')*100,1), num_pur = sum(purpose == 'P'),count = n())
df17=df15 %>% summarise(ave_FICO=round(mean(FICO)),ave_OLTV=round(mean(OLTV)), ave_DTI = round(mean(DTI)),pct_ft_home=round(sum(first_time_home =='Y')/sum(first_time_home =='Y' | first_time_home =='N')*100,1),pct_sec_home=round(sum(occup_stat == 'S' & purpose == 'P')/sum(purpose == 'P')*100,1),pct_inv=round(sum(occup_stat == 'I' & purpose == 'P')/sum(purpose == 'P')*100,1), pct_15_and_lower=round(sum(term <= 180)/n()*100,1),pct_refi=round(sum(purpose == 'C' | purpose == 'N' | purpose == 'R'  )/sum(purpose == 'C' | purpose == 'N' | purpose == 'R' | purpose == 'P' )*100,1),pct_co_refi=round(sum(purpose == 'C')/sum(purpose == 'C' | purpose == 'N' | purpose == 'R' )*100,1),pct_FICO_lt_680=round(sum(FICO <=680)/sum(FICO != 9999 )*100,1),pct_LTV_gt_95_pur=round(sum(OLTV >= 95 & purpose == 'P')/sum(OLTV != 999 & purpose == 'P')*100,1), num_pur = sum(purpose == 'P'),count = n())
df18 = df16 %>% arrange(desc(count))
df19 = df18[!is.na(df18$CSA_Title_Final),][1:20,]
CSA_vec = c("LAX","NYC","WAS","SFO","CHI","BOS","PHX","DEN","DAL","SEA","ATL","SLC","PHI","DET","POR","MIN","HOU","MIA","SAC","ORL")
df20=data.frame(cbind(CSA_vec,df19))
colnames(df20)[1]="CSA"

tmp0_attr=c("FICO <= 680","LTV > 95","DTI >= 50","FIRST TIME HOME","SECOND HOME","INVESTOR HOME","CASH OUT REFI")
tmp1_attr = c("pct_FICO_lt_680","pct_LTV_gt_95_pur","pct_DTI_gt_50","pct_ft_home","pct_sec_home","pct_inv","pct_co_refi")
tmp2_attr= c("Percentage of FICO scores <= 680 across top 20 metros","Percentage of LTV > 95 across top 20 metros","Percentage of DTIs >= 50% across top 20 metros","First time home buyer percentage across top 20 metros","Second home buyer percentage across top 20 metros","Investor home buyer percentage across top 20 metros","Cash out refinance percentage across top 20 metros")
tmp3_attr = c("Percentage (%)","Percentage (%) of all home purchases","Percentage (%)","Percentage (%)","Percentage of all home purchases (%)","Percentage of all home purchases (%)","Percentage of all refinances (%)")
tmp_attr=data.frame(cbind(tmp0_attr,tmp1_attr,tmp2_attr,tmp3_attr))
####################################################
# Graphs
####################################################
print(transpose((tmp34)))

g = ggplot(data=tmp29[tmp29$FICO != 9999,],aes(y=FICO))
g + geom_boxplot(width=0.2) + coord_cartesian(xlim = c(-1, 1)) + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5),axis.ticks.x = element_blank(),axis.text.x = element_blank())  + labs(title= "FICO distribution",y="FICO ")


g = ggplot(data=tmp29[tmp29$OLTV != 999 & tmp29$purpose == 'P',],aes(y=OLTV))
g + geom_boxplot(width=0.2,color="darkblue") + coord_cartesian(xlim = c(-1, 1)) + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5),axis.ticks.x = element_blank(),axis.text.x = element_blank())  + labs(title= "LTV distribution of purchase mortgages",y="LTV ")

g = ggplot(data=tmp29[tmp29$DTI != 999,],aes(y=DTI))
g + geom_boxplot(width=0.2,color="darkblue") + coord_cartesian(xlim = c(-1, 1)) + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5),axis.ticks.x = element_blank(),axis.text.x = element_blank())  + labs(title= "DTI distribution",y="DTI ")



g = ggplot(data=tmp38, aes(x=CSA,y=pct_FICO_lt_680,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Percentage of FICO scores <= 680 across top 20 metros",x="Top 20 Metros",y="Percentage (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_LTV_gt_95_pur,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Percentage of LTV >=95 across top 20 metros",x="Top 20 Metros",y="Percentage (%) of all home purchases") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_DTI_gt_50,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Percentage of DTIs >= 50% across top 20 metros",x="Top 20 Metros",y="Percentage (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_ft_home,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "First time home buyer percentage across top 20 metros",x="Top 20 Metros",y="Percentage (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_sec_home,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Second home buyer percentage across top 20 metros",x="Top 20 Metros",y="Percentage of all home purchases (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_inv,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Investor home buyer percentage across top 20 metros",x="Top 20 Metros",y="Percentage of all home purchases (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))

g = ggplot(data=tmp38, aes(x=CSA,y=pct_co_refi,fill=CSA))
g+geom_col(color='darkblue') + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))  + labs(title= "Cash out refinance percentage across top 20 metros",x="Top 20 Metros",y="Percentage of all refinances (%)") + scale_fill_manual(values = c("DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue","DarkGreen","Yellow","Orange", "Blue"))


input_attr = "LTV > 95"

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


###############################################################################
