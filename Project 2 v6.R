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
g = ggplot(data=df20, aes(x=CSA,y=pct_ft_home))
g+geom_col(color='darkred',fill='blue')
g = ggplot(data=df15[df15$FICO != 9999,],aes(y=FICO))
g + geom_boxplot()
