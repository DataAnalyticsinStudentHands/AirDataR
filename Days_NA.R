NA_per_days=function(date,col_names_NA){
  date= as.Date(date)
  days=format(date,'%A')
  i=1
  days_plot=list()
  NAs_per_day=list()
  for(col in col_names_NA){
    days_list=days[is.na((df[,col]))]
    days_ordered=unique(days_list)
    days_plot[[i]]=days_ordered
    print(days_plot[[i]])
    NAs_per_day[[i]]=table(days[is.na((df[,col]))])
    i=i+1
  }
  
  i=1
  j=1
  datalist_day=list()
  #Initialising a list for mapping the NA count to each week in the "week_name" object
  NA_per_days_order =list()
  
  days_vector=vector()
  m=vector()
  for(i in 1:length(days_plot) ){
    days_vector=c(days_vector,days_plot[[i]])
  }
  days_vector=unique(days_vector)
  days_lev= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  days_vector=factor(days_vector, level = days_lev)
  for(j in 1:length(col_names_NA)){
    c=1
    for(k in days_vector){
      if(is.na(NAs_per_day[[j]][k])){
        m[c]=0
      }
      else{
        m[c]=NAs_per_day[[j]][[k]]
      }
      c=c+1
    }
    dframe_day=data.frame(date_day=days_vector,col_NA=m)
    colnames(dframe_day)[2]=col_names_NA[j]
    print(dframe_day)
    datalist_day[[j]]=dframe_day
  }
  
  
  datalist_day=as.data.frame(datalist_day)
  dframe_new_day=datalist_day[!duplicated(as.list(datalist_day))]
  i=1
  for (i in 2:length(dframe_new_day)){
    print(ggplot(data=dframe_new_day, aes(dframe_new_day$date_day,dframe_new_day[,i]))+
            geom_bar(stat='identity',fill='blue',color='green',width = 1)+
            theme(axis.text.x = element_text(angle = 45,hjust =1))+
            xlab('Days') + ylab(colnames(dframe_new_day)[i]))
  }
} 