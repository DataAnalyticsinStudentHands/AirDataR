NA_per_weeks=function(date,col_names_NA){
  weeks=ISOweek(as.Date(date))
  i=1
  weeks_name=list()
  NAs_per_week=list()
  for(col in col_names_NA){
    weeks_plot=weeks[is.na((df[,col]))]
    weeks_name[[i]]=names(table(weeks[is.na((df[,col]))]))
    NAs_per_week[[i]]=table(weeks[is.na((df[,col]))])
    i=i+1
  }
  
  i=1
  j=1
  datalist_week=list()
  #Initialising a list for mapping the NA count to each week in the "week_name" object
  NA_per_weeks_order =list()
  
  week_vector=vector()
  m=vector()
  for(i in 1:length(NAs_per_week) ){
    week_vector=c(week_vector,names(NAs_per_week[[i]]))
  }
  week_vector=unique(week_vector)
  for(j in 1:length(col_names_NA)){
    c=1
    for(k in week_vector){
      if(is.na(NAs_per_week[[j]][k])){
        m[c]=0
      }
      else{
        m[c]=NAs_per_week[[j]][[k]]
      }
      c=c+1
    }
    dframe_week=data.frame(date_week=week_vector,col_NA=m)
    colnames(dframe_week)[2]=col_names_NA[j]
    print(dframe_week)
    datalist_week[[j]]=dframe_week
  }
  
  
  datalist_week=as.data.frame(datalist_week)
  dframe_new_week=datalist_week[!duplicated(as.list(datalist_week))]
  i=1
  for (i in 2:length(dframe_new_week)){
    print(ggplot(data=dframe_new_week, aes(dframe_new_week$date_week,dframe_new_week[,i]))+
            geom_bar(stat='identity',fill='green',color='red',width = 1)+
            theme(axis.text.x = element_text(angle = 45,hjust =1))+
            xlab('Weeks') + ylab(colnames(dframe_new_week)[i]))
  }
} 