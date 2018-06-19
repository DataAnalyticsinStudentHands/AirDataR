NA_per_months=function(date,col_names_NA){
  months= as.Date(date)
  months=format(months, "%B %Y")
  i=1
  months_name=list()
  NAs_per_month=list()
  months_plot=list()
  for(col in col_names_NA){
    months_list=months[is.na((df[,col]))]
    months_ordered=unique(months_list)
    months_plot[[i]]=months_ordered
    print(months_plot[[i]])
    NAs_per_month[[i]]=table(months[is.na((df[,col]))])
    i=i+1
  }
  i=1
  j=1
  datalist_month=list()
  #Initialising a list for mapping the NA count to each week in the "week_name" object
  NA_per_months_order =list()
  
  months_vector=vector()
  m=vector()
  for(i in 1:length(months_plot) ){
    months_vector=c(months_vector,months_plot[[i]])
  }
  months_vector=unique(months_vector)
  month_lev= months_vector
  months_vector=factor(month_lev, level = month_lev)
  
  for(j in 1:length(col_names_NA)){
    c=1
    for(k in months_vector){
      if(is.na(NAs_per_month[[j]][k])){
        m[c]=0
      }
      else{
        m[c]=NAs_per_month[[j]][[k]]
      }
      c=c+1
    }
    dframe_month=data.frame(date_month=months_vector,col_NA=m)
    colnames(dframe_month)[2]=col_names_NA[j]
    print(dframe_month)
    datalist_month[[j]]=dframe_month
  }
  
  
  datalist_month=as.data.frame(datalist_month)
  dframe_new_month=datalist_month[!duplicated(as.list(datalist_month))] 
  i=1
  for (i in 2:length(dframe_new_month)){
    print(ggplot(data=dframe_new_month, aes(dframe_new_month$date_month,dframe_new_month[,i]))+
            geom_bar(stat='identity',fill='red',color='black',width = 1)+
            theme(axis.text.x = element_text(angle = 45,hjust =1))+
            xlab('Months') + ylab(colnames(dframe_new_month)[i]))
  }  
} 