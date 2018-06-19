NA_per_sites=function(site,col_names_NA){
  
  i=1
  sites_plot=list()
  NAs_per_site=list()
  for(col in col_names_NA){
    sites_list=site[is.na((df[,col]))]
    sites_ordered=unique(sites_list)
    sites_plot[[i]]=sites_ordered
    print(sites_plot[[i]])
    NAs_per_site[[i]]=table(site[is.na((df[,col]))])
    i=i+1
  }
  
  i=1
  j=1
  datalist_site=list()
  #Initialising a list for mapping the NA count to each week in the "week_name" object
  NA_per_sites_order =list()
  
  sites_vector=vector()
  m=vector()
  for(i in 1:length(sites_plot) ){
    sites_vector=c(sites_vector,sites_plot[[i]])
  }
  sites_vector=unique(sites_vector)
  for(j in 1:length(col_names_NA)){
    c=1
    for(k in sites_vector){
      if(is.na(NAs_per_site[[j]][k])){
        m[c]=0
      }
      else{
        m[c]=NAs_per_site[[j]][[k]]
      }
      c=c+1
    }
    dframe_site=data.frame(site_id=sites_vector,col_NA=m)
    colnames(dframe_site)[2]=col_names_NA[j]
    print(dframe_site)
    datalist_site[[j]]=dframe_site
  }
  
  
  datalist_site=as.data.frame(datalist_site)
  dframe_new_site=datalist_site[!duplicated(as.list(datalist_site))] 
  i=1
  for (i in 2:length(dframe_new_site)){
    print(ggplot(data=dframe_new_site, aes(dframe_new_site$site_id,dframe_new_site[,i]))+
            geom_bar(stat='identity',fill='black',color='blue',width = 1)+
            theme(axis.text.x = element_text(angle = 45,hjust =1))+
            xlab('Sites') + ylab(colnames(dframe_new_site)[i]))
  }
}