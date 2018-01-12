
read.combined=function(filename){
  return(read.csv(filename,header=TRUE,sep='\t',row.names=1))
}

gfp=function(matrix,line){
  df=NULL
  for(i in 1:ncol(matrix)){
    temp=NULL
    temp$A=rownames(matrix);temp$B=matrix[,i];temp$C=colnames(matrix)[i];
    temp$D=line[i]
    temp=as.data.frame(temp)
    df=rbind(df,temp)
  }
  
  colnames(df)=c("Month","GFP","Cell_Type","line")
  
  df=df[(is.na(df$GFP)==FALSE),]
  
  plot=ggplot(df,aes(x=as.numeric(as.vector(Month)),y=GFP,group=Cell_Type))+
    geom_line(aes(linetype=df$line),size=2)+geom_point()+scale_y_continuous(name="GFP Percent",limits=c(0,8.25))+
    scale_x_continuous("Month")+
    scale_linetype_manual(name="Cell Type",values=line,labels=colnames(matrix))+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),
          axis.text=element_text(size=60),axis.title=element_text(size=60,face="bold"),title=element_text(size=50))
  return(plot)
}

plot.cumulative.lib=function(List,time,legend,colors){
  numb=length(List)
  plot_data=NULL
  for(i in 1:numb){
    cum=barcodecount(List[[i]],months=time,count="cumulative")
    temp2=barcodecount(List[[i]],months=time,count="unique")
    temp=data.frame(cum)
    temp$B=temp2;temp$A=rownames(temp);temp$C=legend[i];
    colnames(temp)=c("C","U","Month","Library");
    rownames(temp)=seq(1:nrow(temp))+i*100
    plot_data=rbind.data.frame(plot_data,temp,make.row.names=TRUE)
  }
  plot=ggplot(plot_data,aes(x=Month,y=C,group=Library,color=Library))+
    geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=legend)+geom_point(aes(size=2.5))+scale_y_continuous(name="Cumulative Barcode",limits=c(0,4600))+
    scale_x_discrete(limits=seq(0,31))+ theme_bw()+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),
          axis.text=element_text(size=60),axis.title=element_text(size=60,face="bold"),title=element_text(size=50))
  return(plot_data)
}

plot.unique.lib=function(List,time,legend,colors){
  numb=length(List)
  plot_data=NULL
  for(i in 1:numb){
    cum=barcodecount(List[[i]],months=time,count="cumulative")
    temp2=barcodecount(List[[i]],months=time,count="unique")
    temp=data.frame(cum)
    temp$B=temp2;temp$A=rownames(temp);temp$C=legend[i];
    colnames(temp)=c("C","U","Month","Library");
    rownames(temp)=seq(1:nrow(temp))+i*100
    temp=temp[order(rownames(temp)),]
    plot_data=rbind.data.frame(plot_data,temp,make.row.names=TRUE)
  }
  
  plot=ggplot(plot_data,aes(x=Month,y=U,group=Library,color=Library))+
    geom_line(aes(size=2))+scale_color_manual(values=colors,breaks=legend)+geom_point(aes(size=2.5))+scale_y_continuous(name="Unique Barcode",limits=c(0,4600))+
    scale_x_discrete(limits=seq(0,31))+ theme_bw()+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),
          axis.text=element_text(size=60),axis.title=element_text(size=60,face="bold"),title=element_text(size=50))
  return(plot_data)
}

SI_lib=function(list_of_your_data,library_names,list_time_points,colors){
  nlib=length(list_of_your_data);ready_data=NULL;tempc=NULL;library_names=c(library_names,"All")
  for(i in 1:(nlib)){
    temp2=NULL
    your_data=list_of_your_data[[i]]
    your_data=threshold(your_data)
    ntime=length(list_time_points[[i]])
    for(j in 1:ntime){
      time=list_time_points[[i]][j]
      temp=NULL;temp$Barcode=rownames(your_data);temp$Count=your_data[,j];
      temp$Months=time
      temp=as.data.frame(temp)
      temp2=rbind(temp2,temp)
    }
    ready_data[[i]]=temp2
  }
  plot_data=NULL
  for(i in 1:(nlib)){
    temp=ready_data[[i]][c("Months","Barcode","Count")]
    temp_diversity=diversity(temp,type="simpson")
    temp_data=NULL;temp_data$Time=as.numeric(rownames(temp_diversity));temp_data$SI=temp_diversity[,2]
    temp_data$Library=library_names[i]
    temp_data=as.data.frame(temp_data)
    plot_data=rbind(plot_data,temp_data)
  }
  plot=ggplot(plot_data, aes(x=Time, y=SI, group=Library, colour=Library))+
    geom_line(size=2)+
    scale_color_manual(values=colors)+scale_x_continuous("Month",limits=c(0,31))+
    ylim(0.65,1)+scale_y_continuous("Simpson Index",limits=c(.65,1))+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=60),
          axis.title=element_text(size=60,face="bold"),legend.position="none",title=element_text(size=70))
  
  return(plot)
}

SH_lib=function(list_of_your_data,library_names,list_time_points,colors){
  nlib=length(list_of_your_data);ready_data=NULL;tempc=NULL;library_names=c(library_names,"All")
  
  for(i in 1:(nlib)){
    temp2=NULL
    your_data=list_of_your_data[[i]]
    your_data=threshold(your_data)
    time_points=list_time_points[[i]]
    ntime=length(time_points)
    for(j in 1:ntime){
      time=time_points[j]
      temp=NULL;temp$Barcode=rownames(your_data);temp$Count=your_data[,j];
      temp$Months=time
      temp=as.data.frame(temp)
      temp2=rbind(temp2,temp)
    }
    ready_data[[i]]=temp2
  }
  plot_data=NULL
  for(i in 1:(nlib)){
    temp=ready_data[[i]][c("Months","Barcode","Count")]
    temp_diversity=diversity(temp,type="entropy")
    temp_data=NULL;temp_data$Time=as.numeric(rownames(temp_diversity));temp_data$SI=temp_diversity
    temp_data$Library=library_names[i]
    temp_data=as.data.frame(temp_data)
    colnames(temp_data)=c("Time","Shannon","Library")
    plot_data=rbind(plot_data,temp_data)
  }
  plot=ggplot(plot_data, aes(x=Time, y=Shannon, group=Library, colour=Library))+
    geom_line(size=2)+
    scale_color_manual(values=colors)+
    ylim(0,7.5)+scale_y_continuous("Shannon Index",limits=c(0,7))+scale_x_continuous("Month",limits=c(0,30))+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=60),axis.text=element_text(size=60),
          axis.title=element_text(size=60,face="bold"),legend.position="none",title=element_text(size=70))
  
  return(plot)
}

find_top_clones=function(your_data,columns,n_clones){
  #uses last time point
  your_data=your_data[,columns]
  last=ncol(your_data)
  last_time_point_data=your_data[,last]
  r=rank(-last_time_point_data)
  list_of_combinded_files=NULL
  other=vector(length=ncol(your_data));other[]=0
  for(i in 1:nrow(your_data)){
    if(r[i]<=n_clones){
      list_of_combinded_files[[r[i]+1]]=as.data.frame(your_data[i,])
    }else{
      other=other+your_data[i,]
    }
  }
  list_of_combinded_files[[1]]=as.data.frame(other)
  
  return(list_of_combinded_files)
}

make_data=function(list_of_combined_files,time_points,n_clones,addition=NULL){
  titles=vector(length=n_clones+1);titles[]=c(0,seq(1,n_clones))
  for(i in 1:n_clones){
    titles[i+1]=paste("Barcode ",toString(titles[i+1]),addition)
  }
  titles[1]=paste("Other",addition)
  
  ntime_points=length(time_points)
  Libraries=rep(titles,times=ntime_points)
  Months=as.numeric(rep(time_points,each=n_clones+1))
  Barcode_count=vector(length=length(Libraries))
  for(i in 1:(n_clones+1)){
    for(j in 1:ntime_points){
      temp=list_of_combined_files[[i]]
      Barcode_count[(j-1)*(1+n_clones)+i]=temp[,j]
    }
    your_ready_data=(data.frame(Libraries,Months,Barcode_count))}
  return(your_ready_data)
}

combined_stacked=function(list_of_your_data,columns,time_points,n_clones,plot_title=NULL,colors,n,title){
  nlib=length(list_of_your_data)
  ntime_points=length(time_points)
  combined_ready_data=NULL
  sum=vector(length=ntime_points);sum[]=0
  for(lib in 1:nlib){
    your_data=list_of_your_data[[lib]]
    list_of_combined_files=find_top_clones(your_data,columns,n_clones)
    
    combined_ready_data[[lib]]=make_data(list_of_combined_files,time_points,n_clones,toString(lib))
    
    lib_sum=apply(your_data[columns],2,sum)
    sum[]=sum+lib_sum
  }
  SUM=rep(sum,each=n_clones+1)
  n_per_lib=length(combined_ready_data[[1]])
  
  for(i in 1:nlib){
    temp=combined_ready_data[[i]]
    temp$prop=(temp$Barcode_count)/SUM
    combined_ready_data[[i]]=temp
  }
  
  full=NULL
  for(i in 1:nlib){
    temp=combined_ready_data[[4-i]]
    full=rbind(full,temp)
  }
  
  custom_palette=NULL
  for(i in 1:nlib){
    
    getPalette = colorRampPalette(RColorBrewer::brewer.pal(n[4-i],colors[4-i]))
    temp=sapply(20+5,getPalette)[1:(20+1)]
    custom_palette=c(custom_palette,sample(sapply(n_clones,getPalette)),temp[length(temp)])
  }
  Title=paste("Stacked_",title,"_all.pdf",sep="")
  
  print=ggplot(full, aes(x=full$Months, y=(full$prop)*100, group=full$Libraries)) +
    geom_area(aes(fill=full$Libraries), position = position_stack(reverse = T))+
    scale_fill_manual(values = custom_palette)+
    theme(legend.position="none")+scale_x_continuous(limits=c(0,31))+
    ggtitle(plot_title)+xlab("Time (months)")+
    geom_line(colour="black", size=.2, alpha=.4,position = position_stack(reverse = T))+
    ylab("Percent Contribution \n of Barcodes")+theme(axis.text=element_text(size=30),
                                                      axis.title=element_text(size=30,face="bold"))+
    theme(legend.text=element_text(size=15),legend.key.size = unit(1, "cm"),legend.title=element_text(size=25),axis.text=element_text(size=60),
          axis.title=element_text(size=60,face="bold"),legend.position="none",title=element_text(size=70))
  
  return(print) #to see actual numbers
  #return(plot) #makes plot
}

diversity_plot.k <- function(x, a_s = 0.001, b_s = 3000, your_title = ""){
  x <- cbind(x[,1], 1 - x[,2])
  x_model <- nls(x[,2] ~ 1/(1 + exp(-a * (x[,1] - b))), start = list(a = a_s, b = b_s))
  i <- order(x[,1])
  par(cex.main = 1.5)
  plot(x, xlab = "Cells", ylab = "Null hypothesis P value for single cell representation of barcodes",  cex.axis = 2, pch = 19)
  lines(x[,1][i], predict(x_model)[i], lwd  = 5, col = 'red')
  title(paste0('\n', your_title, '\n'))
  legend('topleft', 
         legend = substitute(y == over(1,{1+e^{-a*(x - b)}}),list(a = round(coef(x_model)[1], 3), b = round(coef(x_model)[2], 3))),
         bty = 'n', cex = 1.5)
  solution_ <- (log(1/.05 - 1)/(-coef(x_model)[1]) + coef(x_model)[2])
  legend(x = mean(c(solution_, max(x[,1]))) - 500, y = 0.6,
         legend = paste0("p < 0.05 if \n<", ceiling(solution_), " barcodes"),
         bty = 'n', cex = 1.5, text.col = 'red')
  abline(v = solution_, col = 'red', lwd = 4, lty = 2)
}

create_divplot.k=function(filename,plottitle,read.plottitle,date){
  temp = read.csv(filename,header=FALSE, sep =',')[-1,2:3]
  jpeg(file = paste(date,read.plottitle,"diversity.jpg",sep = "_"), width = 3000, height = 3000, units = "px", res = 300)
  diversity_plot.k(temp, your_title = plottitle)
  dev.off()
}
