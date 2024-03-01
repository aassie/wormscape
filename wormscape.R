########################
###  WormScape v0.8  ###
########################

# February 2024 update
# Streamlining code and making it easier to use

# Todo:
# - Make it a package 
# - Add vignettes

library(tidyverse)
library(vegan)
library(progress)

##################################
###  Things to set up manually:###
##################################

# Your working directory
WDpath="~/Box Sync/SamuelLabShared/LabMemberFiles/Adrien/02_Collaborations/Dana/Width"

# The name that has the WormScapeOutput files
## If you have multiple plates, for your convenience, you can move all the
## WormScape outputs folders in the same location and renames them accordingly
PlateName="WormScapeOutput"

# The name of your metadata file
## Use the template file for simplicity
MetaPath="metadata.txt"

# Set channel colors in the same order as the microscopy channels:
channel.colors=c("Green","Red","Magenta")

# Set pixel scale (µm):
pixelScale=1.5

######################################################
### Run the thing below without changing anything ####
###    (Except if you know what you are doing)    ####
######################################################

# Loading data
setwd(WDpath)
chlist<-list()
a=0
for (i in 2:(length(channel.colors)+1)){
  a=a+1
  chlist[[a]]<-list(list.files(path=PlateName, full.names=TRUE, pattern = paste0(".*Ch",i,".*.txt")),channel.colors[a])
}

meta<-read.csv(MetaPath)

# Functions:

wormscape<-function(channelfiles,ChCol,meta){
  channel.data<-list()
  channel.profile<-list()
  channel.bgflu<-list()
  channel.size<-list()
  
  timeCheck<-function(b,a){
    ti<-paste0(round(as.numeric(difftime(time1 = b, time2 = a, units = "secs")), 3), " seconds")
    return(ti)
  }
  
  sta<-Sys.time()
  pb <- progress_bar$new(total = length(channelfiles))
  cat(paste("Loading files \n"))
  for (i in 1:length(channelfiles)){
    pb$tick()
    name.tmp<-tibble(channelfiles[[i]]) %>%
      separate(col=`channelfiles[[i]]`, into=c("Plate","Well","other"),sep = "_", extra="merge") %>%
      #mutate(Plate=gsub("/.*$","",Plate))
      separate(Plate,into=c("Plate","other"), sep="/", extra = "drop")
    name.tmp<-paste(name.tmp$Plate,"_",name.tmp$Well,"_","Worm",i,sep="")
    channel.data[[i]]<-read.csv(channelfiles[[i]], header = F,sep = "\t")
    names(channel.data)[[i]]<-name.tmp
    channel.profile[[i]]<-tibble(chInt=colMeans(channel.data[[i]])) %>%
      mutate(vp=row_number()/nrow(.)*100)
    ch.spe=channel.profile[[i]]
    tmp<-data.frame()
    for (j in 1:100){
      if (j==1){
        ch.matrix<-as.data.frame(t(colMeans(ch.spe[ch.spe$vp<j,-2], na.rm = TRUE)))
        rownames(ch.matrix)<-1
      } else {
          tmp<-as.data.frame(t(colMeans(ch.spe[between(ch.spe$vp, j-1, j),-2], na.rm = TRUE)))
          rownames(tmp)<-j
        }
      ch.matrix<-rbind(ch.matrix, tmp)
    }
    channel.profile[[i]]=ch.matrix %>% rownames_to_column("vp")
    names(channel.profile)[[i]]<-name.tmp
    colnames(channel.profile[[i]])<-c("vp",name.tmp)
  }
  
  sto<-Sys.time()
  SmollWorm<-names(channel.data[lengths(channel.data)<200])
  GoodWorm<-names(channel.data)[!names(channel.data) %in% SmollWorm]
  
  if(length(SmollWorm)>0){
    cat(paste("Removing the",length(SmollWorm), "following small worms:\n"))
    for(i in 1:length(SmollWorm)){
      cat(paste("\t",SmollWorm[i],"\n"))
    }
  } else{
    cat(paste("All worms are at least 200 px long, good job !\n"))
  }
  
  channel.data<-channel.data[names(channel.data) %in% GoodWorm]
  channel.profile<-channel.profile[names(channel.profile) %in% GoodWorm]
  
  cat(paste("Loading step took:", timeCheck(sto,sta), "\n"))
  
  cat("Merging profiles file into table:\n")
  sta<-Sys.time()
  channel.merge <- channel.profile %>% reduce(full_join,by="vp")
  sto<-Sys.time()
  cat(paste("Profile merge step took:", timeCheck(sto,sta), "\n"))
  
  cat(paste("Generating Summary tables\n"))
  sta<-Sys.time()
  
  channel.long<- channel.merge %>%
    rename(Per=vp) %>% 
    pivot_longer(!Per, names_to="worms", values_to="Intensity")
  channel.long<-channel.long[complete.cases(channel.long),]
  channel.long$ChannelColor<-ChCol
  
  ch.merge<-channel.long %>% 
    separate(worms, into = c("Plate","well","worm"), sep="_") %>% 
    mutate(well=gsub("Well","",well),
           og=paste0(Plate,"_","Well",well,"_",worm)) %>% 
    left_join(meta, by=c("Plate","well"))
  
  ch.Summ<-tibble(ID=names(channel.data),
                  IntMean=sapply(channel.data, sum)/lengths(channel.data),
                  WormLength=lengths(channel.data)*pixelScale) %>%
    separate(ID, into = c("Plate","well","worm"), sep="_") %>%
    mutate(well=gsub("Well","",well)) %>%
    left_join(meta, by=c("Plate","well")) %>%
    mutate(ChannelColor=ChCol)
  
  sto<-Sys.time()
  cat(paste("Summary steps took:", timeCheck(sto,sta),"\n"))
  
  cat(paste("Generating big matrix \n"))
  sta<-Sys.time()
  pb <- progress_bar$new(total = length(channel.data))
  mlist<-list()
  for(i in 1:length(channel.data)){
    pb$tick()
    tmp<-channel.data[[i]] %>%
      mutate(y=round(100/nrow(channel.data[[i]])*row_number())) %>%
      rename_with(~gsub("V","",.x)) %>%
      pivot_longer(!y, names_to = "x",values_to = "Int") %>%
      type_convert(col_types = cols(y = "d",x="d",Int="d")) %>%
      mutate(x=floor((100/max(x))*x)) %>%
      mutate(x=ifelse(x==100, 99,x),x=x+1) %>%
      group_by(y,x) %>%
      summarise(Int=mean(Int), .groups = "keep") %>%
      ungroup()
    colnames(tmp)<-gsub("Int",names(channel.data)[i],colnames(tmp))
    mlist[[i]]<-tmp
  }
  ch.mat<-reduce(mlist,full_join, by = c("y", "x"))
  sto<-Sys.time()
  cat(paste("Generating Matrices steps took:", timeCheck(sto,sta),"\n"))
  
  final<-list(ch.Summ,ch.merge,ch.mat)
  return(final)
}

Metacorrection<-function(meta){
  meta<-meta %>% 
    mutate(
      well=paste(meta$SP.well,"0",meta$SP.row,sep=""),
      well=gsub("010","10",meta$well),
      well=gsub("011","11",meta$well),
      well=gsub("012","12",meta$well)) %>%
    select(!c(SP.well,SP.row))
  return(meta)
}

Processor<-function(chlist){
  if(all(c("SP.well","SP.row","Plate") %in% colnames(meta))){
    cat("")
    cat("Good - I found a row and well column")
    cat("Adjusting Metadata\n")
    meta<-Metacorrection(meta)
  } else if(all(c("well","Plate") %in% colnames(meta))){
    cat("")
    cat("Good - I found a well column - No correction needed")
  } else {
    stop("Metadata not correct")
  }
  cat("\n")
  final<-list()
  for(z in 1:length(chlist)){
    cat(paste("Processing",chlist[[z]][2],"channel \n"))
    final[[z]]<-ChProcessor(unlist(chlist[[z]][1]),unlist(chlist[[z]][2]),meta)
  }
  return(final)
}

#######################################
## The code that analyzes your data: ##
#######################################

WormscapeResults<-wormscape(chlist)

#This function creates a list of list of data frames. One per channel and per plates. 
# If you have 1 plate and 2 channels, you'll have 2 lists of 3 data frames
# If you have 1 plate and 3 channels, you'll have 3 lists of 3 data frames
# If you have 2 plates and 3 channels, you'll have 6 lists of 3 data frames

#First data frame is a tibble with the average fluorescence intensity and length per worm, has metadata
#Second data frame is a tibble with the longitudinal fluorescence profile of each worm, has metadata
#Third data frame is a tibble of a long format matrix of the average pixel value of the worm fluorescence profile along the x and y axis (normalized in percent). Doesn't have metadata
