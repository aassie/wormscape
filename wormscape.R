library(tidyverse)
library(vegan)
library(progress)

##################################
###  Things to set up manually:###
##################################

# Your working directory
WDpath="./20231017_075835_446_output/"

# The name that has the WormScape_output files
## If you have multiple plate, for your convenience you can move all the
## WormScape outputs folder in the same location and rename them accordingly
PlateName="PixelProcessorOutput"

# The name of your metadata file
## Use the template file for simplicity
MetaPath="../gst_4_reporter_10_17_23.metadata.csv"

# Set channel colors:
ch2Col="Green"
ch3Col="Red"

######################################################
### Run the thing below without changing anything ####
###    (Except if you know what you are doing)    ####
######################################################

# Loading data
setwd(WDpath)
ch2files=list.files(path=PlateName, full.names=TRUE, pattern = ".*Ch2.*.txt")
ch3files=list.files(path=PlateName, full.names=TRUE, pattern = ".*Ch3.*.txt")
meta<-read.csv(MetaPath)

chlist<-list(list(ch2files,ch2Col),
             list(ch3files,ch3Col))

# Functions:
channelfiles=ch2files
ChCol=ch2Col

ChProcessor<-function(channelfiles,ChCol,meta){
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
  channel.long$col<-ChCol
  
  ch.merge<-channel.long
  ch.merge<-ch.merge %>% separate(worms, into = c("Plate","well","worm"), sep="_")
  ch.merge$well<-gsub("Well","",ch.merge$well)
  ch.merge$og<-paste(ch.merge$Plate,"_","Well",ch.merge$well,"_",ch.merge$worm,sep="")
  ch.merge<-left_join(ch.merge, meta, by=c("Plate","well"))
  
  ch.Summ<-data.frame(IntMean=sapply(channel.data, sum)/lengths(channel.data)) %>%
    rownames_to_column("ID") %>%
    separate(ID, into = c("Plate","well","worm"), sep="_") %>%
    mutate(well=gsub("Well","",well)) %>%
    left_join(meta, by=c("Plate","well")) %>%
    mutate(channel=ChCol)
  
  sto<-Sys.time()
  cat(paste("Summary steps took:", timeCheck(sto,sta),"\n"))
  
  cat(paste("Generating big matrix \n"))
  sta<-Sys.time()
  pb <- progress_bar$new(total = length(channel.data))
  mlist<-list()
  for(i in 1:length(channel.data)){
    pb$tick()
    tmp<-channel.data[[i]] %>%
      mutate(rper=round(100/nrow(channel.data[[i]])*row_number())) %>%
      rename_with(~gsub("V","",.x)) %>%
      pivot_longer(!rper, names_to = "cper",values_to = "Int") %>%
      type_convert(col_types = cols(rper = "d",cper="d",Int="d")) %>%
      mutate(cper=floor((100/max(cper))*cper)) %>%
      mutate(cper=ifelse(cper==100, 99,cper),cper=cper+1) %>%
      group_by(rper,cper) %>%
      summarise(Int=mean(Int), .groups = "keep") %>%
      ungroup()
    colnames(tmp)<-gsub("Int",names(channel.data)[i],colnames(tmp))
    mlist[[i]]<-tmp
  }
  ch.mat<-reduce(mlist,full_join, by = c("rper", "cper"))
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

# The code that analyzes your data:

final<-Processor(chlist)

gm<-final[[1]][[3]] %>% 
  pivot_longer(!c(rper,cper)) %>% 
  separate(name, into=c("Plate","well","Worm"),sep="_") %>% 
  mutate(well=gsub("Well","",well)) %>% 
  left_join(meta) %>% 
  #filter(Reporter %in% selection ) %>% 
  #mutate(RNAi=gsub("bfl","fl",RNAi)) %>% 
  #mutate(Microbe=gsub("bfl","fl",Microbe)) %>% 
  #mutate(Microbe=factor(Microbe, c("L4440","ahr-1","ahr-1;flp-8", "flp-8","BIGbiome", "MYb71"))) %>%
  group_by(rper,cper,Type,Microbe) %>%
  summarise(n=mean(value))


ggplot(data=gm)+
  theme_bw()+
  geom_raster(aes(x=cper,y=rper,fill=scales::rescale(n)))+
  scale_fill_viridis_c(option = "H")+
  annotation_raster(wormline, ymin = 0,ymax= 100,xmin = 0,xmax = 110)+
  facet_wrap(RNAi~Microbe,ncol=3)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill="black"))+
  coord_fixed(ratio=0.1)+
  ylab("Width (%)")+
  xlab("Length (%)")+
  ggtitle("gst-4 Reporter Fluorescence")


######################################
### Optional code for fancy plots: ###
######################################

# Ploting code for when you have one plate and 2 colors
## Profile comparison

final[[1]][[2]] %>%
  select(Per,Intensity,worm,GP.type,Bacteria) %>%
  rename(Green=Intensity,Comp=Bacteria) %>%
  left_join(final[[2]][[2]] %>%
              select(Per,Intensity,worm) %>%
              rename(Red=Intensity)) %>%
  ggplot(aes(x=as.numeric(Per),y=Green,col=GP.type))+
  geom_smooth()+
  facet_wrap(~Comp)

## Green vs Red comparison

final[[1]][[1]] %>%
  select(!channel) %>%
  rename(Gint=IntMean)  %>%
  left_join(final[[2]][[1]] %>%
              rename(Rint=IntMean) %>%
              select(!channel)) %>%
  ggplot(aes(x=Gint,y=Rint,col=GP.type))+
  geom_point()+
  facet_wrap(~Bacteria,scales = "free")

# Example to parse more than one plate plates

##  Use a template name than can be iterated through (e.g. Plate1, Plate2, etc...)

## Plate folder name:
PlaFol="Plate"
## Number of plate:
NP=4
## Channel colors
ch2Col="Green"
ch3Col="Red"

## Processing:
chlist<-list()
chp<-list()
a=0
for(pn in 1:NP){
  a=a+1 
  chp[[a]]<-list.files(path=paste0(PlaFol,pn), full.names=TRUE, pattern = ".*Ch2.*.txt")
  a=a+1
  chp[[a]]<-list.files(path=paste0(PlaFol,pn), full.names=TRUE, pattern = ".*Ch3.*.txt")
}

for(b in 1:a){
  if(b %in% seq(1,a,2)){
    chlist[[b]]<-list(chp[[b]],ch2Col)
  }else if(b %in% seq(2,a,2)){
    chlist[[b]]<-list(chp[[b]],ch3Col)
  }
}

final<-Processor(chlist)

Pall<-rbind(final[[1]][[1]],final[[3]][[1]],final[[5]][[1]],final[[7]][[1]]) %>%
  select(!channel) %>%
  rename(Gint=IntMean)  %>%
  left_join(rbind(final[[2]][[1]],final[[4]][[1]],final[[6]][[1]],final[[8]][[1]]) %>%
              rename(Rint=IntMean) %>%
              select(!channel))
ggplot(aes(x=Gint,y=Rint,col=GP.type))+
  geom_point()+
  facet_wrap(~Bacteria,scales = "free")



#worm_heatmap
wormline<-png::readPNG("../wormline.png")

gm<-final[[1]][[3]] %>% 
  pivot_longer(!c(rper,cper)) %>% 
  separate(name, into=c("Plate","well","Worm"),sep="_") %>% 
  mutate(well=gsub("Well","",well)) %>% 
  left_join(meta) %>% 
  #mutate(RNAi=gsub("bfl","fl",RNAi)) %>% 
  #mutate(Microbe=gsub("bfl","fl",Microbe)) %>% 
  #mutate(Microbe=factor(Microbe, c("L4440","ahr-1","ahr-1;flp-8", "flp-8","BIGbiome", "MYb71"))) %>%
  group_by(rper,cper,Type,Microbe) %>%
  summarise(n=mean(value))

ggplot(data=gm)+
  theme_bw()+
  geom_raster(aes(x=cper,y=rper,fill=scales::rescale(n)))+
  scale_fill_viridis_c(option = "H")+
  annotation_raster(wormline, ymin = 0,ymax= 100,xmin = 0,xmax = 110)+
  facet_wrap(Type~Microbe,ncol=6)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill="black"))+
  coord_fixed(ratio=0.1)+
  ylab("Width (%)")+
  xlab("Length (%)")+
  ggtitle("gst-4 Reporter mutants on 10.17.2023")
