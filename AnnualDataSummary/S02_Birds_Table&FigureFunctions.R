#This document creates the tables and figures used in annual data summaries for integrated upland using the data from the SCPN_UplandEvent database in SQL Server. 


#-----------------------------------------------------------------------
#VCP Count Table 
  
#get the data for this ecosite and eventyear
vcpdetectiontable <- function(){

  tempvcpclean <- tempvcp %>%
    filter(!(Minute %in% c("X"))) %>% #remove the observation made between sites 
    filter(!is.na(ScientificName)) #remove all missing a scientific name (unknown, unknown hawk, unknown sparrow, etc)
  
  
tempvcpdetect <- tempvcpclean %>%
  select(CommonName, ScientificName, Number_Observed) %>% #selecte needed columns
  group_by(CommonName, ScientificName) %>% #group by each bird type
  summarize(TotalDetections = sum(Number_Observed, na.rm=T)) %>% #calculate total observed for each group
  ungroup() %>%
#  mutate(TotalBirds = sum(TotalDetections, na.rm=T)) %>% #add total or all birds observed to each row
  mutate(ProportionOfDetections = round((TotalDetections/sum(TotalDetections, na.rm=T))*100,2)) %>%
  arrange(desc(TotalDetections))

    filepath <<- paste0(outputpath,"OutputTable/VCP/Total&ProportionDetections_", tempreport$Park, "_", tempreport$Habitat, ".csv")
    write.csv(tempvcpdetect, filepath, row.names=F)   

     datatable(tempvcpdetect, options = list(pageLength = 25), rownames=FALSE)  
          
}


#-----------------------------------------------------------------------
#VCP Mean & Plot Frequency

vcpmeanandfreqtable <- function(){
 
  tempvcpclean <- tempvcp %>%
    filter(!(Minute %in% c("X"))) %>% #remove the observation made between sites 
    filter(!is.na(ScientificName)) #remove all missing a scientific name (unknown, unknown hawk, unknown sparrow, etc)
  
#calculate mean
  if(tempreport$Clustered){ #for habitats with clusters - this section has been an issue on how exactly to collapse.
    #Technique 1:
      tempvcpmeansd <- tempvcpclean %>%
        group_by(Cluster, PointID, ScientificName, CommonName) %>% #for each point
        summarize(Number_Observed_VCP = sum(Number_Observed)) %>% #calculate number observed at each sampling period (summing all counts across minutes)
        ungroup() %>%
        group_by(Cluster, ScientificName, CommonName) %>% #group by cluster
        summarize(MeanCluster = mean(Number_Observed_VCP)) %>% #get the mean number of each species observed by cluster
        ungroup() %>%
        group_by(ScientificName, CommonName) %>% #group by species
        summarize(Mean = round(mean(MeanCluster),2), SD=round(sd(MeanCluster),2)) %>%#calculate the mean number observed for each species
        ungroup() 
  
    #Technique 2: needs fixed if used 
    #  tempplotmeansd <- temp %>%
    #    group_by(Cluster, PointID, SpCode, ScientificName, CommonName, Sampling_Date) %>% #for each vcp count (2 or 3 per location
    #    summarize(Number_Observed_VCP = sum(Number_Observed)) %>% #calculate number observed at each sampling period (summing all counts across minutes)
    #    ungroup() %>%
    #    group_by(Cluster, SpCode, ScientificName, CommonName) %>% #group by cluster
    #    summarize(MeanCluster = mean(Number_Observed_VCP)) %>% #get the mean number of each species observed by cluster
    #    ungroup() %>%
    #    group_by(SpCode, ScientificName, CommonName) %>% #group by species
    #    summarize(Mean = round(mean(MeanCluster),2), SD=round(sd(MeanCluster),2)) %>%#calculate the mean number observed for each species
    #  ungroup()   
  }

  if(!tempreport$Clustered){ #for habitats without clusters
    tempvcpmeansd <- tempvcpclean %>%
      group_by(PointID, ScientificName, CommonName, Sampling_Date) %>%
      summarize(Number_Observed_VCP = sum(Number_Observed)) %>% #calculate number observed at each sampling period (summing all counts across minutes)
      ungroup() %>%
      group_by(ScientificName, CommonName) %>% #group by species
      summarize(Mean = round(mean(Number_Observed_VCP),2), SD=round(sd(Number_Observed_VCP),2)) %>%#calculate the mean number observed for each species
      ungroup() 
  }
   
#calculate plot frequency
tempvcpplotfreq <- tempvcpclean %>%
  group_by(PointID, ScientificName, CommonName) %>% #for each point
  summarize(Number_Observed = sum(Number_Observed)) %>% #get the number of each species observed at each sampling (since there can be multiple counts of the same type of bird at different distances)
  ungroup() %>%
  group_by(ScientificName, CommonName) %>% 
  summarize(PlotFrequency = round(((sum(Number_Observed != 0)/length(Number_Observed))*100), 2)) %>% #calculate plot frequency
  ungroup() 

tempvcpclustfreq <- tempvcpclean %>%
  group_by(Cluster, ScientificName, CommonName) %>% #for each point
  summarize(Number_Observed = sum(Number_Observed)) %>% #get the number of each species observed at each sampling (since there can be multiple counts of the same type of bird at different distances)
  ungroup() %>%
  group_by(ScientificName, CommonName) %>% 
  summarize(ClusterFrequency = round(((sum(Number_Observed != 0)/length(Number_Observed))*100), 2)) %>% #calculate cluster frequency
  ungroup() 

tempvcpmeanandfreq <- tempvcpmeansd %>%
  left_join(tempvcpplotfreq) %>%
  left_join(tempvcpclustfreq) %>%
  arrange(desc(Mean)) %>%
  as.data.frame()

    filepath <<- paste0(outputpath,"OutputTable/VCP/Mean&Frequency_", tempreport$Park, "_", tempreport$Habitat, ".csv")
    write.csv(tempvcpmeanandfreq, filepath, row.names=F)   

     datatable(tempvcpmeanandfreq, options = list(pageLength = 25), rownames=FALSE)  
          
}



#-----------------------------------------------------------------------
#Functional Group Plot
functionalgroupplot <- function() {

#create a grouping variable
  grouper <- data.frame(Category = c("Tree seedlings", "Total shrub and herbaceous cover (no trees)", "Perennial grasses, graminoids", "Annual grasses", "Forbs", "Shrubs, dwarf shrubs and woody vines", "Cacti, succulents", "Standing dead herbaceous", "Woody standing dead"), FunctionalGroupLabel = factor(c("Seedling", "Total Herbaceous","Perennial Grass","Annual Grass","Forb","Shrub","Succulent","Standing Dead Herbaceous","Standing Dead Woody"),levels = c("Seedling", "Total Herbaceous","Perennial Grass","Annual Grass","Forb","Shrub","Succulent","Standing Dead Herbaceous","Standing Dead Woody"), ordered=T), Group=factor(c("Seedlings","Total\nLive", "Live","Live", "Live", "Live", "Live", "Dead", "Dead"), levels = c("Seedlings","Total\nLive","Live","Dead"), ordered=T), stringsAsFactors = F)

 tempfunctional <- tempfunctional %>%
   full_join(grouper)
  
source("birdsplottheme.txt")
  
  functionalgroup <-   ggplot(tempfunctional) +
      geom_boxplot(aes(x=FunctionalGroupLabel, ymin = y00, lower = y25, middle = y50, upper = y75, ymax = y100, fill=FunctionalGroupLabel),  varwidth=TRUE, stat="identity") +
#      geom_point(aes(x=FunctionalGroupLabel, y=MeanPlotCoverMidpoint), size=rel(4), col="red") + #add the mean
      theme_leeplot +
      theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
      labs(x="Functional Group", y="Mean Percent Cover") +
      scale_fill_manual(values = c("blue","darkolivegreen4","darkolivegreen2","darkolivegreen2","darkolivegreen2","darkolivegreen2","darkolivegreen2", "gray50","gray50")) +
#      geom_vline(xintercept = c(1.5,6.5), linetype = "dotted", color="darkgray") +
      facet_grid(.~Group, space="free_x", scale="free_x") +
      ylim(0,100)
 
#create the filepath
filepath <<- paste0(outputpath,"OutputFigure/FunctionalGroup/FunctionalGroup_", tempreport$Unit_Code, "_",  tempreport$Habitat, "_", tempreport$EventYear, ".png")
#save the plot
ggsave(filepath)

functionalgroup     
}
