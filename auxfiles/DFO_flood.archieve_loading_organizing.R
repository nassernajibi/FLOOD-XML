rm(list = ls())

# For loading and organizing the DFO flood Archive xlsx file

library(readxl)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(contactdata)
library(countrycode)
library(lubridate)

my.wd <- "D:/Data/DFO_floods_damages"
#my.wd <- getwd() #check
setwd(my.wd)


## dire to downloaded DFO flood archive Excel file
dir.to.flood.file <- './Data/FloodArchive_1985_2021.xlsx'

flood.file <- read_xlsx(dir.to.flood.file)
colnames(flood.file)

## dates
start_year <- format(as.Date(head(flood.file$Began,1)),"%Y")
end_year <- format(as.Date(tail(flood.file$Began,1)),"%Y")
start_date <- paste0(start_year,"-01-01")
end_date <- paste0(end_year,"-12-31")
dates.seq <- seq(as.Date(start_date),as.Date(end_date),by = "day")
years.seq <- as.numeric(format(format(dates.seq,'%Y')))
months.seq <- as.numeric(format(format(dates.seq,'%m')))
num.years.seq <- length(unique(years.seq))

## group the events based on their lon,lat into country boundary
wlrd <- map_data("world")
# ggplot(wlrd, aes(long, lat, group = group)) +
#   geom_polygon() +
#   coord_equal() +
#   theme_bw()

#point.in.polygon(point.x, point.y, pol.x, pol.y, mode.checked=FALSE)
# OUPUTS: integer array; values are:
# 0: point is strictly exterior to pol;
# 1: point is strictly interior to pol;
# 2: point lies on the relative interior of an edge of pol; 
# 3: point is a vertex of pol.
# point.in.polygon(1:10,1:10,c(3,5,5,3),c(3,3,5,5))
lon.lat_events <- data.frame('lon'=flood.file$long,'lat'=flood.file$lat)
names_country <- unique(wlrd$region)
num.country <- length(names_country)

## remove country with no event at all
no.event.country <- c()
for (k in 1:num.country){
  single_country <- wlrd[wlrd$region%in%names_country[k],]
  output.point_in_polygon <- point.in.polygon(lon.lat_events$lon,
                                              lon.lat_events$lat,
                                              single_country$long,
                                              single_country$lat)
  inside.idx <- which(output.point_in_polygon>0)
  my.counts <- length(inside.idx)
  if (my.counts==0){
    no.event.country <- rbind(no.event.country,c(names_country[k],k)) 
  }
  print(paste(k,'of',num.country))
}
names_country <- unique(wlrd$region)[!(unique(wlrd$region)%in%no.event.country[,1])]

num.country <- length(names_country)
info.obtain <- colnames(flood.file)[c(-1,-2,-4)] # All except 'ID: 1', 'GlideNumber: 2','OtherCountry: 4'
num.vars.to.extract <- length(info.obtain)+1


## create a 3D array: number of countries x number of years x variables
# max number of events across all countries and years
num.country.events <- array(NA,c(num.country))
for (k in 1:num.country){
  print(k)
  single_country <- wlrd[wlrd$region%in%names_country[k],]
  single_country_group <- single_country$group
  dummy.freq <- data.frame(table(single_country_group))
  
  if (dim(dummy.freq)[1]>1){
    idx.sorted.dummy <- order(dummy.freq[,2],decreasing = T)[c(1,2)]
    single_country <- single_country[single_country$group%in%as.numeric(as.matrix(dummy.freq$single_country_group[idx.sorted.dummy])),]
  }
  
  output.point_in_polygon <- point.in.polygon(lon.lat_events$lon,
                                              lon.lat_events$lat,
                                              single_country$long,
                                              single_country$lat)
  inside.idx <- which(output.point_in_polygon>0)
  my.counts <- length(inside.idx)
  years.all <- format(as.Date(flood.file$Began[inside.idx]),'%Y')
  
  sorted.begin.dates <- order(as.Date(flood.file$Began[inside.idx]))
  sorted.inside.idx <- inside.idx[sorted.begin.dates]
  num.country.events[k] <- max(table(years.all))
}

# removing any region (countries, island) with no events (second level)
names_country <- names_country[which(num.country.events>0)]
num.country <- length(names_country)
# 
single.var <- array(NA,c(num.country,num.years.seq,max(num.country.events)))
lst.df.file <- rep(list(single.var),num.vars.to.extract)

names(lst.df.file) <- c(info.obtain,'Duration') # 12 in total



for (k in 1:num.country){
  # single.var <- array(NA,c(num.years.seq,max(num.country.events)))
  # single.var7 <- single.var8 <- single.var9 <- single.var10 <- single.var11 <- single.var12 <- single.var
  # single.var1 <- single.var2 <- single.var3 <- single.var4 <- single.var5 <- single.var6 <- single.var
  # 
  single_country <- wlrd[wlrd$region%in%names_country[k],]
  single_country_group <- single_country$group
  dummy.freq <- data.frame(table(single_country_group))
  
  if (dim(dummy.freq)[1]>1){
    idx.sorted.dummy <- order(dummy.freq[,2],decreasing = T)[c(1,2)]
    single_country <- single_country[single_country$group%in%as.numeric(as.matrix(dummy.freq$single_country_group[idx.sorted.dummy])),]
  }
  
  output.point_in_polygon <- point.in.polygon(lon.lat_events$lon,
                                              lon.lat_events$lat,
                                              single_country$long,
                                              single_country$lat)
  inside.idx <- which(output.point_in_polygon>0)
  my.counts <- length(inside.idx)
  sorted.begin.dates <- order(as.Date(flood.file$Began[inside.idx]))
  sorted.inside.idx <- inside.idx[sorted.begin.dates]
  
  myyears <- as.numeric(format(as.Date(flood.file$Began[inside.idx]),'%Y'))
  idx_years <- which(unique(years.seq) %in% myyears)
  years_freq <- data.frame(table(myyears))
  for(j in 1:dim(years_freq)[1]){
    
    lst.df.file$Country[k,idx_years[j],1:years_freq[j,2]] <- rep(names_country[k],years_freq[j,2]) # country
    
    second.idx <- which(myyears%in%as.numeric(as.character(years_freq[j,1])))
    
    lst.df.file$long[k,idx_years[j],1:years_freq[j,2]] <- flood.file$long[inside.idx[second.idx]] #long
    lst.df.file$lat[k,idx_years[j],1:years_freq[j,2]] <- flood.file$lat[inside.idx[second.idx]]
    lst.df.file$Area[k,idx_years[j],1:years_freq[j,2]] <- flood.file$Area[inside.idx[second.idx]]
    lst.df.file$Began[k,idx_years[j],1:years_freq[j,2]] <- as.Date(flood.file$Began[inside.idx[second.idx]])
    lst.df.file$Ended[k,idx_years[j],1:years_freq[j,2]] <- as.Date(flood.file$Ended[inside.idx[second.idx]])
    lst.df.file$Validation[k,idx_years[j],1:years_freq[j,2]] <- flood.file$Validation[inside.idx[second.idx]]
    lst.df.file$Dead[k,idx_years[j],1:years_freq[j,2]] <- flood.file$Dead[inside.idx[second.idx]]
    lst.df.file$MainCause[k,idx_years[j],1:years_freq[j,2]] <- flood.file$MainCause[inside.idx[second.idx]]
    lst.df.file$Severity[k,idx_years[j],1:years_freq[j,2]] <- flood.file$Severity[inside.idx[second.idx]]
    lst.df.file$Duration[k,idx_years[j],1:years_freq[j,2]] <- 1+as.Date(flood.file$Ended[inside.idx[second.idx]])-as.Date(flood.file$Began[inside.idx[second.idx]])
  }
  print(paste(k,'of',num.country))
}

eventsfreq_names <- cbind((sapply(1:length(names_country),function(x){sum(!is.na(lst.df.file$long[x,,]))})),
                          names_country)

# saved on June 6, 2023 #
# dates are since origin='1970-01-01' below
saveRDS(lst.df.file,
        './Data/processed.data.files/processed.DFO_floods_1985_2021.rds')

## correcting misspellings and reduce the MainCauses to:
# 3 groups (Snow or Ice, Heavy Rain, Storm) for >> 'Climate'
# 1 group (Levee/Dam Breaks or Failure) for >> 'Human-induced'
for(k in 1:num.country){
  print(k)
  one_country <- tolower(lst.df.file$MainCause[k,,])
  for(j in 1:num.years.seq){
    # replacing 'dam', 'levee', and 'levy'
    names_causes.replace <- c('levee','levy','dam')
    names_cause <- c('Infrastructure Failure') # Dam/Levee Failure
    idx_infra <- c()
    for(i in 1:length(names_causes.replace)){
      idx_infra0 <- which(grepl(one_country[j,],pattern=names_causes.replace[i]))
      idx_infra <- append(idx_infra,idx_infra0)
    }
    idx_infra <- unique(idx_infra)
    if(length(idx_infra)>0){
      one_country[j,idx_infra] <- rep(names_cause,length(idx_infra))
    }
    
    # replacing 'ice', 'snow', and 'jam'
    names_causes.replace <- c('snow','ice','glacial','outburst','avalance','avalanche')
    names_cause <- c('Snow or Ice') # Cryosphere 
    idx_infra <- c()
    for(i in 1:length(names_causes.replace)){
      idx_infra0 <- which(grepl(one_country[j,],pattern=names_causes.replace[i]))
      idx_infra <- append(idx_infra,idx_infra0)
    }
    idx_infra <- unique(idx_infra)
    if(length(idx_infra)>0){
      one_country[j,idx_infra] <- rep(names_cause,length(idx_infra))
    }
    
    # replacing any cells with the term 'rain'
    names_causes.replace <- c('rain','ran','landslide') # crazy misspelling
    names_cause <- c('Heavy Rain') # Rainfall
    idx_infra <- c()
    for(i in 1:length(names_causes.replace)){
      idx_infra0 <- which(grepl(one_country[j,],pattern=names_causes.replace[i]))
      idx_infra <- append(idx_infra,idx_infra0)
    }
    idx_infra <- unique(idx_infra)
    if(length(idx_infra)>0){
      one_country[j,idx_infra] <- rep(names_cause,length(idx_infra))
    }
    
    # replacing 'storm', 'hurricane', and 'cyclone'
    names_causes.replace <- c('storm', 'hurricane', 
                              'cyclone','tropical',
                              'stom','typhoon',
                              'tidal','surge','tsunami',
                              'tides') # crazy misspelling
    names_cause <- c('Storm') # Storm
    idx_infra <- c()
    for(i in 1:length(names_causes.replace)){
      idx_infra0 <- which(grepl(one_country[j,],pattern=names_causes.replace[i]))
      idx_infra <- append(idx_infra,idx_infra0)
    }
    idx_infra <- unique(idx_infra)
    if(length(idx_infra)>0){
      one_country[j,idx_infra] <- rep(names_cause,length(idx_infra))
    }
    
    # replacing '0'
    names_causes.replace <- c('0','notes')
    names_cause <- NA # unclear
    idx_infra <- c()
    for(i in 1:length(names_causes.replace)){
      idx_infra0 <- which(grepl(one_country[j,],pattern=names_causes.replace[i]))
      idx_infra <- append(idx_infra,idx_infra0)
    }
    idx_infra <- unique(idx_infra)
    if(length(idx_infra)>0){
      one_country[j,idx_infra] <- rep(names_cause,length(idx_infra))
    }
    
  }
  
  lst.df.file$MainCause[k,,] <- toupper(one_country)
  
}



# saved on June 8, 2023 #
# dates are since origin='1970-01-01' below + causes are grouped by 'dam' and 'levee'
saveRDS(lst.df.file,
        './Data/processed.data.files/processed.plus.DFO_floods_1985_2021.rds')
# each variable: number of countries x number of years x number of max events per year
# names(lst.df.file)
# [1] "Country"    "long"       "lat"        "Area"       "Began"     
# [6] "Ended"      "Validation" "Dead"       "Displaced"  "MainCause" 
# [11] "Severity"   "Duration" 







##--------------------------------------------------------------
## PERT ##
# generate output Excel files for top 5 countries #
idx_sorted <- order(as.numeric(eventsfreq_names[,1]),decreasing = T)
myselec <- 5
c=1 # USA
for(c in 1:myselec){ # dates are since 1900-01-01 origin for Excel
  write.table(25569+as.Date(lst.df.file$Began[idx_sorted[c],,],origin='1970-01-01'),paste0('./Data/','begin.dates_',names_country[idx_sorted[c]],'.csv'),
              col.names = FALSE,row.names = FALSE, sep=' , ')
  write.table(lst.df.file$Duration[idx_sorted[c],,],paste0('./Data/','duration_',names_country[idx_sorted[c]],'.csv'),
              col.names = FALSE,row.names = FALSE, sep=' , ')
  
  write.table(lst.df.file$MainCause[idx_sorted[c],,],paste0('./Data/','MainCause_',names_country[idx_sorted[c]],'.csv'),
              col.names = FALSE,row.names = FALSE, sep=' , ')
  
  
}