
setwd("D:/corona_contingency/naka_bay")

 katie.data <- read.csv("disgusting_format_example.csv", stringsAsFactors = F)
species <- katie.data$Data.Collected[9:nrow(katie.data)]

torm<-which(species=='')
species<-species[-torm]

test <- data.frame("Quadrat.No" = rep(1:20, length(species)),
                   "Quadrat.Depth" = NA,
                   "Species" = rep(species, each = 20),
                   "Colony.Number" = NA,
                   "Colony.Health" = NA)

for ( i in 1:nrow(test)){
  my.col <- which(katie.data[which(katie.data$Data.Collected == "Quadrat No."),] == test$Quadrat.No[i]) # [2,] is idnex for quadrat no 
  # output nof this which is the column to find colony number
  # colunmy nunber + 1 = avg colony health

  my.row <- which(katie.data$Data.Collected == test$Species[i]) 
  #output of this which is the row which is the species in question

  print(katie.data[my.row,my.col])
  
  test$Quadrat.Depth[i] <- katie.data[3,my.col]
  test$Colony.Number[i] <- katie.data[my.row,my.col]
  test$Colony.Health[i] <- katie.data[my.row,my.col + 1 ]
  
}

test$site<-print(name(test))

setwd("D:/corona_contingency/naka_bay/coral_site_18")
#ok now get it to read them all in 
all_site_name<-list.files(pattern='site_')

all_site<-lapply(all_site_name, read.csv)
 

#name the list elements
names(all_site)<-all_site_name

all_site[[1]]

#convert to factors
for (i in 1:length(all_site)){
  for(j in 1: ncol(all_site[[i]])){
  
    all_site[[i]][,j]<-as.character(all_site[[i]][,j])  
  }

  }


#ok now all list is in, need to run harri's code
for(i in 1:length(all_site)){
species<-all_site[[i]]$Data.Collected[9:nrow(all_site[[i]])]
species<-as.character(species)
torm<-which(species=='')


test<- data.frame("Quadrat.No" = rep(1:20, length(species)),
                  "Quadrat.Depth" = NA,
                  "Species" = rep(species, each = 20),
                  "Colony.Number" = NA,
                  "Colony.Health" = NA)
test$Species<-as.character(test$Species)

for ( j in 1:nrow(test)){
  my.col <- which(all_site[[i]][which(all_site[[i]]$Data.Collected == "Quadrat No."),] == test$Quadrat.No[j]) # [2,] is idnex for quadrat no 
  # output nof this which is the column to find colony number
  # colunmy nunber + 1 = avg colony health
  
  my.row <- which(all_site[[i]]$Data.Collected == test$Species[j]) 
  #output of this which is the row which is the species in question
  
  print(all_site[[i]][my.row,my.col])
  
  test$Quadrat.Depth[j] <- all_site[[i]][3,my.col]
  test$Colony.Number[j] <- all_site[[i]][my.row,my.col]
  test$Colony.Health[j] <- all_site[[i]][my.row,my.col + 1 ]
  
}

assign(paste0('long_form_site_',i), test)

  }

long_form_site_1$site<-11
long_form_site_2$site<-12 
long_form_site_3$site<-14  
long_form_site_4$site<-16
long_form_site_5$site<-17
long_form_site_6$site<-19
long_form_site_7$site<-20
long_form_site_8$site<-21
long_form_site_9$site<-23
long_form_site_10$site<-25
long_form_site_11$site<-26
long_form_site_12$site<-26
long_form_site_13$site<-27
long_form_site_14$site<-31
long_form_site_15$site<-7
long_form_site_16$site<-9


all_long<-lapply(ls(pattern='long_form_site_'),get)

#now bind together in one long dataframe

all_long_combined<-do.call(rbind, all_long)

#now remove blanks
torm<- which(all_long_combined$Colony.Number=='')
all_long_comp<-all_long_combined[-c(torm),]


all_long_comp$Colony.Number<-as.numeric(all_long_comp$Colony.Number)
all_long_comp$Colony.Health<-as.numeric(all_long_comp$Colony.Health)

which(is.na(all_long_comp$Colony.Number))
which(is.na(all_long_comp$Colony.Health))

all_long_comp[1842,]
#now combine genera (some had species level data)
library(dplyr)
all_long_comp<-  all_long_comp %>% group_by(Quadrat.No, Quadrat.Depth, Species, site) %>%
  summarise(Colony.Number=sum(Colony.Number, na.rm=TRUE), Colony.Health=mean(Colony.Health,na.rm=TRUE))

#ok now read in the sites with repeated genera (theo's data) and then delete from this data and replace
setwd("D:/corona_contingency/naka_bay")
theo_s<- read.csv('theo_site_reformat.csv')
theo_sites<- unique(theo_s$site)

torm<-which(all_long_comp$site %in% theo_sites)

all_long_comp<- all_long_comp[-torm, ]

unique(all_long_comp$site)

#ok now summarise the theo sites and merge
torm<-which(is.na(theo_s$Colony.Number))
theo_s<-theo_s[-torm, ]

theo_s<- theo_s %>% group_by(Quadrat.No, Quadrat.Depth, Species, site) %>%
  summarise(Colony.Number=sum(Colony.Number, na.rm=TRUE), Colony.Health=mean(Colony.Health,na.rm=TRUE))

theo_s$Species<-as.character(theo_s$Species)

names(theo_s)
names(all_long_comp)

theo_merge<- theo_s[, c(1,2,3,5,6,4)]

all_long_comp$Quadrat.Depth<-as.numeric(all_long_comp$Quadrat.Depth)
which(is.na(all_long_comp$Quadrat.Depth))

all_long_comp[which(is.na(all_long_comp$Quadrat.Depth)),]


#ok now merge
all_long_coral<-rbind(all_long_comp, theo_merge )

write.csv(all_long_coral, 'coral_long_2018.csv')
