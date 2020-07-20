#### species name edit to compare 1976 and now #####

  #setwd("S:/Beger group/Katie Cook/Japan_data/naka_bay")
setwd("D:/corona_contingency/naka_bay")


library(dplyr)
library(ggplot2)
library(worrms)
library(stringr)
library(vegan)
library(reshape2)
library(tidyr)
library(reshape2)

#load checked species names
  new_sp<- read.csv('new_name_sp.csv')
  new_sp$Old.Name<-as.character(new_sp$Old.Name)
  new_sp$New.Name<-as.character(new_sp$New.Name)
  
  
#load 1976 data
  old_sites<- read.csv('old_fish_sites.csv')
  
  old_sites<-old_sites[,c(1,2)]
  
  names(old_sites)<- c('Station', 'Old.Name')
  
  old_sites$Old.Name<-as.character(old_sites$Old.Name)
  

#left join new sp names and old sites
  
  
  all<-left_join(old_sites, new_sp, by='Old.Name')
  
  which(is.na(all$New.Name))
  
  all[148,3]<-'Amblygobius sp'
  
  
  #ok now get rid of old names so just have station and new name
  all<-all[,c(1,3)]
  
  names(all)<-c('station', 'species')
  
  old_survey<-all
  
  #sort out old survey mistakes 
  
  
  
  
  #now get new survey (2018)
  new_survey<-read.csv('transect_ID_clean_2018.csv')

  
  #check new_survey with worms check
 # new_spec<- unique(new_survey$species)

  #correct_name <- data.frame('new_name'= 'new_name', 'old_name'='old_name')
  #correct_name$new_name<-as.character(correct_name$new_name)
  #correct_name$old_name<-as.character(correct_name$old_name)
  
 # survey_sp<-new_spec
 # 
 # survey_sp<-as.character(survey_sp)
  
#  for (i in 234:length(survey_sp)) {
   # check <- NULL
  #  check <- wm_records_taxamatch(survey_sp[i])
    
   # correct_name[i,1] <- check[[1]]$valid_name
  #  correct_name[i,2]<- survey_sp[i]
    
  #  print(i)
 # }
  
  # correct_name[21,1]<-'Chaetodon lunulatus'
 # correct_name[21,2]<-'Chaetodon lunulatus'
  
  
  #correct_name[47,1]<-'Scarus rivulatus'
  #correct_name[47,2]<-'Scarus rivulatus'
  
  #correct_name[71,1]<-'0'
  #correct_name[71,2]<-'0'
  
  #correct_name[124,1]<-'Parrot'
 # correct_name[124,2]<-'Parrot'
  
  #correct_name[138,1]<-'Calotomus japonicus'
  #correct_name[138,2]<-'Calotomus japonicus'
  
  #correct_name[207,1]<-'Choerodon schoenleinii'
  #correct_name[207,2]<-'Choerodon sch'
  
  #correct_name[211,1]<-''
  #correct_name[211,2]<-''
  
  #correct_name[233,1]<-"NO VID "
  #correct_name[233,2]<-"NO VID "
  
  

  #write.csv(correct_name, 'new_fish_wormscheck.csv')
 
#read in 2018 checked species names 
correct_name<- read.csv('new_fish_wormscheck.csv')
  
  #now merge with proper name   
  names(new_survey)<- c( "site"  ,   "transect", "depth_D",  "depth"  ,  "old_name" )
  
  new_survey$old_name<-as.character(new_survey$old_name)
  correct_name$old_name<-as.character(correct_name$old_name)
  
  new_survey<-left_join(new_survey, correct_name, by='old_name')

  
  #now remove funny rows
  new_survey<- new_survey[,-6]
  new_survey<- new_survey[,-5]

  names(new_survey)<-  c( "site"  ,   "transect", "depth_D",  "depth"  ,  "species" )

  
  #add dates so we can start merging together
  new_survey$date<-'2018'
  
  old_survey$date<-'1976'
  
  names(old_survey)<-c("site", "species" ,"date"   )

   
  #get 2019 survey too
  survey19<-read.csv('survey2019_clean.csv')
  
  
  
  #just get the cols that match (first summarise the transect data) 
  
  new_survey$species<-as.character(new_survey$species)
  new_survey$depth<-as.numeric(new_survey$depth)
  
  
 # new_survey_sum<- new_survey %>% group_by(site, date) %>% summarise(species = paste(unique(species)))

  
  sites<- unique(new_survey$site)
  
  for ( i in 1: length(sites)){
    sum <-cbind(rep(sites[i], length(unique(subset(new_survey, new_survey$site == sites[i])$species))),
                          unique(subset(new_survey, new_survey$site == sites[i])$species))
    
    sum<-as.data.frame(sum)
    sum$V1<-as.character(sum$V1)
    sum$V1<-as.numeric(sum$V1)
    sum$V2<-as.character(sum$V2)
    assign(paste0('sum_gr', i), sum)
  }
  
  sum_gr<-lapply(ls(pattern='sum_gr'), get)
  
  new_survey_sum<- do.call(rbind, sum_gr )
  
  new_survey_c<- new_survey_sum

  # ok now clean up new survey 
  
  names(new_survey_c)<-c('site', 'species')

  new_survey_c$year<- '2018'

  #remove zeros 
  to_rm<- which(new_survey_c$species=='0')  

  new_survey_c<- new_survey_c[-c(to_rm), ]  
  
  
#remove na 
  new_survey_c$species<-as.character(new_survey_c$species)
  to_rm<-which(is.na(new_survey_c$species))
  new_survey_c<-new_survey_c[-c(to_rm),]
  
  #remove spaces
  to_rm<- which(new_survey_c$species=='')  
  new_survey_c<- new_survey_c[-(to_rm),]
  
  #remove NO VID 
  to_rm<- which(new_survey_c$species=="NO VID ")  
new_survey_c<- new_survey_c[-(to_rm),]


#OK CLEAN 

#repeat for 2019
#just get the cols that match (first summarise the transect data)

survey19$species<-as.character(survey19$species)

# new_survey_sum<- new_survey %>% group_by(site, date) %>% summarise(species = paste(unique(species)))


sites<- unique(survey19$site)

for ( i in 1: length(sites)){
  sum <-cbind(rep(sites[i], length(unique(subset(survey19, survey19$site == sites[i])$species))),
              unique(subset(survey19, survey19$site == sites[i])$species))
  
  sum<-as.data.frame(sum)
  assign(paste0('19_gr', i), sum)
}

sum_gr<-lapply(ls(pattern='19_gr'), get)

survey19_sum<- do.call(rbind, sum_gr )

survey19_c<- survey19_sum


survey19_c$year='2019'

names(survey19_c)<-c('site','species','year')




## ok all clean
names(old_survey)<- c("site"  ,  "species", "year" )
old_survey$site<-as.character(old_survey$site)
old_survey$site<-as.numeric(old_survey$site)

new_survey_c$site<-as.character(new_survey_c$site)
new_survey_c$site<-as.numeric(new_survey_c$site)

survey19_c$site<-as.character(survey19_c$site)
survey19_c$site<-as.numeric(survey19_c$site)

survey19_c$species<-as.character(survey19_c$species)

#check col names
names(new_survey_c)<- c("site"   , "species" , 'year')    
#rbind

old_new<- rbind(old_survey, new_survey_c)

old_new<-rbind(old_new, survey19_c)

#now make sure the species don't overlap  find missing sp----. 
which(old_new$species=="Acanthurus gahhm")

which(old_new$species=="Bodianus izuensis")
old_new[1201,]

which(old_new$species=='Chysiptera rex')
old_new$species[old_new$species=="Chysiptera rex"] <-'Chrysiptera rex'

which(old_new$species=="Hyporthodus septemfasciatus")
old_new[1440,]

which(old_new$species== "Lethrunus nebulosus" )
old_new$species[old_new$species=="Lethrunus nebulosus"] <-"Lethrinus nebulosus"

old_new$species[old_new$species=="Naso unicornis "] <-"Naso unicornis"

torm<-which(old_new$species==   "Pomacentrus")
old_new<-old_new[-c(torm),]

torm<-which(old_new$species== "Siganus")
old_new<-old_new[-c(torm),]

which(old_new$species=="Stethojulis interrupta " )
old_new$species[old_new$species=="Stethojulis interrupta "] <-"Stethojulis interrupta"
  
old_new$species[old_new$species=="Zebrazoma velifer"] <-'Zebrasoma velifer'

old_new$species[old_new$species=="Acenthurus bariene"] <-'Acanthurus bariene'

old_new$species[old_new$species=="Centropyge vrollkii"] <-'Centropyge vrolikii'


which(old_new$species==  "Chromis cyanea"   )
old_new[1056,] #carribean sp, but only one record, get rid
old_new<-old_new[-1056,]

old_new$species[old_new$species=="Cirrhipectes sp." ] <-'Cirripectes sp.'
  
which(old_new$species==   "Kyphosus pacificus"  )
old_new[1239,]
  
old_new$species[old_new$species=="Meiacanthus grammistes"] <-'Meiacanthus kamoharai'

old_new$species[old_new$species=="Centropyge vrollkii"] <-'Centropyge vrolikii'

which(old_new$species== "Amblygobius sp"   )
old_new$species[old_new$species=="Amblygobius sp"  ] <-'Amblygobius sp.'

old_new$species[old_new$species== "Diploprion bifasciatum " ] <-"Diploprion bifasciatum"

which(old_new$species=='Lepadichthys lineatus')
old_new[1168,] #indo west pacific fish, get rid of this record (dont have book for now and its really small)
old_new<-old_new[-1168,]


old_new$species[old_new$species==  "Meiacanthus sp. "] <-"Meiacanthus sp."

torm<- which(old_new$species=='Parrot')  #delete
old_new<- old_new[-torm,]

torm<- which(old_new$species=="Parupeneus" ) 
old_new<-old_new[-c(torm),]


old_new$species[old_new$species=="Pomacentrus philippinus "  ] <- "Pomacentrus philippinus" 
  
torm<- which(old_new$species=="Stegases sp." ) #remove it cos can't get traits
old_new<-old_new[-c(torm),]

which(old_new$species=='Stegastes punctatus')


old_new$species[old_new$species== "Sufflamen chrysopterum " ] <-  "Sufflamen chrysopterum"
  
old_new$species[old_new$species== "Thalassoma trilobatum "   ] <-  "Thalassoma trilobatum"  
  
old_new$species[old_new$species== "Variola louti "   ] <-   "Variola louti"   

old_new$species[old_new$species== "Zanclus cornutus " ] <-  "Zanclus cornutus"  

old_new$species[old_new$species==  "Zebrasoma veliferum"  ] <-'Zebrasoma velifer'    

#ok only missing ones left are ones that need to be added to the trait db
#run pcas and see if differs

#community analyses?
old_new$abun<-1

old_new$site<-as.factor(old_new$site)
old_new$year<-as.factor(old_new$year)

#write.csv(old_new, 'combined_18_19_76.csv')

#work out the species richness for each site by year
rich<- old_new %>% group_by(site, year) %>% summarise (richness=sum(abun))


ggplot(rich, aes(x=site, y=richness, col=year))+
  geom_point()

#plot bar plot 
ggplot(rich, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())

#remove 1976 sites with no current data
torm<- which(rich$site == 1 | rich$site == 4 | rich$site == 6 |rich$site == 8 |rich$site == 15 |rich$site == 18 | rich$site == 21 | rich$site == 22 |
        rich$site == 24 |rich$site == 28)

rich<- rich[-c(torm), ]

#replot
ggplot(rich, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+
  theme_bw()

species<- unique(all$species)

species<-data.frame(species)

#write.csv(species, 'all_sp_naka.csv')
rich_18<- rich %>% filter(! year==2019)

rich_plot<- ggplot(rich_18, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+
  theme_bw()

rich_plot

#PCA naka species change
#make the sites labelled by year
old_new2<-unite(old_new, c('site', 'year'), col='site_year', remove=FALSE)


#ok now make wide
site_matrix<- acast(old_new2, site_year~species)

which(colnames(site_matrix)=="Amblygobius sp.")

site_matrix<-site_matrix[,-26]

#run PCA
pc1<-rda(site_matrix)
plot(pc1)
summary(pc1)

biplot(pc1, type=c('text')) 

#colour by year
#get year labels for each site
site_name_year<- old_new2 %>% group_by(  site_year, year) %>% summarise()

#check matrix row names are the same as the colour groupings 
identical(site_name_year$site_year, rownames(site_matrix))

year_col<-c('blue', 'black', 'red')[as.factor(site_name_year$year)]

#for plotting:
par(pty='s')  #fixed square aspect ratio


niceplot<-ordiplot(pc1, type='text', ylim=c(-3,3), xlim=c(-3, 5))

#add the colours
points(niceplot, what='sites', col=year_col, pch=19)

#can add elipses, lines etc etc 


##try different plot with ggplot----
#get the PCA values in df
smry<-summary(pc1)
View(smry$sites)

#get the values for PC1 and PC2
site_scores<- data.frame(smry$sites[,1:2]) #PC2 and 2
sp_scores<-data.frame(smry$species[,1:2])  #loadings

library(stringr)
#need to split string in site_name_year so that we can label sites without the year
split<- str_split(site_name_year$site_year, '_', n=2)
split[[1]]

split_df<-do.call(rbind.data.frame, split)

site_name_year$site<- split_df[,1]


#ok now we can plot with ggplot
site_scores$site<- split_df[,1]
site_scores$year<-split_df[,2]


rda.plot<- ggplot(site_scores, aes(x=PC1, y=PC2, col=year))+
  geom_text( aes(label=site), position = position_jitter(width = 1, height=1) )
  
rda.plot

#now add convex hulls
## Create subsetted convex hull
pc76<- site_scores[site_scores$year=='1976',]
pc18<-site_scores[site_scores$year=='2018',]
pc19<-site_scores[site_scores$year=='2019',]

hull76 <- pc76[chull(pc76$PC1, pc76$PC2),]
hull18 <- pc18[chull(pc18$PC1, pc18$PC2),]
hull19 <- pc19[chull(pc19$PC1, pc19$PC2),]

rda.plot<-rda.plot+geom_polygon(data=hull76, aes(x=PC1, y=PC2), fill=NA, colour='red')

rda.plot<-rda.plot +geom_polygon(data=hull18, aes(x=PC1, y=PC2), fill=NA, colour='green')

rda.plot<-rda.plot+  geom_polygon(data=hull19, aes(x=PC1, y=PC2), fill=NA, colour='blue')+
  theme_bw()

rda.plot

#remove sites not present now from old data and replot to see if it changes
#remove 1976 sites with no current data
torm<-which(rownames(site_matrix)== '1_1976' | rownames(site_matrix)== '4_1976' | rownames(site_matrix)== '6_1976' |
   rownames(site_matrix)== '8_1976' | rownames(site_matrix)== '15_1976' | rownames(site_matrix)== '18_1976' |
   rownames(site_matrix)== '21_1976' | rownames(site_matrix)== '22_1976' | rownames(site_matrix)== '24_1976' |
   rownames(site_matrix)== '28_1976')

site_matrix_2<-site_matrix[-c(torm),]

#now plot 
#run PCA
pc2<-rda(site_matrix_2)
plot(pc2)
summary(pc2)


##try different plot with ggplot----
#get the PCA values in df
smry2<-summary(pc2)
View(smry2$sites)

#get the values for PC1 and PC2
site_scores2<- data.frame(smry2$sites[,1:2]) #PC2 and 2
sp_scores2<-data.frame(smry2$species[,1:2])  #loadings


#remove unsused sites from site_name_year
site_name_year2<- site_name_year[-c(torm),]

#remove unsused sites from split_df
split_df_2<-split_df[-c(torm),]

#ok now we can plot with ggplot
site_scores2$site<- split_df_2[,1]
site_scores2$year<-split_df_2[,2]


rda.plot<- ggplot(site_scores2, aes(x=PC1, y=PC2, col=year))+
  geom_text( aes(label=site), position = position_jitter(width = 1, height=1) )

rda.plot

#now add convex hulls
## Create subsetted convex hull
pc762<- site_scores2[site_scores2$year=='1976',]
pc182<-site_scores2[site_scores2$year=='2018',]
pc192<-site_scores2[site_scores2$year=='2019',]

hull762 <- pc762[chull(pc762$PC1, pc762$PC2),]
hull182 <- pc182[chull(pc182$PC1, pc182$PC2),]
hull192 <- pc192[chull(pc192$PC1, pc192$PC2),]

rda.plot<-rda.plot+geom_polygon(data=hull762, aes(x=PC1, y=PC2), fill=NA, colour='red')

rda.plot<-rda.plot +geom_polygon(data=hull182, aes(x=PC1, y=PC2), fill=NA, colour='green')

rda.plot<-rda.plot+  geom_polygon(data=hull192, aes(x=PC1, y=PC2), fill=NA, colour='blue')+
  theme_bw()

rda.plot

#create this with just 2018 and 1976 ----
old_18 <- old_new %>% filter(! year==2019)

#make the sites labelled by year
old_182<-unite(old_18, c('site', 'year'), col='site_year', remove=FALSE)

#ok now make wide
site_matrix_18<- acast(old_182, site_year~species)

#remove 1976 sites with no current data
torm<-which(rownames(site_matrix_18)== '1_1976' | rownames(site_matrix_18)== '4_1976' | rownames(site_matrix_18)== '6_1976' |
              rownames(site_matrix_18)== '8_1976' | rownames(site_matrix_18)== '15_1976' | rownames(site_matrix_18)== '18_1976' |
              rownames(site_matrix_18)== '21_1976' | rownames(site_matrix_18)== '22_1976' | rownames(site_matrix_18)== '24_1976' |
              rownames(site_matrix_18)== '28_1976')

site_matrix_182<-site_matrix_18[-c(torm),]

#now pca 
pca18<- rda(site_matrix_182)

smry18<-summary(pca18)


#get the values for PC1 and PC2

site_scores18<- data.frame(smry18$sites[,1:2]) #PC2 and 2
sp_scores18<-data.frame(smry18$species[,1:2])  #loadings

#ok now split sites into site and year 
#need to split string in site_name_year so that we can label sites without the year

site_name_year18<- site_name_year %>% filter (! year==2019)


#remove unsused sites from site_name_year
site_name_year18<- site_name_year18[-c(torm),]

#check site name year matches what goes into pca
identical (site_name_year18$site_year, rownames(site_matrix_182)) #yes


#ok now we can plot with ggplot
site_scores18$site<- site_name_year18$site
site_scores18$year<-site_name_year18$year


rda.plot18<- ggplot(site_scores18, aes(x=PC1, y=PC2, col=year))+
  geom_text_repel( aes(label=site))

rda.plot18

#now add convex hulls
## Create subsetted convex hull
pc76<- site_scores18[site_scores18$year=='1976',]
pc18<-site_scores18[site_scores18$year=='2018',]


hull76 <- pc76[chull(pc76$PC1, pc76$PC2),]
hull18 <- pc18[chull(pc18$PC1, pc18$PC2),]


rda.plot18<-rda.plot18+geom_polygon(data=hull76, aes(x=PC1, y=PC2), fill=NA, colour='#F8766D')

rda.plot18<-rda.plot18 +geom_polygon(data=hull18, aes(x=PC1, y=PC2), fill=NA, colour='#00BFC4')+
theme_bw()

rda.plot18  

#COME BACK AND ADD GGPLOT COLS----

  
  #ok so plots:
rda.plot18
rich_plot

library(gridExtra)
library(grid)
#inset 
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.45, height = 0.35, x = 0.3, y = 0.25)  # the inset in upper right
print(rda.plot18, vp = vpb_)
print(rich_plot, vp = vpa_)
  


##traits ----
traits<- read.csv('fishtraitsfeb20.csv')

#subset for species that are in naka bay 

#get the species
species<- colnames(site_matrix)

#double check for duplicates
duplicated(species)

#check all naka species in trait db
traits$Species<-as.character(traits$Species)

missing_sp<- which(! species %in% traits$Species)

missing_sp<- species[missing_sp]

missing_sp

#ok all matches 
#now put traits on PCA 


#filter the traits db to naka species
naka_traits<- traits[which(traits$Species %in% species), ]

#extract the traits you want
naka_traits<- naka_traits[, c(2,11,17,19,20,21,33,34,35,36,38,39,40,41)]

#ok now compare trait space 
#bind survey data to trait data
naka_traits<- naka_traits %>% rename(species=Species)

old_new$site<- as.character(old_new$site)
old_new$year<-as.character(old_new$year)

traits_survey<- left_join(old_new, naka_traits, by='species' )

#clean this 
#replace blanks with NA for the whole dataset
traits_survey[traits_survey=='']<-NA
traits_survey[traits_survey==' ']<-NA

#now check for spelling errors
unique(traits_survey$EnvTemp)
traits_survey$EnvTemp[which(traits_survey$EnvTemp=='')]<-NA
traits_survey$EnvTemp[which(traits_survey$EnvTemp=='')]<-NA

unique(traits_survey$Trophic_specific)

unique(traits_survey$Trophic)

traits_survey$Aggregation<-as.factor(traits_survey$Aggregation)
unique(as.character(traits_survey$Aggregation))

traits_survey$Aggregation[which(traits_survey$Aggregation=='Harems')]<-'harems'

unique(traits_survey$Position)
#ok make these groups a little bit less complicated
#Demersal, SubBenthic, Benthic, UpperBenthic, Pelagic
#actually no its fine 

#right now trait based analyses ###actually I think that bit above may have been useless

#so we need a df of functional traits 
#we need a dataframe of pres/abs  #site_matrix2

which (! naka_traits$species  %in% colnames(site_matrix_2) )


naka_traits$species[duplicated(naka_traits$species)]

which(naka_traits$species=='Cheiloprion labiatus')

naka_traits[96,] # take 96 but change trophic spec
naka_traits[97,]

naka_traits<-naka_traits[-97,]
 
naka_traits[96,9]<- 'Obligate corallivore'
naka_traits[96,]

naka_traits[which(naka_traits$species=="Platax pinnatus"),]
naka_traits<-naka_traits[-367,]

#ok now no duplicates should match
identical(naka_traits$species, site_matrix_2)

matrix_ordered<- site_matrix_2[, order(colnames(site_matrix_2))]

traits_ordered<- naka_traits[order(naka_traits$species),]

identical(colnames(matrix_ordered), traits_ordered$species)

#now check the traits like above 
traits_ordered[traits_ordered=='']<-NA
traits_ordered[traits_ordered==' ']<-NA

traits_ordered$Aggregation[which(traits_ordered$Aggregation=='Harems')]<-'harems'

unique(traits_ordered$EnvTemp)
unique(traits_ordered$Trophic_specific)
unique(traits_ordered$Trophic)
unique(traits_ordered$Aggregation)
unique(traits_ordered$Position)
unique(traits_ordered$SpawnMode)
unique(traits_ordered$ParentalMode)

#ok all clean

#traits to use for dbfd
#length, Drange, Trophic, Aggregation, Position, Spawn mode, parental care


traits_final<- traits_ordered[, c(1,3,6, 10,11,12,13,14)]

traits_final<-traits_final %>% rename(Species=species)
rownames(traits_final)<-traits_final$Species

traits_final<-traits_final[,-1]

#TRAITS IS CLEAN, MATRIX IS CLEAN"]

library(FD)

dbFD(traits_final, matrix_ordered)

#some species dont occur in community (maybe because I removed a couple of sites that we didnt resample)
torm<-which(colSums(matrix_ordered)<1)

matrix_ordered <- matrix_ordered[,-c(torm)]
traits_final<- traits_final[-c(torm),]

rownames(traits_final)==colnames(matrix_ordered)

#ok try again!

dbFD(traits_final, matrix_ordered)

which(traits_final[])

#now working maybe some species have too many NAs #remove species with more than 4 NAs
torm<-(which(rowSums(is.na(traits_final))>4)) #4species

traits_final<-traits_final[-torm,]

matrix_ordered<-matrix_ordered[,-torm]

#also here remove rhinecanthus verrucosus because its weird 
which(rownames(traits_final)=='Rhinecanthus verrucosus')
traits_final<-traits_final[-305,]

which(colnames(matrix_ordered)=='Rhinecanthus verrucosus')

matrix_ordered<-matrix_ordered[,-305]

#and try again 

database<-dbFD(traits_final, matrix_ordered, corr="cailliez") #NOT WORKING

gower_distance<-gowdis(traits_final) #try making the distance matrix first

which(is.na(gower_distance)) # no nas #####START HERE'
which(gower_distance==0) #some zeros, these are probably causing problems

database<-dbFD(gower_distance, matrix_ordered, corr="cailliez") #WORKS

Fric<-data.frame(database$FRic)

#split into site year
site_year<-strsplit(rownames(Fric), '_')
unlist<-do.call(rbind.data.frame, site_year)

#funtional richness
Fric$site<-unlist[,1]
Fric$year<-unlist[,2]

#now we can plot 
ggplot(Fric, aes(x=site, y=database.FRic, col=year))+geom_point()

#Functional eveness #cant do this because we dont have abundance 
Fric$Feve<-database$FEve[]
ggplot(Fric, aes(x=site, y=Feve, col=year))+geom_point()


#dendro of traits
dendro<-hclust(gower_distance, method='average')
plot(dendro)


#colour cluster dendro by presence in years 

#number of species
Fric$nbsp<-database$nbsp[]

ggplot(Fric, aes(x=site, y=nbsp, col=year))+geom_point()

#come back to this point for summarising species by year -----
#colour dendrogram by species present now, both and before 
#need to add factor to traits final (1970s, 2010s, both)
old_new2$year<- as.character(old_new2$year)
old_new2$year<-as.numeric(old_new2$year)

species_year<- data.frame(species=old_new2$species, year=old_new2$year)
species_year<-unique(species_year)

species_year_con<- species_year %>% group_by(species) %>% summarise(yearcount= mean(year))

species_year_con$yearcount<-as.character(species_year_con$yearcount)

species_year_con$yearcount[species_year_con$yearcount==1976]<-'old'
species_year_con$yearcount[species_year_con$yearcount==2018]<-'new'
species_year_con$yearcount[species_year_con$yearcount==2019]<-'new'
species_year_con$yearcount[species_year_con$yearcount==2018.5]<-'new'
species_year_con$yearcount[species_year_con$yearcount==((2018+2019+1976)/3)]<-'both'
species_year_con$yearcount[species_year_con$yearcount==1997]<-'both'
species_year_con$yearcount[species_year_con$yearcount==1997.5]<-'both'

#right now make the years into factors
species_year_con$yearcount<-as.factor(species_year_con$yearcount)


#check the species match with dendro species
traits_species<-rownames(traits_final)
check_species<-as.character(species_year_con$species)

torm<- which( ! check_species %in% traits_species)

check_species[torm]

which( traits_species %in% check_species)

#remove the species which dont match 
species_year_con<- species_year_con[-c(torm),]


#now plot a dendrogram with coloured labels 

library(dendextend)

#dendrogram

dend<-as.dendrogram(hclust(gower_distance, method='average'))

# By default, the dend has no colors to the labels
plot(dend)


# let's add some color:
colors_to_use <- as.character(species_year_con$yearcount)

#lets explore if this works
colors_to_use<-colors_to_use[order.dendrogram(dend)]

colors_to_use

colors_to_use[colors_to_use=='old']<-'cornflower blue'

colors_to_use[colors_to_use=='both']<-'orange'

colors_to_use[colors_to_use=='new']<-'pink'

#now we can use them
labels_colors(dend)<-colors_to_use

#now each state has a colour
labels_colors(dend)


plot(dend)

dend %>% set("labels_cex", 0.2) %>% plot


#doesnt show much try with just 2018 data
#remove rhinecanthus verrucosus 


torm<- which(old_new$year==2019)
old18<- old_new[-c(torm), ]

which(old18$species=='Rhinecanthus verrucosus') #outlier too many NAs
old18<-old18[-165,]

old18sp<- old18$species
old18sp<-unique(old18sp)

rownames(traits_final)

old18sp[which(  ! old18sp %in% rownames(traits_final))] #these must be the ones that had no data in trait db. ok 

old18traits<-traits_final[rownames(traits_final) %in% old18sp,] #ok this the trait database or trait 'space' for 76, 18

traits18<-old18traits

library(FD)

gower_dis18<-gowdis(traits18) #try making the distance matrix first


#now plot a dendrogram with coloured labels 

#dendrogram

dend<-as.dendrogram(hclust(gower_dis18, method='average'))

# By default, the dend has no colors to the labels
plot(dend)

#now get the species/year def
species_year_con18<- species_year_con[which (species_year_con$species %in% rownames(traits18)),]


# let's add some color:
colors_to_use <- as.character(species_year_con18$yearcount)

#lets explore if this works
colors_to_use<-colors_to_use[order.dendrogram(dend)]

colors_to_use

colors_to_use[colors_to_use=='old']<-'cornflower blue'

colors_to_use[colors_to_use=='both']<-'orange'

colors_to_use[colors_to_use=='new']<-'pink'

#now we can use them
labels_colors(dend)<-colors_to_use

#now each state has a colour
labels_colors(dend)


plot(dend)

dend %>% set("labels_cex", 0.2) %>% plot





#ok this doesnt show too much try PCOA first with hulls - for 2018 and 19
library(ape)
PCOA<- pcoa(gower_distance)

biplot.pcoa(PCOA)

barplot(PCOA$values$Relative_eig[1:10])


#using MM code
library(ade4)

fishtrait_dist<-gower_distance

fishtrait_dist2 <- cailliez(fishtrait_dist) # make euclidean, try lingoes() if doesnt work

pc2<-dudi.pco(d = fishtrait_dist2, scannf = FALSE, nf = 4) # principal coord analyses of Gower Dist trait data, 4 axes

pc2.dfs <- data.frame(pc2$li, traits_final) # combine PCoA axes with trait/site data
# make species column so that data can be subset using list of species .e.g per site
pc2.dfs$Species<-row.names(pc2.dfs)



#make global hull
glob_hull<-pc2.dfs[chull(pc2.dfs$A1, pc2.dfs$A2),]

# code to setup ggplot enviroment
ppp <- ggplot() + coord_fixed() +
  labs(x="Comp1, Axis1", y="Comp2, Axis2") +
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey")

# plot global hull
ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs, aes(x=A1, y=A2), colour='grey70')+theme_bw()




#subset for year data ----
#make df with the years #using code from above 
#need to add factor to traits final (1970s, 2010s, both)
old_new2$year<- as.character(old_new2$year)
old_new2$year<-as.numeric(old_new2$year)

species_year<- data.frame(species=old_new2$species, year=old_new2$year)
species_year<-unique(species_year)

#make into matrix (1-0)
species_year$abundance<- 1

species_year_mat<-dcast(species_year, species~year, value.var = 'abundance')

species_year_mat[is.na(species_year_mat)]<-0

#ok now remove species that dont match trait db 

torm<- which( ! species_year_mat$species %in% traits_species)

species_year_mat<-species_year_mat[-c(torm),]

#left join to traits final
traits_final$species<-rownames(traits_final)
traits_final<-left_join(traits_final, species_year_mat, by='species')#probs didnt need to do this

#remove the outlier again
which(species_year_mat$species=='Rhinecanthus verrucosus')



#now join year data to pc2.dfs
species_year_mat<- species_year_mat %>% rename(Species=species)
species_year_mat$Species<- as.character(species_year_mat$Species)

pc2.dfs<- left_join(pc2.dfs, species_year_mat, by='Species')

# subset for 76, 18,19  data
pc2_old<-pc2.dfs[pc2.dfs$'1976' == 1,]
pc2_18<-pc2.dfs[pc2.dfs$'2018' == 1,]
pc2_19<-pc2.dfs[pc2.dfs$'2019' == 1,]

# make regional hull
old_hull<-pc2_old[chull(pc2_old$A1, pc2_old$A2),]
hull18<-pc2_18[chull(pc2_18$A1, pc2_18$A2),]
hull19<-pc2_19[chull(pc2_19$A1, pc2_19$A2),]

# plot regional trait space and hull inside global
library(ggrepel)
ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs, aes(x=A1, y=A2), colour='grey70')+
  geom_polygon(data=old_hull,aes(x=A1,y=A2),alpha=0.08, fill='green',colour="green")+
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='red',colour="red" )+
  geom_polygon(data=hull19,aes(x=A1,y=A2),alpha=0.08, fill='yellow',colour="yellow" )+
  geom_text_repel(data = pc2.dfs, aes(x=A1, y=A2, label=Species), size = 2, segment.size = 0.1)
  

#PCOA with just 1976 2018 data 

PCOA<- pcoa(gower_dis18)

biplot.pcoa(PCOA)

barplot(PCOA$values$Relative_eig[1:10])


#using MM code


fishtrait_dist<-gower_dis18

fishtrait_dist2 <- cailliez(fishtrait_dist) # make euclidean, try lingoes() if doesnt work

pc2<-dudi.pco(d = fishtrait_dist2, scannf = FALSE, nf = 4) # principal coord analyses of Gower Dist trait data, 4 axes

pc2.dfs <- data.frame(pc2$li, old18traits) # combine PCoA axes with trait/site data
# make species column so that data can be subset using list of species .e.g per site
pc2.dfs$Species<-row.names(pc2.dfs)


#make global hull
glob_hull<-pc2.dfs[chull(pc2.dfs$A1, pc2.dfs$A2),]

# code to setup ggplot enviroment
ppp <- ggplot() + coord_fixed() +
  labs(x="Comp1, Axis1", y="Comp2, Axis2") +
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey")

# plot global hull
ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs, aes(x=A1, y=A2), colour='grey70')+theme_bw()

#subset for year data
#make df with the years #using code from above 
#need to add factor to traits final (1970s, 2018s, both)

#left join to traits final


#now join year data to pc2.dfs
old18traits$Species <-rownames(old18traits)

traitsp18<-old18traits$Species

#add the year pres/abs data to the pca 
species_year_mat18<- species_year_mat[species_year_mat$Species %in% traitsp18,]

identical (species_year_mat18$Species, pc2.dfs$Species)

pc2.dfs<- left_join(pc2.dfs, species_year_mat18, by='Species')


# subset for 76, 18 data
pc2_old<-pc2.dfs[pc2.dfs$'1976' == 1,]
pc2_18<-pc2.dfs[pc2.dfs$'2018' == 1,]


# make regional hull
old_hull<-pc2_old[chull(pc2_old$A1, pc2_old$A2),]
hull18<-pc2_18[chull(pc2_18$A1, pc2_18$A2),]


# plot regional trait space and hull inside global

ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs, aes(x=A1, y=A2), colour='grey70')+
  geom_polygon(data=old_hull,aes(x=A1,y=A2),alpha=0.08, fill='green',colour="green")+
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='red',colour="red" )

#ok now colour points by 'new' 'old' 'both'
species_year_con18$species<-as.character(species_year_con18$species)

species_year_cat<- species_year_con18
species_year_cat <- species_year_cat %>% rename (Species=species)

pc2.dfs <-left_join(pc2.dfs, species_year_cat, by='Species')

#make pca plot with the points coloured
pca_plot_col<- ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs, aes(x=A1, y=A2, col=yearcount))+
  geom_polygon(data=old_hull,aes(x=A1,y=A2),alpha=0.08, fill='green',colour="green")+
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='red',colour="red" )

pca_old<- pca_plot_col<- ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs[! pc2.dfs$yearcount== 'new',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c('grey', 'red'))+
  geom_polygon(data=old_hull,aes(x=A1,y=A2),alpha=0.08, fill='red',colour="red")+
  theme_bw()

pca_old

pca_new<- pca_plot_col<- ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs[! pc2.dfs$yearcount== 'old',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c('grey', 'blue'))+
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='blue',colour="blue" )+
  theme_bw()
pca_new

pca_old+ geom_text_repel(data = pc2.dfs[! pc2.dfs$yearcount== 'new',], aes(x=A1, y=A2, label=Species), size = 3, segment.size = 0.1)

pca_new+geom_text_repel(data = pc2.dfs[! pc2.dfs$yearcount== 'old',], aes(x=A1, y=A2, label=Species), size = 3, segment.size = 0.1)

#make two panel plot 
grid.arrange(pca_old, pca_new, ncol = 2, nrow = 1)

#########################################################
###############ok make coral figure!#####################----
coral<-read.csv('combined_76_18_coral.csv')

#check the generera
unique(coral$genera)

#ok now load in trait db to check if they match, and if they dont figure out if the genera have changed
coral_traits<-read.csv('coralDB_2705_withrange.csv')

#compare
not_in<-which(! unique(coral$genera) %in% coral_traits$genus)

survey_genera<- unique(coral$genera)

unique(coral$genera)[not_in]
survey_genera[2]

which(coral$genera=='Fungiidae')
which(coral$genera=='Fungia')



#combine Montipora, Acropora, Porites, Astreopora, Hydnophora in trait db
#Add Heliopora, Millepora, Sandalolitha (to replace Parahalomitra),  
#get rid of fugiidae and use fungia from the db 
#spell this correct Seriatpora 
#Protolobophyllia is Cynarina
#Heliopora check the species
#change favia to Dipsastraea


#from MOE list 
#make monsteastrea astrea

#ok 
coral$genera<-as.character(coral$genera)
coral$genera[coral$genera=='Montastrea']<-'Astrea'

which(coral$genera=='Favia')

coral_traits$genus<-as.character(coral_traits$genus)
coral_traits$genus[coral_traits$genus=='Fungiidae_family']<-'Fungiidae'

coral$genera[coral$genera=='Seriatpora']<-'Seriatopora'

coral$genera[coral$genera=='Protolobophyllia']<-'Cynarina'

#change the favia to dipsastraea - might change my mind on this but for now
coral$genera[coral$genera=='Favia']<- 'Dipsastraea'

coral$genera[coral$genera=='Parahalomitra']<-'Sandalolitha'

##ok now alter trait db 
#no trait db traits for heliopora (blue coral ) + millepora (fire coral)
#only Sandalothia robusta is present in okinawa so add data for that (in excel)


#ok now combine the split genera somehow #do this in excel slightly easier 
#mean(coral_traits$Corallite.width.maximum[c(2:7)])

#coral trait db make a new 'branching'cat

#montipora form
#mont<-read.csv('montipora_form.csv')
#mont$abun<-1
#mont<-mont %>% group_by(form) %>% summarise(no=sum(abun)) #montipora is massive(the most are massive/submassive)
#mean(coral_traits$Range.size[c(51:53)])

#ran the same code for porites #branching

#ok traits db sorted #now just combine the 


#now check again 
not_in<-which(! unique(coral$genera) %in% coral_traits$genus)

survey_genera<- unique(coral$genera)

unique(coral$genera)[not_in]

#sorted !

#ok now genus richness for site
coral$abun<-1

rich_coral<- coral %>% group_by(site, year) %>% summarise(richness=sum(abun))

rich_coral$site<-as.factor(rich_coral$site)
rich_coral$year<-as.factor(rich_coral$year)

#plot bar plot 
coral_div_bar<-ggplot(rich_coral, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+theme_bw()+
  theme(legend.position='none')

coral_div_bar

#now pca----
coral2<-unite(coral, c('site','year'), col='site_year', remove=FALSE)

coral2<-coral2[,c(1,3,5)]

#ok now make wide
coral_matrix<- acast(coral2, site_year~genera)

coral_matrix[is.na(coral_matrix)]<-0

#ok now pca
pc_coral<-rda(coral_matrix)


plot(pc_coral)

coral_psum<-summary(pc_coral)

#get the values for PC1 and PC2
site_scores_c<- data.frame(coral_psum$sites[,1:2]) #PC2 and 2
sp_scores_c<-data.frame(coral_psum$species[,1:2])  #loadings

#label sites with year and site split
split_c<- str_split(rownames(site_scores_c), '_', n=2)

split_c[[1]]

split_df_c<-do.call(rbind.data.frame, split_c)

site_name_year_coral<-data.frame(site_year=rownames(site_scores_c), sites=split_df_c[,1], year=split_df_c[,2])

site_scores_c$site<-split_df_c[,1]
site_scores_c$year<-split_df_c[,2]

#ok now plot with ggplot
coral_pca_plot<-ggplot(site_scores_c, aes(x=PC1, y=PC2, col=year))+
  geom_text( aes(label=site), position = position_jitter(width = 1, height=1) )

coral_pca_plot


#now add convex hulls
## Create subsetted convex hull


pc_coral


pc_c_76<- site_scores_c[site_scores_c$year=='1976',]
pc_c_18<-site_scores_c[site_scores_c$year=='2018',]

which(site_scores_c$year=='1976')

glob_hull_coral<-site_scores_c[chull(site_scores_c$PC1, site_scores_c$PC2),]
hull76_c <- pc_c_76[chull(pc_c_76$PC1, pc_c_76$PC2),]
hull18_c <- pc_c_18[chull(pc_c_18$PC1, pc_c_18$PC2),]

#get ggplot cols
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
ggplotColours(n=2)

#set up the overall plot? maybe this is whats wrong

# code to setup ggplot enviroment
ppp <- ggplot() + coord_fixed() +
  labs(x="PC1", y="PC2") +
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey")

  
 
coral_pca_plot<-ppp+ #geom_polygon(data=glob_hull_coral,aes(x=PC1,y=PC2),fill=NA,colour="grey70")+
  geom_polygon(data=hull76_c, aes(x=PC1, y=PC2), fill=NA, col='#F8766D')+
  #geom_point(data=site_scores_c, aes(x=PC1, y=PC2))+
  geom_text_repel( data=site_scores_c, aes(x=PC1, y=PC2, col=year, label=site),size=4)+ #position = position_jitter(width = 1, height=1) 
  geom_polygon(data=hull18_c, aes(x=PC1, y=PC2), fill=NA, col='#00BFC4')+
  theme_bw()

coral_pca_plot

#ok now panel richness into the pca plot
coral_div_bar

#inset 
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.3, height = 0.3, x = 0.3, y = 0.8)  # the inset in upper right
print(coral_pca_plot, vp = vpb_)
print(coral_div_bar, vp = vpa_)

#ok now trait pca

#sort out coral traits
tokeep<-which(coral_traits$genus %in% colnames(coral_matrix))

coral_traits<-coral_traits[tokeep,]

torm<- which(! colnames(coral_matrix) %in% coral_traits$genus)

#remove the octocorals 
coral_matrix2<- coral_matrix[, -c(torm)]

#ok now clean up to traits
unique(coral_traits$Coloniality)
unique(coral_traits$Growth.form.typical)

coral_traits$Growth.form.typical[coral_traits$Growth.form.typical=='digitate']<-'branching'
coral_traits$Growth.form.typical[coral_traits$Growth.form.typical=='branching_closed']<-'branching'

coral_traits$Growth.form.typical<-as.factor(coral_traits$Growth.form.typical)
levels(coral_traits$Growth.form.typical)
unique(coral_traits$Growth.form.typical)

unique(coral_traits$Water.clarity.preference)
unique(coral_traits$Wave.exposure.preference)

unique(coral_traits$Sexual_system)
unique(coral_traits$larval_development)

#now just get traits you want 
coral_traits<-coral_traits[,c(2,3,4,5,7,8,9,11,12,13,15)]

#ok now need to sort out the survey data into years 
coral_year<- coral %>% group_by(genera, year) %>% summarise(abundance=(sum(abun)))

which(coral_year$genera=='Millepora')
coral_year<-coral_year[-43,]
which(coral_year$genera=='Heliopora')
coral_year<-coral_year[-30,]

coral_year_con<- coral_year %>% group_by(genera) %>% summarise (yearcount=mean(year))

coral_year_con$yearcount<- as.character(coral_year_con$yearcount)

coral_year_con$yearcount[coral_year_con$yearcount==((2018+1976)/2)]<-'both'

#ok now left join with traits
coral_year_con<-coral_year_con %>% rename(genus=genera)

coral_year_traits<-left_join(coral_traits, coral_year_con, by='genus')

rownames(coral_year_traits)<- coral_year_traits$genus

#ok now pcoa
rownames(coral_traits)<-coral_traits$genus
coral_traits<-coral_traits[,-1]

coral_gow<-gowdis(coral_traits)

coral_gow<-cailliez(coral_gow)

coral_pcoa<-dudi.pco(d=coral_gow, scannf = FALSE, nf = 4)

pco2_coral.df<- data.frame(coral_pcoa$li, coral_year_traits) # combine PCoA axes with trait/site data



#make global hull
glob_hull_coral<-pco2_coral.df[chull(pco2_coral.df$A1, pco2_coral.df$A2),]

# code to setup ggplot enviroment
ppp <- ggplot() + coord_fixed() +
  labs(x="Comp1, Axis1", y="Comp2, Axis2") +
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey")

# plot global hull
ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_coral.df, aes(x=A1, y=A2), colour='grey70')+theme_bw()


# subset for 76, 18,19  data
pco_coral_old<-pco2_coral.df[! pco2_coral.df$yearcount == '2018',]
pco_coral_new<-pco2_coral.df[! pco2_coral.df$yearcount == '1976',]


# make regional hull
old_coral_hull<-pco_coral_old[chull(pco_coral_old$A1, pco_coral_old$A2),]
new_coral_hull<-pco_coral_new[chull(pco_coral_new$A1, pco_coral_new$A2),]

# plot regional trait space and hull inside global

pca_old<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_coral.df[! pco2_coral.df$yearcount== '2018',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c('salmon', 'grey'))+
  geom_polygon(data=old_coral_hull,aes(x=A1,y=A2),alpha=0.08, fill='salmon',colour="salmon")+
  theme_bw()

pca_old

pca_new<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_coral.df[! pco2_coral.df$yearcount== '1976',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c( '#00BFC4', 'grey'))+
  geom_polygon(data=new_coral_hull,aes(x=A1,y=A2),alpha=0.08, fill='#00BFC4',colour='#00BFC4' )+
  theme_bw()
pca_new


pca_old_lab<- pca_old+ geom_text_repel(data = pco2_coral.df[! pco2_coral.df$yearcount== '2018',], aes(x=A1, y=A2, label=genus), size = 3, segment.size = 0.1)
pca_old_lab

pca_new_lab<- pca_new+ geom_text_repel(data = pco2_coral.df[! pco2_coral.df$yearcount== '1976',], aes(x=A1, y=A2, label=genus), size = 3, segment.size = 0.1)

pca_new_lab


