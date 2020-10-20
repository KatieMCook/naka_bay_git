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
library(vegan)

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

torm<-which(is.na(rich$site))
rich<- rich[-torm, ]

ggplot(rich, aes(x=site, y=richness, col=year))+
  geom_point()

#plot bar plot 
ggplot(rich, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())

#remove 1976 sites with no current data
torm<- which(rich$site == 1 | rich$site == 4 | rich$site == 6 |rich$site == 8 |rich$site == 15 |rich$site == 18 | rich$site == 21 | rich$site == 22 |
        rich$site == 24 |rich$site == 28)

rich<- rich[-c(torm), ]


View(rich)

#replot
ggplot(rich, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+
  theme_bw()

species<- unique(all$species)

species<-data.frame(species)

#write.csv(species, 'all_sp_naka.csv')
rich_18<- rich %>% filter(! year==2019)

which(is.na(rich_18$site))
rich_18<- rich_18[-29,]    

rich_plot<- ggplot(rich_18, aes(x=site, y=richness, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+
  theme_bw()+
  labs(x='Site', y='Species Richness')+
  theme(axis.text=element_text(size=13))+
  theme(axis.title=element_text(size=20))

 



rich_plot

#normal
shapiro.test(rich_18$richness)

#paired sample t-test
before<- rich_18[which(rich_18$year=='1976'),]
after<-rich_18[which(rich_18$year=='2018'),]

before<-before$richness
after<-after$richness

t.test(before, after, paired=TRUE)

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
head(summary(pc2))


screeplot(pc2)
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
              rownames(site_matrix_18)== '28_1976' | rownames(site_matrix_18)== "NA_2018")

site_matrix_182<-site_matrix_18[-c(torm),]



#now pca 
pca18<- rda(site_matrix_182)

head(summary(pca18))


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
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey") +
  geom_point(size=3)
  #geom_text( aes(label=site))

rda.plot18

#now add convex hulls
## Create subsetted convex hull
pc76<- site_scores18[site_scores18$year=='1976',]
pc18<-site_scores18[site_scores18$year=='2018',]


hull76 <- pc76[chull(pc76$PC1, pc76$PC2),]
hull18 <- pc18[chull(pc18$PC1, pc18$PC2),]


rda.plot18<-rda.plot18+geom_polygon(data=hull76, aes(x=PC1, y=PC2 ,alpha=0.03), col='#F8766D', fill='#F8766D')

rda.plot18

rda.plot18<-rda.plot18 +geom_polygon(data=hull18, aes(x=PC1, y=PC2,  alpha=0.03), fill='#00BFC4', col='#00BFC4')+
theme_bw()

rda.plot18

rda.plot18<-rda.plot18 + 
  geom_text_repel(aes(label=site), size=5, col='black')+
  labs(x= 'PC1 (13%)', y='PC2 (9%)')+
  theme(axis.text=element_text(size=13))
  
rda.plot18<-rda.plot18+theme(axis.title=element_text(size=20))



rda.plot18

###ok now run adonis to say the years are significantly different 
#sort out site name year for adonis
site_year_ad<- site_name_year[which(! site_name_year$year==2019),]
site_year_ad<- site_name_year[,c(1,2)]

site_year_ad<-site_year_ad[which(site_year_ad$site_year %in% row.names(site_matrix_182)),]

site_year_ad<-as.data.frame(site_year_ad)

row.names(site_year_ad)<- site_year_ad[,1]

identical(row.names(site_year_ad), row.names(site_matrix_182))

adonis2(site_matrix_182~year, data=site_year_ad, method='bray' )


#now simper to see whats driving it 
s1<- with(site_year_ad, simper(site_matrix_182, year))

summary(s1)


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

colors_to_use[colors_to_use=='old']<- '#F8766D'

colors_to_use[colors_to_use=='both']<-'grey'

colors_to_use[colors_to_use=='new']<-'#00BFC4'



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

scatter(pc2)

#work out the eigen values
sum(pc2$eig)

eig<-pc2$eig
rel_eig<- eig/(sum(pc2$eig))
sum(rel_eig)
rel_eig

 

barplot(rel_eig)


pc2.dfs <- data.frame(pc2$li, old18traits) # combine PCoA axes with trait/site data
# make species column so that data can be subset using list of species .e.g per site
pc2.dfs$Species<-row.names(pc2.dfs)


#make global hull
glob_hull<-pc2.dfs[chull(pc2.dfs$A1, pc2.dfs$A2),]

# code to setup ggplot enviroment
ppp <- ggplot() + coord_fixed() +
  labs(x="PCoA 1 (5.3%)", y="PCoA 2 (3.8%)") +
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
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='#F8766D',colour='#F8766D' )
pca_plot_col

pca_old<- ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs[! pc2.dfs$yearcount== 'new',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c('grey','#F8766D' ))+
  geom_polygon(data=old_hull,aes(x=A1,y=A2),alpha=0.08, fill='#F8766D',colour='#F8766D')+
  theme_bw()+
  theme(axis.text=element_text(size=13))+
  theme(axis.title=element_text(size=20))


pca_old

pca_new<-  ppp+
  geom_polygon(data=glob_hull,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pc2.dfs[! pc2.dfs$yearcount== 'old',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c('grey','#00BFC4' ))+
  geom_polygon(data=hull18,aes(x=A1,y=A2),alpha=0.08, fill='#00BFC4',colour='#00BFC4' )+
  theme_bw()+
  theme(axis.text=element_text(size=13))+
  theme(axis.title=element_text(size=20))

pca_ne

#for maria
gen_pred<- pc2.dfs

gen_pred<- gen_pred[ ! gen_pred$yearcount=='old',]
gen_pred<-gen_pred[gen_pred$Trophic=='Predator',]

gen_pred<-gen_pred[,c(5:12)]


write.csv(gen_pred, 'gen_pred.csv') 

pca_old+
  geom_text_repel(data = pc2.dfs, aes(x=A1, y=A2, label=Species), size = 2, segment.size = 0.1)

#colors_to_use[colors_to_use=='old']<- '#F8766D'

#colors_to_use[colors_to_use=='both']<-'grey'

#colors_to_use[colors_to_use=='new']<-'#00BFC4'


pca_old+ geom_text_repel(data = pc2.dfs[! pc2.dfs$yearcount== 'new',], aes(x=A1, y=A2, label=Species), size = 3, segment.size = 0.1)

pca_new+geom_text_repel(data = pc2.dfs[! pc2.dfs$yearcount== 'old',], aes(x=A1, y=A2, label=Species), size = 3, segment.size = 0.1)

#make two panel plot 
grid.arrange(pca_old, pca_new, ncol = 2, nrow = 1)


###now adonis and simper 
traits_adonis<- traits_final[,c(1:8)]

row.names(traits_adonis)<-traits_adonis$species

traits_adonis<- traits_adonis[, -8]

sp_change<-species_year_cat
sp_change<-as.data.frame(sp_change)
row.names(sp_change)<-sp_change$Species

identical(sp_change$Species, row.names(traits18)) 

adonis2(gower_dis18~yearcount, data=sp_change  )  # doesnt work because 'both is a category but thats no change HELP!
#the only way I can think to do this is to put in species-year repeats?

#try with 1,0 cols 
sp_change<-species_year_mat18
sp_change<- sp_change[,-4]
row.names(sp_change)<-sp_change$Species

names(sp_change)<-c('Species','old','new')

adonis2(gower_dis18~old*new, data=sp_change  )  # doesnt work because 'both is a category but thats no change

(sim <- with(dune.env, simper(dune, Management)))
summary(sim)


#now look at the fish depth compression data
shallow_sp<- traits_final[c(which(traits_final$DRange<=10)),]

#check min and max depth with main trait db
shal<-which(traits$Species %in% shallow_sp$species)

traits_shallow<- traits[shal,] #ok they're all super shallow species  #checking the actual max depth not depth range


shallow_sp<-shallow_sp[,c(8,9,10)]

shallow_sp$sum<- shallow_sp$`1976`+shallow_sp$`2018`

shallow_sp<-shallow_sp[-c(which(shallow_sp$sum==0)),]

shallow_sp$pres<-'fill'

both<- which(shallow_sp$sum==2)

shallow_sp$pres[both]<-'both'

old<- which(shallow_sp$`2018`==0)

shallow_sp$pres[old]<-'1976'

new<-which(shallow_sp$`1976`==0)

shallow_sp$pres[new]<-'2018'

shallow_sp<-as.data.frame(shallow_sp)
#shallow specialists altered / replaced/ stay but still many present/ 


#generalist 
gen_in<- read.csv('okinawa_fish_SGI.csv')

names(gen_in)<-c('X', 'Species', 'occ', 'SGI')

#check the names match
which( gen_in$Species %in% colnames(site_matrix_18)) 

which(gen_in$Species %in% old_182$species)

count(unique(old_182$species))

torm<- which(old_182$site==1 | old_182$site== 4 | old_182$site==6 | old_182$site==8 | old_182$site== 15 | old_182$site==18 |
               old_182$site==21 | old_182$site==22 | old_182$site==24 | old_182$site==28 )

both_sites<- old_182[-c(torm),]

##where did I get the species list from?!
traits_18_sp<- rownames(traits18)
traits_18_sp[ which(! rownames(traits18) %in% gen_in$Species)]


#WHAT IS GOING ON?
a<- colnames(site_matrix_182)[which(! colnames(site_matrix_182) %in% sp_change$Species )]
b<- colnames(site_matrix_182)[which(! colnames(site_matrix_182) %in% gen_in$Species )]

c<- c(a,b)

torm<- which(c %in% gen_in$Species)

c<- c[-(torm)]
c<-unique(c)

missing_sp<- data.frame(Species=c)

write.csv(missing_sp, 'extra_oki_sp_KMC.csv')

sp_change$Species[which (! sp_change$Species %in% colnames(site_matrix_182))]

#ok just deal with what I have for now 

unique(both_sites$species[which(! both_sites$species %in% gen_in$Species)] ) #sent these off to RSS

both_sites_70<- both_sites[both_sites$year==1976,]

matrix_70<- acast(both_sites_70, site~species)

site_pres_70<- data.frame(Species= colnames(matrix_70), pres=colSums(matrix_70))

both_sites_18<- both_sites[both_sites$year==2018,]

matrix_18<- acast(both_sites_18, site~species)

site_pres_18<-data.frame(Species=colnames(matrix_18), pres=colSums(matrix_18))

both_sp<- c(site_pres_18$Species, site_pres_70$Species) 

both_sp [which (! unique(both_sp) %in% gen_in$Species) ]


#add gen in to site pres 19 and site pres 70
site_pres_18<- left_join(site_pres_18, gen_in, by='Species')

site_pres_70 <- left_join(site_pres_70, gen_in, by='Species') 

#remove Nas
site_pres_18$SGI<-as.numeric(site_pres_18$SGI)
torm<- which(is.na(site_pres_18$SGI))

site_pres_18<-site_pres_18[-c(torm),]

site_pres_18$occ<-as.numeric (site_pres_18$occ) 


site_pres_70$SGI<- as.numeric(site_pres_70$SGI)
torm<- which(is.na(site_pres_70$SGI))

site_pres_70<- site_pres_70[-c(torm),]

site_pres_70$occ<- as.numeric (site_pres_70$occ)

#cwm 
cwm_gen_18<- weighted.mean(site_pres_18$SGI, site_pres_18$pres)

cwm_gen_70<-weighted.mean(site_pres_70$SGI, site_pres_70$pres)

#ok for each site change in pc1 by mean generalisation 
gen_in$species<-gen_in$Species
both_sites_70<- left_join(both_sites_70, gen_in, by='species')


both_sites_70$SGI<- as.numeric(both_sites_70$SGI)

both_sites_18<- left_join(both_sites_18, gen_in, by='species')
both_sites_18$SGI<-as.numeric(both_sites_18$SGI)

#remove those below 70 
both_sites_18$occ<- as.numeric(both_sites_18$occ)

both_sites_18$SGI[both_sites_18$occ < 70]<- NA 

both_sites_70$occ<- as.numeric(both_sites_70$occ)

both_sites_70$SGI[both_sites_70$occ < 70] <- NA



#site gen mean
site_gen_mean_70<- both_sites_70 %>% group_by(site) %>% summarise ( mean_gen= mean(SGI, na.rm = TRUE))

site_gen_mean_18<- both_sites_18 %>% group_by(site) %>% summarise (mean_gen= mean(SGI, na.rm=TRUE))

site_gen_mean_70$year<- 1975
site_gen_mean_18$year<- 2018

site_gen_mean<- bind_rows(site_gen_mean_70, site_gen_mean_18)

ggplot(site_gen_mean, aes(x=year, y=mean_gen, col=site))+
  geom_point()+
  geom_line( method='lm')

names(site_gen_mean_18)<- c( "site"  ,   "mean_gen18",  "year") 
names(site_gen_mean_70)<- c("site"  ,   "mean_gen75", "year")

#ok change in generalisation vs PCA
change_gen<- left_join(site_gen_mean_18, site_gen_mean_70, by='site')
change_gen$change<- change_gen$mean_gen18- change_gen$mean_gen75

site_sc_76<-site_scores18[site_scores18$year==1976,] 
names(site_sc_76)<-c( "PC1_76" , "PC2_76",  "site", "year")

site_sc_18<- site_scores18[site_scores18$year==2018,]
names(site_sc_18)<- c("PC1_18" , "PC2_18",  "site", "year")

change_PCA<- left_join(site_sc_76, site_sc_18, by='site')

change_PCA$changePC1<- change_PCA$PC1_18 - change_PCA$PC1_76

change_PCA$changePC2<- change_PCA$PC2_18- change_PCA$PC2_76

change_PCA<- left_join(change_PCA, change_gen, by='site')

ggplot(change_PCA, aes(x=change, y=changePC1, col=site))+
  geom_point()+
  geom_smooth(method='lm')

hist(change_PCA$change)

change_PCA<-as.data.frame(change_PCA) 

m1<- lm(changePC1~change, data=change_PCA)

summary(m1)


ggplot(change_PCA, aes(x=changePC2, y=change, col=site))+
  geom_point()+
  geom_smooth(method='lm')


#change in generalisation 
hist(site_gen_mean_18$mean_gen18)
hist(site_gen_mean_70$mean_gen75)

t.test(site_gen_mean_70$mean_gen75, site_gen_mean_18$mean_gen18)

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

#test to see if the richness has changed stats
library(car)
shapiro.test(rich_coral$richness)

qqplot(rich_coral$richness)

ggplot(rich_coral, aes(x= richness))+
  geom_histogram(binwidth = 3)

# normal ish 
before<-rich_coral[rich_coral$Year==1976,]
after<-rich_coral[rich_coral$Year==2018,]

before<-before[-1,]

before<-before$richness
after<-after$richness

t.test(before, after, paired = TRUE, alternative = "two.sided")

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

coral_pcoa$eig

rel_eig<-coral_pcoa$eig/ sum(coral_pcoa$eig)

rel_eig

#two axes
0.168718037 +0.117573758

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

#now do plot by depth ----
depth<- read.csv('coral_depth_year_abun.csv')
depth$Genus<-as.character(depth$Genus)

#check the genus matches the traits 
depth_gen<-unique(depth$Genus)
depth_gen<-as.character(depth_gen)

pca_gen<- pco2_coral.df$genus

which (! depth_gen %in% pca_gen)

depth_gen[c(6,15,25)]

#spell this correct Seriatpora 
#change favia to 
#make monsteastrea 

depth$Genus[depth$Genus=='Favia']<-'Dipsastraea'
depth$Genus[depth$Genus=='Montastrea']<-'Astrea'
depth$Genus[depth$Genus=='Seriatpora']<-'Seriatopora'

#ok 
#now decide what categories to make deep medium shallow?
depth_70<-filter(depth, year==1975)
hist(depth_70$depth)

range(depth_70$depth) #1.5-12m 0-4, 4-8, 8-12?

depth$cat<- depth$depth

depth$cat[depth$cat > 0 & depth$cat < 5]<- 'shallow'
depth$cat[depth$cat >=  5 & depth$cat < 8]<- 'medium'
depth$cat[! depth$cat == 'shallow' & ! depth$cat =='medium']<- 'deep'

length(which(depth$cat=='shallow')) #13
length(which(depth$cat=='medium')) #27
length(which(depth$cat=='deep')) #18

depth$Genus<-as.factor(depth$Genus)
depth$year<-as.character(depth$year)
depth$year<-as.integer(depth$year)

#ggplot the depths
depth_com_plot<-ggplot(depth, aes(x=year, y=depth, col=Genus))+
  geom_line()+
  geom_point(aes(size=abundance))+
  theme_bw()

depth_com_plot


pco2_coral.df
#ok two panel pca plot 1975 and 2018 colour by deep, med or shallow, can see which corals change and change in abundance

#add depth data to pca 
pco2_coral.df

depth$Genus<-as.character(depth$Genus)

depth<- depth %>% rename(genus=Genus)

#need to make 70s depth pca and a 2018 one
pco2_70<-pco2_coral.df
pco2_18<-pco2_coral.df

depth_70<- depth %>% filter(year=='1975')
depth_18<- depth %>% filter(year=='2018')

pco2_70 <- left_join(pco2_70, depth_70, by='genus')
pco2_18<- left_join(pco2_18, depth_18, by='genus')

pco2_70[which(is.na(pco2_70$depth)),]
pco2_18[which(is.na(pco2_18$depth)),]

pco2_70<- pco2_70 %>% filter(! yearcount=='2018')
pco2_18<-pco2_18 %>% filter(! yearcount=='1975')

pca_old_depth<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_70, aes(x=A1, y=A2, col=cat, size=abundance))+
  theme_bw()

pca_old_depth

pca_new_depth<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_18, aes(x=A1, y=A2, col=cat, size=abundance))+
  theme_bw()
pca_new_depth


grid.arrange(pca_old_depth, pca_new_depth, nrow=1, ncol=2) #ok just need to figure out why some are missing

#ellen needs to email me?

#now read in complete long coral data ----

#read in long form data with abundance 
#now read in complete long coral data
coral_long_18<-read.csv('coral_long_2018.csv')

coral_long_76<- read.csv('coral_long_unclean_76.csv')

torm<-which(coral_long_76$Abundance=='')

coral_long_76<-coral_long_76[-c(torm),]

coral_long_76<-coral_long_76[,-5]

#make numeric
coral_long_76$Abundance<-as.numeric(coral_long_76$Abundance)

#check factor levels
levels(coral_long_76$Genus)   #loads of whitespace, make character, remove whitespace then refactorise

coral_long_76$Genus<-as.character(coral_long_76$Genus)

coral_long_76$Genus<- trimws(coral_long_76$Genus)

coral_long_76$Genus[coral_long_76$Genus=='Porites']<-"Porites"
coral_long_76$Genus[coral_long_76$Genus== "Porites lutea"]<-"Porites"
coral_long_76$Genus[coral_long_76$Genus== "Seriatpora" ]<-"Seriatopora"

coral_long_76$Genus[coral_long_76$Genus== "Favia" ]<-'Dipsastraea'
coral_long_76$Genus[coral_long_76$Genus== "Favea" ]<-'Dipsastraea'


coral_long_76$Genus<-as.factor(coral_long_76$Genus)

levels(coral_long_76$Genus)

#check for 18 data
levels(coral_long_18$Species)

coral_long_18$Species<-as.character(coral_long_18$Species)

coral_long_18$Species[coral_long_18$Species=="Porites " ]<-"Porites"

coral_long_18$Species[coral_long_18$Species=="Pssamocora" ]<- "Psammocora" 

coral_long_18$Species[coral_long_18$Species=="Turbinaria irregularis"]<- "Turbinaria"

coral_long_18$Species[coral_long_18$Species=="Favia"  ]<-'Dipsastraea'

coral_long_18$Species[coral_long_18$Species== "Seriatpora"  ]<-"Seriatopora" 

coral_long_18$Species[coral_long_18$Species== 'Montastrea']<-'Astrea' 

coral_long_18$Species<-as.factor(coral_long_18$Species)

levels(coral_long_18$Species)

coral_long_76$year<-1976

coral_long_18$year<-2018

#check names before merge
names(coral_long_76)

names(coral_long_18)

#merge together
#for 2018 get site average across quadrats 
coral_long_18_sum<- coral_long_18 %>% group_by(Quadrat.Depth, Species, site) %>% 
  summarise(Abundance=mean(Colony.Number),health=mean(Colony.Health))

names(coral_long_18_sum)

names(coral_long_76)<- c("Site" ,     "Year"  ,    "Genus"   ,  "Abundance" ,"depth" ,"health"   , "year"  )

names(coral_long_18_sum)<-c("depth" , "Genus" ,      "Site"   , 'Abundance',         "health")      

coral_long_18_sum$Year<-2018

coral_long_18_sum<- coral_long_18_sum[, c(3,6,2,4,1,5)]

names(coral_long_76)

names(coral_long_18_sum)

coral_long_76<-coral_long_76[,-7]

coral_long_76$Genus<-as.character(coral_long_76$Genus)

coral_long_18_sum$Genus<-as.character(coral_long_18_sum$Genus)

coral<- rbind(coral_long_76, coral_long_18_sum)

unique(coral$Genus)

#ok now load in trait db to check if they match, and if they dont figure out if the genera have changed
coral_traits<-read.csv('coralDB_2705_withrange.csv')

#compare
not_in<-which(! unique(coral$Genus) %in% coral_traits$genus)

survey_genera<- unique(coral$Genus)

unique(coral$Genus)[not_in]

#remove genera not in trait database or check for changes
torm<-which(coral$Genus=="Millepora")
coral<-coral[-c(torm),]

coral$Genus[coral$Genus=="Porities"]<- "Porites"

which(coral$Genus=="Cantharellus")

coral$Genus[coral$Genus=="Cantharellus"]<-'Fungiidae_family'

coral$Genus[coral$Genus=="Pleuractis" ]<-'Fungiidae_family'

coral$Genus[coral$Genus=="Herpolitha"  ]<-'Fungiidae_family'

torm<-which(coral$Genus=='Oculinidae') #family so remove

coral<-coral[-c(torm),]

torm<- which(coral$Genus=='Faviidae') #family so remove

coral<-coral[-c(torm),]

torm<- which(coral$Genus=="Heliopora")

coral<-coral[-c(torm),]

coral$Genus[coral$Genus== "Lobactis"  ]<-'Fungiidae_family'

coral$Genus[coral$Genus=="Hydnopora"]<-"Hydnophora" 

coral$Genus[coral$Genus== "Fungiidae" ]<-'Fungiidae_family'

coral$Genus[coral$Genus=="Pachyseries"  ]<-'Pachyseris'

coral$Genus[coral$Genus== "Ctenactis"  ]<-'Fungiidae_family'

coral$Genus[coral$Genus== "Polyphyllia"   ]<-'Fungiidae_family'

torm<- which(coral$Genus=="Caryophylliidae")

coral<-coral[-c(torm),]

not_in<-which(! unique(coral$Genus) %in% coral_traits$genus)


#already combined coral traits in excel so the growth forms are combined in groups at the bottom 

#ok now genus richness for site


#plot bar plot 
coral_site_genus<- coral[,c(1,2,3)]

coral_site_genus<-distinct(coral_site_genus)

coral_site_genus$abundance<-1

rich_coral<- coral_site_genus %>% group_by(Site, Year) %>% summarise(richness=sum(abundance))

rich_coral$Site<-as.factor(rich_coral$Site)
rich_coral$Year<-as.factor(rich_coral$Year)


coral_div_bar<-ggplot(rich_coral, aes(x=Site, y=richness, fill=Year))+
  geom_bar(stat='identity',position=position_dodge())+theme_bw()+
  labs(x='Site', y='Genera Richness')+
theme(axis.text=element_text(size=13))+
  theme(axis.title=element_text(size=20))


coral_div_bar

#now pca----
coral2<-unite(coral, c('Site','Year'), col='site_year', remove=FALSE)

coral2<-coral2[,c(1,4,5)]

coral2<- coral2 %>% group_by(site_year, Genus) %>% summarise(Abundance=sum(Abundance))

#ok now make wide
coral_matrix<- acast(coral2, site_year~Genus)

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
  geom_text( data=site_scores_c, aes(x=PC1, y=PC2, col=year, label=site),size=4)+ #position = position_jitter(width = 1, height=1) 
  geom_polygon(data=hull18_c, aes(x=PC1, y=PC2), fill=NA, col='#00BFC4')+
  theme_bw()

coral_pca_plot  ####this is with abundance, can also do with pres/abs 



#ok now panel richness into the pca plot
coral_div_bar

#inset 
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.3, height = 0.3, x = 0.3, y = 0.8)  # the inset in upper right
print(coral_pca_plot, vp = vpb_)
print(coral_div_bar, vp = vpa_)

#ok now do without abundance
#now pca----

coral3<-coral2

coral3$Abundance<-1

#ok now make wide
coral_matrix<- acast(coral3, site_year~Genus)

coral_matrix[is.na(coral_matrix)]<-0

#ok now pca
pc_coral<-rda(coral_matrix)

summary(pc_coral)

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
ppp <- ggplot(site_scores_c, aes(x=PC1, y=PC2, col=year))+
  geom_hline(yintercept=0, col="darkgrey") +
  geom_vline(xintercept=0, col="darkgrey") +
  geom_point(size=3)
 


coral_pca_plot<-ppp+ #geom_polygon(data=glob_hull_coral,aes(x=PC1,y=PC2),fill=NA,colour="grey70")+
  geom_polygon(data=hull76_c, aes(x=PC1, y=PC2, alpha=0.03),col='#F8766D', fill='#F8766D' )+
  geom_polygon(data=hull18_c, aes(x=PC1, y=PC2, alpha=0.03),fill='#00BFC4', col='#00BFC4' )+
  theme_bw()

coral_pca_plot<- coral_pca_plot+
  geom_text_repel(aes(label=site), size=5, col='black')+
  labs(x= 'PC1 (25%)', y='PC2 (12%)')+
  theme(axis.text=element_text(size=13))

coral_pca_plot  ####this is with pres/abs 

coral_pca_plot<- coral_pca_plot +theme(axis.title=element_text(size=20))


coral_pca_plot






#### ok now run adonis and simper 

identical(row.names(site_scores_c), row.names(coral_matrix)) 

adonis2(coral_matrix~year, data=site_scores_c )

s2<- with(site_scores_c, simper(coral_matrix, year))

summary(s2)

s2

#ok now trait pca

#sort out coral traits
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
coral_year<- coral %>% group_by(Genus, Year) %>% summarise(abundance=(sum(Abundance)))



coral_year_con<- coral_year %>% group_by(Genus) %>% summarise (yearcount=mean(Year))

coral_year_con$yearcount<- as.character(coral_year_con$yearcount)

coral_year_con$yearcount[coral_year_con$yearcount==((2018+1976)/2)]<-'both'


#ok now left join with traits
coral_year_con<-coral_year_con %>% rename(genus=Genus)

coral_year_traits<-left_join(coral_traits, coral_year_con, by='genus')

#filter for only the survey traits
coral_year_traits<- coral_year_traits[c(which(coral_year_traits$genus %in% coral_year_con$genus)),]

rownames(coral_year_traits)<- coral_year_traits$genus


coral_traits<-coral_traits[c(coral_traits$genus %in% coral_year_con$genus),]

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
  labs(x="PCoA 1 (16.9%)", y="PCoA 2 (11.8%)") +
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

pca_old<- pca_old+theme(axis.text=element_text(size=13))+theme(axis.title=element_text(size=20))

pca_old

pca_new<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_coral.df[! pco2_coral.df$yearcount== '1976',], aes(x=A1, y=A2, col=yearcount))+
  scale_colour_manual(values=c( '#00BFC4', 'grey'))+
  geom_polygon(data=new_coral_hull,aes(x=A1,y=A2),alpha=0.08, fill='#00BFC4',colour='#00BFC4' )+
  theme_bw()

pca_new<- pca_new+theme(axis.text=element_text(size=13))+theme(axis.title=element_text(size=20))

pca_new


pca_old_lab<- pca_old+ geom_text_repel(data = pco2_coral.df[! pco2_coral.df$yearcount== '2018',], aes(x=A1, y=A2, label=genus), size = 4, segment.size = 0.1)
pca_old_lab

pca_new_lab<- pca_new+ geom_text_repel(data = pco2_coral.df[! pco2_coral.df$yearcount== '1976',], aes(x=A1, y=A2, label=genus), size = 4, segment.size = 0.1)

pca_new_lab


#now do plot by depth 
#get the average depth and abundance of genera between the two years
#ok can now work out the average depths for the corals: 
names(coral_long_76)

names(coral_long_18_sum)


#merge these 
#this is 'coral'

#ok now on coral data add a column
coral$depthabun<-coral$Abundance*coral$depth

coral$Year<-as.factor(coral$Year)

which(is.na(coral$Abundance)) 

coral$Abundance[c(224,225,250)]<-1

which(is.na(coral$depth)) 

coral$depthabun<-coral$Abundance*coral$depth

av_depth_all<- coral %>% group_by(Genus, Year) %>% summarise(av_depth=(sum(depthabun))/ sum(Abundance), abundance=sum(Abundance))


av_depth_all$Genus<-as.factor(av_depth_all$Genus)
av_depth_all$Year<-as.character(av_depth_all$Year)
av_depth_all$Year<-as.numeric(av_depth_all$Year)

#make plot of depths
ggplot(av_depth_all, aes(x=Year, y=av_depth, col=Genus))+
  geom_point()+
  geom_line()+theme_bw()



#ok 
#now decide what categories to make deep medium shallow?
depth_70<-filter(av_depth_all, Year==1976)
hist(depth_70$av_depth)

range(depth_70$av_depth) #1.5-12m 0-4, 4-8, 8-12?

av_depth_all$cat<- av_depth_all$av_depth

av_depth_all$cat[av_depth_all$cat > 0 & av_depth_all$cat < 5]<- 'shallow'
av_depth_all$cat[av_depth_all$cat >=  5 & av_depth_all$cat < 8]<- 'medium'
av_depth_all$cat[! av_depth_all$cat == 'shallow' & ! av_depth_all$cat =='medium']<- 'deep'

length(which(av_depth_all$cat=='shallow')) #16
length(which(av_depth_all$cat=='medium')) #34
length(which(av_depth_all$cat=='deep')) #27

av_depth_all$count<-1

av_depth_all$Year<-as.integer(av_depth_all$Year)

#see how the categories have changes
depth_cat_change<- av_depth_all %>% group_by(Year, cat) %>% summarise(sumcount=sum(count))

#ggplot the depths
depth_com_plot<-ggplot(av_depth_all, aes(x=Year, y=av_depth, col=Genus))+
  geom_line()+
  geom_point(aes(size=abundance))+
  theme_bw()

depth_com_plot


#plot with relative abundance
av_depth_all_rel<- av_depth_all %>% group_by(Year) %>% mutate(rel_abun = abundance/sum(abundance, na.rm=TRUE))

depth_com_plot2<-ggplot(av_depth_all_rel, aes(x=Year, y=av_depth, col=Genus))+
  geom_line()+
  geom_point(aes(size=rel_abun))+
  theme_bw()

depth_com_plot2

#ok now add new depth data to coral plot


pco2_coral.df
#ok two panel pca plot 1975 and 2018 colour by deep, med or shallow, can see which corals change and change in abundance

#add depth data to pca 


av_depth_all$Genus<-as.character(av_depth_all$Genus)

av_depth_all<- av_depth_all %>% rename(genus=Genus)

#need to make 70s depth pca and a 2018 one
pco2_70<-pco2_coral.df
pco2_18<-pco2_coral.df

depth_70<- av_depth_all %>% filter(Year=='1976')
depth_18<- av_depth_all %>% filter(Year=='2018')

pco2_70 <- left_join(pco2_70, depth_70, by='genus')
pco2_18<- left_join(pco2_18, depth_18, by='genus')

pco2_70[which(is.na(pco2_70$av_depth)),]
pco2_18[which(is.na(pco2_18$av_depth)),]

pco2_70<- pco2_70 %>% filter(! yearcount=='2018')
pco2_18<-pco2_18 %>% filter(! yearcount=='1975')

library(ggrepel)

pca_old_depth<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_70, aes(x=A1, y=A2, col=cat, size=abundance))+
  geom_text_repel( data=pco2_70, aes(x=A1, y=A2, label=genus),size=4)+
  theme_bw()

pca_old_depth

pca_new_depth<- ppp+
  geom_polygon(data=glob_hull_coral,aes(x=A1,y=A2),fill=NA,colour="grey70")+
  geom_point(data=pco2_18, aes(x=A1, y=A2, col=cat, size=abundance))+
  geom_text_repel( data=pco2_18, aes(x=A1, y=A2, label=genus),size=4)+
  theme_bw()
pca_new_depth


grid.arrange(pca_old_depth, pca_new_depth, nrow=1, ncol=2) #ok just need to figure out why some are missing


#ok get the relative abundance of each group 
rel_abun76<- coral_long_76
rel_abun76$total<- sum(rel_abun76$Abundance)

rel_abun76<- rel_abun76 %>% group_by(Genus) %>% summarise(rel_abun=(sum(Abundance, na.rm= TRUE)))
rel_abun76$rel_abun<- rel_abun76$rel_abun/(sum(rel_abun76$rel_abun))


rel_abun18<-coral_long_18
rel_abun18$Species<- as.character(rel_abun18$Species)

rel_abun18<- rel_abun18 %>% group_by(Species) %>% summarise(rel_abun=(sum(Colony.Number, na.rm = TRUE)))
rel_abun18$rel_abun<- rel_abun18$rel_abun/(sum(rel_abun18$rel_abun))

names(rel_abun18)<-c( "Genus"  ,  "rel_abun")
names(rel_abun76)

rel_abun18$year<- 2018
rel_abun76$year<-1976


rel_abun<-rbind(rel_abun18, rel_abun76)

ggplot(rel_abun, aes(x=year, y=rel_abun, col=Genus))+
  geom_point()+
  geom_line()+
  theme_bw()

growth_form<- coral_traits[,c(1,3)]

growth_form$Genus<- rownames(growth_form)

growth_form<-growth_form[,-1]

rel_abun$Genus<-as.character(rel_abun$Genus)

rel_abun<- left_join(rel_abun, growth_form, by='Genus')

#

rel_abun$Genus<-as.character(rel_abun$Genus)

ggplot(rel_abun, aes(x=year, y=rel_abun, col=Genus))+ #col=Growth.form.typical
  geom_point()+
  geom_line()+
  theme_bw()


View(growth_form)
#add the growth forms 
growth_abun<- rel_abun %>% group_by(year,Growth.form.typical) %>% summarise (rel_abun=sum(rel_abun))

which(is.na(growth_abun$Growth.form.typical))

growth_abun<- growth_abun[-c(6,12),]

abun_plot<-ggplot(growth_abun, aes(x=year, y=rel_abun, group=Growth.form.typical, col=Growth.form.typical))+
  geom_point(size=4)+
  geom_line(size=1.5)+
  scale_x_continuous(breaks=c(1975,2018))+
  labs(x='Year', y='Relative Abundance')+
  theme_bw()

abun_plot<- abun_plot+ theme(axis.text=element_text(size=13))
abun_plot+theme(axis.title=element_text(size=20))


#ggplot the depths
av_depth_all<- left_join(av_depth_all, growth_form, by='Genus')
av_depth_all<-as.data.frame(av_depth_all) 


depth_com_plot<-ggplot(av_depth_all, aes(x=Year, y=av_depth, group=Genus, col=Growth.form.typical ))+ #col=Growth.form.typical
  geom_line(size=1)+
  geom_point(size=2)+
  labs(x='Year', y='Average Depth')+
  scale_x_continuous(breaks=c(1975,2018))+
  theme_bw()

depth_com_plot<- depth_com_plot+ theme(axis.text=element_text(size=13))
depth_com_plot+theme(axis.title=element_text(size=20))

depth_com_plot

rel_abun_change<- left_join(rel_abun76, rel_abun18, by='Genus')

rel_abun_change$change<- rel_abun_change$rel_abun.y-rel_abun_change$rel_abun.x

change_sort<-rel_abun_change[order(rel_abun_change$change),]
  
  newdata <- mtcars[order(mpg),]


#see how shifts in e.darling's groups occur

#ok now load in trait db to check if they match, and if they dont figure out if the genera have changed
setwd("D:/corona_contingency/naka_bay")
d_groups<-read.csv('coralDB_2705_withrange.csv')


#coral cover change
cover<-read.csv('coral_cover.csv')

cover$change<- cover[,3]-cover[,2]

ggplot(cover, aes(x=distance, y=change))+
  geom_point()+
  geom_smooth(method='lm')

ggplot(cover, aes(x=change))+
  geom_histogram(binwidth = 10)

shapiro.test(cover$change)

m1<-lm(change~distance, data=cover)
m1
anova(m1)

cover_edit<-cover

cover_edit[c(17:32),]<- cover_edit[c(1:16),]

View(cover_edit)

cover_edit$year<-1

cover_edit$year[c(1:16)]<-1975
cover_edit$year[c(17:32)]<-2018

cover_edit$coral_cov<-cover_edit$X1975

cover_edit$coral_cov[c(17:32)]<- cover_edit$X2018[c(1:16)]
cover_edit<- cover_edit[,c(1,5,6)]

cover_edit$Site<-as.factor(cover_edit$Site)
cover_edit$year<- as.factor(cover_edit$year)

cc_plot<-ggplot(cover_edit, aes(x=Site, y=coral_cov, fill=year))+
  geom_bar(stat='identity',position=position_dodge())+
  labs(x='Site', y='Coral Coverage (%)')+
  theme_bw()

cc_plot<-cc_plot+ theme(axis.text=element_text(size=13))
cc_plot<-cc_plot+theme(axis.title=element_text(size=20))

cc_plot
