#Process raw FAO Global Forest Assessment data to derive Forest cover change stats for Latin American Atlas
#Compute country level % change in forest cover from 2005-2015 relative to 2005 cover 

#Data url
#url = http://fenixrepo.fao.org/cdn/data/flude/download/1.FOREST%20AREA%20AND%20CHARACTERISTICS.zip
#Read in FAO data
GLO_FAandChars = read.table('1.FOREST AREA AND CHARACTERISTICS.csv',header=TRUE,sep=",",check.names=F,na.strings=c("#REF"),fill=TRUE)

#Look at all regions
levels(GLO_FAandChars$Region)

#Save only American regions
NCA_SAM_FAandChars = subset(GLO_FAandChars,Region %in% c("NCA","SAM"))

#Read in countries in the Atlas
countriesinAtlas = read.table('CountriesinAtlas.csv',header=F,sep=",")
countriesinAtlas = as.data.frame(t(countriesinAtlas))

#Keep only countries in the Atlas
AtlasCountries_FAandChars = merge(NCA_SAM_FAandChars,countriesinAtlas,by.x="Name",by.y=1,all.y=TRUE)

#Just use Year and Forest fields
AtlasCountries_ForestByYear = AtlasCountries_FAandChars[,c("Name","Year","Forest")]

#Look for just 2005 and 2015 
AtlasCountries_Forest_2005_2015 = subset(AtlasCountries_ForestByYear,Year %in% c(2005,2015))

library(reshape2)  
#Reshape the data to have '2005' and '2015' as columns
AtlasCountries_Forest_2005_2015_cast = dcast(AtlasCountries_Forest_2005_2015,formula = Name ~ Year)

#cast is changing the field types - declare them manually
AtlasCountries_Forest_2005_2015_cast$`Name` = as.character(AtlasCountries_Forest_2005_2015_cast$`Name`)
AtlasCountries_Forest_2005_2015_cast$`2005` = as.numeric(AtlasCountries_Forest_2005_2015_cast$`2005`)
AtlasCountries_Forest_2005_2015_cast$`2015` = as.numeric(AtlasCountries_Forest_2005_2015_cast$`2015`)

#Add total row for regional totals
totalrow = data.frame("Name"="Total","2005"=sum(AtlasCountries_Forest_2005_2015_cast[,2]),"2015"=sum(AtlasCountries_Forest_2005_2015_cast[,3]),check.names = FALSE) 
AtlasCountries_Forest_2005_2015_cast = rbind(AtlasCountries_Forest_2005_2015_cast,totalrow)

#Compute change
AtlasCountries_Forest_2005_2015_cast[,"Per_change_2005-2015"] = (AtlasCountries_Forest_2005_2015_cast[,"2015"]-AtlasCountries_Forest_2005_2015_cast[,"2005"])/AtlasCountries_Forest_2005_2015_cast[,"2005"]

#Confirm list of countries is same length as rows
nrow(AtlasCountries_Forest_2005_2015_cast)-1 == nrow(countriesinAtlas)

#Format change as a percentage
AtlasCountries_Forest_2005_2015_cast$`Per_change_2005-2015`= sprintf("%.1f %%", 100*AtlasCountries_Forest_2005_2015_cast$`Per_change_2005-2015`)

#Write out file
write.csv(AtlasCountries_Forest_2005_2015_cast,"ForestCoverChange_forLA_Atlas2ndEdition.csv", row.names = FALSE)
