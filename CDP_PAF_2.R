library(countrycode)
library(readxl)
library(WDI)
library(openxlsx)

daciso = c("AUS","CZE","FIN","IRL","LTU","NZL","SWE","AUT","DEU","FRA","ISL",
	"LUX","POL","USA","BEL","DNK","GBR","ITA","PRT","CAN","ESP","GRC",
	"JPN","NLD","SVK","CHE","EST","HUN","KOR","NOR","SVN")

##---------------------------------------------------------------------------------
### READ IN CRS
source("DoCRS2.R")	
#crsfull = crs 
#crs = crs %>% filter(type %in% c("dac","multi"))
crs$group[crs$group=="Part I unallocated by income"] = "unallocated"
crs$iso3c = countrycode(crs$recipient_name,origin="country.name",destination="iso3c")
crs$iso3c[crs$recipient_name=="Kosovo"] = "XKX"
crs$iso3c[crs$recipient_name=="Micronesia"] = "FSM"
crs$iso3c[crs$recipient_name=="TÃ¼rkiye"] = "TUR"	# obviously need to fix this encoding at some point
crs$idrc = 1*(crs$aid_t %in% c("H02","H03","H04","H05"))
crs$hum  = 1*(crs$sector_code %in% c(720,730,740,700))
crs$ukraine  = 1*(crs$recipient_name=="Ukraine")


##---------------------------------------------------------------------------------
## WDI INDICATORS
WBindicators = c(
'ny.gnp.pcap.pp.kd',		# gni per capita, PPP, constant dollars
'ny.gdp.pcap.pp.kd',		# gdp per capita, PPP, constant dollars
'sp.pop.totl')			# total population

options(timeout=500)
wdi = WDI(country = 'all', indicator=WBindicators,start=2006,end=2022,extra=TRUE)
wdi = wdi[c("country","region","iso3c","income","year",WBindicators[2:3])]
names(wdi)[names(wdi)==WBindicators[2]] = "gdppc"
names(wdi)[names(wdi)==WBindicators[3]] = "pop"
wdi$iso3c[wdi$country=="Kosovo"] = "XKX"
wdi$country=NULL; rm(WBindicators)

##---------------------------------------------------------------------------------
## HISTORICAL WB INCOME CATEGORIES
oghist = read.xlsx("OGHIST.xlsx",sheet="Country Analytical History",startRow=6)
oghist = oghist[-(1:4),]
names(oghist)[1:2] = c("iso3c","recipientname")
oghist = oghist %>% pivot_longer(cols=`1987`:`2023`,names_to="year")
oghist$iso3c[oghist$recipientname=="Kosovo"] = "XKX"
names(oghist)[names(oghist)=="value"] = "wbinc"
oghist = oghist[!is.na(oghist$iso3c),c("iso3c","year","wbinc")]
oghist$year = as.numeric(oghist$year)
## CDP JUST WANT MOST RECENT FOR NOW

crs = left_join(crs,oghist[oghist$year==2023,c("iso3c","wbinc")],by="iso3c")
crs$wbinc[is.na(crs$wbinc) & crs$group=="UMICs"] = "UM"
crs$wbinc[is.na(crs$wbinc) & crs$group=="LMICs"] = "LM"
crs$wbinc[is.na(crs$wbinc) & crs$group=="LDCs"] = "L"

##---------------------------------------------------------------------------------
## READ IN CPA
cpa = "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_CPA,1.1/all?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
cpa = read.csv(cpa)
names(cpa)[names(cpa)=="TIME_PERIOD"] = "year"
names(cpa)[names(cpa)=="OBS_VALUE"] = "value"

cpa = cpa %>% filter(Recipient %in% c("Ukraine","Developing countries") & Donor=="Official donors" & Price.base=="Constant prices") %>% 
	select(year,value,Recipient) %>% pivot_wider(names_from=Recipient,values_from=value,values_fill=0) %>% 
	arrange(year)%>% as.data.frame()
names(cpa) = c("year","cpa","cpa_UA")	
cpa$cpa_nonUA = with(cpa,cpa - cpa_UA)

#===============================================================================================
#===============================================================================================

# HUMANITARIAN AND IDRC FROM CRS TO COMPARE WITH CPA (box 2.2 comment)

# Note: there are 14 observations marked both as IDRC and hum.
# From a look at the descriptions, the IDRC tag looks wrong as these are mainly
# supporting refugees outside donor countrise via orgs such as the red cross or
# IRC. So, where idrc==1 & hum==1, hum will override

crs$hum_idrc = "neither"
crs$hum_idrc[crs$idrc==1] = "idrc"
crs$hum_idrc[crs$hum==1] = "hum"

crshum = as.data.frame(crs %>% filter(ODA==1) %>% group_by(year,hum_idrc,ukraine) %>% 
	summarize(usd=sum(usd_disbursement_defl,na.rm=T)))

crshum$ukraine[crshum$ukraine==1] = "UA"
crshum$ukraine[crshum$ukraine==0] = "nonUA"
crshum = crshum %>% pivot_wider(names_from=c(hum_idrc,ukraine),values_from=usd,values_fill=0)
crshum = crshum %>% mutate(total = rowSums(across(-year)))
crshum = crshum %>% mutate(total_nonUA = rowSums(across(c(hum_nonUA,idrc_nonUA,neither_nonUA))))

crshum = left_join(crshum,cpa,by="year")
crshum$hum  = with(crshum,hum_UA + hum_nonUA)
crshum$idrc = with(crshum,idrc_UA + idrc_nonUA)
crshum$UA   = with(crshum,hum_UA + idrc_UA + neither_UA)

crshum = crshum %>% select(year,total,cpa,hum,idrc,UA,total_nonUA,cpa_nonUA,hum_nonUA,idrc_nonUA,
	cpa_UA,hum_UA,idrc_UA)

##---------------------------------------------------------------------------------
## TOTAL HUMANITARIAN SPLIT (box 2.2)

as.data.frame(crs %>% filter(ODA==1) %>% group_by(year,hum) %>% summarize(usd=sum(Rnetoda)) %>% 
	pivot_wider(names_from=hum,values_from=usd) %>% mutate(pct=`1`*100/(`1`+`0`)))

##---------------------------------------------------------------------------------

## TOTAL OOF/ODA FROM DAC/MULTIS (box 2.2)

# (some overlap between UA and IDRC in previous years but doesn't matter 
# for point in text, so arbitrarily prioritising ukraine)

crs$ua_idrc = "neither"
crs$ua_idrc[crs$idrc==1]    = "idrc"
crs$ua_idrc[crs$ukraine==1] = "ukraine"

as.data.frame(crs %>% filter(type %in% c("dac","multi")) %>% group_by(year,ua_idrc) %>% 
	summarize(usd=sum(Rnetoda)) %>% 
	pivot_wider(names_from=ua_idrc,values_from=usd)) %>% select(year,neither,idrc,ukraine)


##---------------------------------------------------------------------------------

## FLOWCODE BREAKDOWN BY INCOME GROUP (box 2.4)


incf = as.data.frame(crs %>% filter(!is.na(wbinc)) %>% 
	group_by(wbinc,flow_code,year) %>% summarize(usd=sum(usd_disbursement)) %>%
	pivot_wider(names_from=flow_code,values_from=usd,values_fill=0)) %>% 
	mutate(pct=(`13`+`14`)*100/(`13`+`14`+`11`+`30`))

incf %>% filter(year>2011) %>%ggplot(aes(year,pct,color=wbinc))+ geom_line()


test = as.data.frame(crs %>% filter(year==2022 & group=="LDCs") %>% 
	group_by(recipient_name,wbinc) %>% 
	summarize(usd=sum(usd_disbursement*(flow_code==11))/sum(usd_disbursement)) %>% 
	pivot_wider(names_from=wbinc,values_from=usd,values_fill=0)) 

as.data.frame(crs %>% filter(year==2022 & group=="LDCs") %>% 
	group_by(recipient_name,wbinc) %>% 
	summarize(usd=sum(usd_disbursement)) %>% 
	pivot_wider(names_from=wbinc,values_from=usd,values_fill=0)) %>% arrange(desc(LM))





