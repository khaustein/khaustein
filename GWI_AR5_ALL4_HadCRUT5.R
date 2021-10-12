
#--------------------------------------------------------------------------------------------
# START (LOAD PACKAGES)
#--------------------------------------------------------------------------------------------

library("zoo")
library("MASS")
library("RColorBrewer")
library("RNetCDF")
library("ncdf4")
library("png")
library("RCurl")
library("stringr")
library("caTools")
library("plotrix")
library("scales")

#--------------------------------------------------------------------------------------------
# SELECT OPTIONS
#--------------------------------------------------------------------------------------------

# obsrvtn = "full" # GISS/BEST/CW = full Arctic coverage
obsrvtn = "all" # GISS/CW/HadCRUT/NOAA = all four default data set

month = "Dec"
endcount = 1692 # END POINT GISTEMP (add plus 1 for each extra month)
filenameadd = "DEC20_allcov_noerr_GMST_gif" # DEC20_erradj_GMST_gif
CURRENTDATE = "DEC20_allcov_noerr_GMST"     # DEC20_erradj_GMST

errbars = 1
allplot  = 1
save = 1

eend =  20      # 20 / 20 for default update (response model range)
ivend = 20      # 20 / 50 for full update (internal variability)
crend = 40      # 40 / 100 for full update (CRU4 ensemble member)
fcend = 80      # 80 / 200 for full update (Piers forcing uncertainty)

relwarm="1850_1900" # BASELINE TIME PERIOD WARMING IS RELATIVE TO

#--------------------------------------------------------------------------------------------
# READING OBSERVATIONAL AND RAW FORCING DATA
#--------------------------------------------------------------------------------------------

relwarm2 = paste(substr(relwarm,1,4),"-",substr(relwarm,6,9),sep="")

# READ HADCRUT5 MEAN, UPPER AND LOWER BOUND

allcov19 = open.nc("HadCRUT5_anom_1961_1990_fldmean.nc")
time39=var.get.nc(allcov19,"time") ; gmst39=var.get.nc(allcov19,"tas") ; close.nc(allcov19)
allcov20 = open.nc("HadCRUT5_lower_1961_1990_fldmean.nc") ; lower39 = var.get.nc(allcov20,"tas") ; close.nc(allcov20)
allcov21 = open.nc("HadCRUT5_upper_1961_1990_fldmean.nc") ; upper39 = var.get.nc(allcov21,"tas") ; close.nc(allcov21)
mydata19 = data.frame(time39,gmst39,lower39,upper39)

# READ COWTAN/WAY AND CONCATENATE HADSST4 VERSION WITH LATEST MONTHLY UPDATES

mydata101 = read.table("had4_krig_v2_0_0.txt") ; llhad4 = length(mydata101[,1])
mydata10 = read.table("had4sst4_krig_v2_0_0.txt")
diff_had4 = round(mean(mydata10[2017:2028,2]),3) - round(mean(mydata101[2017:2028,2]),3)
mydata10[2029:llhad4,1] = mydata101[2029:llhad4,1]
mydata10[2029:llhad4,2] = mydata101[2029:llhad4,2] + diff_had4

# READ GISS, BERKELEY, NOAA

tmp11 = open.nc("GISS_ERSSTv5_anom_1961_1990_fldmean.nc")
time11=var.get.nc(tmp11,"time") ; gmst11 = var.get.nc(tmp11,"tempanomaly")/100
mydata11 = data.frame(time11,gmst11) ; close.nc(tmp11)
tmp12 = open.nc("Berkeley_TPMN_anom_1961_1990_fldmean.nc")
time12 = var.get.nc(tmp12,"time") ; gmst12 = var.get.nc(tmp12,"temperature")
mydata12 = data.frame(time12,gmst12) ; close.nc(tmp12)
tmp13 = open.nc("NOAA_MLOST_anom_1961_1990_fldmean.nc")
time13 = var.get.nc(tmp13,"time") ; gmst13 = var.get.nc(tmp13,"anom")
mydata13 = data.frame(time13,gmst13) ; close.nc(tmp13)
mthbest = rep(1:12,200)

#--------------------------------------------------------------------------------------------

# READ VOLCANIC FORCING DATA AND OBSERVATIONAL STANDARD DEVIATION

mydata2 = read.table("Volcanic_forcing_monthly_GISS.txt")
allcov3 = open.nc("HadCRUT5_ensstd_anom_1961_1990_fldmean.nc")
time33=var.get.nc(allcov3,"time") ; gmst33=var.get.nc(allcov3,"tas")
mydata3 = data.frame(time33,gmst33) ; close.nc(allcov3)

# READ AR5 (DEFAULT) OR PRELIMINARY AR6 FORCING DATA (COMMENTED)

mydata6 = read.table("AR5Forcing_ext2015_GHGrevised.txt",skip=4)
# mydata6 = read.table("AR6_ERF_SSP245_ChrisSmith_updated.txt",skip=1) # Chris

# READ FORCING UNCERTAINTY FOR TOTAL, ANTHRO AND NATURAL FORCING CONTRIBUTIONS

mydata7=0 ; mydata8=0 ; mydata9=0
mydata7 = read.csv("rf_total_1750_2016.csv",header=FALSE,sep=",")
mydata8 = read.csv("rf_anthro_1750_2016.csv",header=FALSE,sep=",")
mydata9 = read.csv("rf_nat_1750_2016.csv",header=FALSE,sep=",")

#--------------------------------------------------------------------------------------------
# MONTHLY GMST OBSERVATIONS
#--------------------------------------------------------------------------------------------

# NAME OF DATASET AS FUNCTION OF WHICH DATASET IS SELECTED (ALL_FOUR and FULL_COVERAGE below)

if (obsrvtn=="cru") {  mydata1 = mydata19 ; obsname="HadCRUT5" }
if (obsrvtn=="cw")  {  mydata1 = mydata10 ; obsname="HadCRUT4-CW" }
if (obsrvtn=="giss") { mydata1 = mydata11 ; obsname="NASA/GISS" }
if (obsrvtn=="best") { mydata1 = mydata12 ; obsname="Berkeley" }
if (obsrvtn=="noaa") { mydata1 = mydata13 ; obsname="NOAA/MLOST" }

#--------------------------------------------------------------------------------------------
# DEFAULT (STANDARD) COMPOSITE WITH MONTHLY DATA (HADCRUT5)
#--------------------------------------------------------------------------------------------

if (obsrvtn=="all") { mydata99 = 0 ; lgth=length(mydata19[,1]) ; mydata99[1:360] <- rowMeans(cbind(mydata19[1:360,2],mydata10[1:360,2]))
mydata99[361:lgth] <- rowMeans(cbind(mydata19[361:lgth,2],mydata10[361:lgth,2],mydata11[1:endcount,2],mydata13[1:endcount,2])) ; obsname="ALL_FOUR"
mydata1 = data.frame(mydata19[,1],mydata99) 

LEER = vector(mode='numeric',lgth)

temp_mthly = data.frame(round(mydata101[,1],3),round(mydata19[,2],3),round(mydata10[,2],3),round(mydata99,3),LEER)
colnames(temp_mthly) <- c("date","HadCRUT5","Cowtan/Way","ALLFOUR","empty")

write.table(temp_mthly,file = paste("AWI_",obsrvtn,"_observations.csv",sep="")) }

#--------------------------------------------------------------------------------------------
# EXPERIMENTAL OBSERVATIONAL COMPOSITES (currently all commented)
#--------------------------------------------------------------------------------------------

# MONTHLY DATA BUT ONLY OBSERVATIONS WITH FULL ARCTIC COVERAGE

# if (obsrvtn=="full") { mydata99 = 0 ; lgth=length(mydata19[,1]) ; mydata99[1:360] <- rowMeans(cbind(mydata12[1:360,2],mydata10[1:360,2]))
# mydata99[361:lgth] <- rowMeans(cbind(mydata12[361:lgth,2],mydata10[361:lgth,2],mydata11[1:endcount,2])) ; obsname="FULL_COVERAGE"
# mydata1 = data.frame(mydata19[,1],mydata99) }

# MONTHLY TEMPERATURE TIMESERIES AS PROPOSED FOR AR6 (from Joeri Rogelj) ... PLUS MANUALLY SETTING THE REFERENCE PERIOD (1961-1990)

# if (obsrvtn=="all") { 
# mydata_ar6 = read.csv("AR6_FGD_timeseries_GMST_GSAT.csv",header=TRUE,sep=",")
# end_ar6 = length(mydata_ar6[,1]) ; pp=12*end_ar6 ; mydata_ar6[(end_ar6+1),] = mydata_ar6[end_ar6,]

# HAD5_clim = mean(as.numeric(mydata_ar6[112:141,2])) 
# NOAA_clim = mean(as.numeric(mydata_ar6[112:141,3]))
# BEST_clim = mean(as.numeric(mydata_ar6[112:141,4]))
# CWAY_clim = mean(as.numeric(mydata_ar6[112:141,5]))
# KADW_clim = mean(as.numeric(mydata_ar6[112:141,6]))
# GISS_clim = mean(as.numeric(mydata_ar6[112:141,8]))

# pp=pp+12 ; ab=5 ; cd=13-ab
# tmp <- approx(mydata_ar6[,2], y = NULL, method = "linear",n = pp) ; HAD5_new = round(tmp$y[ab:(length(tmp$y)-cd)]-HAD5_clim,4) # HadCRUT5
# tmp <- approx(mydata_ar6[,3], y = NULL, method = "linear",n = pp) ; NOAA_new = round(tmp$y[ab:(length(tmp$y)-cd)]-NOAA_clim,4) # NOAA
# tmp <- approx(mydata_ar6[,4], y = NULL, method = "linear",n = pp) ; BEST_new = round(tmp$y[ab:(length(tmp$y)-cd)]-BEST_clim,4) # Berkeley
# tmp <- approx(mydata_ar6[,5], y = NULL, method = "linear",n = pp) ; CWAY_new = round(tmp$y[ab:(length(tmp$y)-cd)]-CWAY_clim,4) # Cowtan/Way
# tmp <- approx(mydata_ar6[,6], y = NULL, method = "linear",n = pp) ; KADW_new = round(tmp$y[ab:(length(tmp$y)-cd)]-KADW_clim,4) # Kadow
# tmp <- approx(mydata_ar6[,8], y = NULL, method = "linear",n = pp) ; GISS_new = round(tmp$y[ab:(length(tmp$y)-cd)]-GISS_clim,4) # GISTEMP

# DEFAULT COMPOSITE BUT WITH ANNUAL DATA (UPDATED WITH HADCRUT5) ... TESTING ONLY!

# mydata99 = 0 ; lgth=length(mydata19[,1]) ; mydata99[1:360] <- rowMeans(cbind(HAD5_new[1:360],CWAY_new[1:360]))
# mydata99[361:lgth] <- rowMeans(cbind(HAD5_new[361:lgth],CWAY_new[361:lgth],NOAA_new[361:lgth],GISS_new[361:lgth])) ; obsname="ALL_FOUR"

# ABOVE AR6 COMPOSITE BUT WITH ANNUAL DATA ... TESTING ONLY AS WELL!

# mydata99 = 0 ; lgth=length(mydata19[,1]) ; mydata99 <- rowMeans(cbind(HAD5_new,NOAA_new,BEST_new,KADW_new)) ; obsname="ALL_FOUR"
# mydata1 = data.frame(mydata19[,1],mydata99)

#--------------------------------------------------------------------------------------------

}

#--------------------------------------------------------------------------------------------
# DATE CONVERSION TO HAVE CONSISTENT MONTHLY INTERVALS IN ALL TIME SERIES
#--------------------------------------------------------------------------------------------

CRU4_YEAR = as.numeric(substr(mydata1[,1],1,4))
if (obsrvtn=="all") { CRU4_MNTH = (as.numeric(substr(mydata1[,1],5,6)) - 1) / 12 + 0.042 }
if (obsrvtn=="full") { CRU4_MNTH = (as.numeric(substr(mydata1[,1],5,6)) - 1) / 12 + 0.042 }
if (obsrvtn=="cru") { CRU4_MNTH = (as.numeric(substr(mydata1[,1],5,6)) - 1) / 12 + 0.042 }
if (obsrvtn=="cw") { CRU4_MNTH = (as.numeric(paste("0.",substr(mydata1[,1],6,8),sep="")) - 0.0) }
if (obsrvtn=="giss") { CRU4_MNTH = (as.numeric(substr(mydata1[,1],5,6)) - 1) / 12 + 0.042 }
if (obsrvtn=="best") { CRU4_MNTH = (as.numeric(mthbest[1:length(mydata1[,2])] - 1)) / 12 + 0.042 }
if (obsrvtn=="noaa") { CRU4_MNTH = (as.numeric(substr(mydata1[,1],5,6)) - 1) / 12 + 0.042 }
CRU4_DATE = CRU4_YEAR + CRU4_MNTH
mth_curr = CRU4_DATE[length(CRU4_DATE)]

yrone=as.numeric(substr(relwarm,1,4)) ; yrend=as.numeric(substr(relwarm,6,9))
for (n in 1:length(CRU4_DATE)) { if (CRU4_DATE[n] > yrone) { start_cru4=n ; break }}
for (n in 1:length(CRU4_DATE)) { if (CRU4_DATE[n] > yrend) { stop1_cru4=n ; break }}
for (n in 1:length(CRU4_DATE)) { if (CRU4_DATE[n] >= mth_curr) { stop2_cru4=n ; break }}
nameadd = paste(yrone,"_",yrend,"_",filenameadd,sep="")

CRU4_TRAW_ALL = round(as.numeric(mydata1[,2]),4)

CRU4_TIME = CRU4_DATE[start_cru4:stop2_cru4]
CRU4_CLIM = mean(as.numeric(mydata1[start_cru4:stop1_cru4,2]))
CRU4_ANOM = round(as.numeric(mydata1[start_cru4:stop2_cru4,2]) - CRU4_CLIM,4)
CRU4_TRAW = as.numeric(mydata1[start_cru4:stop2_cru4,2])
cru4_mthly = data.frame(CRU4_DATE[start_cru4:stop2_cru4],CRU4_ANOM)
colnames(cru4_mthly) <- c("year","T_anom")

#--------------------------------------------------------------------------------------------

# CALCULATE DIFFERENCE BETWEEN CRU4 AND COMPOSITE MEAN (REQUIRED FOR UNCERTAINTY ESTIMATE)

cru_clim = mean(as.numeric(mydata19[start_cru4:stop1_cru4,2]))
cru_anom = round(as.numeric(mydata19[start_cru4:stop2_cru4,2]) - cru_clim,4)
cru_traw = as.numeric(mydata19[start_cru4:stop2_cru4,2])
diff_CRU4 = CRU4_ANOM - cru_anom

#--------------------------------------------------------------------------------------------
# OBSERVATIONAL UNCERTAINTY
#--------------------------------------------------------------------------------------------

jj = length(CRU4_DATE)
CRU4_TRAW_ENS <- matrix(data=0,nrow=jj,ncol=crend)

for (c in 1:crend) { v=c+100 ; allcov0 = open.nc(paste("HadCRUT5_ensemble_",v,".nc",sep=""))
time09=var.get.nc(allcov0,"time") ; gmst09=var.get.nc(allcov0,"tas") ; close.nc(allcov0)
mydata0 = data.frame(time09,gmst09) ; mydata0[1:jj,2] = mydata0[1:jj,2] + diff_CRU4[1:jj]
CRU4_TRAW_ENS[1:jj,c] = as.numeric(mydata0[1:jj,2]) }

CRU4_TRAW_SD = vector(mode='numeric',length=jj)
CRU4_TRAW_UPP = vector(mode='numeric',length=jj)
CRU4_TRAW_LOW = vector(mode='numeric',length=jj)

if ((obsrvtn=="cru") || (obsrvtn=="cw") || (obsrvtn=="all") || (obsrvtn=="full")) {

if (crend == 1) { for (d in 1:jj) { CRU4_TRAW_SD[d] = round(sd(CRU4_TRAW_ENS[d,1:crend]),4) } } else {
for (d in 1:jj) { CRU4_TRAW_SD[d] = round(sd(CRU4_TRAW_ENS[d,1:crend]),4)
                  CRU4_TRAW_UPP[d] = round(quantile(CRU4_TRAW_ENS[d,1:crend],c(0.975)),3)
                  CRU4_TRAW_LOW[d] = round(quantile(CRU4_TRAW_ENS[d,1:crend],c(0.025)),3) } }

CRU4_NEW = data.frame(CRU4_DATE,CRU4_TRAW_ALL,CRU4_TRAW_UPP,CRU4_TRAW_LOW)
colnames(CRU4_NEW) <- c("date","Tmean","Tupp","Tlow") }

#--------------------------------------------------------------------------------------------
# OBSERVATIONAL STANDARD DEVIATION
#--------------------------------------------------------------------------------------------

ll = length(CRU4_DATE[start_cru4:stop2_cru4]) ; CRU4_AN_ENS <- matrix(data=0,nrow=ll,ncol=crend)

if (crend == 1) { allcov0 = open.nc("HadCRUT5_anom_1961_1990_fldmean.nc")
time09=var.get.nc(allcov0,"time") ; gmst09=var.get.nc(allcov0,"tas") ; close.nc(allcov0) ; mydata0 = data.frame(time09,gmst09)
CRU4_CLIM_ENS = CRU4_TRAW - as.numeric(mydata1[start_cru4:stop2_cru4,2])
CRU4_AN_ENS[1:ll,1] = round(CRU4_ANOM - CRU4_CLIM_ENS,4) } else {

for (c in 1:crend) { v=c+100 ; allcov0 = open.nc(paste("HadCRUT5_ensemble_",v,".nc",sep=""))
time09=var.get.nc(allcov0,"time") ; gmst09=var.get.nc(allcov0,"tas") ; close.nc(allcov0) ; mydata0 = data.frame(time09,gmst09)
CRU4_CLIM_ENS = cru_traw - as.numeric(mydata0[start_cru4:stop2_cru4,2])
CRU4_AN_ENS[1:ll,c] = round(cru_anom - CRU4_CLIM_ENS + diff_CRU4,4) } }

CRU_UPPER = round(as.numeric(mydata19[start_cru4:stop2_cru4,4]) - cru_clim + diff_CRU4,4)
CRU_LOWER = round(as.numeric(mydata19[start_cru4:stop2_cru4,3]) - cru_clim + diff_CRU4,4)
CRU4_ANOM_SDENS = vector(mode='numeric',length=ll)

for (d in 1:ll) {
CRU4_ANOM_SDENS[d] = round(sd(CRU4_AN_ENS[d,1:crend]),4) }                       # SD ENSEMBLE ONLY
CRU4_ANOM_SDCOR = round(as.numeric(mydata3[start_cru4:stop2_cru4,2]),4)          # SD ADDITIONAL CORR
CRU4_ANOM_SDALL = round(sqrt(1.96*CRU4_ANOM_SDENS^2 + 1.96*CRU4_ANOM_SDCOR^2),4) # SD ENSEMBLE + ADD CORR
CRU4_ANOM_SDTOT = round((CRU_UPPER - CRU_LOWER)/3.92,4)                          # SD TOTAL (INCLUDING UNCORR)

#--------------------------------------------------------------------------------------------
# SAVE UNCERTAINTY DATA
#--------------------------------------------------------------------------------------------

CRU4_SDTOT = data.frame(CRU4_DATE,CRU4_TRAW_ALL,CRU4_TRAW_UPP,CRU4_TRAW_LOW)
colnames(CRU4_NEW) <- c("date","Tmean","Tupp","Tlow")

#--------------------------------------------------------------------------------------------
# HadCRUT5 UNCERTAINTY CONTRIBUTIONS
#--------------------------------------------------------------------------------------------

if (crend > 1) {
CRU4_AL_ENS <- matrix(data=0,nrow=ll,ncol=crend) ; CRU4_SD_ENS <- matrix(data=0,nrow=ll,ncol=crend)
for (d in 1:ll) { CRU4_AL_ENS[d,1:40] = round(sort(rnorm(crend, mean = CRU4_ANOM[d], sd = CRU4_ANOM_SDALL[d])),4)
                  CRU4_SD_ENS[d,1:40] = round(sort(rnorm(crend, mean = CRU4_ANOM[d], sd = CRU4_ANOM_SDTOT[d])),4) } }

# UPPER AND LOWER LIMIT (TWO SIGMA ENVELOPE)

CRU4_AN_UPP = CRU4_ANOM + 1.96*CRU4_ANOM_SDENS
CRU4_AN_LOW = CRU4_ANOM - 1.96*CRU4_ANOM_SDENS
if (crend > 1) {
CRU4_AL_UPP = CRU4_ANOM + 1.96*CRU4_ANOM_SDALL 
CRU4_AL_LOW = CRU4_ANOM - 1.96*CRU4_ANOM_SDALL
CRU4_SD_UPP = CRU4_ANOM + 1.96*CRU4_ANOM_SDTOT
CRU4_SD_LOW = CRU4_ANOM - 1.96*CRU4_ANOM_SDTOT }
if (crend == 1) { CRU4_AL_UPP = CRU4_ANOM ; CRU4_AL_LOW = CRU4_ANOM ; CRU4_SD_UPP = CRU4_ANOM ; CRU4_SD_LOW = CRU4_ANOM }

#--------------------------------------------------------------------------------------------
# INTERNAL VARIABILITY
#--------------------------------------------------------------------------------------------

CMIP5_IV_ENS <- matrix(data = 0,nrow = ll,ncol = ivend)

# LOOP OVER 52 CMIP5 PRE-INDUSTRIAL CONTROL RUN ENSEMBLE MEMBER (LENGTH OF TIME SLICES ACCORDING TO OBSERVATIONS)

for (e in 1:ivend) { mydata4 = open.nc(paste("CMIP5_IV_",e,".nc",sep=""))

CMIP5_TIME <- var.get.nc(mydata4,"time")
CMIP5_ANOM <- var.get.nc(mydata4,"tas")
close.nc(mydata4)
year4 = as.numeric(substr(CMIP5_TIME,1,4))
mth4 = (as.numeric(substr(CMIP5_TIME,5,6)) - 1) / 12
month4 = as.numeric(substr(CMIP5_TIME,5,6))
CMIP5_DATE = year4 + mth4

if (ivend == 1) { CMIP5_IV_ENS[1:ll,1] = 0 } else {
CMIP5_IV_ENS[1:ll,e] = round(as.numeric(CMIP5_ANOM[1:ll]),6) } }

CRU4_AN_ENS_IV <- matrix(data=0,nrow=ll,ncol=crend*ivend) ; civ = 0
for (c in 1:crend) { for (e in 1:ivend) { civ = civ + 1
CRU4_AN_ENS_IV[1:ll,civ] = CRU4_AN_ENS[1:ll,c] + CMIP5_IV_ENS[1:ll,e] }}

CRU4_ANOM_SDENS_IV = vector(mode='numeric',length=ll)
for (d in 1:ll) { CRU4_ANOM_SDENS_IV[d] = round(sd(CRU4_AN_ENS_IV[d,1:crend*ivend]),4) }

#--------------------------------------------------------------------------------------------
# MONTHLY VOLCANIC GISS FORCING: 01/1850 - 12/2020
#--------------------------------------------------------------------------------------------

# SINCE FORCING DATA ARE ONLY ANNUAL, MONTHLY GISS FORCING IS USED FOR VOLCANOES DUE TO SHORT RESPONSE TIME

GISS_YEAR = as.numeric(substr(mydata2[,1],1,4))
GISS_MNTH = (as.numeric(substr(mydata2[,1],6,7)) - 1) / 12 + 0.042
GISS_DATE = GISS_YEAR + GISS_MNTH
GISS_VOLC = as.numeric(mydata2[,2])
giss_mthly = data.frame(GISS_DATE,GISS_VOLC)
colnames(giss_mthly) <- c("year","F_vol")

#--------------------------------------------------------------------------------------------
# FORCING CONTRIBUTIONS (LINEAR INTERPOLATION FROM ANNUAL TO MONTHLY): 01/1750 - 12/2020
#--------------------------------------------------------------------------------------------

end=length(mydata6[,1]) ; nn=12*end # number of months
FORC_YEAR = as.numeric(mydata6[,1])

temp_totl = as.numeric(mydata6[,14]) # TOTAL
temp_antr = as.numeric(mydata6[,2]+mydata6[,3]+mydata6[,4]+mydata6[,5]+mydata6[,7]+mydata6[,8]+mydata6[,9]+mydata6[,10]+mydata6[,11]) # ANTHRO

temp_solr = as.numeric(mydata6[,12]) # SOLAR
temp_volc = as.numeric(mydata6[,13]) # VOLCANIC (annual)
bias_volc = as.numeric(mydata6[,13]) # VOLCANIC (annual) ... for difference estimate
temp_co2 = as.numeric(mydata6[,2]) # CO2
temp_oth = as.numeric(mydata6[,3]) # OTHER GHG FORCING
temp_aero = as.numeric(mydata6[,7]) # AEROSOLS
temp_o3T = as.numeric(mydata6[,4]) # TROPOSPHERIC OZONE
temp_o3S = as.numeric(mydata6[,5]) # STRATOSPHERIC OZONE
temp_luc = as.numeric(mydata6[,8]) # LAND USE CHANGE

temp_svap = as.numeric(mydata6[,9]) # STRATOSPHERIC WATER VAPOUR
temp_bcsn = as.numeric(mydata6[,10]) # BLACK CARBON ON SNOW
temp_cont = as.numeric(mydata6[,11]) # CONTRAILS

#--------------------------------------------------------------------------------------------
# Preliminary Chris Smith AR6 Foring (FOR TESTING ONLY AT THE MOMENT) # COMMENTED
#--------------------------------------------------------------------------------------------

AER = 1.0
# temp_totl = as.numeric(mydata6[,18])
# temp_antr = as.numeric(mydata6[,2]+mydata6[,3]+mydata6[,4]+mydata6[,5]+mydata6[,6]+mydata6[,7]+mydata6[,8]+mydata6[,9]*AER+mydata6[,10]*AER+mydata6[,11]+mydata6[,12]) # ANT

#temp_solr = as.numeric(mydata6[,15])
#temp_volc = as.numeric(mydata6[,14])
#bias_volc = as.numeric(mydata6[,14])
#temp_co2 = as.numeric(mydata6[,2])
#temp_oth = as.numeric(mydata6[,3]+mydata6[,4]+mydata6[,5])
#temp_aero = as.numeric(mydata6[,10]+mydata6[,11])*AER
#temp_o3T = as.numeric(mydata6[,6])
#temp_o3S = as.numeric(mydata6[,7])
#temp_luc = as.numeric(mydata6[,13])

#temp_svap = as.numeric(mydata6[,8])
#temp_bcsn = as.numeric(mydata6[,12])
#temp_cont = as.numeric(mydata6[,9])

#--------------------------------------------------------------------------------------------
# DEFINE ARRAYS FOR FORCING CONTRIBUTIONS
#--------------------------------------------------------------------------------------------

FORC_DATE = vector(mode='numeric',nn) ; FORC_DATE[1] = FORC_YEAR[1] + 0.042

for (yes in 2:nn) { FORC_DATE[yes] = FORC_DATE[yes-1] + (1/12) }
FORC_ANTR <- approx (temp_antr, y = NULL, method = "linear", n = nn)
FORC_SOLR <- approx (temp_solr, y = NULL, method = "linear", n = nn)
FORC_VOLC <- approx (temp_volc, y = NULL, method = "linear", n = nn)
DIFF_VOLC <- approx (bias_volc, y = NULL, method = "linear", n = nn)
FORC_CO2  <- approx (temp_co2 , y = NULL, method = "linear", n = nn)
FORC_OTH  <- approx (temp_oth , y = NULL, method = "linear", n = nn)
FORC_AERO <- approx (temp_aero, y = NULL, method = "linear", n = nn)
FORC_O3T  <- approx (temp_o3T , y = NULL, method = "linear", n = nn)
FORC_O3S  <- approx (temp_o3S , y = NULL, method = "linear", n = nn)
FORC_LUC  <- approx (temp_luc , y = NULL, method = "linear", n = nn)

FORC_SVAP  <- approx (temp_svap , y = NULL, method = "linear", n = nn)
FORC_BCSN  <- approx (temp_bcsn , y = NULL, method = "linear", n = nn)
FORC_CONT  <- approx (temp_cont , y = NULL, method = "linear", n = nn)

for (m in 1:length(FORC_DATE)) { if (FORC_DATE[m] >= 1850) { strt_forc=m ; break } }
for (m in 1:length(FORC_DATE)) { if (FORC_DATE[m] >= 2008) { stop_delta=m ; break } }
for (m in 1:length(FORC_DATE)) { if (FORC_DATE[m] > mth_curr-0.001) { fhalt=m ; break } }
for (m in 1:length(FORC_DATE)) { if (FORC_DATE[m] >= 2020) { stop_forc=m ; break } }
for (n in 1:length(GISS_DATE)) { if (GISS_DATE[n] > 1850) { strt_giss=n ; break } }
for (n in 1:length(GISS_DATE)) { if (GISS_DATE[n] > mth_curr-0.001) { ghalt=n ; break } }
for (n in 1:length(GISS_DATE)) { if (GISS_DATE[n] >= 2020) { stop_giss=n ; break } }
for (m in 1:length(FORC_DATE)) { if (FORC_DATE[m] > yrone) { strt_tmp=m ; break } }

CRU4_TMP = vector(mode='numeric',fhalt) ; CRU4_TMP[strt_tmp:fhalt] = CRU4_ANOM[1:length(CRU4_ANOM)]
FILL = vector(mode='numeric',fhalt)

#--------------------------------------------------------------------------------------------

# CHANGE VOLCANIC FORCING TO MONTHLY GISS FORCING

FORC_VOLC$y[strt_forc:stop_forc] = GISS_VOLC[strt_giss:stop_giss] 

#--------------------------------------------------------------------------------------------
# SAVE AND WRITE THE FORCING DATA
#--------------------------------------------------------------------------------------------

FORC_NATL = FORC_VOLC$y + FORC_SOLR$y
FORC_TOTL = FORC_ANTR$y + FORC_VOLC$y + FORC_SOLR$y

forc_mthly = data.frame(round(FORC_DATE[1:fhalt],3),CRU4_TMP,round(FORC_TOTL[1:fhalt],4)
,round(FORC_ANTR$y[1:fhalt],4),round(FORC_NATL[1:fhalt],4),round(FORC_CO2$y[1:fhalt],4),round(FORC_OTH$y[1:fhalt],4)
,round(FORC_AERO$y[1:fhalt],4),round(FORC_O3T$y[1:fhalt],4),round(FORC_O3S$y[1:fhalt],4),round(FORC_LUC$y[1:fhalt],4)
,round(FORC_SVAP$y[1:fhalt],4),round(FORC_BCSN$y[1:fhalt],4),round(FORC_CONT$y[1:fhalt],4)
,round(FORC_SOLR$y[1:fhalt],4),round(FORC_VOLC$y[1:fhalt],4),FILL)
colnames(forc_mthly) <- c("date","observations","F_total","F_anthro","F_nat","F_co2","F_wmghg","F_aero","F_o3Tr","F_o3St"
,"F_luc","F_stratvap","F_bc_snow","F_contr","F_solar","F_volc","empty")

write.table(forc_mthly,file = paste("AWI_",obsrvtn,"_forcing_CH4updated.csv",sep=""))

# DIFFERENCE BETWEEN INTERPOLATED AR5 VOLCANIC FORCING AND GISS

DIFF_NATL = 0 ; DIFF_NATL[1:nn] = 0
DIFF_NATL[(strt_forc):(stop_forc)] = DIFF_VOLC$y[strt_forc:stop_forc] - GISS_VOLC[strt_giss:stop_giss]

#--------------------------------------------------------------------------------------------
# FORCING UNCERTAINTY: 1750 - 2015
#--------------------------------------------------------------------------------------------

# CURRENT VERSION OF AR5 FORCING DATA ENDS IN 2015, THEREFORE LINEAR EXTRAPOLATION THEREAFTER

dF_total = as.numeric(mydata7[267,1:fcend] - mydata7[266,1:fcend])
dF_anthro= as.numeric(mydata8[267,1:fcend] - mydata8[266,1:fcend])
mydata7[268,1:fcend] = mydata7[267,1:fcend] + dF_total
mydata8[268,1:fcend] = mydata8[267,1:fcend] + dF_anthro
mydata9[268,1:fcend] = mydata9[267,1:fcend]
mydata7[269,1:fcend] = mydata7[268,1:fcend] + dF_total
mydata8[269,1:fcend] = mydata8[268,1:fcend] + dF_anthro
mydata9[269,1:fcend] = mydata9[268,1:fcend]
mydata7[270,1:fcend] = mydata7[269,1:fcend] + dF_total
mydata8[270,1:fcend] = mydata8[269,1:fcend] + dF_anthro
mydata9[270,1:fcend] = mydata9[269,1:fcend]
mydata7[271,1:fcend] = mydata7[270,1:fcend] + dF_total
mydata8[271,1:fcend] = mydata8[270,1:fcend] + dF_anthro
mydata9[271,1:fcend] = mydata9[270,1:fcend]

mean_unc = 0 ; for (x in 1:271) { mean_unc[x] = round(mydata9[x,mean(1:fcend)],4) } 
delta_unc = temp_volc - mean_unc

# ADD UNCERTAINTY TO NATURAL FORCING ENSEMBLE DURING 2008-2020 PERIOD (DUE TO UNDERESTIMATED ENSEMBLE SPREAD)

for (z in 1:fcend) { mydata9[259:271,z] = mydata9[259:271,z] + delta_unc[259:271] }

UNC_FORC_TOTL <- matrix(data = 0,nrow = nn,ncol = fcend)
UNC_FORC_ANTR <- matrix(data = 0,nrow = nn,ncol = fcend)
UNC_FORC_NATL <- matrix(data = 0,nrow = nn,ncol = fcend)
UNC_FORC_NAT1 <- matrix(data = 0,nrow = nn,ncol = fcend)
UNC_FORC_NAT2 <- matrix(data = 0,nrow = nn,ncol = fcend)

if(fcend == 1) {
UNC_FORC_TOTL[,1] = FORC_TOTL ; UNC_FORC_ANTR[,1] = FORC_ANTR$y ; UNC_FORC_NATL[,1] = FORC_NATL
} else {
for (z in 1:fcend) { tmp1 <- approx (mydata7[,z], y = NULL, method = "linear", n = nn) ; UNC_FORC_TOTL[1:nn,z] <- round(tmp1$y,5)
                     tmp2 <- approx (mydata8[,z], y = NULL, method = "linear", n = nn) ; UNC_FORC_ANTR[1:nn,z] <- round(tmp2$y,5)
                     tmp3 <- approx (mydata9[,z], y = NULL, method = "linear", n = nn) ; UNC_FORC_NAT1[1:nn,z] <- round(tmp3$y,5)
                     tmp4 <- tmp3$y - DIFF_NATL ; UNC_FORC_NAT2[1:nn,z] <- round(tmp4,5) } # ADJUST ANNUAL UNCERTAINTY SHAPE TO MATCH GISS

UNC_FORC_NATL = UNC_FORC_NAT1*0.6 + UNC_FORC_NAT2*0.6 }

#--------------------------------------------------------------------------------------------
# VECTOR SIZE ESTIMATE
#--------------------------------------------------------------------------------------------

if ((obsrvtn=="cru") || (obsrvtn=="all") || (obsrvtn=="full") || (obsrvtn=="cw") || (obsrvtn=="best") ) { time = forc_mthly[,1]
for (n in 1:length(time)) { if (time[n] > yrone) { start_antr=n ; break }}
for (n in 1:length(time)) { if (time[n] > yrend) { stop1_antr=n ; break }}
for (n in 1:length(time)) { if (time[n] >= mth_curr-0.002) { stop2_antr=n ; break }}
matl = (stop2_antr)-start_antr+1
vecl = length(time) }

if ((obsrvtn=="giss") || (obsrvtn=="noaa")) { time = forc_mthly[,1]
for (n in 1:length(time)) { if (time[n] > yrone) { start_antr=n ; break }}
for (n in 1:length(time)) { if (time[n] > yrend) { stop1_antr=n ; break }}
for (n in 1:length(time)) { if (time[n] >= mth_curr-0.002) { stop2_antr=n ; break }}
matl = (stop2_antr)-start_antr+1
vecl = length(time) }

#--------------------------------------------------------------------------------------------
# FORCING UNCERTAINTY ENSEMBLE
#--------------------------------------------------------------------------------------------

Fant_clim = mean(as.numeric(forc_mthly[start_antr:stop1_antr,4]))
Fant_anom = round(as.numeric(forc_mthly[start_antr:(stop2_antr),4]) - Fant_clim,4)
Fnat_clim = mean(as.numeric(forc_mthly[start_antr:stop1_antr,5]))
Fnat_anom = round(as.numeric(forc_mthly[start_antr:(stop2_antr),5]) - Fnat_clim,4)

UNC_FORC_TOTL_ANOM <- matrix(data=0,nrow=matl,ncol=fcend)
UNC_FORC_ANTR_ANOM <- matrix(data=0,nrow=matl,ncol=fcend)
UNC_FORC_NATL_ANOM <- matrix(data=0,nrow=matl,ncol=fcend)

for (z in 1:fcend) {

UTOT_CLIM_2 = mean(as.numeric(UNC_FORC_TOTL[start_antr:stop1_antr,z]))
UNC_FORC_TOTL_ANOM[1:matl,z] = round(as.numeric(UNC_FORC_TOTL[start_antr:(stop2_antr),z]) - UTOT_CLIM_2,4)
UANT_CLIM_2 = mean(as.numeric(UNC_FORC_ANTR[start_antr:stop1_antr,z]))
UNC_FORC_ANTR_ANOM[1:matl,z] = round(as.numeric(UNC_FORC_ANTR[start_antr:(stop2_antr),z]) - UANT_CLIM_2,4)
UNAT_CLIM_2 = mean(as.numeric(UNC_FORC_NATL[start_antr:stop1_antr,z]))
UNC_FORC_NATL_ANOM[1:matl,z] = round(as.numeric(UNC_FORC_NATL[start_antr:(stop2_antr),z]) - UNAT_CLIM_2,4) }

#--------------------------------------------------------------------------------------------
# CONVERT DECAY CONSTANTS TO FRACTION DECLINE OVER TIME
#--------------------------------------------------------------------------------------------

ecstcr = c(2.8,1.6)
para = c(4,220,5.35)
dt = forc_mthly[2,1] - forc_mthly[1,1]

a0 = vector(mode='numeric',length=4)
b0 <- matrix(data=1,nrow=2,ncol=2)

# SET PARAMETERS FOR THE RESPONSE MODEL (SEE HAUSTEIN ET AL. 2017 FOR DETAILS)

b0[2,1:2] = (1.-(1.-exp(-70./para[1:2]))/(70./para[1:2]))
a0[1:2] = solve(b0)%*%ecstcr/(para[3]*log(2))
a0[3:4]=1.0-(exp(-dt/para[1:2]))

#--------------------------------------------------------------------------------------------
# INTEGRATE TEMPERATURE COMPONENTS (ANTHRO, NATURAL, TOTAL)
#--------------------------------------------------------------------------------------------

TIME_FIT = time[start_antr:(stop2_antr)]

Tcomp1 <- matrix(data=0,nrow=2,ncol=vecl)
Tcomp2 <- matrix(data=0,nrow=2,ncol=vecl) ; Tant_fast = vector(mode='numeric',length=matl) ; Tant_slow = vector(mode='numeric',length=matl)
Tcomp3 <- matrix(data=0,nrow=2,ncol=vecl) ; Tnat_fast = vector(mode='numeric',length=matl) ; Tnat_slow = vector(mode='numeric',length=matl)
T_tot = vector(mode='numeric',length=vecl) ; TTOT_ANOM = vector(mode='numeric',length=matl)
T_ant = vector(mode='numeric',length=vecl) ; TACT_ANOM = vector(mode='numeric',length=matl)
T_nat = vector(mode='numeric',length=vecl) ; TNAT_ANOM = vector(mode='numeric',length=matl)

# CALCULATE THE SLOW AND FAST RESPONSE FOR EACH TIME STEP (AND THE THREE COMPONENTS)

for (i in 1:(vecl-1)) {

Tcomp1[1:2,i+1] = Tcomp1[1:2,i] + 0.5 * (forc_mthly[i+1,3]+forc_mthly[i,3]) * a0[1:2] * a0[3:4] - Tcomp1[1:2,i] * a0[3:4]
Tcomp2[1:2,i+1] = Tcomp2[1:2,i] + 0.5 * (forc_mthly[i+1,4]+forc_mthly[i,4]) * a0[1:2] * a0[3:4] - Tcomp2[1:2,i] * a0[3:4]
Tcomp3[1:2,i+1] = Tcomp3[1:2,i] + 0.5 * (forc_mthly[i+1,5]+forc_mthly[i,5]) * a0[1:2] * a0[3:4] - Tcomp3[1:2,i] * a0[3:4]
T_tot[i+1] = round(sum(Tcomp1[1:2,i+1]),4)
T_ant[i+1] = round(sum(Tcomp2[1:2,i+1]),4)
T_nat[i+1] = round(sum(Tcomp3[1:2,i+1]),4) }

Tcom_clim = mean(as.numeric(Tcomp2[1,start_antr:stop1_antr]))
Tant_fast = round(as.numeric(Tcomp2[1,start_antr:(stop2_antr)]) - Tcom_clim,4)
Tcom_clim = mean(as.numeric(Tcomp2[2,start_antr:stop1_antr]))
Tant_slow = round(as.numeric(Tcomp2[2,start_antr:(stop2_antr)]) - Tcom_clim,4)
Tcom_clim = mean(as.numeric(Tcomp3[1,start_antr:stop1_antr]))
Tnat_fast = round(as.numeric(Tcomp3[1,start_antr:(stop2_antr)]) - Tcom_clim,4)
Tcom_clim = mean(as.numeric(Tcomp3[2,start_antr:stop1_antr]))
Tnat_slow = round(as.numeric(Tcomp3[2,start_antr:(stop2_antr)]) - Tcom_clim,4)

TTOT_CLIM = mean(as.numeric(T_tot[start_antr:stop1_antr]))
TTOT_ANOM = round(as.numeric(T_tot[start_antr:(stop2_antr)]) - TTOT_CLIM,4)
TANT_CLIM = mean(as.numeric(T_ant[start_antr:stop1_antr]))
TANT_ANOM = round(as.numeric(T_ant[start_antr:(stop2_antr)]) - TANT_CLIM,4)
TNAT_CLIM = mean(as.numeric(T_nat[start_antr:stop1_antr]))
TNAT_ANOM = round(as.numeric(T_nat[start_antr:(stop2_antr)]) - TNAT_CLIM,4)

regr_mthly = data.frame(TIME_FIT,CRU4_ANOM,CRU_UPPER,CRU_LOWER,TANT_ANOM,TNAT_ANOM)
colnames(regr_mthly) <- c("Time","Tcru","Tupp","Tlow","Tant","Tnat")

#--------------------------------------------------------------------------------------------
# LINEAR REGRESSION OF RESPONSE MODEL RESULTS WITH OBSERVATIONS
#--------------------------------------------------------------------------------------------

REGR_ANTR = lm(Tcru ~ Tant, data = regr_mthly)
REGR_NATL = lm(Tcru ~ Tnat, data = regr_mthly)
SLOPE_ANTR = coef(REGR_ANTR)[[2]] ; SLOPE_ANTR_BEST = SLOPE_ANTR
SLOPE_NATL = coef(REGR_NATL)[[2]]

# THE SLOPE IS THE SCALING FACTOR FOR EACH COMPONENT

T_ANT_FIT = round(SLOPE_ANTR * TANT_ANOM,4)
T_NAT_FIT = round(SLOPE_NATL * TNAT_ANOM,4)
T_TOT_FIT = round(T_ANT_FIT + T_NAT_FIT,4)

if(save == 1 ) { save.image(file=paste("AWI_",obsrvtn,"_IMAGE_",relwarm,"_",CURRENTDATE,".RData",sep="")) }
if(save == 1 ) { save(TIME_FIT,T_ANT_FIT,T_NAT_FIT,T_TOT_FIT,file=paste("AWI_",obsrvtn,"_Tmean_",relwarm,"_",CURRENTDATE,".RData",sep="")) }

#--------------------------------------------------------------------------------------------
# LOOP OVER ECS/TCR RATIO AND TIME CONSTANT UNCERTAINTY 
#--------------------------------------------------------------------------------------------

if(errbars == 1 ) { creg = eend * crend * fcend * ivend

#--------------------------------------------------------------------------------------------

U_ANT_FIT = matrix(data=0,nrow=matl,ncol=creg)
U_NAT_FIT = matrix(data=0,nrow=matl,ncol=creg)
U_TOT_FIT = matrix(data=0,nrow=matl,ncol=creg)

U_ANT_SLOPE = vector(mode='numeric',length=creg)

tcnt = 0
cnt = 0
ecs = c(2.8,2.0,2.0,2.0,2.0,2.4,2.4,2.4,2.4,2.8,2.8,2.8,3.2,3.2,3.2,3.2,3.6,3.6,3.6,3.6)
short=c(4,2.5,4,6.5,9.5,2.5,4,6.5,9.5,2.5,6.5,9.5,2.5,4,6.5,9.5,2.5,4,6.5,9.5)

#--------------------------------------------------------------------------------------------

for (e in 1:eend) { print(paste("ECS =",ecs[e]," Time1 =",short[e]),sep="")

#--------------------------------------------------------------------------------------------

ecstcr = c(ecs[e],1.6)
para = c(short[e],220,5.35)
dt = forc_mthly[2,1] - forc_mthly[1,1]

a1 = vector(mode='numeric',length=4)
b1 <- matrix(data=1,nrow=2,ncol=2)

b1[2,1:2] = (1.-(1.-exp(-70./para[1:2]))/(70./para[1:2]))
a1[1:2] = solve(b1)%*%ecstcr/(para[3]*log(2))
a1[3:4]=1.0-(exp(-dt/para[1:2]))

#--------------------------------------------------------------------------------------------
# INTEGRATING UNCERTAINTIES (SAME AS BEFORE BUT FOR EACH UNCERTAINTY ENSEMBLE MEMBER)
#--------------------------------------------------------------------------------------------

Ucomp1 <- matrix(data=0,nrow=2,ncol=vecl)
Ucomp2 <- matrix(data=0,nrow=2,ncol=vecl) ; Uant_fast = matrix(data=0,nrow=matl,ncol=fcend) ; Uant_slow = matrix(data=0,nrow=matl,ncol=fcend)
Ucomp3 <- matrix(data=0,nrow=2,ncol=vecl) ; Unat_fast = matrix(data=0,nrow=matl,ncol=fcend) ; Unat_slow = matrix(data=0,nrow=matl,ncol=fcend)
U_tot <- matrix(data=0,nrow=nn,ncol=fcend) ; UTOT_ANOM = matrix(data=0,nrow=matl,ncol=fcend)
U_ant <- matrix(data=0,nrow=nn,ncol=fcend) ; UANT_ANOM = matrix(data=0,nrow=matl,ncol=fcend)
U_nat <- matrix(data=0,nrow=nn,ncol=fcend) ; UNAT_ANOM = matrix(data=0,nrow=matl,ncol=fcend)

for (z in 1:fcend) { 
for (i in 1:(vecl-1)) {

Ucomp1[1:2,i+1] = Ucomp1[1:2,i] + 0.5 * (UNC_FORC_TOTL[i+1,z]+UNC_FORC_TOTL[i,z]) * a1[1:2] * a1[3:4] - Ucomp1[1:2,i] * a1[3:4]
Ucomp2[1:2,i+1] = Ucomp2[1:2,i] + 0.5 * (UNC_FORC_ANTR[i+1,z]+UNC_FORC_ANTR[i,z]) * a1[1:2] * a1[3:4] - Ucomp2[1:2,i] * a1[3:4]
Ucomp3[1:2,i+1] = Ucomp3[1:2,i] + 0.5 * (UNC_FORC_NATL[i+1,z]+UNC_FORC_NATL[i,z]) * a1[1:2] * a1[3:4] - Ucomp3[1:2,i] * a1[3:4]
U_tot[i+1,z] = round(sum(Ucomp1[1:2,i+1]),4)
U_ant[i+1,z] = round(sum(Ucomp2[1:2,i+1]),4)
U_nat[i+1,z] = round(sum(Ucomp3[1:2,i+1]),4) }

Ucom_clim = mean(as.numeric(Ucomp2[1,start_antr:stop1_antr]))
Uant_fast[1:matl,z] = round(as.numeric(Ucomp2[1,start_antr:(stop2_antr)]) - Ucom_clim,4)
Ucom_clim = mean(as.numeric(Ucomp2[2,start_antr:stop1_antr]))
Uant_slow[1:matl,z] = round(as.numeric(Ucomp2[2,start_antr:(stop2_antr)]) - Ucom_clim,4)
Ucom_clim = mean(as.numeric(Ucomp3[1,start_antr:stop1_antr]))
Unat_fast[1:matl,z] = round(as.numeric(Ucomp3[1,start_antr:(stop2_antr)]) - Ucom_clim,4)
Ucom_clim = mean(as.numeric(Ucomp3[2,start_antr:stop1_antr]))
Unat_slow[1:matl,z] = round(as.numeric(Ucomp3[2,start_antr:(stop2_antr)]) - Ucom_clim,4)

UTOT_CLIM = mean(as.numeric(U_tot[start_antr:stop1_antr,z]))
UTOT_ANOM[1:matl,z] = round(as.numeric(U_tot[start_antr:(stop2_antr),z]) - UTOT_CLIM,4)
UANT_CLIM = mean(as.numeric(U_ant[start_antr:stop1_antr,z]))
UANT_ANOM[1:matl,z] = round(as.numeric(U_ant[start_antr:(stop2_antr),z]) - UANT_CLIM,4)
UNAT_CLIM = mean(as.numeric(U_nat[start_antr:stop1_antr,z]))
UNAT_ANOM[1:matl,z] = round(as.numeric(U_nat[start_antr:(stop2_antr),z]) - UNAT_CLIM,4) }

if (e == 1) {
Uant_fast_1 = matrix(data=0,nrow=matl,ncol=fcend) ; Uant_slow_1 = matrix(data=0,nrow=matl,ncol=fcend)
Unat_fast_1 = matrix(data=0,nrow=matl,ncol=fcend) ; Unat_slow_1 = matrix(data=0,nrow=matl,ncol=fcend)
UANT_ANOM_1 = matrix(data=0,nrow=matl,ncol=fcend) ; UNAT_ANOM_1 = matrix(data=0,nrow=matl,ncol=fcend)
Uant_fast_1 = Uant_fast ; Uant_slow_1 = Uant_slow ; Unat_fast_1 = Unat_fast
Unat_slow_1 = Unat_slow ; UANT_ANOM_1 = UANT_ANOM ; UNAT_ANOM_1 = UNAT_ANOM }

#--------------------------------------------------------------------------------------------
# REGRESSION OF FORCING UNCERTAINTY AND OBSERVATIONS (AGAIN, SAME AS BEFORE)
#--------------------------------------------------------------------------------------------

cru4iv = crend * ivend

for (c in 1:cru4iv) { print(cnt) ; fcnt = 0 ; tcnt = tcnt + 1
for (z in 1:fcend) { cnt = cnt + 1 ; fcnt = fcnt + 1

regr_mthly2 = data.frame(CRU4_AN_ENS_IV[,c],UANT_ANOM[,z],UNAT_ANOM[,z])
colnames(regr_mthly2) <- c("Tcru","Tant","Tnat")

REGR_ANTR = lm(Tcru ~ Tant, data = regr_mthly2)
REGR_NATL = lm(Tcru ~ Tnat, data = regr_mthly2)
SLOPE_ANTR = coef(REGR_ANTR)[[2]]
SLOPE_NATL = coef(REGR_NATL)[[2]]

U_ANT_FIT[,cnt] = round(SLOPE_ANTR * UANT_ANOM[,z],4)
U_NAT_FIT[,cnt] = round(SLOPE_NATL * UNAT_ANOM[,z],4)
U_TOT_FIT[,cnt] = round(sqrt((U_ANT_FIT[,cnt]^2) + (U_NAT_FIT[,cnt])^2),4)

U_ANT_SLOPE[cnt] = SLOPE_ANTR

#--------------------------------------------------------------------------------------------

}} # end regression (c) and (z)

#--------------------------------------------------------------------------------------------

} # end of loop over ecs range (e)

#--------------------------------------------------------------------------------------------
# SAVE DATA FILE AND GENERATE NETCDF FILE 
#--------------------------------------------------------------------------------------------

print(cnt)

if(save == 1 ) { save(TIME_FIT,U_ANT_FIT,U_NAT_FIT,U_TOT_FIT,file=paste("AWI_",obsrvtn,"_dTall_",relwarm,"_",CURRENTDATE,".RData",sep="")) 

#--------------------------------------------------------------------------------------------

if(eend > 1) {

U_ANT_FIT_short = U_ANT_FIT[2017:length(TIME_FIT),]
TIME_FIT_short = TIME_FIT[2017:length(TIME_FIT)]

tim = length(TIME_FIT_short) ; tunits3 <- "day as %Y%m%d.%f" ; ntim3 <- tim
sam = length(U_ANT_FIT_short[1,]) ; samplecount=(1:sam) ; sample3 <- sam

tmp_array3 <- array(U_ANT_FIT_short,dim=c(ntim3,sample3))
timedim <- ncdim_def("time",tunits3,as.double(TIME_FIT_short))
samdim <- ncdim_def("count","member",as.double(samplecount))

fillvalue <- 1e20
dlname <- "anthropogenic response"
tmp_def <- ncvar_def("Tmean","deg_C",list(timedim,samdim),fillvalue,dlname,prec="single")

ncfname <- paste("AWI_",obsrvtn,"_dTall_",relwarm,".nc",sep="")
ncout <- nc_create(ncfname,list(tmp_def),force_v4=T)
ncvar_put(ncout,tmp_def,tmp_array3)

ncatt_put(ncout,"count","axis","X")
ncatt_put(ncout,"time","axis","T")

nc_close(ncout) }

#--------------------------------------------------------------------------------------------

} # END SAVE LOOP

#--------------------------------------------------------------------------------------------

# save(TIME_FIT,U_ANT_FIT,U_NAT_FIT,U_TOT_FIT,file=paste("AWI_",obsrvtn,"_dTall_dForcing.RData",sep=""))
# save(TIME_FIT,U_ANT_FIT,U_NAT_FIT,U_TOT_FIT,file=paste("AWI_",obsrvtn,"_dTall_dObserved.RData",sep=""))
# save(TIME_FIT,U_ANT_FIT,U_NAT_FIT,U_TOT_FIT,file=paste("AWI_",obsrvtn,"_dTall_dInternal.RData",sep=""))
# save(TIME_FIT,U_ANT_FIT,U_NAT_FIT,U_TOT_FIT,file=paste("AWI_",obsrvtn,"_dTall_dResponse.RData",sep=""))

#--------------------------------------------------------------------------------------------
# SAVE OUTPUT OF TMEAN (LOWER AND UPPER BOUND)
#--------------------------------------------------------------------------------------------

U_TOT_QLOW = 0 ; for (y in 1:matl) { U_TOT_QLOW[y] = quantile(U_TOT_FIT[y,1:creg],c(0.05))}
U_TOT_QUPP = 0 ; for (y in 1:matl) { U_TOT_QUPP[y] = quantile(U_TOT_FIT[y,1:creg],c(0.95))}
U_ANT_QLOW = 0 ; for (y in 1:matl) { U_ANT_QLOW[y] = quantile(U_ANT_FIT[y,1:creg],c(0.05))}
U_ANT_QUPP = 0 ; for (y in 1:matl) { U_ANT_QUPP[y] = quantile(U_ANT_FIT[y,1:creg],c(0.95))}
U_NAT_QLOW = 0 ; for (y in 1:matl) { U_NAT_QLOW[y] = quantile(U_NAT_FIT[y,1:creg],c(0.05))}
U_NAT_QUPP = 0 ; for (y in 1:matl) { U_NAT_QUPP[y] = quantile(U_NAT_FIT[y,1:creg],c(0.95))}

U_ANT_SLOPE_UPP = quantile(U_ANT_SLOPE[1:creg],c(0.95))
U_ANT_SLOPE_LOW = quantile(U_ANT_SLOPE[1:creg],c(0.05))

if(save == 1 ) { save(U_TOT_QLOW,U_TOT_QUPP,U_ANT_QLOW,U_ANT_QUPP,U_NAT_QLOW,U_NAT_QUPP,UANT_ANOM_1,UNAT_ANOM_1
,Uant_fast_1,Uant_slow_1,Unat_fast_1,Unat_slow_1,file=paste("AWI_",obsrvtn,"_Quant_",relwarm,"_",CURRENTDATE,".RData",sep="")) }

out_ant = data.frame(TIME_FIT,round(U_ANT_QLOW,4),T_ANT_FIT,round(U_ANT_QUPP,4))
colnames(out_ant) <- c("date","lower","best","upper")
print(out_ant)

#--------------------------------------------------------------------------------------------

} # END MAIN LOOP (if errorbars are set to 1)

#--------------------------------------------------------------------------------------------
# SAVE TEXT FILE
#--------------------------------------------------------------------------------------------

x=data.frame(round(TIME_FIT,3),round(CRU4_ANOM,3),round(U_ANT_QLOW,3),round(T_ANT_FIT,3),round(U_ANT_QUPP,3),round(U_NAT_QLOW,3),round(T_NAT_FIT,3),round(U_NAT_QUPP,3),round(U_TOT_QLOW,3),round(T_TOT_FIT,3),round(U_TOT_QUPP,3))

colnames(x) <- c("DATE","Observations","T_ANT_05","T_ANT_FIT","T_ANT_95","T_NAT_05","T_NAT_FIT","T_NAT_95","T_TOT_05","T_TOT_FIT","T_TOT_95")
write.csv(x,file = paste("AWI_",obsrvtn,"_Tmean_",relwarm,"_Quant_",CURRENTDATE,".csv",sep=""))

#xxx=data.frame(TIME_FIT,T_ANT_FIT) ; annualyr <- as.numeric(substr(TIME_FIT,1,4))
#agg.annual <- aggregate(xxx,by=list(annualyr),FUN="mean",nfrequency = 12)
#y = data.frame(round(agg.annual[,2],0),round(agg.annual[,3],3)) ; colnames(y) <- c("YEAR","GWI")
#write.csv(y,file = paste("AWI_",obsrvtn,"_Tmean_",relwarm,"_ANNUAL_",CURRENTDATE,".csv",sep=""))

y = data.frame(floor(rollapply(TIME_FIT, width = 12, by = 12, FUN = mean, align = "left")),round(rollapply(T_ANT_FIT, width = 12, by = 12, FUN = mean, align = "left"),3))
colnames(y) <- c("YEAR","GWI") ; write.csv(y,file = paste("AWI_",obsrvtn,"_Tmean_",relwarm,"_ANNUAL_",CURRENTDATE,".csv",sep="")) 

#--------------------------------------------------------------------------------------------
# PRINT RELEVANT METRICS
#--------------------------------------------------------------------------------------------

# LOGO ENVIRONMENTAL CHANGE INSTITUTE UNIVERSITY OXFORD

eciurl<-"https://www.eci.ox.ac.uk/assets/img/eci-logo-colour.png"
eci_logo <-  readPNG(getURLContent(eciurl))

logoing_func<-function(logo, x, y, size){
dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
AR<-dims[1]/dims[2]
par(usr=c(0,1,0,1))
rasterImage(logo, x-(size/1.5), y-(AR*size/1), x+(size/1.5), y+(AR*size/1), interpolate=TRUE) }

#--------------------------------------------------------------------------------------------

# CALCULATE TCR AND TREND ESTIMATES AND PRINT ON SCREEN

T_ANT_FIT2 = T_ANT_FIT[1:(length(T_ANT_FIT)-0)]
current = T_ANT_FIT2[length(T_ANT_FIT2)]
trend = (T_ANT_FIT2[length(T_ANT_FIT2)] - T_ANT_FIT2[(length(T_ANT_FIT2)-120)]) / 1
trendpsec = trend/60/60/24/(365.25/12)/120
forcing = FORC_TOTL[fhalt]
tcr = current * 3.71 / forcing
fit <- lm((T_ANT_FIT[TIME_FIT>=1880&TIME_FIT<2013])~(TIME_FIT[TIME_FIT>=1880&TIME_FIT<2013]))
sr15warmg = round(mean(T_ANT_FIT[TIME_FIT>=2006&TIME_FIT<2016])-mean(T_ANT_FIT[TIME_FIT>=1850&TIME_FIT<1901]),3)
sr15trend = round(fit$coef[[2]]*length(TIME_FIT[TIME_FIT>=1880&TIME_FIT<2013])/120,3)

print(paste("trend per decade = ",trend,"K",sep=""))
print(paste("trend per second = ",trendpsec,"K",sep=""))
print(paste("last three months: ",T_ANT_FIT[(length(T_ANT_FIT)-2)],"K ",T_ANT_FIT[(length(T_ANT_FIT)-1)],"K ",T_ANT_FIT[length(T_ANT_FIT)],"K",sep=""))
print(paste("trend 1880 - 2012: ",sr15trend,"C/decade",sep=""))
print(paste("warming 1850-1900 vs 2006-2015: ",sr15warmg,"C",sep=""))
print(paste("Transient Climate Response = ",round(tcr,4),"K",sep=""))
print(paste("Scaling factor Anthro: ",round(SLOPE_ANTR_BEST,4),sep=""))
print(" ")

#--------------------------------------------------------------------------------------------
# FINAL PLOTTING
#--------------------------------------------------------------------------------------------

if (allplot == 1) {

#--------------------------------------------------------------------------------------------

ymin = -0.6 ; ymax = 1.4
ymin2 = -3.5 ; ymax2 = 3.5

dat1.axis = c("1860","1880","1900","1920","1940","1960","1980","2000","2020")
tmp1.axis = c("-0.6","-0.4","-0.2","0.0","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6","1.8","2.0")
dat2.axis = c("1860","1880","1900","1920","1940","1960","1980","2000","2020")
tmp2.axis = c("-3.5","-3.0","-2.5","-2.0","-1.5","-1.0","-0.5","0.0","0.5","1.0","1.5","2.0","2.5","3.0","3.5")

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"1_Tonly.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.6
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""),xaxt="n",yaxt="n")

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.8,col="black",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
polygon(c(CRU4_TIME,rev(CRU4_TIME)),c(CRU4_AL_LOW,rev(CRU4_AL_UPP)),col=alpha("grey30",0.7),border=NA)
polygon(c(CRU4_TIME,rev(CRU4_TIME)),c(CRU4_SD_LOW,rev(CRU4_SD_UPP)),col=alpha("grey50",0.5),border=NA)
par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.8,col="black",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)

lego = c(paste(obsname," monthly observations",sep=""),paste(obsname," correlated uncertainty (5-95%)",sep=""),paste(obsname," total uncertainty range (5-95%)",sep=""))
legcol = c("black","grey30","grey70") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"2_Fonly.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020),ylim=c(ymin2,ymax2),xlab=""
,ylab=bquote("Forcing relative to"~.(relwarm2)~"("~W/m^2~")"),xaxt="n",yaxt="n"
,cex.lab=1.5,cex.main=1.6,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""))

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-3.5","-3.0","-2.5","-2.0","-1.5","-1.0","-0.5","0.5","1.0","1.5","2.0","2.5","3.0","3.5"),col="grey30",lty=3,lwd=0.5)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

par(new=T) ; plot(TIME_FIT,Fant_anom,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin2,ymax2))
par(new=T) ; plot(TIME_FIT,Fnat_anom-0.2,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin2,ymax2))
for (z in 1:fcend) { par(new=T)
plot(TIME_FIT,UNC_FORC_ANTR_ANOM[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020)
,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin2,ymax2)) ; par(new=T)
plot(TIME_FIT,UNC_FORC_NATL_ANOM[,z]-0.2,type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020)
,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin2,ymax2)) }

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat2.axis, labels=dat2.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp2.axis, labels=tmp2.axis,tick = TRUE,cex.axis=1.4,las=1)

lego = c("","",paste("ΔF human-induced (AR5)",sep=""),paste("ΔF natural (AR5)",sep=""))
legcol = c("white","white","darkorange1","royalblue") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"3_Ttwobox.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.6
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""),xaxt="n",yaxt="n")

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.6,col="grey65",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,Tant_fast,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,Tant_slow,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,Tnat_fast,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,Tnat_slow,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

for (z in 1:fcend) { par(new=T)
plot(TIME_FIT,Uant_fast_1[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) ; par(new=T)
plot(TIME_FIT,Uant_slow_1[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) ; par(new=T)
plot(TIME_FIT,Unat_fast_1[,z],type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) ; par(new=T)
plot(TIME_FIT,Unat_slow_1[,z],type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) }

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)

lego = c(paste(obsname," monthly observations",sep=""),"",paste("ΔT human-induced (slow and fast)",sep=""),paste("ΔT natural (slow and fast)",sep=""))
legcol = c("grey65","white","darkorange1","royalblue") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"4_Tsumbox.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.6
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""),xaxt="n",yaxt="n")

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.6,col="grey65",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,TANT_ANOM,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,TNAT_ANOM,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
for (z in 1:fcend) { par(new=T)
plot(TIME_FIT,UANT_ANOM_1[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) ; par(new=T)
plot(TIME_FIT,UNAT_ANOM_1[,z],type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) }

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)

lego = c(paste(obsname," monthly observations",sep=""),"",paste("ΔT human-induced (before regression)",sep=""),paste("ΔT natural (before regression)",sep=""))
legcol = c("grey65","white","darkorange1","royalblue") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"5_TsumboxALL.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.6
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""),xaxt="n",yaxt="n")

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.6,col="grey65",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,TANT_ANOM,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,TNAT_ANOM,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
for (z in 1:fcend) { par(new=T)
plot(TIME_FIT,UANT_ANOM_1[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) ; par(new=T)
plot(TIME_FIT,UNAT_ANOM_1[,z],type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) }
par(new=T) ; plot(TIME_FIT,TTOT_ANOM,type="l",lty=1,lwd=1.5,col="red",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)

lego = c(paste(obsname," monthly observations",sep=""),paste("ΔT combined (before regression)",sep="")
,paste("ΔT human-induced (before regression)",sep=""),paste("ΔT natural (before regression)",sep=""))
legcol = c("grey65","red","darkorange1","royalblue") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

if (crend > 100) { # doesn't work too small sample size

#--------------------------------------------------------------------------------------------

uu=500
U_ANT_FIT_NEW <- U_ANT_FIT[,sample(1:ncol(U_ANT_FIT),uu,replace=FALSE)]
U_NAT_FIT_NEW <- U_NAT_FIT[,sample(1:ncol(U_NAT_FIT),uu,replace=FALSE)]

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"7_TregresALL.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="black",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.6
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep=""),xaxt="n",yaxt="n")

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.6,col="grey50",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,T_ANT_FIT,type="l",lty=1,lwd=2,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(TIME_FIT,T_NAT_FIT,type="l",lty=1,lwd=2,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
for (z in 1:uu) { par(new=T)
plot(TIME_FIT,U_NAT_FIT_NEW[,z],type="l",lty=1,lwd=0.1,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) }
for (z in 1:uu) { par(new=T)
plot(TIME_FIT,U_ANT_FIT_NEW[,z],type="l",lty=1,lwd=0.1,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax)) }
par(new=T) ; plot(TIME_FIT,T_TOT_FIT,type="l",lty=1,lwd=1.5,col="red",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

lego = c(paste(obsname," monthly observations",sep=""),paste("ΔT combined (subsample after regression)",sep="")
,paste("ΔT human-induced (subsample after regression)",sep=""),paste("ΔT natural (subsample after regression)",sep=""))
legcol = c("grey50","red","darkorange1","royalblue") ; legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

} # end case for limited sample size

#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"8_SD.png",sep=""),width=780,height=559,bg = "white",family="Helvetica")
par(mar=c(5,5,4,3))

if ((obsrvtn=="all") || (obsrvtn=="full")) {
plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="grey22",xlim=c(1860,2020),xaxt="n",yaxt="n"
,ylim=c(ymin,ymax),xlab="",ylab=paste("GMST Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.8
,main=paste("Global Warming Index (aggregate observations) - updated to ",month," 2020  ",sep=""))
} else {
plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="grey22",xlim=c(1860,2020),xaxt="n",yaxt="n"
,ylim=c(ymin,ymax),xlab="",ylab=paste("GMST Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=1.5,cex.main=1.8
,main=paste("Global Warming Index based on ",obsname," - updated to ",month," 2020",sep="")) }

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.6,col="grey22",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
polygon(c(CRU4_TIME,rev(CRU4_TIME)),c(CRU4_AN_LOW,rev(CRU4_AN_UPP)),col=alpha("black",0.1),border=NA)
par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="p",pch=19,cex=0.15,col="black",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

par(new=T)
plot(TIME_FIT,T_TOT_FIT,type="l",lty=1,lwd=2,col="red",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
if(errbars == 1 ) { polygon(c(TIME_FIT,rev(TIME_FIT)),c(U_TOT_QLOW,rev(U_TOT_QUPP)),col=alpha("red",0.0),border=NA) }
par(new=T)
plot(TIME_FIT,T_ANT_FIT,type="l",lty=1,lwd=4,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
if(errbars == 1 ) { polygon(c(TIME_FIT,rev(TIME_FIT)),c(U_ANT_QLOW,rev(U_ANT_QUPP)),col=alpha("darkorange1",0.2),border=NA) }
par(new=T)
plot(TIME_FIT,T_NAT_FIT,type="l",lty=1,lwd=3,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
if(errbars == 1 ) { polygon(c(TIME_FIT,rev(TIME_FIT)),c(U_NAT_QLOW,rev(U_NAT_QUPP)),col=alpha("royalblue",0.15),border=NA) }
par(new=T)

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=1.4)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=1.4,las=1)
text(1869,0.79,col="darkorange2",cex=1.5,pos=4,paste("GWI on 15 ",month," 2020: +",current,"°C",sep=""))
logoing_func(eci_logo, x=0.89, y=0.10, size=0.13)

if (obsrvtn=="all") { 
lego = c(paste("Monthly observations",sep=""),paste("Human-induced warming",sep=""),paste("Natural warming and cooling",sep=""),paste("Combined response",sep="")) }
if (obsrvtn=="full") { 
lego = c(paste("monthly observations (GISTEMP/Cru4CW/Berkeley)",sep=""),paste("ΔT combined",sep=""),paste("ΔT human-induced",sep=""),paste("ΔT natural",sep="")) }
legcol = c("grey20","darkorange1","royalblue","red")
legend("topleft",lwd=2,col=legcol,bty="n",cex=1.5,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.818,0.9025,0.96,1)
,col=c("black","red","black","grey80"),side=1,line=3.2,cex=1.1,font=2)
dev.off()

#--------------------------------------------------------------------------------------------
# LARGE PLOT
#--------------------------------------------------------------------------------------------

png(filename=paste("AWI_AR5_",obsrvtn,"_",nameadd,"8_SD_large.png",sep=""),width=1600,height=1140,bg = "white",family="Helvetica")
par(mar=c(8,9,5,5),mgp=c(6,2,0),las=1)

plot(CRU4_TIME,CRU4_ANOM,type="n",lty=1,lwd=0.6,col="grey22",xlim=c(1860,2020)
,ylim=c(ymin,ymax),xlab="",ylab=paste("GMST Warming relative to ",relwarm2," (°C)",sep=""),cex.lab=3.0,cex.main=3.6
,main=paste("Global Warming Index (aggregate observations) - updated to ",month," 2020 ",sep=""),xaxt="n",yaxt="n")

par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="l",lty=1,lwd=0.8,col=alpha("grey22",0.6),xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T) ; plot(CRU4_TIME,CRU4_ANOM,type="p",pch=19,cex=0.3,col="black",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))

par(new=T)
plot(TIME_FIT,T_TOT_FIT,type="l",lty=1,lwd=3,col="red",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
par(new=T)
plot(TIME_FIT,T_ANT_FIT,type="l",lty=1,lwd=5,col="darkorange1",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
if(errbars == 1 ) { polygon(c(TIME_FIT,rev(TIME_FIT)),c(U_ANT_QLOW,rev(U_ANT_QUPP)),col=alpha("darkorange1",0.2),border=NA) }
par(new=T)
plot(TIME_FIT,T_NAT_FIT,type="l",lty=1,lwd=4,col="royalblue",xlim=c(1860,2020),xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(ymin,ymax))
if(errbars == 1 ) { polygon(c(TIME_FIT,rev(TIME_FIT)),c(U_NAT_QLOW,rev(U_NAT_QUPP)),col=alpha("royalblue",0.15),border=NA) }
par(new=T)

abline(h = c("0.0"),col="grey60",lty=1,lwd=1)
abline(v = c("1860","1880","1900","1920","1940","1960","1980","2000","2020"),col="grey30",lty=3,lwd=0.5)
abline(h = c("-0.6","-0.4","-0.2","0.2","0.4","0.6","0.8","1.0","1.2","1.4","1.6"),col="grey30",lty=3,lwd=0.5)
axis(1,at=dat1.axis, labels=dat1.axis,tick = TRUE,cex.axis=3.0)
axis(2,at=tmp1.axis, labels=tmp1.axis,tick = TRUE,cex.axis=3.0,las=1)
text(1869,0.9,col="darkorange2",cex=3.0,pos=4,paste("GWI on 15 ",month," 2020: +",current,"°C",sep=""))
logoing_func(eci_logo, x=0.90, y=0.10, size=0.12)

if (obsrvtn=="all") {
lego = c(paste("Monthly observations",sep=""),paste("Human-induced warming",sep=""),paste("Natural warming and cooling",sep=""),paste("Combined response",sep="")) }
if (obsrvtn=="full") {
lego = c(paste("monthly observations (GISTEMP/Cru4CW/Berkeley)",sep=""),paste("ΔT combined",sep=""),paste("ΔT human-induced",sep=""),paste("ΔT natural",sep="")) }
legcol = c("grey20","darkorange1","royalblue","red")
legend("topleft",lwd=2,col=legcol,bty="n",cex=3.0,lego)

mtext(text=c("global","warming","index",".org"),adj=c(0.85,0.915,0.96,0.99)
,col=c("black","red","black","grey80"),side=1,line=5.5,cex=1.8,font=2)
dev.off()

#--------------------------------------------------------------------------------------------

} # END ALLPLOT

#--------------------------------------------------------------------------------------------
# END
#--------------------------------------------------------------------------------------------

