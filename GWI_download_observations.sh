
#!/bin/bash
#set -x

#---------------------------------------------------------------
# DOWNLOAD AND POSTPROCESS OBSERVATIONAL DATA
#---------------------------------------------------------------

#---------------------------------------------------------------
# HadCRUT4
#---------------------------------------------------------------

# REMOVE OLD FILES

rm "HadCRUT.4.6.0.0.monthly_ns_avg.txt"
rm "HadCRUT.4.6.0.0.monthly_ns_avg."?".txt"
rm "HadCRUT.4.6.0.0.monthly_ns_avg."??".txt"
rm "HadCRUT.4.6.0.0.monthly_ns_avg.100.txt"

# DOWNLOAD AND UNZIP LATEST MONTHLY UPDATED TEXT FILE

wget "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt"
wget "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg_realisations.zip"
unzip "HadCRUT.4.6.0.0.monthly_ns_avg_realisations.zip"

rm "HadCRUT.4.6.0.0.monthly_ns_avg_realisations.zip"

#---------------------------------------------------------------
# HadCRUT5
#---------------------------------------------------------------

# REMOVE OLD FILES

rm "HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.nc"
rm "HadCRUT.5.0.1.0.analysis.ensemble_series.global.monthly.nc"
rm "HadCRUT5_ensemble_"???".nc"
rm "HadCRUT5_ensstd_anom_1961_1990_fldmean.nc"

# DOWNLOAD AND UNZIP LATEST MONTHLY UPDATED TEXT FILE

wget "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.nc"
wget "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/HadCRUT.5.0.1.0.analysis.ensemble_series.global.monthly.nc"

# SELECT MEAN, UPPER AND LOWER BOUND

cdo -a -chname,tas_mean,tas -selvar,tas_mean "HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.nc" "HadCRUT5_anom_1961_1990_fldmean.nc"
cdo -a -chname,tas_lower,tas -selvar,tas_lower "HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.nc" "HadCRUT5_lower_1961_1990_fldmean.nc"
cdo -a -chname,tas_upper,tas -selvar,tas_upper "HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.nc" "HadCRUT5_upper_1961_1990_fldmean.nc"

# REARRANGE DIMENSIONS AND SPLIT ENSEMBLE INTO 100 INDIVIDUAL MEMBERS

ncpdq -a time,realization "HadCRUT.5.0.1.0.analysis.ensemble_series.global.monthly.nc" tmp.nc
cdo -a -selvar,tas tmp.nc "HadCRUT5_ensemble_anom_1961_1990_fldmean.nc" ; rm tmp.nc
cdo -splitlevel HadCRUT5_ensemble_anom_1961_1990_fldmean.nc test_ ; cdo -ensstd "test_000"???".nc" "HadCRUT5_ensstd_anom_1961_1990_fldmean.nc"
rename s/test_000/HadCRUT5_ensemble_/g "test_000"???".nc"

#---------------------------------------------------------------
# Cowtan/Way
#---------------------------------------------------------------

# 1st file contains the latest monthly update but is currently still based on HadSST3
# 2nd file contains the updated data with HadSST4 which currently ends in Dec 2018

#---------------------------------------------------------------

# REMOVE OLD AND DOWNLOAD NEW FILE(S)
# SOMETIMES THE UPDATE IS LATE IN WHICH CASE ONE MIGHT FILL THE MISSING MONTHS WITH ADJUSTED HadCRUT4 DATA 

rm "had4_krig_v2_0_0.txt"
wget "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4_krig_v2_0_0.txt" 
rm "had4sst4_krig_v2_0_0.txt"
wget "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4sst4_krig_v2_0_0.txt"

#---------------------------------------------------------------
# GISTEMP
#---------------------------------------------------------------

# REMOVE OLD AND DOWNLOAD AND UNZIP NEW FILE

rm "gistemp1200_GHCNv4_ERSSTv5.nc"
wget "https://data.giss.nasa.gov/pub/gistemp/gistemp1200_GHCNv4_ERSSTv5.nc.gz"
gunzip "gistemp1200_GHCNv4_ERSSTv5.nc.gz"

# CHANGE THE REFERENCE PERIOD FOR THE ANOMALIES TO 1961-1990

cdo -a -fldmean -ymonsub "gistemp1200_GHCNv4_ERSSTv5.nc" -ymonmean -seldate,1961-01-01,1990-12-31 "gistemp1200_GHCNv4_ERSSTv5.nc" "GISS_ERSSTv5_anom_1961_1990_fldmean.nc"

#---------------------------------------------------------------
# NOAA
#---------------------------------------------------------------

# THIS NEEDS TO BE DONE BEFORE THE SCRIPT CAN BE STARTED !!
# CHECK: https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v5/access/gridded
# COPY THE CRYPTIC FILENAME FOR LATEST MONTH FROM URL HERE:

url_path="https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v5/access/gridded/NOAAGlobalTemp_v5.0.0_gridded_s188001_e202101_c20210208T133312.nc"
filename=`echo "$url_path" | rev | cut -d \/ -f 1 | rev`

#---------------------------------------------------------------

REMOVE OLD AND DOWNLOAD NEW FILE

rm "NOAAGlobalTemp.gridded.nc"
wget $url_path ; mv $filename "NOAAGlobalTemp.gridded.nc"
# wget "ftp://ftp.cdc.noaa.gov/Datasets/noaaglobaltemp/air.mon.anom.nc"

# CHANGE THE REFERENCE PERIOD FOR THE ANOMALIES TO 1961-1990

cdo -a -fldmean -ymonsub "NOAAGlobalTemp.gridded.nc" -ymonmean -seldate,1961-01-01,1990-12-31 "NOAAGlobalTemp.gridded.nc" "NOAA_MLOST_anom_1961_1990_fldmean.nc"

#---------------------------------------------------------------

exit

#---------------------------------------------------------------
# BERKELEY EARTH (NOT INCLUDED AT THE MOMENT)
#---------------------------------------------------------------

rm "Land_and_Ocean_LatLong1.nc"
wget "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc"

cdo -selvar,temperature "Land_and_Ocean_LatLong1.nc" "Berkeley_global_TPMN_monthly.nc"
cdo -a -setcalendar,365days -setreftime,1850-01-16,00:00,1month "Berkeley_global_TPMN_monthly.nc" "Berkeley_global_TPMN_monthly_anom.nc" ; rm "Berkeley_global_TPMN_monthly.nc"

cdo -fldmean -ymonsub "Berkeley_global_TPMN_monthly_anom.nc" -ymonmean -seldate,1961-01-01,1990-12-31 "Berkeley_global_TPMN_monthly_anom.nc" "Berkeley_TPMN_anom_1961_1990_fldmean.nc"
cp "Berkeley_TPMN_anom_1961_1990_fldmean.nc" ..

#---------------------------------------------------------------

exit

#---------------------------------------------------------------
# END
#---------------------------------------------------------------

# ADDITIONAL LINES IN CASE IT IS INTENDED TO START THE MAIN PLOTTING SCRIPT AUTOMATICALLY (NOT RECOMMENDED)
# UN-COMMENT THE 'exit' command IF YOU WISH TO USE THESE LINES 

#MTHM1="Sep"
#MONTH="Oct"

#rm AWI_AR5_sample_gif_exe.R
#sed -e "s/INPUT/cru/g" \
#    -e "s/MONTH/$MONTH/g" > AWI_AR5_sample_gif_revised.R > AWI_AR5_sample_gif_exe.R

#Rscript AWI_AR5_sample_gif_exe.R 

#exit

#---------------------------------------------------------------


