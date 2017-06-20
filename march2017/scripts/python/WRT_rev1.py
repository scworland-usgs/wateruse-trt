## Jordan Wilson, 2017

### Importing required packages

import pandas as pd
import feather
from pyproj import Proj, transform
import calendar as cldr
from geopy.geocoders import Nominatim
import datetime
import numpy as np

pd.options.display.max_rows = 999
pd.options.display.max_columns = 999


# Importing feather datasets
FL_df = feather.read_dataframe("feather_files\FL_raw.feather")
VA_df = feather.read_dataframe("feather_files\VA_raw.feather")
PA_df = feather.read_dataframe("feather_files\PA_raw.feather")
OR_df = feather.read_dataframe("feather_files\OR_raw.feather")
OR_loc_df = feather.read_dataframe("feather_files\OR_locations_raw.feather")
NJ_df = feather.read_dataframe(r"feather_files\NJ_raw.feather")
NJ_loc_df = feather.read_dataframe(r"feather_files\NJ_locations_raw.feather")
MD_df = feather.read_dataframe("feather_files\MD_raw.feather")
ID_df = feather.read_dataframe("feather_files\ID_raw.feather")


# Importing data Scott Worland compiled for PA, VA, and FL 
PA_VA_FL_df = pd.read_csv("PA_VA_FL\public_supply_data_pa_va_fl_rev2.csv")


# Defining 'uid' column as a string to export to fether
PA_VA_FL_df["id"] = PA_VA_FL_df["id"].astype("str")


# Pulling out each state database to process separately
PVF_df = PA_VA_FL_df.rename(columns={'MG':'Mg', 'lon':'X_WGS','lat':'Y_WGS',' state':'State'})

FL_df = PVF_df[:].query('state == "FL"')
VA_df = PVF_df[:].query('state == "VA"')
PA_df = PVF_df[:].query('state == "PA"')


# Exporting data to feather files
feather.write_dataframe(FL_df,"feather_files\FL_raw.feather")
feather.write_dataframe(VA_df,"feather_files\VA_raw.feather")
feather.write_dataframe(PA_df,"feather_files\PA_raw.feather")


## FL
#Reading feather file
FL_df = feather.read_dataframe("feather_files\FL_raw.feather")


#Changing month column from string to integer
FL_df['month'] = FL_df["month"].astype("float")
FL_df['month'] = FL_df["month"].astype("int")

#creating date and Mgd columns
FL_df['date'] = ""
FL_df["Mgd"]=""

# extracting month and year for each row
FL_df['date'] = FL_df.apply(lambda x: pd.datetime.strptime("{0} {1}".format(x['month'],x['year']), "%m %Y"),axis=1)

#setting the dataframe index
FL_df.set_index("date",inplace=True)

renaming id 
FL_agg = FL_df.rename(columns={'MG': 'Mg'}, inplace=True)

#Summing up monthly entries to yearly entries
FL_agg=FL_df.groupby(by='id').resample('1AS').sum()

#Dropping unneeded columns
FL_agg.drop(['year','month','lat','lon'], inplace=True, axis=1)

#calculating the Mgd
FL_agg["Mgd"] = FL_agg["Mg"]/FL_agg["days"]

#Applying state name
FL_agg['State']="FL"

#resetting index to integer
FL_agg.reset_index(level=1, inplace=True)

#Pulling out location data to merge back to yearly data
FL_xy = FL_df[["id","lon","lat"]]
FL_xy.columns=['id','X_WGS','Y_WGS']
FL_xy.set_index('id',inplace=True)

#Joining location data with yearly data
FL_agg=FL_agg.join(FL_xy, how='inner')

#Grouping data by year and date 
FL_agg.reset_index(inplace=True)
FL_agg = FL_agg.groupby(['id','date']).first()
FL_agg.reset_index(inplace=True)

#Dropping numbers of days
FL_agg.drop('days',1,inplace=True)

#Exporting aggregated data to feather file
feather.write_dataframe(FL_df,r"feather_files\FL_agg.feather")


## VA
#Reading feather file
VA_df = feather.read_dataframe("feather_files\VA_raw.feather")

# #Calculating days in each year to get Mgd
VA_df['date'] = ""
VA_df["Mgd"]=""
VA_df["days"]=""
VA_df['date'] = VA_df.apply(lambda x: pd.datetime.strptime("{0}".format(x['year']), "%Y"),axis=1)
VA_df['days']=VA_df.apply(lambda row: cldr.monthrange(row["year"],1)[1]
                         +cldr.monthrange(row["year"],2)[1]
                          +cldr.monthrange(row["year"],3)[1]
                          +cldr.monthrange(row["year"],4)[1]
                          +cldr.monthrange(row["year"],5)[1]
                          +cldr.monthrange(row["year"],6)[1]
                          +cldr.monthrange(row["year"],7)[1]
                          +cldr.monthrange(row["year"],8)[1]
                          +cldr.monthrange(row["year"],9)[1]
                          +cldr.monthrange(row["year"],10)[1]
                          +cldr.monthrange(row["year"],11)[1]
                          +cldr.monthrange(row["year"],12)[1],axis=1)

VA_df.set_index("date",inplace=True)

#Renaming uid column to id
VA_df.rename(columns={'uid': 'id'}, inplace=True)

#Summing up monthly data to yearly data
VA_agg=VA_df.groupby(by='id').resample('1AS').sum()

#Dropping unneeded columns
VA_agg.drop(['year','month','lat','lon'], inplace=True, axis=1)

#Calculating Mgd
VA_agg["Mgd"] = VA_agg["Mg"]/VA_agg["days"]

#Adding state abbreviation
VA_agg['State']="VA"

#Resetting index to integers
VA_agg.reset_index(level=1, inplace=True)

#Pulling out location data to merge back to yearly data
VA_xy = VA_df[["id","lon","lat"]]
VA_xy.columns=['id','X_WGS','Y_WGS']
VA_xy.set_index('id',inplace=True)

#Joining location data with yearly data 
VA_agg=VA_agg.join(VA_xy, how='inner')

#Grouping data by year and date 
VA_agg.reset_index(inplace=True)
VA_agg = VA_agg.groupby(['id','date']).first()
VA_agg.reset_index(inplace=True)

#Dropping days column
VA_agg.drop('days',1,inplace=True)

#Exporting aggregated data to feather file
feather.write_dataframe(VA_df,"feather_files\VA_agg.feather")


## PA
#Importing feather file
PA_df = feather.read_dataframe("feather_files\PA_raw.feather")

#Calculating days in each year to get Mgd
PA_df['month'] = PA_df["month"].astype("float")
PA_df['date'] = ""
PA_df["Mgd"]=""
PA_df['month'] = PA_df["month"].astype("int")
PA_df['date'] = PA_df.apply(lambda x: pd.datetime.strptime("{0} {1}".format(x['month'],x['year']), "%m %Y"),axis=1)

# Summing each monthly to a yearly 
PA_df.set_index("date",inplace=True)
PA_df.rename(columns={'uid': 'id'}, inplace=True)
PA_agg=PA_df.groupby(by='id').resample('1AS').sum()

#Dropping unneeded columns
PA_agg.drop(['year','month','lat','lon'], inplace=True, axis=1)

#Calculating Mgd
PA_agg["Mgd"] = PA_agg["Mg"]/PA_agg["days"]

#Inserting stat abbreviation
PA_agg['State']="PA"

#Pulling out location data to merge back to yearly data
PA_agg.reset_index(level=1, inplace=True)
PA_xy = PA_df[["id","lon","lat"]]
PA_xy.columns=['id','X_WGS','Y_WGS']
PA_xy.set_index('id',inplace=True)

#Joining the location data with yearly data
PA_agg=PA_agg.join(PA_xy, how='inner')
PA_agg.reset_index(inplace=True)

#Grouping data by year and date 
PA_agg = PA_agg.groupby(['id','date']).first()
PA_agg.reset_index(inplace=True)
PA_agg.drop('days',1,inplace=True)

#Exporting aggregated data to feather file
feather.write_dataframe(PA_df,"feather_files\PA_agg.feather")

## OR
# Importing water-use data from xlsx file
OR_df = pd.read_excel("OR\OWRD_PS_water_use.xlsx")

# Importing location data from xlsx file
OR_loc_df = pd.read_excel(r"OR\OR_locations_merged.xlsx")

# Exporting raw data to feather files
feather.write_dataframe(OR_df,"feather_files\OR_raw.feather")
feather.write_dataframe(OR_loc_df,"feather_files\OR_locations_raw.feather")

# Importing feather files of raw data
OR_df = feather.read_dataframe('feather_files\OR_raw.feather')
OR_loc_df = feather.read_dataframe('feather_files\OR_locations_raw.feather')

#Setting up empty columns 
OR_df['month']=""
OR_df['year']=""
OR_df['date'] = ""
OR_df['days'] = ""
OR_df["Mgd"]=""
OR_df["Mg"]=""

OR_source_df = OR_df[['wur_report_id','water source']]
OR_source_df = OR_source_df.rename(columns={'wur_report_id':'id','water source':'source'})

OR_source_df = OR_source_df.groupby('id').first()
OR_source_df.reset_index(inplace=True)

#Converting from water month to calander month
def wtr_month_to_month(row):
    if row['water_month'] >3:
        return row['water_month']-3
    else:
        return row['water_month'] + 9
OR_df.month = OR_df.apply(lambda row: wtr_month_to_month(row), axis=1)

#Converting from water year to calander year
def wtr_yr_to_yr(row):
    if row['water_month'] <4:
        return row['water_year']-1
    else:
        return row['water_year']
OR_df.year = OR_df.apply(lambda row: wtr_yr_to_yr(row), axis=1)

#Calculating the date field for each year and month combo
OR_df['date'] = OR_df.apply(lambda x: pd.datetime.strptime("{0} {1}".format(x['month'],x['year']), "%m %Y"),axis=1)

#Calculating the number of days in each month
OR_df["days"] = OR_df.apply(lambda row: cldr.monthrange(row["year"],row["month"])[1],axis=1)

#Summing monthly data to yearly data by grouped id
OR_df.set_index("date",inplace=True)
OR_agg=OR_df.groupby(by='wur_report_id').resample('1AS').sum()

#Converting units of water use in acre feet to Mg
OR_agg["Mg"] = OR_agg["water_used (acre feet)"]*325851/1000000

#calculating Mgd based on number of days in year
OR_agg["Mgd"] = OR_agg["Mg"]/OR_agg["days"]

#Dropping unneeded columns
OR_agg.drop(['month','year','water_month','water_year','wur_report_id','water_used (acre feet)'],1,inplace=True)

#Setting the stat abbreviation
OR_agg['state']="OR"

#Extracting and joining the location data with yearly data
OR_agg.index.names=['id','date']
OR_loc_df = OR_loc_df.rename(columns={'wur_report_id':'id'})
OR_loc_df = OR_loc_df[['id','X_WGS','Y_WGS']]
OR_agg.reset_index(inplace=True)
OR_loc_df.reset_index(inplace=True)

OR_agg = OR_agg.merge(OR_loc_df, on='id', how='left')

OR_agg = OR_agg.merge(OR_source_df, on='id',how='left')

OR_agg.drop(['index','days'],1,inplace=True)
OR_agg.drop('Mgd',1,inplace=True)
OR_agg.rename(columns={'date': 'year'}, inplace=True)
OR_agg['year'] = OR_agg['year'].dt.strftime('%Y')

#Exporting aggregated data to feather file
feather.write_dataframe(OR_agg,"feather_files\OR_agg.feather")

## NJ
# Importing water-use data from xlsx file
NJ_df = pd.read_excel("NJ\Withdrawals by HUC.xlsx")

# Importing location data from xlsx file
NJ_loc_df = pd.read_excel(r"NJ\NJ_PS_location_data.xlsx")

# Exporting feather files
feather.write_dataframe(NJ_df,r"feather_files\NJ_raw.feather")
feather.write_dataframe(NJ_loc_df,r"feather_files\NJ_locations_raw.feather")

#Importing feather files
NJ_df = feather.read_dataframe(r"feather_files\NJ_raw.feather")
NJ_loc_df = feather.read_dataframe(r"feather_files\NJ_locations_raw.feather")

#Calculating date field and number of days for each month
NJ_df['month']=""
NJ_df['year']=""
NJ_df['date'] = ""
NJ_df['days'] = ""
NJ_df["Mgd"]=""
NJ_df['date'] = NJ_df.apply(lambda x: pd.datetime.strptime("{0} {1}".format(x['Month'],x['Year']), "%m %Y"),axis=1)
NJ_df["days"] = NJ_df.apply(lambda row: cldr.monthrange(row["Year"],row["Month"])[1],axis=1)

NJ_source_df = NJ_df[['SiteName','source','date']]
NJ_source_df = NJ_source_df.rename(columns={'SiteName':'id'})

#Summing all monthlys to yearlys
NJ_df.set_index('date', inplace=True)
NJ_agg=NJ_df.groupby(by='SiteName').resample('1AS').sum()

#Calculating Mgd
NJ_agg["Mgd"] = NJ_agg["WithdrawalMG"]/NJ_agg["days"]

#Dropping unneeded columns
NJ_agg.drop(['Month','Year','HUC14','days'],1,inplace=True)

#Setting state abbreviation
NJ_agg['state']="NJ"

#Renaming columns and indexes
NJ_agg.index.names=['id','date']
NJ_agg.columns=[['Mg', 'Mgd', 'state']]

#Reprojecting the location data from state plane to WGS84
outProj = Proj(proj='latlong', datum='WGS84', ellps='WGS84')
NJinProj = Proj(init='epsg:6527', preserve_units=True)
x,y = NJ_loc_df["NJEasting"].values,NJ_loc_df["NJNorthing"].values
NJ_loc_df["X_WGS"],NJ_loc_df["Y_WGS"] = transform(NJinProj,outProj,x,y)

# Joining location with water use data
NJ_xy = NJ_loc_df[["SiteName","X_WGS","Y_WGS"]]
NJ_xy.columns=['id','X_WGS','Y_WGS']
NJ_xy.set_index('id',inplace=True)
NJ_agg=NJ_agg.join(NJ_xy, how='inner')
NJ_source_df.set_index(['id','date'],inplace=True)
NJ_agg=NJ_agg.join(NJ_source_df, how='left')
NJ_agg.reset_index(inplace=True)
NJ_agg.drop('Mgd',1,inplace=True)
NJ_agg.rename(columns={'date': 'year'}, inplace=True)
NJ_agg['year'] = NJ_agg['year'].dt.strftime('%Y')

#Exporting aggregated data to feather file
feather.write_dataframe(NJ_agg,r"feather_files\NJ_agg.feather")


## MD
# Importing water-use and location data from xlsx file
MD_df = pd.read_excel("MD\ScottWorland-Allsites-monthly-withdrawals-Maryland.xlsx")

# Exporting raw data to feather file
feather.write_dataframe(MD_df,r"feather_files\MD_raw.feather")


# In[33]:

#Importing feather file
MD_df = feather.read_dataframe(r"feather_files\MD_raw.feather")

#Calculating total water use based on number of days for each month * monthly water use in each row.
MD_df['Mg']=""
MD_df['Mg'] = MD_df.apply(lambda row: row['January Value']*cldr.monthrange(row["Year"],1)[1]
            +row['February Value']*cldr.monthrange(row["Year"],2)[1]
            +row['March Value']*cldr.monthrange(row["Year"],3)[1]
            +row['April Value']*cldr.monthrange(row["Year"],4)[1]
            +row['May Value']*cldr.monthrange(row["Year"],5)[1]
            +row['June Value']*cldr.monthrange(row["Year"],6)[1]
            +row['July Value']*cldr.monthrange(row["Year"],7)[1]
            +row['August Value']*cldr.monthrange(row["Year"],8)[1]
            +row['September Value']*cldr.monthrange(row["Year"],9)[1]
            +row['October Value']*cldr.monthrange(row["Year"],10)[1]
            +row['November Value']*cldr.monthrange(row["Year"],11)[1]
            +row['December Value']*cldr.monthrange(row["Year"],12)[1],axis=1)

#Calculating the date column 
MD_df['date'] = ""
MD_df['date'] = MD_df.apply(lambda x: pd.datetime.strptime("{0}".format(x['Year']), "%Y"),axis=1)

MD_source_df = MD_df[['From Site Number','date','From Site Type Code']]
MD_source_df = MD_source_df.rename(columns={'From Site Number':'id','From Site Type Code':'source'})

MD_df.set_index("date",inplace=True)

#Renaming columns
MD_df.rename(columns={'From Decimal Latitude': 'Y_WGS', 'From Decimal Longitude': 'X_WGS', 'Annual Value': 'Mgd','From Site Number': 'id'}, inplace=True)

#Grouping sites by id
MD_agg=MD_df.groupby(by='id').resample('1AS').sum()

#Pulling out only needed columns
MD_agg = MD_agg[[ 'Mgd','Mg']]

#Attributing state abbreviation
MD_agg['state']="MD"

MD_agg.reset_index(inplace=True)
MD_agg.set_index(['id','date'], inplace=True)
MD_source_df.set_index(['id','date'], inplace=True)
MD_df.reset_index(inplace=True)

MD_xy = MD_df[['Y_WGS', 'X_WGS','id','date']].groupby(['id','date']).first()

MD_agg = MD_agg.join(MD_xy, how='inner')
MD_agg = MD_agg.join(MD_source_df, how='left')

#Resseting index to integers
MD_agg.reset_index(inplace=True)
MD_agg.drop('Mgd',1,inplace=True)
MD_agg.rename(columns={'date': 'year'}, inplace=True)
MD_agg['year'] = MD_agg['year'].dt.strftime('%Y')

#Exporting feather of aggregated data
feather.write_dataframe(MD_agg,r"feather_files\MD_agg.feather")


## ID

# Importing water-use and location data and setting column data type for feather export
ID_df = pd.read_excel("ID\Idaho-monthly-SWUDS-data.xlsx")
ID_df["FROM_COORD_ACY_CD"] = ID_df["FROM_COORD_ACY_CD"].astype("str")
ID_df["FROM_ALT_VA"] = ID_df["FROM_ALT_VA"].astype("str")
ID_df["FROM_ALT_ACY_VA"] = ID_df["FROM_ALT_ACY_VA"].astype("str")

# Exporting to feather file
feather.write_dataframe(ID_df,r"feather_files\ID_raw.feather")

#Importing feather file
ID_df = feather.read_dataframe(r"feather_files\ID_raw.feather")

#Calculating yearly water use based on monthly values in columns and number of days
ID_df['Mg']=""
ID_df['Mgd']=""
ID_df['Mg'] = ID_df.apply(lambda row: row['JAN_VAL']*cldr.monthrange(row["YEAR"],1)[1]
            +row['FEB_VAL']*cldr.monthrange(row["YEAR"],2)[1]
            +row['MAR_VAL']*cldr.monthrange(row["YEAR"],3)[1]
            +row['APR_VAL']*cldr.monthrange(row["YEAR"],4)[1]
            +row['MAY_VAL']*cldr.monthrange(row["YEAR"],5)[1]
            +row['JUN_VAL']*cldr.monthrange(row["YEAR"],6)[1]
            +row['JUL_VAL']*cldr.monthrange(row["YEAR"],7)[1]
            +row['AUG_VAL']*cldr.monthrange(row["YEAR"],8)[1]
            +row['SEP_VAL']*cldr.monthrange(row["YEAR"],9)[1]
            +row['OCT_VAL']*cldr.monthrange(row["YEAR"],10)[1]
            +row['NOV_VAL']*cldr.monthrange(row["YEAR"],11)[1]
            +row['DEC_VAL']*cldr.monthrange(row["YEAR"],12)[1], axis=1)

ID_df['date'] = ""

#Calculating Mgd based on the yearly water use divided by total number of days in each month
ID_df['Mgd']=ID_df['Mg']/ID_df.apply(lambda row: cldr.monthrange(row["YEAR"],1)[1]
            +cldr.monthrange(row["YEAR"],2)[1]
            +cldr.monthrange(row["YEAR"],3)[1]
            +cldr.monthrange(row["YEAR"],4)[1]
            +cldr.monthrange(row["YEAR"],5)[1]
            +cldr.monthrange(row["YEAR"],6)[1]
            +cldr.monthrange(row["YEAR"],7)[1]
            +cldr.monthrange(row["YEAR"],8)[1]
            +cldr.monthrange(row["YEAR"],9)[1]
            +cldr.monthrange(row["YEAR"],10)[1]
            +cldr.monthrange(row["YEAR"],11)[1]
            +cldr.monthrange(row["YEAR"],12)[1],axis=1)

#Calculating date column
ID_df['date'] = ID_df.apply(lambda x: pd.datetime.strptime("{0}".format(x['YEAR']), "%Y"),axis=1)

ID_source_df = ID_df[['SITE_NO','FROM_SITE_TP_CD','date']]
ID_source_df = ID_source_df.rename(columns={'SITE_NO':'id','FROM_SITE_TP_CD':'source'})

ID_df.set_index("date",inplace=True)

#Renaming columns
ID_df.rename(columns={'FROM_DEC_LAT_VA': 'Y_WGS', 'FROM_DEC_LONG_VA': 'X_WGS','SITE_NO': 'id'}, inplace=True)

#Extracting needed columns and grouping by id
ID_agg=ID_df[['Mgd','Mg','id']].groupby(by='id').resample('1AS').sum()
ID_agg.drop('id',1, inplace=True)

#Attributing state abbreviation
ID_agg['state']="ID"

#Extracting location data and joining to aggregated data
ID_df.reset_index(inplace=True)
ID_xy = ID_df[['Y_WGS', 'X_WGS','id','date']].groupby(['id','date']).first()
ID_agg = ID_agg.join(ID_xy, how='inner')
ID_source_df.set_index(['id','date'], inplace=True)
ID_agg = ID_agg.join(ID_source_df, how='left')
ID_agg.reset_index(inplace=True)
ID_agg.drop('Mgd',1,inplace=True)
ID_agg.rename(columns={'date': 'year'}, inplace=True)
ID_agg['year'] = ID_agg['year'].dt.strftime('%Y')

#Exporting aggregated data to feather file
feather.write_dataframe(ID_agg,r"feather_files\ID_agg.feather")

# Merging all aggregated state data
WU_app = OR_agg
WU_app = WU_app.append([PA_df, VA_df, FL_df, ID_agg, MD_agg, NJ_agg])
WU_app['id'] = WU_app['id'].astype("str")
WU_app['source'] = WU_app['source'].astype("str")
WU_app['year'] = WU_app['year'].astype("int")

# Exporting aggregated data to feather file
feather.write_dataframe(WU_app,r"feather_files\WaterUse_aggregated.feather")

#Exporting to csv
WU_app.to_csv('WU_merged.csv')




