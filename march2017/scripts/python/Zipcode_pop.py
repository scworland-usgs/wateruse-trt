## Jordan Wilson, 2017

### Run on py35 for feather import

get_ipython().magic(u'matplotlib notebook')
# import arcpy
import feather
import pandas as pd
# import geopandas as gp
# from shapely.geometry import Point
# arcpy.env.overwriteOutput = True
import matplotlib.pyplot as plt
import numpy as np

pd.options.display.max_rows = 999
pd.options.display.max_columns = 999

WU_app = feather.read_dataframe('feather_files\WaterUse_aggregated.feather')
WU_app.to_csv('feather_files\WaterUse_aggregated.csv')


# ### Run on py27 for arcpy 

get_ipython().magic(u'matplotlib notebook')
import arcpy
# import feather
import pandas as pd
import geopandas as gp
from shapely.geometry import Point
arcpy.env.overwriteOutput = True
import matplotlib.pyplot as plt
import numpy as np

pd.options.display.max_rows = 999
pd.options.display.max_columns = 999

# Reading in aggregated water-use data

WU_app = pd.read_csv('feather_files\WaterUse_aggregated.csv')

# Assigning geometry based on Lat/Long fields for use in ArcPy

geometry = [Point(xy) for xy in zip(WU_app.X_WGS, WU_app.Y_WGS)]
WU_geo = gp.GeoDataFrame(WU_app, crs={'init' :'epsg:4326'}, geometry=geometry)

#Saving spatial water-use dataset as shapefile
WU_geo.to_file(r'U:\WRT\public_supply\GIS\WU_agg.shp')

#Setting file locations
target_features = "U:\WRT\public_supply\GIS\WU_agg.shp"
join_features = "U:\WRT\public_supply\Zipcode_pop\cb_2015_us_zcta510_500k_WGS84.shp"
out_feature_class = "U:\WRT\public_supply\GIS\WU_zip.shp"

#Spatial join between zipcode shapefile and well locations
arcpy.SpatialJoin_analysis(target_features, join_features, out_feature_class,match_option="CLOSEST")

#Reading in zipcode-joined water-use data
WU_zip = gp.read_file("U:\WRT\public_supply\GIS\WU_zip.shp")

#converting data type of year to a float
WU_zip['year'] = WU_zip['year'].astype('float')

zip = pd.read_csv(r'Zipcode_pop\Merged_zip.csv')

#Importing zip code population into Pandas
zip1990 = pd.read_csv(r'Zipcode_pop\1990_zip_pop.csv')
zip2000 = pd.read_csv(r'Zipcode_pop\2000_zip_pop.csv')
zip2010 = pd.read_csv(r'Zipcode_pop\2010_zip_pop.csv')

#Changing data type of population to a string
zip1990['Zip'] = zip1990['Zip'].astype('str')
zip2000['Zip'] = zip2000['Zip'].astype('str')
zip2010['GEOID'] = zip2010['GEOID'].astype('str')

#Formatting zipcode to a 5 digit with leading zeros
zip1990['Zip'] = zip1990.apply(lambda row: '0'+ row['Zip'] if len(row['Zip'])==4 else row['Zip'],axis=1)
zip2010['GEOID'] = zip2010.apply(lambda row: '0'+ row['GEOID'] if len(row['GEOID'])==4 else '00'+ row['GEOID'] if len(row['GEOID'])==3 else row['GEOID'],axis=1)

#Renaming columns for consistency
zip2010.rename(columns={'POP10':'pop2010', 'GEOID':'Zip'}, inplace=True)
zip1990.rename(columns={'Pop':'pop1990'}, inplace=True)
zip2000.rename(columns={'Pop':'pop2000'}, inplace=True)

#Replacing bad data with NaN
zip2000.loc[zip2000['pop2000']=='(part)','pop2000']=np.nan

#Merging all three census datasets into one
zip_data1 = zip1990.merge(zip2000,on='Zip',how='outer')
zip_data = zip_data1.merge(zip2010,on='Zip',how='outer')

#Extracting on the necessary columns
zip_data = zip_data[['Zip','pop1990','pop2000','pop2010']]

#Merging zipcode population data at each site
WU_zipped = WU_zip.merge(zip_data, how='left',left_on='GEOID10',right_on='Zip')

#Converting population data types to float
WU_zipped['pop1990']=WU_zipped['pop1990'].astype('float')
WU_zipped['pop2000']=WU_zipped['pop2000'].astype('float')
WU_zipped['pop2010']=WU_zipped['pop2010'].astype('float')

#Interpolating population data 
WU_zipped["Population"] = WU_zipped.apply(lambda row: round((row['pop2000']-row['pop1990'])/10*(row['year']-1990)+row['pop1990'],0) 
                                            if (row['year']<2000)&(row['year']>=1990) 
                                            else round((row['pop2010']-row['pop2000'])/10*(row['year']-2000)+row['pop2000'],0) 
                                            if (row['year']>=2000)&(row['year']<2011) 
                                            else np.nan,axis=1)


											
#Extracting only necessary columns
WU = WU_zipped[['id','year','Mg','source','state','X_WGS','Y_WGS','Population']]
WU = WU[['id','year','Mg','state','source','X_WGS','Y_WGS','Population']]

#Exporting to csv
WU['id'] = WU['id'].astype('str')

WU.to_csv('WU_withpop_final.csv')

#Exporting to feather file
WU = pd.read_csv('WU_withpop_final.csv')

WU.drop('Unnamed: 0',1,inplace=True)

WU["id"] = WU["id"].astype("str")

feather.write_dataframe(WU,'feather_files\WaterUse_aggregated_zipcode_pop.feather')