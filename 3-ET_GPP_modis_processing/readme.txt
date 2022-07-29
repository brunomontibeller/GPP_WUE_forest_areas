1-The first step was to convert the DOY's (day of the year) from the ET and GPP images do "date" format. (Scripts 0_et_gpp_dates_generator)

2-Next, we use gdal command lines to reproject the MODIS ET and GPP data and rename then using the dates retrieved in the previous step. We used Notepadd++ to replicate the gdal command lines (scripts 1_gpp_rep.bat and 2_et_rep.bat).

3-Then we processed the GPP and ET .tif files (e.g., rescale, mask for Europe, calculate the monthly mean, check the matching dates for both ET and GPP). In this part, we also created a .shp file that contains all MODIS pixels with valid observations in both variables (ET and GPP) within Europe.
(script 3_et_gpp_processing).

4-The last process in this section was done in QGIS. We used the intersection tool to interesect the MODIS pixels .shp and the forest core areas .shp from section 1. Then, we filtered the intersected output by an area threshold (>12.5 hectares). At the end, a total of 18538 pixels. We then convert this shapefile to points and used the function in QGIS to extract the monthly ET and GPP file from the rasters output of step 3. 

