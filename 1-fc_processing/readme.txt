1-We first had to update the null values of the forest dataset in order to apply correctly the next gdal calc functions (script 1_update_null_data.bat)

2-The .tifs algebra was done using gdal commands to retrieve the forest cover for the year 2020 (script 2_retrieving_forest_cover_2020.bat).

3-The forest edge areas were indetified using the distance calculation gdal function (script 3_distance_calculation.bat)

4-The forest core areas were retrieved by eliminating the forest edge areas that were within a distance of 17 pixels of 30 m resoltion. Therefore, we eliminate forest edge areas whithin 510 meters from the edge. (script 4_retrieving_forest_core_areas)

5-convert the forest core areas to shapefile format (script 5_convert_nationals-forest_core_areas_to_shp.bat). We merged the .shp files from step 5 using qgis function. We also applied a dissolve to eliminate potential overlappings

6-We merged the forest cover tif from step 2 to calculate the total forest area in Europe and the number of patches. The number of forest patches was calculated using the r.clump function of grass and the rasters output from step 2.

7-The total forest cover area for 2020 was calculating by calculating the number of pixel using python script (7_count_unique_cells_forest_cover_tifs.ipynb)
