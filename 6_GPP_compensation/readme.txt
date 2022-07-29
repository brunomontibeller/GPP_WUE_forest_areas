To calculate the GPP compensations, we used the GPP trend calculation results from step 4
We merged the trend calculation results with the points shapefile using the ID as common index. 
We calculated the total amount of carbon per ID and per month using the area values and the sen's slope values. When calculating, we converted the values to tonnes
Then, we summed the monthly total amount of carbon per id (per points). 
Using the result of the previous sum, we classified each id's (points) into "Deficit" and "Surplus". Deficit was asigned for those pixels with negative amount of carbon, whereas Surplus was asigned for those pixels with positive amount of carbon.
Later, we summed the pixels with Deficit and Surplus to have the total amount of carbon for each category (Deficit and Surplus). 
We also saved the Deficit and Surplus as points to create a map later. 