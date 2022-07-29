We calculated the trends using the values extract to points from step 3. 
Before calculating the trends, we converted the monthly GPP mean values into monthly values by, first dividing the values by 8, and then multiplying by the number of days of each month. 
We then converted from kg to g. 
The trends were calculated using the "mannKen" function from the "wql" package. The calculation was performed by ID (by points) and per month. After the trend's calculation, we used the P-values to classify the ID's into "Decrease", "Increase" and "No trend". 
A similar process was used to calculate the trend for evapotranspiration. 
The water-use-efficiency trends were calculating after merging of the GPP and ET month values. The merge was done using the ID, Month and Year values. After that, we calculate the ratio (WUE=GPP/ET). Using the ratio results, the trends were calculated using the same process described before. 