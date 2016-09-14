# Teachers-affordability-report
Estimating the relative affordability of homes on the market for teachers.

### Methodology
Using data from the Bureau of Labor statistics, we gathered salaries for all teachers, then calculated a weighted average by county as well as the entire state for elementary, middle, and secondary teachers (except special education). Using these average salaries by county, we calculated the max monthly mortgage payment using a 30% of gross income threshold. We also calcuated an estimated max home price, assuming a 3.5 percent interest rate.

Next, we gathered all MLS listings currently active on the market (see redfin.com) for the given state, along with the estimated property tax rates by county. Then using the list price, current hoa dues, and county property tax rate for each listing we calculated the estimated monthly mortgage cost with a 3.5% interest rate and 10% down payment of the current listing price. 

Then for each county we counted the percentage of homes where the monthly mortgage was less than the max monthly mortgage payment for that county, as well as the state overall.

The final piece is performing the exact same previous steps for 2012 to make a four-year comparison in salaries and affordable listings by area.

This script gathers the necessary data from the BLS, performs the entire analysis, outputs the raw and formatted data to excel, writes a clean HTML table with affordable listing examples, and, finally, makes an interactive heatmap of the findings in fully self-contained HTML.
