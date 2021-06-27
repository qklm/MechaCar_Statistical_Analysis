# MechaCar Statistical Analysis
UCB-Data-15
## Linear Regression to Predict MPG
* vehicle length and ground clearance have a non-random impact on mp values.
* we have a non-zero slope, as both non-random factors have significant parity
* this model does not predict mpg of prototypes well. There are multiple included factors that pull the regressive model in random directions.

## Summary Statistics on Suspension
![Summary of suspension coil in prototype models](https://github.com/qklm/MechaCar_Statistical_Analysis/blob/main/total_summary.png?raw=true)
![Summary of suspension coil in prototype models separated by Lot](https://github.com/qklm/MechaCar_Statistical_Analysis/blob/main/lot_summary.png?raw=true)
* Lot 1 has the lowest variance, and very stable PSI throughout. Lot 2 sees a little more variance, but has consistent PSI erring a statistically insignificant amount higher.
* Lot 3 sees wildly high variance with the mean and median seeing subpar results, dragging down the total summary mean and median. Certainly the least reliable Lot of the group.
* Lot 1 and 2 pass the requested scope. Lot 3 needs to be reissued before pushing to production.

## T-Tests on Suspension Coils
Examining the individual lots a little closer with individual t-tests affirms the assumptions in the summary data. Lot 1 is one the nose, Lot 2 is darn near perfect, Lot 3 is the problem child.

## Study Design: MechaCar vs Competition
Taking into account a refreshed batch of Lot 3, bringing "MechaCar" to market entails making sure it meets industry standard in the price range its built for. I.E. T-Tests on common indicators such as general and specific fuel economy depending, and upkeep/maintenence costs. 

If the failure rate of industry standard components continues at 1/3, there is a significant chance this is a bad investment to continue on. 
If the suspension coil production error is a fluke in a small data set, then continiuing to test components against the standard is preferred. 
