library(dplyr)
mechacar <- read.csv(file='MechaCar_mpg.csv')

# Linear model to test for nonrandom factors
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mechacar))

# Loading in suspension coil
suscoil <- read.csv(file='Suspension_Coil.csv')

# Summary of PSI in dataset
total_summary <- suscoil  %>% summarise(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), StD = sqrt(var(PSI)))

# Dividing summary data into lots
lot_summary  <- suscoil  %>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), StD = sqrt(var(PSI)))
t.test(suscoil$PSI, mu=1500)

lot_one <- suscoil[suscoil$Manufacturing_Lot == "Lot1",]
lot_two <- suscoil[suscoil$Manufacturing_Lot == "Lot2",]
lot_three <- suscoil[suscoil$Manufacturing_Lot == "Lot3",]

t.test(lot_one$PSI, mu=mean(suscoil$PSI))
t.test(lot_two$PSI, mu=mean(suscoil$PSI))
t.test(lot_three$PSI, mu=mean(suscoil$PSI))

