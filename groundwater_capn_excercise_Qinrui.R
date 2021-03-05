# 2. Take a look at the regression results and means
region_data['watercoeff']
## View(region_data[["watercoeff"]])
region_data['mlogitcoeff']
## View(region_data[['mlogitcoeff']])
## View(region_data[['cropamts']])


# 3. Take a look at the summary statistic means
region_data['wwdmeans']
## View(region_data[['wwdmeans']])
region_data['mlogitmeans']
## View(region_data[['mlogitmeans']])


# 6. Plot water withdrawals as a function of water depth
Wwd(21.5, gw.data)

data.ww <- matrix(0, nrow = 100, ncol = 2)
colnames(data.ww) <- c("water","water.withdrawal")

for(j in 1:100){
  data.ww[j,1] <- j
  data.ww[j,2] <- Wwd(j,gw.data)
}

View(data.ww)
data.ww <- as.data.frame(data.ww)

ggplot() +
  geom_line(data = data.ww, aes(x = water, y = water.withdrawal),
            color = 'blue') +
  labs(
    x= "Stored groundwater",
    y = "Water Withdrawal") +
  theme( #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )


# 7. Plot crop land as a function of water depth
cropFwater(21.5, gw.data)

data.cs <- matrix(0, nrow=100, ncol=7)
colnames(data.cs) <- c("water","alfalfa", "corn", "sorghum", "soy", "wheat", "fallow")

for(j in 1:100){
  data.cs[j,1] <- j
  for (x in 1:6){
   data.cs[j,x+1] <- cropFwater(j,gw.data)[x]
  }
}

View(data.cs)
data.cs <- as.data.frame(data.cs)

irr_acres <- cbind(data.cs[,1], rowSums(data.cs[,2:7]))
colnames(irr_acres) <- c("water","irrigated_acres")
irr_acres <- as.data.frame(irr_acres)

## Line plot
ggplot() +
  geom_line(data = irr_acres, aes(x = water, y = irrigated_acres),
            color = 'blue') +
  labs(
    x= "Stored groundwater",
    y = "Total area planted for irrigated crops") +
  theme( #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA)
  )

## Pie charts in a 2*3 grid
pies <- list(NULL, NULL, NULL, NULL, NULL, NULL)

for (i in 1:6) {
  
wdep <- c(2,5,15,20,50,90)[i]

my.pie<- data.frame(
  crops = c("alfalfa", "corn", "sorghum", "soy", "wheat","fallow"),
  fracs = cropFwater(wdep ,gw.data)
)

pies[[i]] <- ggplot(my.pie, aes(x = factor(1), y=fracs, fill=factor(crops)) )+
  geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
  labs(
    x= "",
    y = "") +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "transparent",color = NA),
        plot.background = element_rect(fill = "transparent",color = NA)
  )+
  guides(fill=guide_legend(title="crop fractions"))

}

library(cowplot)
plot_grid(pies[[1]], pies[[2]], pies[[3]], 
          pies[[4]], pies[[5]], pies[[6]], 
          labels=paste("Water depths:",c(2,5,15,20,50,90)), 
          ncol=3, nrow=2, greedy=FALSE)


# 8. Investigate various discount rates and water depths
## load the function to alter parameters in the simulations (credits to Andie!)
source("https://raw.githubusercontent.com/a5creel/myWaterSim/main/myWaterSim.R")

## What is the effect of discount rates?
myWaterSim(myStock = 21.5, myDiscountRate = 0.03)$shadowp
myWaterSim(myStock = 21.5, myDiscountRate = 0.05)$shadowp
myWaterSim(myStock = 21.5, myDiscountRate = 0.07)$shadowp

## How about different water depths?
myWaterSim(myStock = 18.0, myDiscountRate = 0.03)$shadowp
myWaterSim(myStock = 18.0, myDiscountRate = 0.05)$shadowp
myWaterSim(myStock = 18.0, myDiscountRate = 0.07)$shadowp


# 9. Investigate different recharge rate
recharge_sensitivity <- as.data.frame(matrix(0, nrow = 3, ncol = 4)) 
rownames(recharge_sensitivity) <- c("wdep=21.5","wdep=18","wdep=30")
colnames(recharge_sensitivity) <- c("0.5*recharge.baseline","1*recharge.baseline","2*recharge.baseline","5*recharge.baseline")

for (i in 1:3) {
  wdep <- c(21.5, 18, 30)[i]
  for (j in 1:4) {
    recharge.multiplier <- c(0.5, 1, 2, 5)[j]
    recharge_sensitivity[i,j] <- myWaterSim(myStock = wdep, myRechargeMultiplier =recharge.multiplier)$shadowp
  }
}

View(recharge_sensitivity)


# 10. Inclusive wealth
myWaterSim()$stock * myWaterSim()$shadowp - myWaterSim()$iw  # get 0

## Test whether inclusive wealth = water stock * shadow price by adjusting the table above
test <- as.data.frame(matrix(0, nrow = 3, ncol = 4)) 
rownames(test) <- c("wdep=21.5","wdep=18","wdep=30")
colnames(test) <- c("0.5*recharge.baseline","1*recharge.baseline","2*recharge.baseline","5*recharge.baseline")

for (i in 1:3) {
  wdep <- c(21.5, 18, 30)[i]
  for (j in 1:4) {
    recharge.multiplier <- c(0.5, 1, 2, 5)[j]
    results <- myWaterSim(myStock = wdep, myRechargeMultiplier =recharge.multiplier)
    test[i,j] <- results$stock * results$shadowp - results$iw
  }
}

View(test)


# 11. Inclusive wealth
output <- psim(pcoeff = pC,
             stock = 21.5,
             wval = profit(21.5, gw.data),
             sdot = sdot(21.5, recharge, gw.data))
output$vfun

myWaterSim(myStock = 21.5, myQuest11 = TRUE)$vfun - myWaterSim(myStock = 20.5, myQuest11 = TRUE)$vfun
myWaterSim(myStock = 21.5, myQuest11 = TRUE)$vfun - myWaterSim(myStock = 18.5, myQuest11 = TRUE)$vfun

myWaterSim(myStock = 21.5)$iw - myWaterSim(myStock = 20.5)$iw
myWaterSim(myStock = 21.5)$iw - myWaterSim(myStock = 18.5)$iw


# 12-14. Adjust taxed corn price and inspect wheat price
ksdata.alt <- ksdata

ksdata.alt$State$mlogitmeans[14,2] <- 0.9*ksdata.alt$State$mlogitmeans[14,2]

View(ksdata.alt$State$mlogitmeans)
ksdata.alt$State$mlogitmeans[14,2] # corn price
ksdata.alt$State$mlogitmeans[17,2] # wheat price


# 15-22. 10% tax on corn revenue
ksdata.alt$State$mlogitmeans[14,2] <- 0.9*ksdata.alt$State$mlogitmeans[14,2]

gw.data.alt <- datasetup(region, dataset = ksdata.alt)

results <- myWaterSim(myGW.data = gw.data, myQuest11 = TRUE)
results.alt <- myWaterSim(myGW.data = gw.data.alt, myQuest11 = TRUE)

## 17. Water withdrawal
results$wval
results.alt$wval

## 18. Total acreage planted
cropFwater(21.5, gw.data)
cropFwater(21.5, gw.data.alt)
sum(cropFwater(21.5, gw.data))
sum(cropFwater(21.5, gw.data.alt))

## 19. Share of crops
pies.alt <- list(NULL, NULL)

for (i in 1:2) {
  
  wdep <- 21.5
  data <- list(gw.data, gw.data.alt)[[i]]
  
  my.pie.alt <- data.frame(
    crops = c("alfalfa", "corn", "sorghum", "soy", "wheat","fallow"),
    fracs = cropFwater(wdep ,data)
  )
  
  pies.alt[[i]] <- ggplot(my.pie.alt, aes(x = factor(1), y=fracs, fill=factor(crops)) )+
    geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
    labs(
      x= "",
      y = "") +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          panel.background = element_rect(fill = "transparent",color = NA),
          plot.background = element_rect(fill = "transparent",color = NA)
    )+
    guides(fill=guide_legend(title="crop fractions"))
  
}

library(cowplot)
plot_grid(pies.alt[[1]], pies.alt[[2]], 
          labels=paste("Data:",c("gw.data", "gw.data.alt")), 
          ncol=2, nrow=1)

## 20. Farm profit
profit(21.5, gw.data)
profit(21.5, gw.data.alt)

## 21. Shadow price
results$shadowp
results.alt$shadowp

## 22. Wealth
results$iw
results.alt$iw

results$vfun
results.alt$vfun


# 23. 10% subsidy on wheat
### Basically copy and paste code from Q#15-22, except multiplying wheat prices by 1.1 rather than multiplying corn prices by 0.9
ksdata.alt2 <- ksdata
ksdata.alt2$State$mlogitmeans[17,2] <- 1.1*ksdata.alt2$State$mlogitmeans[17,2]

gw.data.alt2 <- datasetup(region, dataset = ksdata.alt2)

results <- myWaterSim(myGW.data = gw.data, myQuest11 = TRUE)
results.alt2 <- myWaterSim(myGW.data = gw.data.alt2, myQuest11 = TRUE)

## 17*. Water withdrawal
results$wval
results.alt2$wval

## 18*. Total acreage planted
cropFwater(21.5, gw.data)
cropFwater(21.5, gw.data.alt2)
sum(cropFwater(21.5, gw.data))
sum(cropFwater(21.5, gw.data.alt2))

## 19*. Share of crops
pies.alt2 <- list(NULL, NULL)

for (i in 1:2) {
  
  wdep <- 21.5
  data <- list(gw.data, gw.data.alt2)[[i]]
  
  my.pie.alt2 <- data.frame(
    crops = c("alfalfa", "corn", "sorghum", "soy", "wheat","fallow"),
    fracs = cropFwater(wdep ,data)
  )
  
  pies.alt2[[i]] <- ggplot(my.pie.alt2, aes(x = factor(1), y=fracs, fill=factor(crops)) )+
    geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
    labs(
      x= "",
      y = "") +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          panel.background = element_rect(fill = "transparent",color = NA),
          plot.background = element_rect(fill = "transparent",color = NA)
    )+
    guides(fill=guide_legend(title="crop fractions"))
  
}

library(cowplot)
plot_grid(pies.alt2[[1]], pies.alt2[[2]], 
          labels=paste("Data:",c("gw.data", "gw.data.alt2")), 
          ncol=2, nrow=1)

## 20*. Farm profit
profit(21.5, gw.data)
profit(21.5, gw.data.alt2)

## 21*. Shadow price
results$shadowp
results.alt2$shadowp

## 22*. Wealth
results$iw
results.alt2$iw

results$vfun
results.alt2$vfun


