# Read in the data files using RStudio's Import Dataset

summary(citytemps)

# Make a histogram for San Diego
hist(citytemps$Temp.SanDiego)

# Compute summary statistics that measure dispersion
mean(citytemps$Temp.SanDiego)
sd(citytemps$Temp.SanDiego)
quantile(citytemps$Temp.SanDiego)
quantile(citytemps$Temp.SanDiego, probs=c(0.05, 0.95))

# Now make a histogram for for Rapid City,
# while changing the default title and x-axis label
hist(citytemps$Temp.RapidCity, main="Average Daily Temperatures in Rapid City, 1995-2011", xlab='Temperature')

# Put histograms for San Diego and Rapid City next to each other
par(mfrow=c(2,1))
hist(citytemps$Temp.SanDiego)
hist(citytemps$Temp.RapidCity)

#####
# Good statistical graphics facilitate comparison.
# Change the x axis, y axis, and bins on the two plots so they're comparable
#####

# First define a custom set of bin breakpoints
mybreaks = seq(-20, 92, by=2)

# Now make the histograms using these custom bins
par(mfrow=c(2,1))
hist(citytemps$Temp.SanDiego, breaks=mybreaks, xlim=c(-20,100), ylim=c(0, 760))
hist(citytemps$Temp.RapidCity, breaks=mybreaks, xlim=c(-20,100), ylim=c(0, 760))





### Below are all the commands that make the plots pretty

mybreaks = seq(-20, 92, by=2)
par(mfrow=c(1,1), mar=c(4,4,1,0), mgp=c(2.5,1,0))
hist(citytemps$Temp.SanDiego, breaks=mybreaks, xlab="Average Daily Temperature (F)", main="", border="darkgrey", col="grey", axes=FALSE, ylim=c(0, 760))
hist(citytemps$Temp.RapidCity,breaks=mybreaks,add=TRUE, border=rgb(0,100,0,100,maxColorValue=255), col= rgb(0,100,0,50,maxColorValue=255))
axis(2, at=seq(0,700,by=100))
axis(1,pos=0)
text(55, 770, "San Diego, CA", pos=4, font=2)
text(30, 260, "Rapid City, SD", pos=4, font=2)




### Ditch the axis ticks and use a white line

mybreaks = seq(-20, 92, by=2)
par(mfrow=c(1,1), mar=c(4,4,1,0), mgp=c(2.8,1,0))
hist(citytemps$Temp.SanDiego, breaks=mybreaks, xlab="Average Daily Temperature (F)", main="", border="darkgrey", col="grey", axes=FALSE, ylim=c(0, 760))
hist(citytemps$Temp.RapidCity,breaks=mybreaks,add=TRUE, border=rgb(0,100,0,100,maxColorValue=255), col= rgb(0,100,0,50,maxColorValue=255))
axis(2,at=seq(0,700,by=100), las=1,tick=FALSE)
axis(1,pos=0)
text(55, 770, "San Diego, CA", pos=4, font=2)
text(30, 260, "Rapid City, SD", pos=4, font=2)
for(i in seq(100,700,by=100))
{
	abline(h=i,col='white', lwd=1)
}



### Axis labels on the right

mybreaks = seq(-20, 92, by=2)
par(mfrow=c(1,1), mar=c(3,0,1,3), mgp=c(2,1,0))
hist(citytemps$Temp.SanDiego, breaks=mybreaks, xlab="Average Daily Temperature (F)", main="", border="darkgrey", col="grey", axes=FALSE, ylim=c(0, 760))
hist(citytemps$Temp.RapidCity,breaks=mybreaks,add=TRUE, border=rgb(0,100,0,100,maxColorValue=255), col= rgb(0,100,0,50,maxColorValue=255))
axis(4,at=seq(0,700,by=100), las=1,tick=FALSE)
axis(1,pos=0)
text(55, 770, "San Diego, CA", pos=4, font=2)
text(30, 260, "Rapid City, SD", pos=4, font=2)
for(i in seq(100,700,by=100)) {
	abline(h=i,col='white', lwd=1)
}






### Now just rapid city with mean and sd plotted 
mybreaks = seq(-20, 92, by=2)
par(mar=c(4,4,1,0), mgp=c(2.5,1,0))
hist(citytemps$Temp.RapidCity,breaks=mybreaks, border=rgb(0,100,0,100,maxColorValue=255), col= rgb(0,100,0,50,maxColorValue=255), xlab="Average Daily Temperature (F)", main="", axes=FALSE, ylim=c(0,255))
axis(2, at=seq(0,250,by=50))
axis(1, pos=0, at=seq(-20,90,by=10))

mu = mean(citytemps$Temp.RapidCity)
s = sd(citytemps$Temp.RapidCity)
points(mu,247, pch=19, col="blue",cex=1.5)
lines(c(mu-s,mu+s), c(247, 247), col='blue')






# An aside: could use merge to handle this like a relational database if you wished
D = merge(casandie, sdrapcty, by = c("Year","Month", "Day"), sort=FALSE, all=TRUE)

