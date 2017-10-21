
plot6 <- function(){
  vehicles <-grepl("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE)
  vSCC <- SCC[vehicles,]$SCC
  
  NEI$year <- as.factor(NEI$year)
  subNEI <- NEI[NEI$fips=="24510" | NEI$fips == "06037", c("fips", "SCC", "Emissions", "year")]
  subNEI$fips <- gsub("24510", "Baltimore City", subNEI$fips)  
  subNEI$fips <- gsub("06037", "LA County, California", subNEI$fips) 
  subNEI$fips <- as.factor(subNEI$fips)
  
  vNEI <- subNEI[subNEI$SCC %in% vSCC, ]
  subNEI <- NULL
  vNEI <- vNEI[,c("fips", "year", "Emissions")]
  
  s <- aggregate(Emissions~year+fips, vNEI,FUN=sum)
  
  plot6 <- ggplot(s,aes(year, Emissions, fill=fips))+geom_bar(stat="identity")+facet_grid(.~fips)+xlab("Year")+ylab("PM2.5 (tons)")+ggtitle("PM2.5 Emissions from Motor Vehicles in LA County and Baltimore City")+guides(fill=FALSE)
  
  print(plot6)
}

plot6()
