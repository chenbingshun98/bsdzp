GDPC <- readxl::read_xlsx()
countryvec <- GDPC[,1]#A character vector
#country name
#make a panel of 42 countries for 17 years
countryname_panel <- c()
for (i in 1:42) {
  x <- rep(countryvec[i],17)
  countryname_panel <- append(countryname_panel,x)
}

##years
##make a panel of 42 countries for 17 years
years_panel <- rep(1998:2014,42)

GDPC_panel <- c()
for (i in 1:42) {
  x <- GDPC[i,]#select row of the country
  x <- x[-c(1,2,3)]#select only data from year 1998 to 2014
  x <- t(x)#Matrix Transpose
  GDPC_panel <- rbind(GDPC_panel,x)
}
rm(GDPC)#to avoid confusion
subsahara <- cbind(countryname_panel,years_panel,GDPC_panel)
colnames(subsahara) <- c()
