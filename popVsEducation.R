#Install needed package.
install.packages("stringi")

#Load needed libraries.
library(stringi)
library(ggplot2)

#Read in main data set. Subset with only the relevant columns.
dataSet <- read.table("DataSet.txt", header = TRUE, sep = ",")
dataSet <- subset(dataSet, select = c("fips","PST045213","EDU635212"))
colnames(dataSet) <- c("County Code", "2013 Population", "High School Educated People")

#Cleanse dataSet of individual states' populations, and the overall US population entry.
for (i in 1:length(dataSet$`County Code`)) {
  if (stri_sub(dataSet$`County Code`[i], -3, -1) == "000" || dataSet$`County Code`[i] == 0) {
    dataSet$`County Code`[i] = NA
  }
}
dataSet <- na.omit(dataSet)

#Read in FIPS County Code data.  Create and fill a third column for state.
countyCodes <- read.fwf("FIPS_CountyName.txt", skip = 1, widths = (c(5, 38)))
countyCodes[, "state"] <- c(stri_sub(countyCodes[,2], -2, -1))

#Map county code in dataSet to countyCodes, and replace dataSet's county codes with state acronyms.
dataSet$`County Code` <- with(countyCodes, countyCodes$state[match(dataSet$`County Code`, countyCodes$V1)])

#Plot as a scatterplot with facets.  Set X and Y axis labels, remove legend, add in linear best fit line.
graph <- ggplot(data = dataSet, aes(dataSet$`High School Educated People`, dataSet$`2013 Population`)) + geom_point(aes(colour = factor(dataSet$`County Code`)))
graph + facet_wrap(~dataSet$`County Code`) + theme(legend.position = "none") + stat_smooth(method = "lm") + labs(x = "Percent of Population High School Educated", y = "2013 County Population", title = "Population vs. Education")
