require(xlsx) # for read.xlsx()
require(likert) # for likert()
require(plyr) # for mapvalues()
require(Hmisc) # for errbar()


thisDir <- getwd()
setwd(thisDir)

### load data 
# csvFile <- "Results_experiments_20141120.csv"
xlsxFile <- "Results_experiments_20141120.xlsx"

factorLabels <- c("This experiment failed from my point of view.",
                  "It's still an experiment, but it seems to fail.",
                  "Let's continue experimenting.",
                  "It's still an experiment, but it seems to work.",
                  "This experiment is a success and already best practice.") 
factorShortLabels <- c("failed",
                  "seems to fail",
                  "continue experimenting",
                  "seems to work",
                  "success") 

# rawData <- read.csv(csvFile, stringsAsFactors=TRUE)
rawData <- read.xlsx(xlsxFile, 1, stringsAsFactors=TRUE)
data <- rawData[,-c(1,9)]
colnames(data) <- sapply(colnames(data), FUN=substring, first=5)

for (j in 1:dim(data)[2]) {
  data[,j] <- factor(data[,j], levels=factorLabels)  
  data[,j] <- mapvalues(data[,j], from=factorLabels, to=factorShortLabels)
}


### write frequency count into file
ops <- options(width=60)
sink(file="Experiments_frequencies.txt")
summary(data)
sink()
options(ops)


### bar plot with percentages for low/neutral/high answers
pdf(file="Experiments_plot_percentages.pdf", width=15, height=7)
plot(likert(data))
dev.off()



### get summary statistics
median.na <- function(x) { median(as.numeric(x), na.rm=TRUE) }
med <- c()
for (j in 1:dim(data)[2]) {
  med[j] <- median.na(data[,j]) 
}
stats <- summary(likert(data))
stats <- cbind(stats, med)


### scatter plot with error bars (based on mean and sd)
pdf(file="Experiments_plot_mean.pdf", width=15, height=7)
with(stats,errbar(x=stats$Item, 
                  y=stats$mean, 
                  yplus=stats$mean + stats$sd, 
                  yminus=stats$mean - stats$sd,
                  ylim=c(1,6), lty=2))
title("Experiments Sprint 38")
points(stats$med, 1:dim(stats)[1])
abline(v=2:5, lty="dotted", col="lightgrey")
legend("topright", c("mean","median", "sd"), pch=c(16,1,NA), lty=c(NA,NA,2), bty="n")
text(x=5.3, y=seq(from=3.5, to=1.5, length=5), labels=paste(1:5,factorShortLabels), adj=0)
dev.off()
