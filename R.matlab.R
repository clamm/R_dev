library(R.matlab)
# Start Matlab server on the local machine (if this fails,
# see help(Matlab) for alternatives).
# Matlab$startServer()
# start MATLAB and add externals path to current path: 
# 	addpath('C:\Users\Cindy Lamm\Documents\R\win-library\2.12\R.matlab\externals')
# 	MatlabServer
# to find externals path run in R:
# print(system.file("externals", package="R.matlab"))

# Create a Matlab client object used to communicate with Matlab:
matlab <- Matlab()
# Check status of Matlab connection (not yet connected):
print(matlab)
isOpen <- open(matlab)
if (!isOpen)
throw("Matlab server is not running: waited 30 seconds.")
print(matlab) # Check status of Matlab connection (now connected)

# Run Matlab expressions on the Matlab server:
evaluate(matlab, "whos")

# Get Matlab variables:
data <- getVariable(matlab, 'GARCHlrets')
GARCHlrets <- data$GARCHlrets

data <- getVariable(matlab, 
		c('isLinked','isN100','neg','neut','pos',
		  'noUnitRoot','acfs','pacfs','forGarch',)
cat("Received variables:\n")
str(data)

close(matlab)
ls()

# library(pastecs) # for stat.desc()
# stat.desc(GARCHlrets)

library(tseries) # for adf.test() and garch()
GARCHlrets<- ts(GARCHlrets)

# tmp<- apply(GARCHlrets,MARGIN=2,FUN=adf.test)#HO:No stationary
# tmp<- apply(GARCHlrets,MARGIN=2,FUN=pp.test)#H0:No stationary
# pp.test(LRt,lshort=FALSE)
# kpss.test(LRt)#Attention:Ho:Stationary
# kpss.test(LRt,lshort=FALSE)

data(EuStockMarkets)  
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
plot(dax.garch)          # conditional normality seems to be violated



