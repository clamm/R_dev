## define used packages and check for their existence:
usedPackages = c('onion','sqldf','wordcloud','rmongodb','parallel','rmr2')
# package RMongo doesn't work on Mac OSX

for (pkg in usedPackages) {
  if (!require(pkg, character.only=TRUE)) {
    install.packages(pkg)
    require(pkg, character.only=TRUE)   
  }
}


##sqldf: run SQL statements on R data frames

data("iris")
library("sqldf")

sql1 <- "select * from iris limit 2"
sql2 <- "select count(*) from iris"
sql3 <- "select species, count(*) from iris group by species"
sqldf(sql3)



## Training: Big Data Analyses with R
# Exercise 2

library(rmongodb)

#connect with mongo on default host (localhost) and port (27017)
mongo <- mongo.create()

mongo.get.databases(mongo) #which databases are existing?

mongo.get.database.collections(mongo, "test") #which collections exist in test?


# example 1

# create BSON: 

buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "state", "FL")
mongo.bson.buffer.append(buf, "city", "MIAMI")
query <- mongo.bson.from.buffer(buf)
query

# query to find all cities that start with L in Florida
# json query: db.zips.find({ city: /^L/ , state:"FL"})
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "state", "FL")
#wrap regex in subdocument:
mongo.bson.buffer.start.object(buf, "city")
mongo.bson.buffer.append(buf, "$regex", "^L")
mongo.bson.buffer.finish.object(buf)
query <- mongo.bson.from.buffer(buf)
query



# QUERY
mongo.count(mongo, "test.zips", query)
cursor <- mongo.find(mongo, "test.zips", query) #find all zip codes in Florida

res <- NULL
while (mongo.cursor.next(cursor)){
  print(mongo.cursor.value(cursor))
  tmp <- mongo.bson.to.list(mongo.cursor.value(cursor))
  res <- rbind(res, tmp)
}
mongo.cursor.destroy(cursor)

head(res)


x <- 1:10000 #there is a dfference in time for a vector of 1 to 10.000.000
x
sapply(x, function(y) y^2)
?mclapply

system.time(unlist(lapply(x,function(y) y^2)))
system.time(unlist(mclapply(x,function(y) y^2)))