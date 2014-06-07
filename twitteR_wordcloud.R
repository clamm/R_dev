#!/usr/local/bin/Rscript

#install the necessary packages
#install.packages("ROAuth")
#install.packages("twitteR")
#install.packages("wordcloud")
#install.packages("tm")

library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem")

#to get consumerKey and consumerSecret see the twitteR documentation for instructions
cred <- OAuthFactory$new(consumerKey='',
                         consumerSecret='',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='http://api.twitter.com/oauth/access_token',
                         authURL='http://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")


#save(cred, file="twitter authentication.Rdata")
registerTwitterOAuth(cred)

r_stats<- searchTwitter("#Rstats", n=1500)
