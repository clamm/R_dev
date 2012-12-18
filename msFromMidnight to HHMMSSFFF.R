#ms=54123834
#modms=mod(ms,1000)
#sec=floor(ms./1000)
#secms=ms./1000
#modsec=mod(sec,60)
#min=floor(sec./60)
#modmin=mod(min,60)
#h=floor(min./60)

#strcat([h ':' modmin ':' secms])

ms<- 54123834
modms<- ms %% 1000
sec<- floor(ms/1000)
secms<- ms/1000
modsec<- sec %% 60
mm<- floor(sec/60)
modmin<- mm %% 60
hh<- floor(mm/60)
paste(hh,':',modmin,':',modsec,'.',modms)