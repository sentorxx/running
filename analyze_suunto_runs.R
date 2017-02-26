library(XML)
library(raster)

##https://joergsteinkamp.wordpress.com/2015/09/22/reading-gpx-tracks-in-r/
getTrack <- function(file, vars=c("time", "ele")) {
    pfile <- htmlTreeParse(file, error=function (...) {}, useInternalNodes=TRUE)
    trkpt <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
    create.df.str <- 'out.df <- data.frame(lon=as.numeric(trkpt["lon",]), lat=as.numeric(trkpt["lat",])'
    for (n in vars) {
        if (n=="ele") {
            ele <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
            create.df.str <- paste(create.df.str, ", ele=ele", sep="")
        } else if (n=="time") {
            time <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
            time <- strptime(time, format = "%Y-%m-%dT%H:%M:%OS")
            create.df.str <- paste(create.df.str, ", time=time", sep="")
        } else {
            eval(parse(text=paste(n, ' <- xpathSApply(pfile, path = //trkpt/', n, ', xmlValue)', sep="")))
            create.df.str <- paste(create.df.str, ", ",n, "=", n, sep="")
        }
    }
    create.df.str <- paste(create.df.str, ")", sep="")
    eval(parse(text=create.df.str))
    return(out.df)
}

track <- getTrack("track.gpx")

track$delta.dist = 0
track$delta.dist[2:nrow(track)] = pointDistance(track[2:nrow(track), c("lon", "lat")], track[1:(nrow(track)-1), c("lon", "lat")], lonlat=TRUE)

track$delta.time = 0
track$delta.time[2:nrow(track)] = as.numeric(difftime(track$time[2:nrow(track)], track$time[1:(nrow(track)-1)], units="secs"))

track$speed = 3.6 * track$delta.dist / track$delta.time

## print(track$speed)
## na.rm means remove NaN otherwise it's the result 
## print(max(track$speed,na.rm=TRUE))
## print(min(track$speed,na.rm=TRUE))
## print(mean(track$speed,na.rm=TRUE))

my_mean_speed = mean(track$speed,na.rm=TRUE)
my_time_minutes = sum(track$delta.time)/60
my_distance_km = sum(track$delta.dist) / 1000
##print(sum(track$delta.time)/60) + "minutes"
paste("run duration:", my_time_minutes, "minutes ;","speed mean:" ,my_mean_speed, "km/h", "distance:",my_distance_km, "km" )

