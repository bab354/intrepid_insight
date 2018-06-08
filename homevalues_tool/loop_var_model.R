## import data, clean and create var model.
library(vars)
library(tseries, quietly = T)
library(forecast, quietly = T)
library('reshape2')
library('data.table')
library('stringr')
library('ggplot2')
library('plotly')
library('formattable')

Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


setwd("C:/Users/jakek/Documents/intrepidinsight/homeprices")

## get data.
zillowdata<-read.csv("resource/City_MedianValuePerSqft_AllHomes.csv")
zillowdata<-data.table(zillowdata)
zillowdata<-melt(zillowdata, measure=patterns("^X"), value.name="pricepersq", variable.name="monthlydate")

# the exogenous mortgage rate data.
mortgage<-data.table(read.csv("resource/MORTGAGE30US.csv"))
mortgage<-mortgage[,DATE := as.Date(DATE, "%Y-%m-%d")][MORTGAGE30US!="."][,month_year := format(DATE, "%Y-%m")]
# keep the first mortgage rate in the month
mortgage<-mortgage[,first_date := min(DATE), by=month_year][DATE==first_date][,first_date:=NULL]
setnames(mortgage, "MORTGAGE30US", "mort_rate")

## prep the zillow data
zillowdata<-zillowdata[,monthlydate := as.Date(paste0(substring(monthlydate, 2),".01"), "%Y.%m.%d")][,month_year := format(monthlydate, "%Y-%m")]
setkey(zillowdata,RegionID, monthlydate)
zillowdata<-zillowdata[,diff := monthlydate - shift(monthlydate, fill=first(monthlydate)), by = RegionID]
zillowdata<-zillowdata[,gap := is.na(pricepersq) & 
                         !is.na(shift(pricepersq, fill=first(pricepersq), type="lag")) & 
                                  !is.na(shift(pricepersq, fill=first(pricepersq), type="lead")), by = RegionID]



## limit to CA for now
zillowdata<-zillowdata[State=="CA"]

## destroy NA records
zillowdata<-zillowdata[!is.na(pricepersq)]

## create cityname from region and state
zillowdata<-zillowdata[,city := str_replace_all(RegionName, "-", " ")]
zillowdata<-zillowdata[CountyName=="San Francisco" | CountyName=="San Mateo", CountyName:="San Mateo + San Francisco"]

## no internal gaps?
stopifnot(unique(zillowdata[,diff])<=31)
stopifnot(unique(zillowdata[,gap])==FALSE)

save(zillowdata, file="data/processed_zillow.RData")

##
##### LOOP OVER EACH PAIR OF CITIES WITHIN A COUNTY
## unique(zillowdata[,CountyName])
for (county in "San Mateo + San Francisco"){
  print(paste("Now doing:",county))
  for (c in unique(zillowdata[CountyName==county,city])){
    for (cc in unique(zillowdata[CountyName==county,city])){
      
      if (c==cc | file.exists(paste0("out/",str_replace_all(paste(c, cc), " ", "_"),".RData")) | file.exists(paste0("out/",str_replace_all(paste(cc,c), " ", "_"),".RData"))) {
        next
      }
      
      print(paste(c, "vs.",cc))
      
      selectname1=c
      selectname2=cc
      
      l = 0
      ## declare as time series, each one.
      for (i in c(selectname1, selectname2)){
        ## create a name from the cities.
        l = l +1
        assign(paste0("city",as.character(l)),paste0("ts_",str_replace_all(i, " ", "_")))
      assign(paste0("ts_",str_replace_all(i, " ", "_")), ts(data=zillowdata[city==i,pricepersq], 
                  start=c(as.numeric(format(min(zillowdata[city==i,monthlydate]),"%Y")), 
                  as.numeric(format(min(zillowdata[city==i,monthlydate]),"%m"))), frequency=12))
      }
      
      ts_mort<-ts(data=mortgage[,mort_rate], 
                  start=c(as.numeric(format(min(mortgage[,DATE]),"%Y")),
                        as.numeric(format(min(mortgage[,DATE]),"%m"))), frequency=12)
      
      ## limit to just when they overlap.
      together=ts.intersect(get(city1), get(city2), ts_mort)
      together=data.table(together)
      setnames(together, "get(city1)", city1)
      setnames(together, "get(city2)", city2)
      
      ## vectorize mort rate. remove from df.
      mort_vector <-together[,ts_mort]
      together<-together[,ts_mort := NULL]
      
      ## use var select.
      choicemodel<-VARselect(together, lag.max=12,type="trend", season=3, exogen = cbind(mort_vector))
      choice<-Mode(cbind(choicemodel$selection))
      
      ## when there is a tie, use the more simple model (less lags)
      choice<-min(choice)
      
      var_model=VAR(together, p=choice, type="trend",season=3, exogen = cbind(mort_vector))
      summary(var_model)
      
      ## granger causality
      ##granger<-causality(var_model)
      
      irf<-irf(var_model, n.ahead=12, impulse=city1, seed=99418, runs=250, ortho=FALSE)
      
      ## is there serial correlation remaining.
      ##serial.test(var_model, type="PT.asymptotic", lags.pt = 20)
      
      ## make plotly graph
      dt_city1_irf<-data.table(cbind(irf$Upper[[city1]], irf$irf[[city1]], irf$Lower[[city1]]))
      
      setnames(dt_city1_irf, c("city1_upper", "city2_upper", "city1", "city2", "city1_lower", "city2_lower"))
      dt_city1_irf<-dt_city1_irf[,ord := 1:.N -1 ][,city2_upper:= currency(city2_upper)][,city1_upper:= currency(city1_upper)]
      dt_city1_irf<-dt_city1_irf[,city2:= currency(city2)][,city1:= currency(city1)][,city2_lower:= currency(city2_lower)][,city1_lower:= currency(city1_lower)]
      ## we only graph the response of 

      ## make plotly graph
      irf2<-irf(var_model, n.ahead=12, impulse=city2, seed=99418, runs=500, ortho=FALSE)
      dt_city2_irf<-data.table(cbind(irf2$Upper[[city2]], irf2$irf[[city2]], irf2$Lower[[city2]]))

      setnames(dt_city2_irf, c("city1_upper", "city2_upper", "city1", "city2", "city1_lower", "city2_lower"))
      dt_city2_irf<-dt_city2_irf[,ord := 1:.N -1 ][,city2_upper:= currency(city2_upper)][,city1_upper:= currency(city1_upper)]
      dt_city2_irf<-dt_city2_irf[,city2:= currency(city2)][,city1:= currency(city1)][,city2_lower:= currency(city2_lower)][,city1_lower:= currency(city1_lower)]
      
      save(selectname1, selectname2, dt_city2_irf, dt_city1_irf, file=paste0("out/",str_replace_all(paste(selectname1, selectname2), " ", "_"),".RData"))
    }      
  }
}
