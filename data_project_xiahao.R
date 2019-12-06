suppressMessages(require(data.table))
suppressMessages(require(lubridate))
suppressMessages(require(zoo))
suppressWarnings(require(dplyr))
suppressWarnings(require(moments))
rm(list=ls())

########################## Cleaning up CRSP data ########################## 
crsp_monthly<- as.data.table(read.csv("crsp_monthly.csv"))

crsp_monthly[, date:= ymd(date)]

setorder(crsp_monthly, PERMCO)

# Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
crsp_monthly <- crsp_monthly[SHRCD %in% c(10,11)]
crsp_monthly <- crsp_monthly[EXCHCD %in% c(1,2,3)]

# Filter out missing ret and dlret data
for(i in c('RET','DLRET')){
  crsp_monthly[,paste0(i) := as.character(get(i))]
  crsp_monthly[get(i) %in% c('', ' ','A','C','P','S','T','B','C'), paste0(i) := NA]
  crsp_monthly[, paste0(i) := as.numeric(get(i))]
  crsp_monthly[get(i) %in% c(-66,-77,-88,-99), paste0(i) := NA]
}

# convert the ret and delisting return into numeric data type for easy calculation
crsp_monthly[, PRC := abs(as.numeric(as.character(PRC)))]
crsp_monthly[, SHROUT := as.numeric(as.character(SHROUT))]

# calculates the cum-Dividend returns
crsp_monthly[, `:=`(Ret, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]

# Market Cap and find the MktCap aggregate by PERMCO, which is the same firm
crsp_monthly[, Mkt_cap := abs(PRC) * SHROUT/1000]
setorder(crsp_monthly, PERMNO, date)

# set the year and month as integer
crsp_monthly[, Year:= year(date)]
crsp_monthly[, Month:= month(date)]
######################################################################################## 


##################################### Cleaning up compustat data #####################################
compustat_annual<- as.data.table(read.csv("compustat.csv"))
linktable<- as.data.table(read.csv("linktable.csv"))

merged <- merge(crsp_monthly, linktable, by.x='PERMCO', by.y = 'LPERMCO', allow.cartesian = T)
setkey(merged)


merged[,LINKDT := ymd(LINKDT)]
merged[LINKENDDT == 'E', LINKENDDT := NA]
merged[,LINKENDDT := ymd(LINKENDDT)]
merged <- merged[(is.na(LINKDT) | date >= LINKDT) & (is.na(LINKENDDT) | date <= LINKENDDT)]
setorder(merged, GVKEY, date)

# Multiple GVKEYs per PERMCO

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LINKTYPE == 'LC'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P linkprim, only keep p
merged[, prob := .N > 1, by= .(PERMCO, date)]
merged[, Good_match := sum(LINKPRIM == 'P'), by =.(PERMCO, date)]
merged <- merged[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not liid, only keep 1 
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(LIID == 1), by =.(PERMCO,date)]
merged <- merged[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
merged <- merged[!(prob==T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match := NULL]
merged[is.na(LINKENDDT), LINKENDDT := as.Date('2019-12-31', '%Y-%m-%d')]
merged[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged <- merged[!(prob==T & Good_match != T)]

### Sixth, use the GVKEY that has been around the longest
merged[, prob := .N > 1, by = .(PERMCO, date)]
merged[, Good_match :=NULL]
setorder(merged, GVKEY, LINKDT)
merged[prob == T, start_Date := LINKDT[1], by = .(GVKEY)]
setorder(merged, GVKEY, LINKENDDT)
merged[prob == T, end_Date := LINKENDDT[.N], by = .(GVKEY)]
merged[, Date_diff := as.integer(end_Date - start_Date)]
setorder(merged, PERMCO, date, Date_diff)
merged[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
merged <- merged[!(prob == T & Good_match != T)]

### Seventh, use the smaller GVKEY
setorder(merged, PERMCO, date, GVKEY)
merged <- unique(merged, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
merged <- merged[, .(GVKEY, date, EXCHCD, Mkt_cap, PERMCO, PERMNO, Ret , Year, Month)]
#####################################################################################


################################ Merge CRSP and Compustat together ############################################### 
compustat_merged_data <- compustat_annual[, .(gvkey, fyear, sale)]

finaldata <- merged %>%
  left_join(compustat_merged_data,by =c("GVKEY"="gvkey","Year"="fyear"))%>%
  select(GVKEY, date, PERMNO, PERMCO, EXCHCD, Ret, Mkt_cap,sale, Year,Month) %>% as.data.table
#####################################################################################

############################### get sales growth ############################### 
setorder(finaldata, PERMNO, Year, Month)
finaldata[, "lag_sale":= shift(sale, n = 6, type="lag"),by =.(PERMCO)]
finaldata[, "GS" := log(lag_sale/shift(lag_sale, n=12)), by =.(PERMCO)]

# rebalance at April
crsp_mergedBy_April <- finaldata[Month == 6, ]

# average of growth sales
crsp_mergedBy_April[, avg5yrGS := shift(rollapply(GS, 5, function(x){mean(x, na.rm = T)}, fill = NA, align="right", partial = T)), by = c("PERMCO")]
setorder(crsp_mergedBy_April, Year)
crsp_mergedBy_April[, GS_decile := findInterval(avg5yrGS, quantile(avg5yrGS, seq(0.1,0.9,0.1), na.rm =T), left.open = T) + 1,by = .(Year)]

setorder(finaldata, Year, Month)
setorder(crsp_mergedBy_April, Year)

GS_decile_data <- crsp_mergedBy_April[!is.na(GS_decile), .(PERMCO, Year, avg5yrGS, GS_decile)]
crsp_size_merged <- merge(finaldata, GS_decile_data, by.x = c("PERMCO","Year"), by.y= c("PERMCO", "Year") , all.x = T, allow.cartesian = T)
crsp_size_merged <- crsp_size_merged[Year > 1989]
crsp_size_merged <- crsp_size_merged[!is.na(GS_decile)]

GS_portfolio <- crsp_size_merged[,.(GS_Ret = mean(Ret, na.rm = TRUE)), .(Year, Month, GS_decile)]

setkey(GS_portfolio, Year, Month, GS_decile)


# get risk free rate 
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))

# convert date to date datatype
FF_mkt[, X:= ymd(X, truncated = 1)]

# change from percentage to decimal
FF_mkt[, RF := RF/100]

# add year month column
FF_mkt[,Year:= year(X)]
FF_mkt[,Month:= month(X)]

# 1990 to 2018
FF_mkt <- FF_mkt[Year>=1990 & Year<=2018]

FF_mkt_rf <- FF_mkt[, .(Year, Month, RF)]

merge_port <- merge(GS_portfolio,FF_mkt_rf, by=c("Year", "Month"))

# create an empty matrix to get excess return stats
summary <- matrix(0, nrow = 4, ncol =11)

result <- merge_port[, list(MeanExRet = mean(GS_Ret - RF), StdDev = sd(GS_Ret - RF)),  by = GS_decile]
summary[1, 1:10] <- result$MeanExRet * 12
summary[2, 1:10] <- result$StdDev * sqrt(12)
summary[3, 1:10] <- summary[1, 1:10]/summary[2, 1:10]
summary[4, 1:10]  <- merge_port[,  list(sk_m = skewness(log(1+GS_Ret))), by = GS_decile]$sk_m
colnames(summary) <- c("Decile 1","Decile 2","Decile 3","Decile 4","Decile 5"
                       ,"Decile 6","Decile 7","Decile 8","Decile 9","Decile 10","WML")
rownames(summary) <- c("Excess Return","Volatility", "SR", "SK(m)")
summary[1, 1:10] <- summary[1, 1:10] * 100
summary[2, 1:10] <- summary[2, 1:10] * 100

# winner minus losers
winner <- merge_port[GS_decile ==1,]
loser <- merge_port[GS_decile ==10,]
wml <-  winner$GS_Ret - loser$GS_Ret

summary[1, 11] <- mean(wml)*12 * 100
summary[2, 11] <- sd(wml) * sqrt(12) * 100
summary[3, 11] <- summary[1, 11]/summary[2, 11]
summary[4, 11] <- skewness(log(1 + wml + merge_port[GS_decile ==10, RF]))

# construct the summary table to be used in the report
write.table(summary, file = "summary.csv", row.names=FALSE, sep=",")
write.table(merge_port, file = "gs_return.csv", row.names=FALSE, sep=",")

## SP 500 benchmarkt portfolio
sp500_mkt <- as.data.table(read.csv("sp500_vwretd.csv"))
sp500_mkt[, date:= ymd(caldt)]
# set the year and month as integer
sp500_mkt[, Year:= year(date)]
sp500_mkt[, Month:= month(date)]
write.table(sp500_mkt, file = "sp500_mkt.csv", row.names=FALSE, sep=",")
