 #Data Table - Learnign how to work with it
#DT[i, j, by]
##   R:                 i                 j        by
## SQL:  where | order by   select | update  group by
#Take DT, subset/reorder rows using i, then calculate j, grouped by by.
#Source=https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
#
#
library(data.table)
#
input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}

flights <- fread(input)

###Subsetting rows
#Get all the flights with "JFK" as the origin airport in the month of June.
ans <- flights[origin == "JFK" & month == 6]
#Get the rows 4&5
ans <- flights[4:5,,]
ans <- flights[4:5]
#Sort flights first by column origin in ascending order, and then by dest in descending order
ans <- flights[order(origin,-distance)]

###Subsetting columns
#Select arr_delay column, but return it as a vector
ans <- flights[,arr_delay]
head(ans)
#Select both arr_delay and dep_delay columns
ans <- flights[,list(arr_delay,dep_delay)]
ans
#Select both arr_delay and dep_delay columns and rename them to delay_arr and delay_dep
ans <- flights[,list(delay_arr=arr_delay,delay_dep=dep_delay)]
head(ans)
#Doing in the same way that the data.frame way: select both arr_delay and dep_delay columns 
ans <- flights[, c("arr_delay", "dep_delay")]
head(ans)


###Doing computations
#How many trips have had total delay < 0?
ans <- flights[,sum((arr_delay + dep_delay)<0)]
ans
#Calculate the average arrival and departure delay for all flights with "JFK" as 
#the origin airport in the month of June.
ans <- flights[origin == "JFK" & month == 6,
               list(m.arr= mean(arr_delay),m.dep= mean(dep_delay))
ans
#How many trips have been made in 2014 from "JFK" airport in the month of June?
ans <- flights[origin == "JFK" & year == 2014 & month == 6,length(dest)]
#length determines the length of the vector, giving the number of rows
ans

ans <- flights[origin == "JFK" & year == 2014 & month == 6,.N] #.N gives the number of rows

###Aggregations
#How can we get the number of trips corresponding to each origin airport?
ans <- flights[,.N,by=.(origin)]
ans
#How can we calculate the number of trips for each origin airport for carrier code "AA"?
ans <- flights[carrier == "AA",.N,by=origin]
ans
#How can we get the total number of trips for each origin, dest pair for carrier code "AA"?
ans <- flights[carrier == "AA",.N,by=.(origin,dest)]
ans
#How can we get the average arrival and departure delay for each orig,dest pair
#for each month for carrier code "AA"?
ans <- flights[carrier == "AA",
               .(m.arr= mean(arr_delay),m.dep= mean(dep_delay)),
               by=.(origin,dest,month)]
ans
#So how can we directly order by all the grouping variables?
ans <- flights[carrier == "AA",
               .(m.arr= mean(arr_delay),m.dep= mean(dep_delay)),
               keyby=.(origin,dest,month)]
ans

#Doing cumulative sums, accorign to some specirif order and by specific key
dt.hp <- dt.hp  [order(CountryExp,DateRep),
Total.Cases.cumsum := cumsum(NewConfCases), by=.(CountryExp)]

#Chaining expressions
#So how can we directly order by all the grouping variables?
ans <- flights[carrier == "AA", .N, by = .(origin, dest)]
ans
#Sort by origin/dest
ans <- flights[carrier == "AA", .N, by = .(origin, dest)][order(origin, -dest)]
ans

#Expressions in by (not only columns)
#Number of flights that started late but arrived early (or on time), started and arrived late
ans <- flights[, .N, by =.(dep_delay>0, arr_delay>0,carrier)]
ans

ans <- flights[(dep_delay>0 OR arr_delay>0), .N, by =.(carrier)]
ans

#Special symbol .SD:
#It stands for "Subset of Data". It is a data.table by itself 
#that holds the data for the current group defined using by.

flights[carrier == "AA",                       ## Only on trips with carrier "AA"
        lapply(.SD, mean),                     ## compute the mean
        by = .(origin, dest, month),           ## for every 'origin,dest,month'
        .SDcols = c("arr_delay", "dep_delay", "air_time")] ## for just those specified in .SDcols

#How can we return the first two rows for each month?
ans <- flights[, head(.SD, 2), by = month]
head(ans)

#How can we specify just the columns we would like to compute the mean?
flights[carrier == "AA",lapply(.SD, mean),
        by = .(origin, dest, month),
        .SDcols = c("arr_delay", "dep_delay")] 

