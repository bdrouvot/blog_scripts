#!/usr/bin/env Rscript
#
# Author: Bertrand Drouvot
# Visit my blog : http://bdrouvot.wordpress.com/
# V1.0 (2013/03)
#
# Description:
# Utility used to display graphically for a wait event:
#
#			- time waited ms
#			- number of waits
#			- ms per wait
#			- histogram of ms per wait
#
# from AWR over a period of time 
#
#
# Usage:
# 
# ./graph_awr_event.r
#
# You will be prompted for:
#
#     Building the thin jdbc connection string....
#
#     host ?: <- HOST FOR THE SERVICE
#     port ?: <- PORT FOR THE SERVICE
#     service_name ?: <- SERVICE NAME
#     system password ?: <- SYSTEM PASSWORD
#     Display which event ?: <- WAIT EVENT TO DISPLAY
#     Please enter nb_day_begin_interval: <- Number of days to go back as the time frame starting point
#     Please enter nb_day_end_interval: <- Number of days to go back  as the time frame ending point
#
# Output : <event>.txt : The result in txt format
#          <event>.pdf : The graph saved in pdf format
#
#
# !!!!! You need to adapt drv to your env to access ojdbc6.jar
#
# drv <-JDBC("oracle.jdbc.driver.OracleDriver","/ec/prod/server/oracle/olrprod1/u000/product/11.2.0.3/jdbc/lib/ojdbc6.jar")
#
# Check for new version : http://bdrouvot.wordpress.com/R-scripts/
#
# Remark: You need to purchase the Diagnostic Pack in order to be allowed to query the AWR repository
#----------------------------------------------------------------#

cat ("Building the thin jdbc connection string....\n")
cat ("\n")

# which host ?

cat("host ?: ")
srv_host_name<-readLines(con="stdin", 1)

# which port ?

cat("port ?: ")
srv_port<-readLines(con="stdin", 1)

# which service_name ?

cat("service_name ?: ")
srv_name<-readLines(con="stdin", 1)


# system password ?

cat("system password ?: ")
system_pwd<-readLines(con="stdin", 1)

# Which event to display ?

cat("Display which event (no quotation marks) ?: ")
event<-readLines(con="stdin", 1)

# begin awr snap time in days ? (2 days ago, 3 days ago...)
cat("Please enter nb_day_begin_interval: ")
nb_day_begin_interval<-readLines(con="stdin", 1)

# end awr snap time in days ? (2 days ago, 3 days ago...)
cat("Please enter nb_day_end_interval: ")
nb_day_end_interval<-readLines(con="stdin", 1)

library(RJDBC)
#
# set up the JDBC connection 
# configure "drv" for your env

drv <-JDBC("oracle.jdbc.driver.OracleDriver","/ec/prod/server/oracle/olrprod1/u000/product/11.2.0.3/jdbc/lib/ojdbc6.jar")

conn_string<-"jdbc:oracle:thin:@//"
conn_string<-paste(conn_string,srv_host_name,sep="")
conn_string<-paste(conn_string,":",sep="")
conn_string<-paste(conn_string,srv_port,sep="")
conn_string<-paste(conn_string,"/",sep="")
conn_string<-paste(conn_string,srv_name,sep="")

conn<-dbConnect(drv,conn_string,"system",system_pwd)

# My query
myquery<-"
select to_char(s.begin_interval_time,'YYYY/MM/DD HH24:MI') as dateevt,e.TIME_WAITED_MS / TOTAL_WAITS as ms_per_wait,TOTAL_WAITS as TOTAL_WAITS,TIME_WAITED_MS as TIME_WAITED_MS
from
(
select instance_number,snap_id,WAIT_CLASS,event_name,
total_waits - first_value(total_waits) over (partition by event_name order by snap_id rows 1 preceding) as TOTAL_WAITS,
(time_waited_micro - first_value(time_waited_micro) over (partition by event_name order by snap_id rows 1 preceding))/1000 as TIME_WAITED_MS
from
dba_hist_system_event
where
event_name ='"

# Add the event

myquery<-paste(myquery,event,sep="")

# add the rest of the query
myquery<-paste(myquery,"'
and instance_number = (select instance_number from v$instance)
) e, dba_hist_snapshot s
where e.TIME_WAITED_MS > 0
and e.instance_number=s.instance_number
and e.snap_id=s.snap_id
and s.BEGIN_INTERVAL_TIME >= trunc(sysdate-",sep="")

# add nb_day_begin_interval
myquery<-paste(myquery,nb_day_begin_interval,sep="")

# add the rest of the query

myquery<-paste(myquery,"
+1) and s.BEGIN_INTERVAL_TIME <= trunc(sysdate-",sep="")

# add nb_day_end_interval
myquery<-paste(myquery,nb_day_end_interval,sep="")

#add the rest of the query

myquery<-paste(myquery,"+1) order by s.begin_interval_time asc",sep="")

# Import the data into a data.frame
#
sqevent<-dbGetQuery(conn,myquery)
#

# Save the query output to a file
file_event<-gsub(" ", "_", event,fixed=TRUE)
capture.output(sqevent,file=paste(file_event,".txt",sep=""), append=FALSE)

# X11 issue
x11_issue<-'no'

# Display plot to the screen and catch X11 error
tryCatch(x11(width=15,height=10),error=function(err) {
print("Not able to display, so only pdf generation...")
pdf(file=paste(file_event,".pdf",sep=""),width = 15, height = 10)
x11_issue<<-'yes'
})

#Format first field as a date
sqevent$DATEEVT<-strptime(sqevent$DATEEVT,"%Y/%m/%d %H:%M")

# build an Axis function
Axis<-function(side,at,skip,format) {
	if ( skip <= 1) {
	axis.POSIXct(side,at=at,format=format,labels=T,las=2)
	} else {
	axis.POSIXct(side,at=at[ ( (1:length(at)  %% skip ) == 1 )] ,format=format,labels=T,las=2)
	}
}


# Plot the 4 graphs
par(mfrow =c(4,1))
#skip for x Axis (Let's display 40 tick)
skip<-length(sqevent$DATEEVT)/40
plot(sqevent[,'DATEEVT'],sqevent[,'TIME_WAITED_MS'],type="l",col="blue",xaxt="n",main=event,cex.main=2,xlab="",ylab="TIME_WAITED_MS")
Axis(side=1,at=sqevent$DATEEVT,round(skip),format="%Y/%m/%d %H:%M")
plot(sqevent[,'DATEEVT'],sqevent[,'TOTAL_WAITS'],type="l",col="blue",xaxt="n",xlab="",ylab="NB_WAITS")
Axis(side=1,at=sqevent$DATEEVT,round(skip),format="%Y/%m/%d %H:%M")
plot(sqevent[,'DATEEVT'],sqevent[,'MS_PER_WAIT'],type="l",col="blue",xaxt="n",xlab="",ylab="MS_PER_WAIT")
Axis(side=1,at=sqevent$DATEEVT,round(skip),format="%Y/%m/%d %H:%M")
hist(sqevent[,'MS_PER_WAIT'],xlab="MS_PER_WAIT",main=NULL,border="blue")

#
cat("Please enter any key to exit:\n")
cont<-readLines(con="stdin", 1)

# save the plot to a file

# If we got X11 issue then do not replace pdf file with the "empty" output graph
if (x11_issue == 'yes') {
print ("Graph has been created into pdf file")
} else {
d<-dev.copy(pdf,paste(file_event,".pdf",sep=""),width=15,heigh=10)
}
d<-dev.off()
