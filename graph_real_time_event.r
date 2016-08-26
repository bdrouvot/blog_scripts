#!/usr/bin/env Rscript
#
# Author: Bertrand Drouvot
# Visit my blog : http://bdrouvot.wordpress.com/
# V1.0 (2013/06)
#
# Description:
# Utility used to display graphically in real-time (auto-refresh) for a wait event:
#
#			- time waited ms
#			- number of waits
#			- ms per wait
#			- histogram of ms per wait
#
# !!!! The script does not create any objects into the database !!!!
#
# It takes a snapshot based on the v$system_event view and computes the differences with the previous snapshot
#
# Usage:
# 
# ./graph_real_time_event.r
#
# You will be prompted for:
#
#     Building the thin jdbc connection string....
#
#     host ?: <- HOST FOR THE SERVICE
#     port ?: <- PORT FOR THE SERVICE
#     service_name ?: <- SERVICE NAME
#     system password ?: <- SYSTEM PASSWORD
#     Display which event ?: <- event to display
#     Number of snapshots <- number of snapshots
#     Refresh interval (seconds): <- refresh interval 
#
# Output :  <event>.pdf : The graph saved in pdf format
#           <event>.txt : Query output
#
# !!!!! You need to adapt drv to your env to access ojdbc6.jar
#
# drv <-JDBC("oracle.jdbc.driver.OracleDriver","/ec/prod/server/oracle/olrprod1/u000/product/11.2.0.3/jdbc/lib/ojdbc6.jar")
#
# Check for new version : http://bdrouvot.wordpress.com/R-scripts/
#
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

# Which metric to display ?

cat("Display which system event (no quotation marks) ?: ")
event<-readLines(con="stdin", 1)

# How many snapshots ?
cat("Number of snapshots: ")
nb_refresh<<-readLines(con="stdin", 1)

# Refresh Interval ?
cat("Refresh interval (seconds): ")
refresh_interval<<-readLines(con="stdin", 1)

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

# Here is the connection
conn<-dbConnect(drv,conn_string,"system",system_pwd)

# My query
# select on MS_PER_WAIT is useless as it will be computed per snap
# its purpose into the select is just to give the right number of column for the output

myquery<-"
Select  to_char(sysdate,'YYYY/MM/DD HH24:MI:SS') as DATEEVT, EVENT,TOTAL_WAITS,TIME_WAITED_MICRO/1000 as TIME_WAITED_MS,1 MS_PER_WAIT 
from v$system_event 
where event='"

# Add the event
myquery<-paste(myquery,event,sep="")

# add the rest of the query
myquery<-paste(myquery,"'",sep="")

# Graph(s) will be saved into this file
file_event<-gsub(" ", "_", event,fixed=TRUE)


# X11 issue ?
x11_issue<-'no'

# Display plot to the screen and catch X11 error
tryCatch(x11(width=15,height=10),error=function(err) {
print("Not able to display, so only pdf generation...")
pdf(file=paste(file_event,".pdf",sep=""),width = 15, height = 10)
x11_issue<<-'yes'
})

# build an Axis function 
Axis<-function(side,at,skip,format) {
	if ( skip <= 1) {
	axis.POSIXct(side,at=at,format=format,labels=T,las=2)
	} else {
	axis.POSIXct(side,at=at[ ( (1:length(at)  %% skip ) == 1 )] ,format=format,labels=T,las=2)
	}
}


# Get the first values
qoutput<-dbGetQuery(conn,myquery)
qoutput$DATEEVT<-strptime(qoutput$DATEEVT,"%Y/%m/%d %H:%M:%S")

# Put query output into .txt file
capture.output(qoutput[,1:4],file=paste(file_event,".txt",sep=""), append=FALSE)

# Keep them
prev_tw<<-qoutput[,'TIME_WAITED_MS']
prev_twaits<<-qoutput[,'TOTAL_WAITS']

# So we want 4 graphs
par(mfrow =c(4,1))

# Launch the loop for the real-time graph
nb_refresh <- as.integer(nb_refresh)

for(i in seq(nb_refresh)) {
  # Get the new data
  Sys.sleep(refresh_interval)
  qoutput<-dbGetQuery(conn,myquery)
  qoutput$DATEEVT<-strptime(qoutput$DATEEVT,"%Y/%m/%d %H:%M:%S")

  # Put query output into .txt file
  capture.output(qoutput[,1:4],file=paste(file_event,".txt",sep=""), append=TRUE)

  # Keep the current value
  current_tw<-qoutput[,'TIME_WAITED_MS']
  current_twaits<-qoutput[,'TOTAL_WAITS']

  # compute difference between snap for time_waited_ms, total_waits and then compute ms_per_wait
  qoutput[,'TIME_WAITED_MS']<-current_tw-prev_tw
  qoutput[,'TOTAL_WAITS']<-current_twaits-prev_twaits

  if (qoutput[,'TOTAL_WAITS'] == 0) {
  qoutput[,'MS_PER_WAIT']<-0
  } else {
  qoutput[,'MS_PER_WAIT']<-qoutput[,'TIME_WAITED_MS']/qoutput[,'TOTAL_WAITS']
  }

  # Keep the old one
  prev_tw<<-current_tw
  prev_twaits<<-current_twaits
  
  # Add the new points to the data frame
  if (i == 1) { 
  allpoints<-qoutput
  } else {
  allpoints<-rbind(allpoints,qoutput)
  }

  # Find Y max value
  time_waited_max_g<-max(allpoints[,'TIME_WAITED_MS'])
  nb_wait_max<-max(allpoints[,'TOTAL_WAITS'])
  ms_per_wait_max<-max(allpoints[,'MS_PER_WAIT'])

  #skip for x Axis (Let's display 40 tick)
  skip<-length(allpoints[,'DATEEVT'])/40

  # How many distinct dates ?
  nb_distinct<-length(unique(allpoints[,'DATEEVT']))

  # If only one distinct date then print a point else print lines

  if (nb_distinct == 1) {
  # print a point
  plot(allpoints[,'DATEEVT'],allpoints[,'TIME_WAITED_MS'],col="blue",ylim=c(0,time_waited_max_g),xaxt="n",xlab="",ylab="TIME_WAITED_MS",main=paste("Real time for: ",event,sep=""),cex.main=2)
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  plot(allpoints[,'DATEEVT'],allpoints[,'TOTAL_WAITS'],col="blue",ylim=c(0,nb_wait_max),xaxt="n",xlab="",ylab="NB_WAITS")
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  plot(allpoints[,'DATEEVT'],allpoints[,'MS_PER_WAIT'],col="blue",ylim=c(0,ms_per_wait_max),xaxt="n",xlab="",ylab="MS_PER_WAIT")
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  hist(allpoints[,'MS_PER_WAIT'],xlab="MS_PER_WAIT",main=NULL,border="blue")
  } else {	
  # print lines
  plot(allpoints[,'DATEEVT'],allpoints[,'TIME_WAITED_MS'],col="blue",ylim=c(0,time_waited_max_g),xaxt="n",xlab="",ylab="TIME_WAITED_MS",type="l",main=paste("Real time for: ",event,sep=""),cex.main=2)
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  plot(allpoints[,'DATEEVT'],allpoints[,'TOTAL_WAITS'],col="blue",ylim=c(0,nb_wait_max),xaxt="n",xlab="",ylab="NB_WAITS",type="l")
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  plot(allpoints[,'DATEEVT'],allpoints[,'MS_PER_WAIT'],col="blue",ylim=c(0,ms_per_wait_max),xaxt="n",xlab="",ylab="MS_PER_WAIT",type="l")
  Axis(side=1,at=allpoints[,'DATEEVT'],round(skip),format="%H:%M:%S")
  hist(allpoints[,'MS_PER_WAIT'],xlab="MS_PER_WAIT",main=NULL,border="blue")
  }
}

#
cat("Please enter any key to exit:\n")
cont<-readLines(con="stdin", 1)

# save the graph(s) to a file

# If we got X11 issue then do not replace pdf file with the "empty" output graph
if (x11_issue == 'yes') {
print ("Graph has been created into pdf file")
} else {
d<-dev.copy(pdf,paste(file_event,".pdf",sep=""),width=15,heigh=10)
}
d<-dev.off()
