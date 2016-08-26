#!/usr/bin/env Rscript
#
# Author: Bertrand Drouvot
# Visit my blog : http://bdrouvot.wordpress.com/
# V1.0 (2013/06)
#
# Description:
# Utility used to display the database activity graphically in real-time (auto-refresh):
#
# It provides 3 graphs for:
#
# - The wait class distribution refreshed for each snap interval
# - The wait event distribution of the wait class having the max time waited during the last snap
# - The wait class activity distribution since the script has been launched
#
# - The DB Cpu is also added so that we can see its distribution with the wait class
#
# !!!! The script does not create any objects into the database !!!!
#
# It takes a snapshot based on the v$system_event and v$sys_time_model views and computes the differences with the previous snapshot
#
# Usage:
# 
# ./graph_real_time_db_activity.r
#
# You will be prompted for:
#
#     Building the thin jdbc connection string....
#
#     host ?: <- HOST FOR THE SERVICE
#     port ?: <- PORT FOR THE SERVICE
#     service_name ?: <- SERVICE NAME
#     system password ?: <- SYSTEM PASSWORD
#     Number of snapshots <- number of snapshots
#     Refresh interval (seconds): <- refresh interval 
#
# Output :  <service_name>_activity.pdf : The graph saved in pdf format
#           <service_name>_activity.txt : Snaps computations
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

myquery<-"
select to_char(sysdate,'YYYY/MM/DD HH24:MI:SS') as DATEEVT,
wait_class as WAIT_CLASS,
time_waited_micro/1000 as TIME_WAITED_MS,
EVENT as EVENT
from v$system_event where WAIT_CLASS != 'Idle'
union
select to_char(sysdate,'YYYY/MM/DD HH24:MI:SS') as DATEEVT,
'CPU' as WAIT_CLASS
,sum(value/1000) as TIME_WAITED_MS
,'CPU' as EVENT
from
v$sys_time_model
where
stat_name IN ('background cpu time', 'DB CPU')
"

# Graph(s) will be saved into this file
file_db_activity<-gsub(" ", "_",srv_name,fixed=TRUE)


# X11 issue ?
x11_issue<-'no'

tryCatch(x11(width=15,height=10),error=function(err) {
print("Not able to display, so only pdf generation...")
pdf(file=paste(file_db_activity,"_activity.pdf",sep=""),width = 15, height = 10)
x11_issue<<-'yes'
})

# Set bg to light grey for "erase.screen" to work
par (bg="lightgrey")


# Split the screen
no_display<-split.screen( figs = c( 2, 1 ) )

# Split the second screen
no_display<-split.screen( figs = c( 1, 2 ), screen=2 )


######################## FUNCTIONS ##############################

# build an Axis function 
Axis<-function(side,at,skip,format) {
	if ( skip <= 1) {
	axis.POSIXct(side,at=at,format=format,labels=T,las=2)
	} else {
	axis.POSIXct(side,at=at[ ( (1:length(at)  %% skip ) == 1 )] ,format=format,labels=T,las=2)
	}
}

# Build the compute_xlim function
compute_xlim<-function(current_wait_class_values) {
my_first_date<<-as.POSIXlt(tail(current_wait_class_values[,'DATEEVT'],1))
my_last_date<<-my_first_date

# compute the last date (Add 3 minutes to the initial date)
my_last_date$sec<-min(my_last_date$sec+180,my_last_date$sec+((nb_refresh+3)*as.integer(refresh_interval)))

my_first_date<<-as.POSIXct(my_first_date)
my_last_date<<-as.POSIXct(my_last_date)

}

# Build the init_graph function
init_graph<-function(what){

  #Add extra space to right of plot area for the legend
  par(mar=c(5.1, 4.1, 4.1, 12.1), xpd=TRUE)
   
  # Initialize the graph
  plot(NULL,NULL,ylim=yrange_time_waited_ms,xaxt="n",xlab="",ylab="TIME_MS",main=paste("Activity of ",srv_name,sep=""),cex.main=2,xlim=c(my_first_date,my_last_date))

  # Put the legend
  legend("topright",legend=unique(wait_class_only[,what]),fill=colors,cex=0.8,inset=c(-0.15,0.4))

  # Disable change clipping to figure
  par(mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)

  # Print the Axis
  Axis(side=1,at=unique(allpoints_wait_class[,'DATEEVT']),skip=1,format="%H:%M:%S")

}

# Build the compute_sum_by_wait_class function

compute_sum_by_wait_class<-function(qoutput){

# Compute sum by wait class
wait_class_only<<-rowsum(qoutput[,'TIME_WAITED_MS'], qoutput[,'WAIT_CLASS'])

# Convert to data frame
wait_class_only<<-as.data.frame(wait_class_only)

# Label the column
colnames(wait_class_only)<<-c("TIME_WAITED_MS")

# Add the DATEEVT column
wait_class_only["DATEEVT"]<<-unique(qoutput$DATEEVT)

# Add the WAIT_CLASS column
wait_class_only["WAIT_CLASS"]<<-row.names(wait_class_only)

# Convert to date
wait_class_only$DATEEVT<<-strptime(wait_class_only$DATEEVT,"%Y/%m/%d %H:%M:%S")
}

######################## MAIN ##############################

# Go on screen 1
screen(1)

# Get the first values
qoutput<-dbGetQuery(conn,myquery)

# Compute sum by wait class
compute_sum_by_wait_class(qoutput)

# Convert to date
qoutput$DATEEVT<-strptime(qoutput$DATEEVT,"%Y/%m/%d %H:%M:%S")

# Extract event only (Remove the fake 'CPU' wait class)
event_only<-subset(qoutput,WAIT_CLASS!=EVENT)

# Keep them

previous_wait_class_values<<-wait_class_only
previous_event_values<<-event_only

# Launch the loop for the real-time graph
nb_refresh <- as.integer(nb_refresh)

# Set range max to 0
ymax_time_waited_ms<<-0

for(i in seq(nb_refresh)) {
  # Get the new data
  Sys.sleep(refresh_interval)
  qoutput<-dbGetQuery(conn,myquery)

  # Compute sum by wait class
  compute_sum_by_wait_class(qoutput)

  # Convert to date
  qoutput$DATEEVT<-strptime(qoutput$DATEEVT,"%Y/%m/%d %H:%M:%S")

  # Extract event only (Remove the fake 'CPU' wait class)
  event_only<-subset(qoutput,WAIT_CLASS!=EVENT)

  # Keep current values
  current_wait_class_values<-wait_class_only 
  current_event_values<-event_only

  # current dateevt
  current_dateevt<<-unique(wait_class_only[,'DATEEVT'])

  # Put results into .txt file
  #capture.output(wait_class_only[,1:4],file=paste(file_db_activity,"_activity.txt",sep=""), append=TRUE)

  # Compute for each wait_class

  for (wait_class_name in (row.names(wait_class_only))) {

  current_tw<-wait_class_only[wait_class_name,'TIME_WAITED_MS']
  previous_tw<-previous_wait_class_values[wait_class_name,'TIME_WAITED_MS']

  # compute difference between snap for time_waited_ms
  wait_class_only[wait_class_name,'TIME_WAITED_MS']<-current_tw-previous_tw

  if (i > 1) {
  # Keep the sum for the pie "activity since the beginning"
  sum_points_wait_class[wait_class_name,'TIME_WAITED_MS']<-sum_points_wait_class[wait_class_name,'TIME_WAITED_MS']+wait_class_only[wait_class_name,'TIME_WAITED_MS']
  } 
  }

  # Keep the old one
  previous_wait_class_values<-current_wait_class_values

  # Add the new points to the data frame
  if (i == 1) { 
  allpoints_wait_class<<-wait_class_only
  sum_points_wait_class<<-wait_class_only
  # Put results into .txt file
  capture.output(wait_class_only,file=paste(file_db_activity,"_activity.txt",sep=""), append=FALSE)
  } else {
  allpoints_wait_class<<-rbind(allpoints_wait_class,wait_class_only)
  # Put results into .txt file
  capture.output(wait_class_only,file=paste(file_db_activity,"_activity.txt",sep=""), append=TRUE)
  }

 
  # Find Y range 
  yrange_time_waited_ms<-range(allpoints_wait_class[,'TIME_WAITED_MS'])

  # The Y max has changed ? 
  previous_ymax_time_waited_ms<-ymax_time_waited_ms
  ymax_time_waited_ms<-max(allpoints_wait_class[,'TIME_WAITED_MS'])

  # For which wait_class is the max reached for the current data
  max_wait_class_idx<-which.max(wait_class_only[,'TIME_WAITED_MS'])

  # Compute the values for the events of the wait class having the max pour this snap
  # Subset without CPU

  no_cpu_wait_class<-wait_class_only[-grep("CPU", row.names(wait_class_only)), ]

  wait_class_having_max<-no_cpu_wait_class[which.max(no_cpu_wait_class[,'TIME_WAITED_MS']),'WAIT_CLASS']

  # compute difference between snap for time_waited_ms
 
  snap_points_event<<-event_only
   
  for (event_name in event_only[,'EVENT'][event_only[,'WAIT_CLASS']==wait_class_having_max]) {

  current_tw<-event_only[,'TIME_WAITED_MS'][event_only[,'EVENT']==event_name]
  previous_tw<-previous_event_values[,'TIME_WAITED_MS'][previous_event_values[,'EVENT']==event_name]
  snap_points_event[,'TIME_WAITED_MS'][event_only[,'EVENT']==event_name]<-current_tw-previous_tw
 
  }

  # Keep the old one
  previous_event_values<-event_only

  # Computed values for event
  snap_points_event<-subset(snap_points_event,WAIT_CLASS==wait_class_having_max)

  # Put results into .txt file
  capture.output(snap_points_event[,3:4],file=paste(file_db_activity,"_activity.txt",sep=""), append=TRUE)

  # which serie ?
  series_number<<-0

  # Set the colors
  colors<-rainbow(length(unique(wait_class_only[,'WAIT_CLASS'])))
  colors_event<-rainbow(length(unique(snap_points_event[,'EVENT'])))

  # Conditions to draw or re-draw the whole graph
  if ((ymax_time_waited_ms > previous_ymax_time_waited_ms) || (current_dateevt > my_last_date)) {
  erase.screen(1)
  if ((i == 1) || (current_dateevt > my_last_date)) { 
  # First plot or need to change X Axis lim
  compute_xlim(current_wait_class_values) 
  }
  # re-draw whole graph
  init_graph(what='WAIT_CLASS')
  } else { 
  screen(1,new=FALSE)
  # Just set the Axis without need to skip 
  Axis(side=1,at=unique(wait_class_only[,'DATEEVT']),skip=1,format="%H:%M:%S")
  }

  # Loop on series and draw
  for (wait_class_name in unique(wait_class_only[,'WAIT_CLASS'])) {

  # Set the serie number
  series_number<<-series_number+1
 
  if ((ymax_time_waited_ms > previous_ymax_time_waited_ms)) {
  # Re-draw all the points
  points(allpoints_wait_class[,'DATEEVT'][allpoints_wait_class[,'WAIT_CLASS']==wait_class_name],allpoints_wait_class[,'TIME_WAITED_MS'][allpoints_wait_class[,'WAIT_CLASS']==wait_class_name],col=colors[series_number],type="b",ylim=yrange_time_waited_ms,lwd=2)
  } else {
  # Draw the ligne between the last 2 points
  last_two_dates<-tail(allpoints_wait_class[,'DATEEVT'][allpoints_wait_class[,'WAIT_CLASS']==wait_class_name],2)
  last_two_time_waited_ms<-tail(allpoints_wait_class[,'TIME_WAITED_MS'][allpoints_wait_class[,'WAIT_CLASS']==wait_class_name],2)
  points(last_two_dates,last_two_time_waited_ms,col=colors[series_number],type="b",ylim=yrange_time_waited_ms,lwd=2)
  }
  }

  # Draw the pie for events distribution
  screen(3,new=TRUE)
 
  # Put margin
  par(mar=c(5.1,0,4.1,2.1))

  pct <- round(snap_points_event[,'TIME_WAITED_MS']/sum(snap_points_event[,'TIME_WAITED_MS'])*100)
  pct <- paste(pct,"%",sep="")
  pie(snap_points_event[,'TIME_WAITED_MS'], labels = pct, col=colors_event, main=paste("Events distribution for last snap for ",wait_class_having_max,sep=""))
  legend(x=1.05,y=1,legend=snap_points_event[,'EVENT'],fill=colors_event,cex=0.78)
 
  # Put margin
  par(mar=c(5.1,4.4,4.1,2.1)) 
  
  # Draw the pie for the activity distribution since the beginning
  screen(4,new=TRUE)
  pct <- round(sum_points_wait_class[,'TIME_WAITED_MS']/sum(sum_points_wait_class[,'TIME_WAITED_MS'])*100)
  pct <- paste(pct,"%",sep="")
  pie(sum_points_wait_class[,'TIME_WAITED_MS'], labels = pct, col=colors, main="Activity distribution since the beginning")
  legend(x=1.2,y=1,legend=sum_points_wait_class[,'WAIT_CLASS'],fill=colors,cex=0.8)

  # Back to screen 1
  screen(1,new=FALSE)
}

cat("Please enter any key to exit:\n")
cont<-readLines(con="stdin", 1)

# save the graph(s) to a file

# If we got X11 issue then do not replace pdf file with the "empty" output graph
if (x11_issue == 'yes') {
print ("Graph has been created into pdf file")
} else {
d<-dev.copy(pdf,paste(file_db_activity,"_activity.pdf",sep=""),width=15,heigh=10)
}
d<-dev.off()
