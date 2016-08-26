#!/usr/bin/env Rscript
#
# Author: Bertrand Drouvot
# Visit my blog : http://bdrouvot.wordpress.com/
# V1.0 (2013/05)
#
# Description:
# Utility used to display ASM diskgroup usage per DB
#
# Usage:
# 
# ./graph_asm_dg_usage.r
#
# You will be prompted for:
#
#     Building the thin jdbc connection string....
#
#     host ?: <- HOST FOR ASM INSTANCE
#     port ?: <- PORT FOR THE ASM INSTANCE
#     service_name ?: <- SERVICE NAME
#     sys as sysasm password ?: <- SYS PASSWORD
#     Display which Disk Group ?: <- Disk Group TO DISPLAY
#
# Output : <dg>_usage_per_db.txt : The result in txt format
#          <dg>_usage_per_db.pdf : The graph saved in pdf format
#
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

# which ASM service_name ?

cat("service_name for ASM ?: ")
srv_name<-readLines(con="stdin", 1)


# sys as sysasm password ?

cat("sys as sysasm password ?: ")
sys_pwd<-readLines(con="stdin", 1)

# Which dg to display ?

cat("Display which disk group (no quotation marks, no +) ?: ")
dg<-readLines(con="stdin", 1)

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

conn<-dbConnect(drv,conn_string,"sys as sysasm",sys_pwd)

# My query
myquery<-"select db_name, round(sum(space) / 1024 / 1024 / 1024,2) as SIZE_GB
  from ((select a1.name as db_name, SUM(vf.space) as space
           from V$ASM_ALIAS          a1,
                V$ASM_ALIAS          a2,
                V$ASM_ALIAS          a3,
                V$ASM_FILE           vf,
                V$ASM_DISKGROUP_STAT dg
          where a2.parent_index = a1.reference_index
            and a3.parent_index = a2.reference_index
            and (mod(a1.parent_index, 16777216) = 0)
            and a1.alias_directory = 'Y'
            and a2.alias_directory = 'Y'
            and a3.system_created = 'Y'
            and a3.alias_directory = 'N'
            and dg.group_number = a1.group_number
            and dg.group_number = vf.group_number
            and vf.file_number >= 256
            and vf.file_number = a3.file_number
            and (dg.state = 'MOUNTED' or dg.state = 'CONNECTED')
            and dg.name ='"

# Add the dg
myquery<-paste(myquery,dg,sep="")

# add the rest of the query
myquery<-paste(myquery,"'
group by a1.name) UNION ALL
        (select a1.name as db_name, SUM(vf.space) as space
           from V$ASM_ALIAS          a1,
                V$ASM_ALIAS          a2,
                V$ASM_ALIAS          a3,
                V$ASM_ALIAS          a4,
                V$ASM_FILE           vf,
                V$ASM_DISKGROUP_STAT dg
          where a2.parent_index = a1.reference_index
            and a3.parent_index = a2.reference_index
            and a4.parent_index = a3.reference_index
            and (mod(a1.parent_index, 16777216) = 0)
            and a1.alias_directory = 'Y'
            and a2.alias_directory = 'Y'
            and a3.alias_directory = 'Y'
            and a4.system_created = 'Y'
            and a4.alias_directory = 'N'
            and dg.group_number = a1.group_number
            and dg.group_number = vf.group_number
            and vf.file_number >= 256
            and vf.file_number = a4.file_number
            and (dg.state = 'MOUNTED' or dg.state = 'CONNECTED')
            and dg.name = '",sep="")

# Add the dg
myquery<-paste(myquery,dg,sep="")

# add the rest of the query
myquery<-paste(myquery,"'
group by a1.name) UNION ALL
        (select a1.name as db_name, SUM(vf.space) as space
           from V$ASM_ALIAS          a1,
                V$ASM_ALIAS          a2,
                V$ASM_ALIAS          a3,
                V$ASM_ALIAS          a4,
                V$ASM_ALIAS          a5,
                V$ASM_FILE           vf,
                V$ASM_DISKGROUP_STAT dg
          where a2.parent_index = a1.reference_index
            and a3.parent_index = a2.reference_index
            and a4.parent_index = a3.reference_index
            and a5.parent_index = a4.reference_index
            and (mod(a1.parent_index, 16777216) = 0)
            and a1.alias_directory = 'Y'
            and a2.alias_directory = 'Y'
            and a3.alias_directory = 'Y'
            and a4.alias_directory = 'Y'
            and a5.system_created = 'Y'
            and a5.alias_directory = 'N'
            and dg.group_number = a1.group_number
            and dg.group_number = vf.group_number
            and vf.file_number >= 256
            and vf.file_number = a5.file_number
            and (dg.state = 'MOUNTED' or dg.state = 'CONNECTED')
            and dg.name = '",sep="")


# Add the dg
myquery<-paste(myquery,dg,sep="")

# add the rest of the query
myquery<-paste(myquery,"'group by a1.name))
 group by db_name
union all
select 'FREE' DB_NAME, round(NVL(dg.free_mb, 0)/1024,2)
from V$ASM_DISKGROUP_STAT dg
where dg.name = '",sep="")

# Add the dg
myquery<-paste(myquery,dg,sep="")

# add the rest of the query
myquery<-paste(myquery,"' order by 1",sep="")

# Launch the query
dg_space<-dbGetQuery(conn,myquery)

# Save the query output to a file
file_dg_space<-gsub(" ", "_",dg,fixed=TRUE)
capture.output(dg_space,file=paste(file_dg_space,"_usage_per_db.txt",sep=""), append=FALSE)

# X11 issue
x11_issue<-'no'


# Display the graph to the screen and catch X11 error
tryCatch(x11(width=15,height=10),error=function(err) {
print("Not able to display, so only pdf generation...")
pdf(file=paste(file_dg_space,"_usage_per_db.pdf",sep=""),width = 15, height = 10)
x11_issue<<-'yes'
})

# compute percentages
pct <- round(dg_space[,'SIZE_GB']/sum(dg_space[,'SIZE_GB'])*100)

# add % 
pct <- paste(pct,"%",sep="")

# Add db size to db_name
db_name_size<-paste(dg_space[,'DB_NAME']," (",sep="")
db_name_size<-paste(db_name_size,dg_space[,'SIZE_GB'],sep="")
db_name_size<-paste(db_name_size," GB)",sep="")

# Set the colors
colors<-rainbow(length(dg_space[,'DB_NAME']))

# Plot
pie(dg_space[,'SIZE_GB'], labels = pct, col=colors, main=paste(dg," Disk Group Usage",sep=""))

# Add a legend
legend(x=1.2,y=0.5,legend=db_name_size,fill=colors,cex=0.8)

# Any key to exit
cat("Please enter any key to exit:\n")
cont<-readLines(con="stdin", 1)

# save the plot to a file
# If we got X11 issue then do not replace pdf file with the "empty" output graph
if (x11_issue == 'yes') {
print ("Graph has been created into pdf file")
} else {
d<-dev.copy(pdf,paste(file_dg_space,"_usage_per_db.pdf",sep=""),width=15,heigh=10)
}
d<-dev.off()
