#!/usr/bin/env perl
#
# Author: Bertrand Drouvot
# csv_exadata_metrics_history.pl : V1.0 (2015/06)
# Visit my blog : http://bdrouvot.wordpress.com/
#
# Utility used to extract exadata METRICHISTORY in a CSV format
# Then feel free to graph with the tool of your choice
# A cell os user allowed to run dcli without password (celladmin for example) can launch the script (ORACLE_HOME must be set).
#
#
#----------------------------------------------------------------#

BEGIN {
die "ORACLE_HOME not set\n" unless $ENV{ORACLE_HOME};
unless ($ENV{OrAcLePeRl}) {
$ENV{OrAcLePeRl} = "$ENV{ORACLE_HOME}/perl";
$ENV{PERL5LIB} = "$ENV{PERL5LIB}:$ENV{OrAcLePeRl}/lib:$ENV{OrAcLePeRl}/lib/site_perl";
$ENV{LD_LIBRARY_PATH} = "$ENV{LD_LIBRARY_PATH}:$ENV{ORACLE_HOME}/lib32:$ENV{ORACLE_HOME}/lib";
exec "$ENV{OrAcLePeRl}/bin/perl", $0, @ARGV;
}
}

use strict;
use Time::Local;

#
# Variables
#
my $nbmatch=-1;
my $help=0;
my $goodparam=0;

my $cell_pattern='';
my $dclicomm='';
my $groupcell_pattern='';
my $name_pattern='ALL';
my $dcli_serial_ornot='dcli ';
my $metrictype_pattern='ALL';
my $metricobjectname_pattern='ALL';
my $notname_pattern='EMPTY_PATTERN';
my $notmetricobjectname_pattern='EMPTY_PATTERN';
my $agovalue_pattern=1;
my $agounit_pattern='hour';
my $result;

# Parameter parsing

foreach my $para (@ARGV) {

if ( $para =~ m/^help.*/i ) {
$nbmatch++;
$help=1;
}

if ( $para =~ m/^serial(.*)$/i ) {
$nbmatch++;
$dcli_serial_ornot='dcli --serial ';
}

if ( $para =~ m/^type=(.*)$/i ) {
$nbmatch++;
$metrictype_pattern=$1;
}

if ( $para =~ m/^name=(.*)$/i ) {
$nbmatch++;
$name_pattern=$1;
}

if ( $para =~ m/^objectname=(.*)$/i ) {
$nbmatch++;
$metricobjectname_pattern=$1;
}

if ( $para =~ m/^name!=(.*)$/i ) {
$nbmatch++;
$notname_pattern=$1;
}

if ( $para =~ m/^objectname!=(.*)$/i ) {
$nbmatch++;
$notmetricobjectname_pattern=$1;
}

if ( $para =~ m/^ago_value=(.*)$/i ) {
$nbmatch++;
$agovalue_pattern=$1;
}

if ( $para =~ m/^ago_unit=(.*)$/i ) {
$nbmatch++;
$agounit_pattern=$1;
}

if ( $para =~ m/^cell=(.*)$/i ) {
$nbmatch++;
$cell_pattern=$1;
$goodparam++;
}

if ( $para =~ m/^groupfile=(.*)$/i ) {
$nbmatch++;
$groupcell_pattern=$1;
$goodparam++;
}
}

# Check if cell pattern and groupfile are empty

if ((!$goodparam) | $goodparam > 1) {
print "\n Error while processing parameters : CELL or GROUPFILE (mutually exclusive) parameter is mandatory !!! \n\n" unless ($help);
$help=1;
}

# Print usage if a difference exists between parameters checked
#
if ($nbmatch != $#ARGV | $help) {
print "\n Error while processing parameters \n\n" unless ($help);
print " \nUsage: $0 [cell=|groupfile=] [serial] [type=] [name=] [objectname=] [name!=] [objectname!=] [ago_unit=] [ago_value=]  \n\n";

printf (" %-25s %-60s %-10s \n",'Parameter','Comment','Default');
printf (" %-25s %-60s %-10s \n",'---------','-------','-------');
printf (" %-25s %-60s %-10s \n",'CELL=','comma-separated list of cells','');
printf (" %-25s %-60s %-10s \n",'GROUPFILE=','file containing list of cells','');
printf (" %-25s %-60s %-10s \n",'SERIAL','serialize execution over the cells (default is no)','');
printf (" %-25s %-60s %-10s \n",'TYPE=','Metrics type to extract: Cumulative|Rate|Instantaneous ','ALL');
printf (" %-25s %-60s %-10s \n",'NAME=','Metrics to extract (wildcard allowed)','ALL');
printf (" %-25s %-60s %-10s \n",'OBJECTNAME=','Objects to extract (wildcard allowed)','ALL');
printf (" %-25s %-60s %-10s \n",'NAME!=','Exclude metrics (wildcard allowed)','EMPTY');
printf (" %-25s %-60s %-10s \n",'OBJECTNAME!=','Exclude objects (wildcard allowed)','EMPTY');
printf (" %-25s %-60s %-10s \n",'AGO_UNIT=','Unit to retrieve historical metrics back: day|hour|minute','HOUR');
printf (" %-25s %-60s %-10s \n",'AGO_VALUE=','Value associated to Unit to retrieve historical metrics back',1);
print ("\n");
print ("utility assumes passwordless SSH from this cell node to the other cell nodes\n");
print ("utility assumes ORACLE_HOME has been set (with celladmin user for example)\n");
print ("\n");
print ("Example : $0 cell=cell\n");
print ("Example : $0 groupfile=./cell_group\n");
print ("Example : $0 cell=cell objectname='CD_disk03_cell' \n");
print ("Example : $0 cell=cell name='.*BY.*' objectname='.*disk.*' \n");
print ("Example : $0 cell=enkcel02 name='.*DB_IO.*' objectname!='ASM' name!='.*RQ.*' ago_unit=minute ago_value=4 \n");
print ("Example : $0 cell=enkcel02 type='Instantaneous' name='.*DB_IO.*' objectname!='ASM' name!='.*RQ.*' ago_unit=hour ago_value=4 \n");
print ("Example : $0 cell=enkcel01,enkcel02 type='Instantaneous' name='.*DB_IO.*' objectname!='ASM' name!='.*RQ.*' ago_unit=minute ago_value=4 serial \n");
print "\n\n";
exit 0;
}

# Build the dcli command

if ($metrictype_pattern =~ /all/i) {
$metrictype_pattern = ""
}
else
{
$metrictype_pattern = " and metricType like \\'".$metrictype_pattern."\\' "
}

if ($name_pattern =~ /all/i) {
$name_pattern = ""
}
else
{
$name_pattern = " and name like \\'".$name_pattern."\\' "
}

if ($metricobjectname_pattern =~ /all/i) {
$metricobjectname_pattern = ""
}
else
{
$metricobjectname_pattern =  " and metricObjectName like \\'".$metricobjectname_pattern."\\' "
}

if ($notname_pattern =~ /EMPTY_PATTERN/i) {
$notname_pattern = ""
}
else
{
$notname_pattern = " and name not like \\'".$notname_pattern."\\' "
}

if ($notmetricobjectname_pattern =~ /EMPTY_PATTERN/i) {
$notmetricobjectname_pattern = ""
}
else
{
$notmetricobjectname_pattern =  " and metricObjectName not like \\'".$notmetricobjectname_pattern."\\' "
}


my $cellclicom='"cellcli -e \"LIST METRICHISTORY attributes metricType,collectionTime,name,metricObjectName,metricValue WHERE collectionTime > \'\"`date --date \''.$agovalue_pattern.' '.$agounit_pattern.' ago\' \"+%Y-%m-%dT%H:%M:%S%:z\"`\"\'\"';

#print "$cellclicom\n";

if ($cell_pattern)
{
$dclicomm=$dcli_serial_ornot."-c ".$cell_pattern." ".$cellclicom.$name_pattern.$metricobjectname_pattern.$notname_pattern.$notmetricobjectname_pattern.$metrictype_pattern."\"";
}
# Then group file is used
else
{
$dclicomm=$dcli_serial_ornot."-g ".$groupcell_pattern." ".$cellclicom.$name_pattern.$metricobjectname_pattern.$notname_pattern.$notmetricobjectname_pattern.$metrictype_pattern."\"";
}

#print "$dclicomm\n";

# Launch the dcli command
my $result=`$dclicomm` ;

if ( $? != 0 ) 
{ 
print "\n";
print "\n";
die "Something went wrong executing [$dclicomm]\n"; 
}

# Print the header
print "Cell;metricType;DateTime;name;objectname;value;unit\n";

# Split the string into array
my @array_result = split(/\n/, $result);

foreach my $line ( @array_result ) {
# drop tab
$line =~ s/\t+/ /g;
# drop blanks
$line =~ s/\s+/ /g;

#Split each line on 7 pieces based on blanks
my @tab1 = split (/ +/,$line,7);

# Supress : from the cell name
$tab1[0] =~ s/://;

# Add "N/A" if no Unit
if ($tab1[6] eq "") {$tab1[6]="N/A"};

# join the element with ";" and print the result
print join(';',@tab1)."\n";
}
