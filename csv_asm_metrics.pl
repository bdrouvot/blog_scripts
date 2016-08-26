#!/usr/bin/env perl

#
# Author: Bertrand Drouvot
# Visit my blog : http://bdrouvot.wordpress.com/
# V1.0 (2014/07)
#
# Description:
#
# Utility used to create a csv file from the asm_metrics.pl output.
# So that you can graph/visualize the metrics with the tool of your choice accepting csv as input.
#
# Remark:
#
# asm_metrics.pl needs to be previously launched showing all the fields i.e:
#
#    -show=inst,dbinst,fg,dg,dsk for ASM >= 11g
#    -show=inst,fg,dg,dsk for ASM < 11g
#
# Usage:
#
# ./csv_asm_metrics.pl -help
#
# Chek for new version : https://github.com/bdrouvot/csv_asm_metrics 
#
#----------------------------------------------------------------#


use strict;
use Time::Local;
use Getopt::Long;

##  Variables
##
our $infic;
our $outfic;
our $ligne;
our $help=0;
our $maxsize=0;
our $header=0;
our $prevhour=-1;
our $dayfirst=0;
our $year=0;
our $month=0;
our $day=0;
our $hour=0;
our $write_read_err=0;
our @date_tab=0;

sub main {
&get_the_options(@ARGV);

if (!-e "$infic")
{
print "\n $infic file does not exist : Exiting....\n";
exit 0;
}

########## Extract Year, Month and Day

@date_tab = split (/\//,$dayfirst);
$year=$date_tab[0];
$month=$date_tab[1];
$day=$date_tab[2];

######## Open and read the input file

open(ASMFILE,"$infic");
my(@lines) = <ASMFILE>;
close (ASMFILE);

######## Open the output file

open (OUTFILE,">$outfic");

######## Remove some lignes

&no_comment( \@lines );
&no_minus( \@lines );

# Get the maximum number of fields

foreach my $line ( @lines ) {
# drop tab
$line =~ s/\t+/ /g;
# drop blanks
$line =~ s/\s+/ /g;
#Split each line into pieces based on blanks
my @tab1 = split (/ +/,$line);
my $size = @tab1;
if ($size > $maxsize) {
$maxsize=$size;
}
}

# Now put into the output file only the rows matching max number of fields

foreach my $line ( @lines ) {
# drop tab
$line =~ s/\t+/ /g;
# drop blanks
$line =~ s/\s+/ /g;
#Split each line into pieces based on blanks
my @tab1 = split (/ +/,$line);
my $size = @tab1;
if ($size == $maxsize) {
if (($tab1[1] eq 'INST') && ($header == 0)) {
# change some fields
$tab1[0]='Snap Time';
for (my $i = 1; $i < @tab1; $i++) {
($tab1[$i] eq 'Read/s') ? $tab1[$i] = 'Kby Read/s' : $tab1[$i] = $tab1[$i];
($tab1[$i] eq 'Read') ? $tab1[$i] = 'By/Read' : $tab1[$i] = $tab1[$i];
($tab1[$i] eq 'Write/s') ? $tab1[$i] = 'Kby Write/s' : $tab1[$i] = $tab1[$i];
($tab1[$i] eq 'Write') ? $tab1[$i] = 'By/Write' : $tab1[$i] = $tab1[$i];
if ($tab1[$i] eq 'Errors') {
if ($write_read_err==0)
{
$tab1[$i]='Read Errors';
} else {
$tab1[$i]='Write Errors';
}
$write_read_err++;
}
}
# rebuild a line with comma 
$line=join(",",@tab1);
printf OUTFILE "$line \n";
$header++;
}
if ($tab1[1] ne 'INST') {
# Get the hour
$hour=substr($tab1[0], 0, index($tab1[0], ':'));
if ($hour < $prevhour) {
# This is a new day, let's find it
my $secs = timegm(0,0,0,$day,$month -1 ,$year);

# Add one day in seconds
$secs = $secs + 86400;
# Revert seconds to DMYHMS
@date_tab= localtime($secs);

# Extract year, month and day from this new date
$year=$date_tab[5] + 1900;
$month=$date_tab[4] + 1;
$day=$date_tab[3];
$dayfirst=$year.'/'.$month.'/'.$day;
}
$tab1[0]=$dayfirst.' '.$tab1[0];
$line=join(",",@tab1);
printf OUTFILE "$line \n";
}
$prevhour=$hour;
}
}
close (OUTFILE);
}

sub get_the_options {
my $help;
GetOptions('help|h' => \$help,
          'if:s'=>\$infic,
          'of:s'=>\$outfic,
          'd:s'=>\$dayfirst) or &usage();
&usage() if ($help);
}


sub no_comment {
  my ($ref_tableau) = @_;
  return keys %{ { map { s/\.\..*$//g } @{$ref_tableau} } };
}

sub no_minus {
  my ($ref_tableau) = @_;
  return keys %{ { map { s/---.*$//g } @{$ref_tableau} } };
}

sub usage {
print " \nUsage: $0 [-if] [-of] [-d] [-help]\n";
print "\n";
printf ("  %-15s   %-75s  \n",'Parameter','Comment');
printf ("  %-15s   %-75s  \n",'---------','-------');
printf ("  %-15s   %-75s  \n",'-if=','Input file name (output of asm_metrics)');
printf ("  %-15s   %-75s  \n",'-of=','Output file name (the csv file)');
printf ("  %-15s   %-75s  \n",'-d=','Day of the first snapshot (YYYY/MM/DD)');
print ("\n");
print ("Example: $0 -if=asm_metrics.txt -of=asm_metrics.csv -d='2014/07/04'\n");
print "\n\n";
exit 1;
}

&main(@ARGV);
