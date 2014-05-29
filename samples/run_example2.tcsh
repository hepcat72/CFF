#!/bin/tcsh

#USAGE: tcsh run_example2.tcsh

setenv STARTTIME   `perl -e 'print(scalar(time()))'`
setenv Z           2
setenv K           2
setenv MAG         30
setenv INDIR       Caporaso_FASTQ
setenv ANALYSISDIR Caporaso_FASTQ_out
setenv GLOB        "L6S2?.fastq"
setenv LIB         example2.glib

if ( -e $ANALYSISDIR ) then
  echo ''
  echo 'Removing previous run'
  \rm -rf $ANALYSISDIR
endif

echo ''
echo "RUNNING run_example2.tcsh"
echo "-------------------------"
echo "Start time:                  "`date`
echo "Z-score threshold:           $Z"
echo "Magnitude over N0 Threshold: $MAG"
echo "Nominations threshold:       $K"
echo "INPUT DIRECTORY:             $INDIR"
echo "File Pattern:                $GLOB"
echo "OUTPUT DIRECTORY:            $ANALYSISDIR"
echo ''

echo -n "mergeSeqs.pl     $INDIR/$GLOB -f $LIB --outdir $ANALYSISDIR -o .lib -p ''"
mergeSeqs.pl $INDIR/$GLOB -f $LIB --outdir $ANALYSISDIR -o .lib -p '' --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command mergeSeqs.pl failed"
 goto scriptend
endif

echo -n "neighbors.pl     $ANALYSISDIR/$LIB -o .nbrs"
neighbors.pl $ANALYSISDIR/$LIB -o .nbrs --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command neighbors.pl failed"
 goto scriptend
endif

#Don't need to run this
#errorRates.pl $ANALYSISDIR/$LIB -n $ANALYSISDIR/$LIB.nbrs -h .zhist --overwrite

echo -n "errorRates.pl    $ANALYSISDIR/$LIB -n $ANALYSISDIR/$LIB.nbrs -z $Z -o .erates"
errorRates.pl $ANALYSISDIR/$LIB -n $ANALYSISDIR/$LIB.nbrs -z $Z -o .erates --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command errorRates.pl failed"
 goto scriptend
endif

echo -n "nZeros.pl        $ANALYSISDIR/$GLOB.lib -n $ANALYSISDIR/$LIB.nbrs -r $ANALYSISDIR/$LIB.erates -o .n0s"
nZeros.pl $ANALYSISDIR/$GLOB.lib -n $ANALYSISDIR/$LIB.nbrs -r $ANALYSISDIR/$LIB.erates -o .n0s --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command nZeros.pl failed"
 goto scriptend
endif

echo -n "getCandidates.pl $ANALYSISDIR/$GLOB.lib.n0s -o .cands -f .rejects -h $MAG"
getCandidates.pl $ANALYSISDIR/$GLOB.lib.n0s -o .cands -f .rejects --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command getCandidates.pl failed"
 goto scriptend
endif

echo -n "getReals.pl      $ANALYSISDIR/$GLOB.lib.n0s.cands -d '$ANALYSISDIR/$GLOB.lib' -f $ANALYSISDIR/$LIB -k $K"
getReals.pl $ANALYSISDIR/$GLOB.lib.n0s.cands -d "$ANALYSISDIR/$GLOB.lib" -f $ANALYSISDIR/$LIB -k $K --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command getReals.pl failed"
 goto scriptend
endif

echo -n "filterIndels.pl  $ANALYSISDIR/$LIB.reals -o .filt -f .indels"
filterIndels.pl $ANALYSISDIR/$LIB.reals -o .filt -f .indels --overwrite
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

if ( $status ) then
 echo "ERROR: Command filterIndels.pl failed"
 goto scriptend
endif

perl -e 'print("\nEXAMPLE2 DONE\nOUTFILES:  $ARGV[0]/*\n")' $ANALYSISDIR

scriptend:

echo "Stop time: "`date`
perl -e 'print STDERR ("RUN TIME:  ",(scalar(time()) - $ARGV[0])," seconds\n")' $STARTTIME
