#!/bin/tcsh

#USAGE: tcsh run_CFF_on_FastA.tcsh trimlen outdir "fasta-files"
# E.G.: tcsh run_CFF_on_FastA.tcsh 130 myanalysis "some-dir/*.fa"

setenv TRIMLEN  `echo "$argv" | cut -f 1 -d " "`
setenv MINABUND `echo "$argv" | cut -f 2 -d " "`
setenv ANALDIR  `echo "$argv" | cut -f 3 -d " "`
setenv FASTAS   `echo "$argv" | cut -f 4-999 -d " "`

setenv STARTTIME `perl -e 'print(scalar(time()))'`
setenv Z         2
setenv K         2
setenv MAG       10
setenv LIB       global_library.fna
setenv STUBS     `perl -e 'print(join(",",map {s%.*/%%;"$_"} @ARGV))' $FASTAS`

if ( -e $ANALDIR ) then
  echo
  echo "ERROR: Output directory [$ANALDIR] already exists."
  echo '       Please remove it or supply a new uncreated one.'
  echo
  exit
endif

echo
echo "RUNNING run_CFF_on_FastA.tcsh"
echo "-----------------------------"
echo "Start time:                  "`date`
echo "Trim length:                 $TRIMLEN"
echo "Z-score threshold:           $Z"
echo "Magnitude over N0 Threshold: $MAG"
echo "Nominations threshold:       $K"
echo "OUTPUT DIRECTORY:            $ANALDIR"
echo

mkdir $ANALDIR

echo -n "filterIndels.pl  $FASTAS -o .filt -f .indels --454-mode -a $MINABUND --outdir $ANALDIR/0_indels"
filterIndels.pl $FASTAS -o .filt -f .indels --454-mode -a $MINABUND --outdir $ANALDIR/0_indels --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command filterIndels.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "mergeSeqs.pl     $ANALDIR/0_indels/*.filt -f $LIB --outdir $ANALDIR/1_lib -o .lib"
mergeSeqs.pl $ANALDIR/0_indels/*.filt -f $LIB --outdir $ANALDIR/1_lib -o .lib --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command mergeSeqs.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "neighbors.pl     $ANALDIR/1_lib/$LIB -o .nbrs"
neighbors.pl $ANALDIR/1_lib/$LIB -o .nbrs --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command neighbors.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

#Don't need to run this
#errorRates.pl $ANALDIR/1_lib/$LIB -n $ANALDIR/1_lib/$LIB.nbrs -h .zhist --overwrite

echo -n "errorRates.pl    $ANALDIR/1_lib/$LIB -n $ANALDIR/1_lib/$LIB.nbrs -z $Z -o .erates"
errorRates.pl $ANALDIR/1_lib/$LIB -n $ANALDIR/1_lib/$LIB.nbrs -z $Z -o .erates --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command errorRates.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "nZeros.pl        $ANALDIR/1_lib/*.filt.lib -n $ANALDIR/1_lib/$LIB.nbrs -r $ANALDIR/1_lib/$LIB.erates -o .n0s --outdir $ANALDIR/2_n0s"
nZeros.pl $ANALDIR/1_lib/{$STUBS}.filt.lib -n $ANALDIR/1_lib/$LIB.nbrs -r $ANALDIR/1_lib/$LIB.erates -o .n0s --outdir $ANALDIR/2_n0s --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command nZeros.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "getCandidates.pl $ANALDIR/2_n0s/*.filt.lib.n0s -o .cands -h $MAG --outdir $ANALDIR/3_cands"
getCandidates.pl $ANALDIR/2_n0s/{$STUBS}.filt.lib.n0s -o .cands -h $MAG --outdir $ANALDIR/3_cands --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getCandidates.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "getReals.pl      $ANALDIR/3_cands/*.filt.lib.n0s.cands -d '$ANALDIR/1_lib/*.filt.lib' -f $ANALDIR/1_lib/$LIB -k $K --outdir $ANALDIR/4_reals_table"
getReals.pl $ANALDIR/3_cands/{$STUBS}.filt.lib.n0s.cands -d "$ANALDIR/1_lib/{$STUBS}.filt.lib" -f $ANALDIR/1_lib/$LIB -k $K --outdir $ANALDIR/4_reals_table --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getReals.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

perl -e 'print("\nrun_CFF_on_FastA.tcsh DONE\nOUTFILES:  $ARGV[0]/*\n")' $ANALDIR

scriptend:

echo "Stop time: "`date`
perl -e 'print STDERR ("RUN TIME:  ",(scalar(time()) - $ARGV[0])," seconds\n")' $STARTTIME
