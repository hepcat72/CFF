#!/bin/tcsh

#USAGE: tcsh run_CFF_on_FastQ.tcsh trimlen outdir "fastq-files"
# E.G.: tcsh run_CFF_on_FastQ.tcsh 130 myanalysis "some-dir/*.fa"

setenv TRIMLEN   `echo "$argv" | cut -f 1 -d " "`
setenv ANALDIR   `echo "$argv" | cut -f 2 -d " "`
setenv FASTQS    `echo "$argv" | cut -f 3-999 -d " "`

setenv STARTTIME `perl -e 'print(scalar(time()))'`
setenv Z         2
setenv K         2
setenv MAG       10
setenv ASCII     64
setenv LIB       global_library.fna
setenv QUAL      2
setenv STUBS     `perl -e 'print(join(",",map {s%.*/%%;$_} @ARGV))' $FASTQS`

if ( -e $ANALDIR ) then
  echo
  echo "ERROR: Output directory [$ANALDIR] already exists."
  echo '       Please remove it or supply a new uncreated one.'
  echo
  exit
endif

echo
echo "RUNNING run_CFF_on_FastQ.tcsh"
echo "-----------------------------"
echo "Start time:                  "`date`
echo "Trim length:                 $TRIMLEN"
echo "Z-score threshold:           $Z"
echo "Magnitude over N0 Threshold: $MAG"
echo "Nominations threshold:       $K"
echo "Fastq ASCII:                 $ASCII"
echo "Trunc. Quality:              $QUAL"
echo "OUTPUT DIRECTORY:            $ANALDIR"
echo

mkdir $ANALDIR
mkdir $ANALDIR/0_1_qc
mkdir $ANALDIR/0_2_drp

foreach f ( $FASTQS )

set NAME=`perl -e 'print(join(",",map {s%.*/%%;$_} @ARGV))' $f`

echo -n "usearch -fastq_filter $f -fastq_trunclen $TRIMLEN -fastq_maxee 1 -fastaout $ANALDIR/0_1_qc/$NAME.qc.fna -relabel "$NAME"_ -eeout -fastq_ascii $ASCII -fastq_truncqual $QUAL -quiet"
usearch -fastq_filter $f -fastq_trunclen $TRIMLEN -fastq_maxee 1 -fastaout $ANALDIR/0_1_qc/$NAME.qc.fna -relabel "$NAME"_ -eeout -fastq_ascii $ASCII -fastq_truncqual $QUAL -quiet >& /dev/null
if ( $status ) then
 echo
 echo
 echo "ERROR: Command 'usearch -fastq_filter' failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "usearch -derep_fulllength $ANALDIR/0_1_qc/$NAME.qc.fna -output $ANALDIR/0_2_drp/$NAME.drp.fna -sizeout -quiet"
usearch -derep_fulllength $ANALDIR/0_1_qc/$NAME.qc.fna -output $ANALDIR/0_2_drp/$NAME.drp.fna -sizeout -quiet >& /dev/null
if ( $status ) then
 echo
 echo
 echo "ERROR: Command 'usearch -derep_fulllength' failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

end

echo

echo -n "mergeSeqs.pl     $ANALDIR/0_2_drp/*.drp.fna -f $LIB --outdir $ANALDIR/1_lib -o .lib -t fasta"
mergeSeqs.pl $ANALDIR/0_2_drp/{$STUBS}.drp.fna -f $LIB --outdir $ANALDIR/1_lib -o .lib -t fasta --overwrite
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

echo -n "nZeros.pl        $ANALDIR/1_lib/*.drp.fna.lib -n $ANALDIR/1_lib/$LIB.nbrs -r $ANALDIR/1_lib/$LIB.erates -o .n0s --outdir $ANALDIR/2_n0s"
nZeros.pl $ANALDIR/1_lib/{$STUBS}.drp.fna.lib -n $ANALDIR/1_lib/$LIB.nbrs -r $ANALDIR/1_lib/$LIB.erates -o .n0s --outdir $ANALDIR/2_n0s --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command nZeros.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "getCandidates.pl $ANALDIR/2_n0s/*.drp.fna.lib.n0s -o .cands -h $MAG --outdir $ANALDIR/3_cands"
getCandidates.pl $ANALDIR/2_n0s/{$STUBS}.drp.fna.lib.n0s -o .cands -h $MAG --outdir $ANALDIR/3_cands --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getCandidates.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "getReals.pl      $ANALDIR/3_cands/*.drp.fna.lib.n0s.cands -d '$ANALDIR/1_lib/*.drp.fna.lib' -f $ANALDIR/1_lib/$LIB -k $K --outdir $ANALDIR/4_reals_table"
getReals.pl $ANALDIR/3_cands/{$STUBS}.drp.fna.lib.n0s.cands -d "$ANALDIR/1_lib/{$STUBS}.drp.fna.lib" -f $ANALDIR/1_lib/$LIB -k $K --outdir $ANALDIR/4_reals_table --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getReals.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

echo -n "filterIndels.pl  $ANALDIR/4_reals_table/$LIB.reals -o .filt -f .indels --outdir $ANALDIR/5_indels"
filterIndels.pl $ANALDIR/4_reals_table/$LIB.reals -o .filt -f .indels --outdir $ANALDIR/5_indels --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command filterIndels.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds total\n")' $STARTTIME

perl -e 'print("\nrun_CFF_on_FastQ.tcsh DONE\nOUTFILES:  $ARGV[0]/*\n")' $ANALDIR

scriptend:

echo "Stop time: "`date`
perl -e 'print STDERR ("RUN TIME:  ",(scalar(time()) - $ARGV[0])," seconds\n")' $STARTTIME
