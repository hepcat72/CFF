#!/bin/tcsh

#VERSION: 1.4

#USAGE: tcsh run_CFF_on_FastQ.tcsh trimlen ascii_offset* outdir "fastq-files-pattern"
# E.G.: tcsh run_CFF_on_FastQ.tcsh 130 64 myanalysis "some-dir/*.fq"

# * - Usually 64, details: http://drive5.com/usearch/manual/fastq_params.html

setenv STARTTIME `perl -e 'print(scalar(time()))'`
setenv TRIMLEN   `echo "$argv" | cut -f 1 -d " "`
setenv ASCII     `echo "$argv" | cut -f 2 -d " "`      #Either 33 or 64*
setenv ANALDIR   `echo "$argv" | cut -f 3 -d " "`
setenv FASTQS    `echo "$argv" | cut -f 4-999999 -d " "`
setenv LASTTIME  `perl -e 'print(scalar(time()))'`

#Make space in the script environment for eventual long command lines
unset argv

setenv USEARCH   usearch
setenv Z         2
setenv K         2
setenv MAG       10
setenv LIB       global_library.fna
setenv QUAL      2
setenv STUBS     "{"`perl -e 'print(join(",",map {s%.*/%%;s/([\*\}\{\[\]\?])/\\$1/g;$_} map {split(/ +/)} @ARGV))' "$FASTQS"`"}"
setenv SHOWSTUBS "{"`perl -e '$fs=join(",",map {s%.*/%%;s/([\*\}\{\[\]\?])/\\$1/g;$_} map {split(/ +/)} @ARGV);print((length($fs) > 80 ? "YOUR LONG PATHLESS FILES HERE" : $fs));' "$FASTQS"`"}"


#Preemptive success check...
#This script is most reliable when receiving a file glob pattern as a string for the input
#files (hence the quotes around the files in the example above).  This is in order to
#avoid limits on number/length of file names expanded by the shell.  The following is an
#approximate courtesy check to make sure that the cumulative number of characters in the
#file names string supplied will not cause an "Argument list too long." error during the
#getReals.pl step (the longest command in the pipeline in terms of characters).
#Calculations may be off for some systems, as the space available for commands may be
#handled differently, thus this check could prevent a run that might otherwise work or may
#allow a run that will encounter the error.  Feel free to comment out everything from
#"@ CHECKMAX..." to "unset CHECKMAX" if this turns out to be a problem or the calculated
#numbers in error make no sense.
@ CHECKMAX = ( `getconf ARG_MAX` - `env | grep -v FASTQS | wc -c` - `set | wc -c` )
@ CHECKIN  = ( `echo "$STUBS" | perl -e 'print((length(scalar(<STDIN>)) - 1) * 2 + length($ARGV[0]) * 4 + length($ARGV[1]) + length($ARGV[2]))' $ANALDIR $K $LIB` + 113 )
if ( $CHECKMAX != "" && $CHECKIN > $CHECKMAX ) then
  echo
  echo "ERROR: Your file list length will be too big for the lengthiest command in this"
  echo "       pipeline (approx. [$CHECKIN] characters with your files).  Your system max"
  echo "       [`getconf ARG_MAX`] after deducting the size for the stored files in the"
  echo "       script environment is: [$CHECKMAX]."
  echo
  echo '       Please use a pattern in quotes (e.g. "*.fastq"), shorten the file names,'
  echo '       or in extreme cases - consider running on a system with larger command'
  echo '       length limits or check your environment for large amounts of data.'
  echo
  exit
endif
unset CHECKIN
unset CHECKMAX


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

#usearch hits an error if you gice it an empty file, so let's skip empties
if ( -z $f || ((-e $f) == 0) ) then
 echo
 echo "Skipping empty file $f"
 echo
 goto loopend
endif

set NAME=`perl -e 'print(join(",",map {s%.*/%%;$_} @ARGV))' $f`
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "$USEARCH -fastq_filter     $f -fastq_trunclen $TRIMLEN -fastq_maxee 1 -fastaout $ANALDIR/0_1_qc/$NAME.qc.fna -relabel "$NAME"_ -eeout -fastq_ascii $ASCII -fastq_truncqual $QUAL -quiet"
$USEARCH -fastq_filter $f -fastq_trunclen $TRIMLEN -fastq_maxee 1 -fastaout $ANALDIR/0_1_qc/$NAME.qc.fna -relabel "$NAME"_ -eeout -fastq_ascii $ASCII -fastq_truncqual $QUAL -quiet >& /dev/null
if ( $status ) then
 echo
 echo
 echo "ERROR: Command '$USEARCH -fastq_filter' failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "$USEARCH -derep_fulllength $ANALDIR/0_1_qc/$NAME.qc.fna -output $ANALDIR/0_2_drp/$NAME.drp.fna -sizeout -quiet"
$USEARCH -derep_fulllength $ANALDIR/0_1_qc/$NAME.qc.fna -output $ANALDIR/0_2_drp/$NAME.drp.fna -sizeout -quiet >& /dev/null
if ( $status ) then
 echo
 echo
 echo "ERROR: Command '$USEARCH -derep_fulllength' failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


loopend:
end


#Don't need $FASTQS anymore, so make space for eventual long command lines
unsetenv FASTQS


echo


echo -n "mergeSeqs.pl        '$ANALDIR/0_2_drp/$SHOWSTUBS.drp.fna' -f '$LIB' --outdir '$ANALDIR/1_lib' -o .lib -t fasta"
mergeSeqs.pl "$ANALDIR/0_2_drp/$STUBS.drp.fna" -f "$LIB" --outdir "$ANALDIR/1_lib" -o .lib -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command mergeSeqs.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "neighbors.pl        '$ANALDIR/1_lib/$LIB' -o .nbrs -t fasta"
neighbors.pl "$ANALDIR/1_lib/$LIB" -o .nbrs -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command neighbors.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


#Don't need to run this
#errorRates.pl "$ANALDIR/1_lib/$LIB" -n "$ANALDIR/1_lib/$LIB.nbrs" -h .zhist --overwrite


echo -n "errorRates.pl       '$ANALDIR/1_lib/$LIB' -n '$ANALDIR/1_lib/$LIB.nbrs' -z $Z -o .erates -t fasta"
errorRates.pl "$ANALDIR/1_lib/$LIB" -n "$ANALDIR/1_lib/$LIB.nbrs" -z $Z -o .erates -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command errorRates.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "nZeros.pl           '$ANALDIR/1_lib/$SHOWSTUBS.drp.fna.lib' -n '$ANALDIR/1_lib/$LIB.nbrs' -r '$ANALDIR/1_lib/$LIB.erates' -o .n0s --outdir '$ANALDIR/2_n0s' -t fasta"
nZeros.pl "$ANALDIR/1_lib/$STUBS.drp.fna.lib" -n "$ANALDIR/1_lib/$LIB.nbrs" -r "$ANALDIR/1_lib/$LIB.erates" -o .n0s --outdir "$ANALDIR/2_n0s" -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command nZeros.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "getCandidates.pl    '$ANALDIR/2_n0s/$SHOWSTUBS.drp.fna.lib.n0s' -o .cands -h $MAG --outdir '$ANALDIR/3_cands' -t fasta"
getCandidates.pl "$ANALDIR/2_n0s/$STUBS.drp.fna.lib.n0s" -o .cands -h $MAG --outdir "$ANALDIR/3_cands" -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getCandidates.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "getReals.pl      -i '$ANALDIR/3_cands/$SHOWSTUBS.drp.fna.lib.n0s.cands' -n '$ANALDIR/2_n0s/$SHOWSTUBS.drp.fna.lib.n0s' -f '$ANALDIR/1_lib/$LIB' -k $K --outdir '$ANALDIR/4_reals_table' -t fasta"
getReals.pl -i "$ANALDIR/3_cands/$STUBS.drp.fna.lib.n0s.cands" -n "$ANALDIR/2_n0s/$STUBS.drp.fna.lib.n0s" -f "$ANALDIR/1_lib/$LIB" -k $K --outdir "$ANALDIR/4_reals_table" -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command getReals.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "filterIndels.pl     '$ANALDIR/4_reals_table/$LIB.reals' -o .filt -f .indels --outdir '$ANALDIR/5_indels' -t fasta"
filterIndels.pl "$ANALDIR/4_reals_table/$LIB.reals" -o .filt -f .indels --outdir "$ANALDIR/5_indels" -t fasta --overwrite
if ( $status ) then
 echo
 echo
 echo "ERROR: Command filterIndels.pl failed"
 goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "cff2qiime.pl        '$ANALDIR/5_indels/$LIB.reals.filt' -s '$ANALDIR/4_reals_table/$LIB.smry' --outdir '$ANALDIR/optional_6_qiime' -t fasta"
cff2qiime.pl "$ANALDIR/5_indels/$LIB.reals.filt" -s "$ANALDIR/4_reals_table/$LIB.smry" --outdir "$ANALDIR/optional_6_qiime" -t fasta --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command cff2qiime.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "interestingPairs.pl '$ANALDIR/1_lib/$LIB' -s '$ANALDIR/4_reals_table/$LIB.smry' -o '$LIB.pairs' --outdir '$ANALDIR/optional_7_interestingPairs' -t fasta"
interestingPairs.pl "$ANALDIR/1_lib/$LIB" -s "$ANALDIR/4_reals_table/$LIB.smry" -o "$LIB.pairs" --outdir "$ANALDIR/optional_7_interestingPairs" -t fasta --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command interestingPairs.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo
echo DONE
echo "OUTFILES:"
echo "  $ANALDIR/*"
echo "  run_all_qiime_tax_commands.sh"
echo
echo "If you have Qiime installed, edit the script './run_all_qiime_tax_commands.sh' based on your preferred qiime parameters and then run it to generate a biom file with taxonomic information."

scriptend:

echo "Stop time: "`date`
perl -e 'print STDERR ("RUN TIME:  ",(scalar(time()) - $ARGV[0])," seconds\n\n")' $STARTTIME
