#!/bin/tcsh

#VERSION: 1.3

#USAGE: tcsh run_CFF_on_FastA.tcsh trimlen outdir "fasta-files-pattern"
# E.G.: tcsh run_CFF_on_FastA.tcsh 130 myanalysis "some-dir/*.fa"

setenv STARTTIME `perl -e 'print(scalar(time()))'`
setenv TRIMLEN   `echo "$argv" | cut -f 1 -d " "`
setenv ANALDIR   `echo "$argv" | cut -f 2 -d " "`
setenv FASTAS    `echo "$argv" | cut -f 3-999999 -d " "`
setenv LASTTIME  `perl -e 'print(scalar(time()))'`

#Make space in the script environment for eventual long command lines
unset argv

setenv Z         2
setenv K         2
setenv MAG       10
setenv LIB       global_library.fna
setenv STUBS     "{"`perl -e 'print(join(",",map {s%.*/%%;s/([\*\}\{\[\]\?])/\\$1/g;$_} map {split(/ +/)} @ARGV))' "$FASTAS"`"}"
setenv SHOWSTUBS "{"`perl -e '$fs=join(",",map {s%.*/%%;s/([\*\}\{\[\]\?])/\\$1/g;$_} map {split(/ +/)} @ARGV);print((length($fs) > 80 ? "YOUR LONG PATHLESS FILES HERE" : $fs));' "$FASTAS"`"}"


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
@ CHECKMAX = ( `getconf ARG_MAX` - `env | grep -v FASTAS | wc -c` - `set | wc -c` )
@ CHECKIN  = ( `echo "$STUBS" | perl -e 'print((length(scalar(<STDIN>)) - 1) * 2 + length($ARGV[0]) * 4 + length($ARGV[1]) + length($ARGV[2]))' $ANALDIR $K $LIB` + 97 )
if ( $CHECKMAX != "" && $CHECKIN > $CHECKMAX ) then
  echo
  echo "ERROR: Your file list length will be too big for the lengthiest command in this"
  echo "       pipeline (approx. [$CHECKIN] characters with your files).  Your system max"
  echo "       [`getconf ARG_MAX`] after deducting the size for the stored files in the"
  echo "       script environment is: [$CHECKMAX]."
  echo
  echo '       Please use a pattern in quotes (e.g. "*.fasta"), shorten the file names,'
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

setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "mergeSeqs.pl        '$FASTAS' -f '$LIB' --outdir '$ANALDIR/2_lib' -o .lib -b $TRIMLEN -p ''"
mergeSeqs.pl "$FASTAS" -f "$LIB" --outdir "$ANALDIR/1_lib" -o .lib -p '' -b $TRIMLEN --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command mergeSeqs.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


#Don't need $FASTAS anymore, so make space for eventual long command lines
unsetenv FASTAS


echo -n "neighbors.pl        '$ANALDIR/1_lib/$LIB' -o .nbrs"
neighbors.pl "$ANALDIR/1_lib/$LIB" -o .nbrs --overwrite
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


echo -n "errorRates.pl       '$ANALDIR/1_lib/$LIB' -n '$ANALDIR/1_lib/$LIB.nbrs' -z $Z -o .erates"
errorRates.pl "$ANALDIR/1_lib/$LIB" -n "$ANALDIR/1_lib/$LIB.nbrs" -z $Z -o .erates --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command errorRates.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "nZeros.pl           '$ANALDIR/1_lib/$SHOWSTUBS.lib' -n '$ANALDIR/1_lib/$LIB.nbrs' -r '$ANALDIR/1_lib/$LIB.erates' -o .n0s --outdir '$ANALDIR/2_n0s'"
nZeros.pl "$ANALDIR/1_lib/$STUBS.lib" -n "$ANALDIR/1_lib/$LIB.nbrs" -r "$ANALDIR/1_lib/$LIB.erates" -o .n0s --outdir "$ANALDIR/2_n0s" --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command nZeros.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "getCandidates.pl    '$ANALDIR/2_n0s/$SHOWSTUBS.lib.n0s' -o .cands -h $MAG --outdir '$ANALDIR/3_cands'"
getCandidates.pl "$ANALDIR/2_n0s/$STUBS.lib.n0s" -o .cands -h $MAG --outdir "$ANALDIR/3_cands" --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command getCandidates.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "getReals.pl      -i '$ANALDIR/3_cands/$SHOWSTUBS.lib.n0s.cands' -n '$ANALDIR/2_n0s/$SHOWSTUBS.lib.n0s' -f '$ANALDIR/1_lib/$LIB' -k $K --outdir '$ANALDIR/4_reals_table'"
getReals.pl -i "$ANALDIR/3_cands/$STUBS.lib.n0s.cands" -n "$ANALDIR/2_n0s/$STUBS.lib.n0s" -f "$ANALDIR/1_lib/$LIB" -k $K --outdir "$ANALDIR/4_reals_table" --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command getReals.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "filterIndels.pl     '$ANALDIR/4_reals_table/$LIB.reals' -o .filt -f .indels --outdir '$ANALDIR/5_indels'"
filterIndels.pl "$ANALDIR/4_reals_table/$LIB.reals" -o .filt -f .indels --outdir "$ANALDIR/5_indels" --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command filterIndels.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "cff2qiime.pl        '$ANALDIR/5_indels/$LIB.reals.filt' -s '$ANALDIR/4_reals_table/$LIB.smry' --outdir '$ANALDIR/optional_6_qiime'"
cff2qiime.pl "$ANALDIR/5_indels/$LIB.reals.filt" -s "$ANALDIR/4_reals_table/$LIB.smry" --outdir "$ANALDIR/optional_6_qiime" --overwrite
if ( $status ) then
  echo
  echo
  echo "ERROR: Command cff2qiime.pl failed"
  goto scriptend
endif
perl -e 'print STDERR (" -- ",(scalar(time()) - $ARGV[0])," seconds\n")' $LASTTIME
setenv LASTTIME `perl -e 'print(scalar(time()))'`


echo -n "interestingPairs.pl '$ANALDIR/1_lib/$LIB' -s '$ANALDIR/4_reals_table/$LIB.smry' -o '$LIB.pairs' --outdir '$ANALDIR/optional_7_interestingPairs'"
interestingPairs.pl "$ANALDIR/1_lib/$LIB" -s "$ANALDIR/4_reals_table/$LIB.smry" -o "$LIB.pairs" --outdir "$ANALDIR/optional_7_interestingPairs" --overwrite
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
