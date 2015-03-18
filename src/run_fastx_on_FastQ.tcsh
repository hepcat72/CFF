#!/bin/tcsh

#VERSION: 1.3

#USAGE: tcsh run_fastx_on_FastQ.tcsh barcode_file.fastq sequences.fastq barcode_list.txt

setenv STARTTIME   `perl -e 'print(scalar(time()))'`
setenv BARCODESEQS `echo "$argv" | cut -f 1 -d " "`
setenv SEQUENCES   `echo "$argv" | cut -f 2 -d " "`
setenv BARCODELIST `echo "$argv" | cut -f 3 -d " "`

if ( $BARCODELIST == "" ) then
  echo
  echo
  echo "USAGE: tcsh run_fastx_on_FastQ.tcsh barcode_file.fastq sequences.fastq barcode_list.txt"
  echo
  echo Make sure fastx_barcode_splitter.pl, paste, and sed are in your PATH
  echo
  echo
  exit
endif

echo 'Splitting barcodes...'
paste -d '' <(echo; sed -n '1,${n;p;}' $BARCODESEQS | sed G) $SEQUENCES | sed '/^$/d' | fastx_barcode_splitter.pl --bol --bcfile $BARCODELIST --prefix "debarcoded/" --suffix ".fastq"

if ( $status ) then
  echo
  echo
  echo "ERROR: Command failed"
  goto scriptend
endif

echo
echo DONE
echo "OUTFILES:"
echo "  debarcoded/*.fastq"
echo

scriptend:

echo "Stop time: "`date`
perl -e 'print STDERR ("RUN TIME:  ",(scalar(time()) - $ARGV[0])," seconds\n\n")' $STARTTIME
