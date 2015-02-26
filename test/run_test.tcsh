#!/bin/tcsh

#VERSION: 1.0

@ PASSES = ( 0 );
setenv FULLTEST 1

##
## Test 1 on the Caporaso FastA data
##

if( -e ../samples/Caporaso_FASTA_out == 0 ) then
 cd ../samples
 echo "Running samples/run_example1.tcsh. Please wait."
 ./run_example1.tcsh >& /dev/null
 cd ../test
else
 setenv FULLTEST 0
endif

cut -f 2- ../samples/Caporaso_FASTA_out/4_reals_table/global_library.fna.smry | grep -v \# | perl -ne 's/\s+(\S+)/$1/g;print if(/^\d+\s*$/)' | sort > ./Caporaso_FASTA_check.smry.sort.test

if ( `diff Caporaso_FASTA_check.smry.sort Caporaso_FASTA_check.smry.sort.test` != "" ) then
 echo "Test 1 failed. Your installation does not appear to be working as expected. Please contact rleach@princeton.edu for help."
 if ( $FULLTEST == 0 ) then
  echo "This test used pre-existing results from ../samples/Caporaso_FASTA_out. To force a full re-test, please delete that directory and rerun this script."
 else
  rm -rf ../samples/Caporaso_FASTA_out
 endif
 exit 1
endif

if ( `diff Caporaso_FASTA_check.smry.sort Caporaso_FASTA_check.smry.sort.test` == "" ) then
 echo "Test 1 passed."
 @ PASSES = ( $PASSES + 1 );
endif

#Cleanup
if ( $FULLTEST == 1 ) then
 rm -rf ../samples/Caporaso_FASTA_out
endif
rm -f Caporaso_FASTA_check.smry.sort.test



##
## Test 2 on the Caporaso FastQ data
##

if( -e ../samples/Caporaso_FASTQ_out == 0 ) then
 cd ../samples
 echo "Running samples/run_example2.tcsh. Please wait."
 ./run_example2.tcsh >& /dev/null
 cd ../test
else
 setenv FULLTEST 0
endif

cut -f 2- ../samples/Caporaso_FASTQ_out/4_reals_table/global_library.fna.smry | grep -v \# | perl -ne 's/\s+(\S+)/$1/g;print if(/^\d+\s*$/)' | sort > ./Caporaso_FASTQ_check.smry.sort.test

if ( `diff Caporaso_FASTQ_check.smry.sort Caporaso_FASTQ_check.smry.sort.test` != "" ) then
 echo "Test 2 failed. Your installation does not appear to be working as expected. Please contact rleach@princeton.edu for help."
 if ( $FULLTEST == 0 ) then
  echo "This test used pre-existing results from ../samples/Caporaso_FASTQ_out. To force a full re-test, please delete that directory and rerun this script."
 else
  rm -rf ../samples/Caporaso_FASTQ_out
 endif
 exit 2
endif

if ( `diff Caporaso_FASTQ_check.smry.sort Caporaso_FASTQ_check.smry.sort.test` == "" ) then
 echo "Test 2 passed."
 @ PASSES = ( $PASSES + 1 );
endif

#Cleanup
if ( $FULLTEST == 1 ) then
 rm -rf ../samples/Caporaso_FASTQ_out
endif
rm -f Caporaso_FASTQ_check.smry.sort.test



##
## Test 3 on the Caporaso Gut Samples
##

if( -e ../samples/Caporaso_GutSamples_out == 0 ) then
 cd ../samples
 echo "Running samples/run_example3.tcsh. Please wait."
 ./run_example3.tcsh >& /dev/null
 cd ../test
else
 setenv FULLTEST 0
endif

cut -f 2- ../samples/Caporaso_GutSamples_out/4_reals_table/global_library.fna.smry | grep -v \# | perl -ne 's/\s+(\S+)/$1/g;print if(/^\d+\s*$/)' | sort > ./Caporaso_GutSamples_check.smry.sort.test

if ( `diff Caporaso_GutSamples_check.smry.sort Caporaso_GutSamples_check.smry.sort.test` != "" ) then
 echo "Test 3 failed. Your installation does not appear to be working as expected. Please contact rleach@princeton.edu for help."
 if ( $FULLTEST == 0 ) then
  echo "This test used pre-existing results from ../samples/Caporaso_GutSamples_out. To force a full re-test, please delete that directory and rerun this script."
 else
  rm -rf ../samples/Caporaso_GutSamples_out
 endif
 exit 3
endif

if ( `diff Caporaso_GutSamples_check.smry.sort Caporaso_GutSamples_check.smry.sort.test` == "" ) then
 echo "Test 3 passed."
 @ PASSES = ( $PASSES + 1 );
endif

#Cleanup
if ( $FULLTEST == 1 ) then
 rm -rf ../samples/Caporaso_GutSamples_out
endif
rm -f Caporaso_GutSamples_check.smry.sort.test


if ( $PASSES == 3 ) then
 echo "All tests passed. CFF is working as expected. You're good to go!"
 if ( $FULLTEST == 0 ) then
  echo "Note: This test used pre-existing results (that you previously generated) from ../samples/Caporaso_*_out. To force a full re-test, please delete these directories and rerun this script."
 endif
endif
