### STEP 2: run Cluster-free filtering ###
# Tikhonov, Leach, Wingreen, "Interpreting 16S metagenomic data without clustering to achieve sub-OTU resolution"
# Supplementary file 2
#
# Script usage: 
#   ./run_cff.sh dataDir seqLength
# Here dataDir is the name of the folder containing the the original demultiplexed fastq files 
# (same as used for step 1), used as prefix for all other folder names;
# seqLength is the trimming length used for quality-filtering the data.
#
# For usage examples, see preprocess.sh (Supplementary file 1) 
#


# Assumes the data has already been pre-processed, and that the quality-controlled and dereplicated
# FASTA samples are located in folder $1_drp

dataDir=$1 # directory with your fastq files
seqLen=$2
if [ -z "$seqLen" ]
then
	echo "Please provide the length that all sequences were trimmed to as the second argument."
	exit
fi

verbosity="" # "--verbose" # "" to turn verbosity off
#export PATH=$PATH:~/myPathHere # path to cluster-free filtering scripts if not on path

drpDir=$dataDir'_drp'
libDir=$dataDir'_lib'
n0Dir=$dataDir'_n0'
candDir=$dataDir'_cand'


#######################################
### CLUSTER-FREE FILTERING ROUTINES ###
#######################################

# Input: quality-controlled & dereplicated data, one file per sample

echo
T="$(date +%s)"
echo "Creating library and renaming sequences in samples (mergeSeqs.pl):"
mergeSeqs.pl -i "$drpDir/*.drp.fna" -o .lib -f $libDir/library.fna --outdir $libDir $verbosity
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"


echo
T="$(date +%s)"
echo "Generating neighbor structure to speed up the analysis of a large dataset (neighbors.pl):"
neighbors.pl -i $libDir/library.fna -o .neighb -a 10 $verbosity
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"

echo
T="$(date +%s)"
echo "Using the combined library to estimate error rates (errorRates.pl):"
errorRates.pl -i $libDir/library.fna -n $libDir/library.fna.neighb -e 10 -z 2 -o .err $verbosity --noheader
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"

echo
T="$(date +%s)"
echo "Using the error rates to predict null model abundance (nZeros.pl):"
nZeros.pl -i "$libDir/*.fna.lib" -n $libDir/library.fna.neighb -r $libDir/library.fna.err -o .n0 --outdir $n0Dir $verbosity
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"

#echo
#T="$(date +%s)"
#echo "Same, but without using the pre-calculated neighbor structure:"
#nZeros.pl -i "$libDir/*.fna.lib" -r $libDir/library.fna.err -a 10 -o .n0_ --outdir $n0Dir $verbosity
#T="$(($(date +%s)-T))"
#echo "Elapsed time: ${T} seconds"


echo
T="$(date +%s)"
echo "Identifying possible substitution errors (getCandidates.pl):"
getCandidates.pl -i "$n0Dir/*.n0" -a 10 -h 10 -o .cand --outdir $candDir $verbosity
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"


echo
T="$(date +%s)"
echo "Identifying sequences that passed filtering criteria in 2 samples:"
# Use USEARCH to find sequences present in at least two samples
cat $candDir/*.cand > $candDir/mergedCand.cand
usearch -derep_fulllength $candDir/mergedCand.cand -minuniquesize 2 --output $n0Dir/cff_cands.fna --sizeout -quiet
rm $candDir/mergedCand.cand
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"

echo
T="$(date +%s)"
echo "Identifying chimeras (UCHIME):"
# The recommended practice is to look for chimeras in the pooled data from all samples. 
# The "library" file contains the pooled abundance information, but we only need to run UCHIME on a 
# subset of those sequences: those in our candidate list. This can be done very fast, since sequences 
# have identifiers that number them (no need to comapre the actual sequences!). But for illustration 
# purposes, here I do it the much slower way, but one that uses usearch only:
usearch -usearch_global $libDir/library.fna -db $n0Dir/cff_cands.fna -id 1.0 -idprefix $seqLen -matched $n0Dir/candsWithGlobalAbundance.fna -strand plus -quiet
usearch -uchime_denovo $n0Dir/candsWithGlobalAbundance.fna -minuniquesize 2 -nonchimeras $n0Dir/cff_reals.fna -uchimealns $n0Dir/chimeras.alns -quiet
T="$(($(date +%s)-T))"
echo "Elapsed time: ${T} seconds"

echo
echo "Done. "
echo
echo "The result of cluster-free filtering is in directory $n0Dir."
echo "The .n0 files are FASTA files that make up the global sequence abundance table:"
echo "     Sequences are labeled with identifiers \"lib_###\", which are consistent across samples."
echo "     Headers describe the actual (size=###) and null model (N0=###) abundance in that sample."
echo
echo "The file cff_cands.fna lists all sequences that passed multi-sample filtering criteria."
echo "In this file, the \"size=##\" in the header indicates the number of samples where a given "
echo "sequence passed the conservative filtering criteria, generating a confident above-background"
echo "detection."
echo
echo "Finally, the file cff_reals.fna lists all candidates that passed chimera filtering with UCHIME."
echo "Size annotations correspond to the total abundance of the sequence in all samples combined."
echo "These are the rows that should be retained. "
echo "The file chimeras.alns describes the detected high-abundance chimeras."
echo


