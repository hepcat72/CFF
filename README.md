CFF README
==========
Created: 2/12/2014
Copyright 2014

DESCRIPTION
-----------

This analysis package, called 'CFF' (Cluster Free Filtering), provides a way to filter metagenomic data for likely sequencing errors.  The pipeline can be run with a single command (run_CFF_on_FastA.tcsh or run_CFF_on_FastQ.tcsh), but each script component is also available.  The pipeline is an 8-step process:

	1. mergeSeqs.pl     merges, truncates, & names sequence samples
	2. neighbors.pl     generates a list of hamming distance 1 seqs
	3. errorRates.pl    generates a Z-Score histogram (see -h)
	4. errorRates.pl    generates error rate estimates (see -z)
	5. nZeros.pl        generates expected error abundances
	6. getCandidates.pl filters sequences by N/N0 threshold
	7. getReals.pl      filters less frequent/chimeric candidates
	8. filterIndels.pl  filters less abundant sequences with indels

Use these scripts when you have metagenomic samples of ungapped & aligned sequences to help give you an idea which sequences might be real and which are likely the result of PCR substitution errors.  The method employed in the filtering does not make use of clustering, but rather exploits low sequencing error rates to gauge the amount of errors one would expect (N0) after observing safely-called errors from all hamming distance 1 neighbor sequences.  The N0 expected error abundances are used to compare with actual abundance and separated using a simple threshold (N/N0).  I.e. If the actual abundance is much larger than the abundance you would expect to see if a sequence is the result of PCR substitution errors, then you would expect the sequence to be real (i.e. exist in the original sample).  Across a series of samples (e.g. a time-series), each with candidate sequences, putative "real" sequences are more stringently filtered by removing predicted chimeras and requiring a minimum number of candidate "nominations".

The pipeline is designed to filter for errors that are the result of PCR substitutions.  However, an additional post-filtering step has been provided to remove lesser abundant sequences that only differ from more abundant sequences by indels.

Helpful definitions:

        Candidate:         A sequence that is suspected to not be the
	                   result of a PCR substitution error and may
	                   exist in the original biological sample.
	Real sequence:     A candidate sequence that was found in an
	                   arbitrary minimum threshold number of
	                   samples (and may have been optionally
	                   screened for chimerism).
	Fake sequence:     A sequence that is presumed to be the result
	                   of a PCR substitution error and is believed
	                   to not exist in the original biological
	                   sample.
        Indel:             A DNA insertion or deletion.
	Mother sequence:   A theoretical real sequence from which
	                   erroneous sequences are derived.  Each
	                   sequence in column 1 of the neighbors file
	                   is referred to as a mother sequence when
	                   comparing it to its neighbors on that row.
	N0 ("N zero"):     The abundance of a sequence that would be
	                   expected if it is the result of a PCR
	                   substitution error.
	N/N0:              A.k.a. the "abundance/N0" fraction.  This is
	                   the abundance of a sequence divided by the
	                   expected abundance if the sequence is a fake
	                   sequence (i.e. the result of a PCR
	                   substitution error).
	Neighbor sequence: A sequence that differs by 1 substitution
	                   from a mother sequence.  Also referred to as
	                   "1st neighbor" or "hamming distance 1
	                   neighbor".
	Real sequence:     A sequence that is presumed to not be the
	                   result of a PCR substitution error and is
	                   believed to exist in the original biological
	                   sample.
	Reverse Spillover: An error that reverses a previous error to
	                   be correct by chance, or an error that
	                   causes a real sequence to turn into a
	                   different real sequence.
	Z Score:           During the estimation of the error rates, a
	                   Z Score is calculated for each neighbor.  It
	                   is computed as:

	                   z = (An - Amu) / Astddev

	                   where An is the abundance of a particular
	                   neighbor, Amu is the average neighbor
	                   abundance, and Astddev is the standard
	                   deviation of neighbor abundance.  It is then
	                   used with a supplied threshold to filter out
	                   potential real sequences from the estimation
	                   of the error background.

Note, each script is designed to handle multiple input files.  If you supply 1 of one type of file and 10 of another type of file, the same single file will be used for processing each of the other 10 files.  E.g.:

	nZeros.pl -i "*.fna" -n all_neighbors.nbrs -r my_error_rates.err -o .n0

nZeros.pl will use the same neighbors and error rates files when processing each sequence file.

LICENSE
-------

Read LICENSE

DOCUMENTATION
-------------

Other than this README and the LICENSE, documentation is in the scripts themselves and can be viewed by supplying --help (or --help --extended).  Usage can be viewed by simply running the scripts without any parameters.

DOWNLOAD
--------

https://github.com/hepcat72/CFF/archive/master.zip

INSTALL
-------

Dependencies:

	muscle - alignment tool required by filterIndels.pl
	http://www.drive5.com/muscle/downloads.htm

	uchime (i.e. usearch) - chimera tool required by getReals.pl
	http://drive5.com/uchime/uchime_download.html

Install the above dependencies and make sure muscle is in your PATH, then install CFF.  You may install muscle afterwards, but you will see an error.

In a terminal window, cd into the CFF directory and run the following commands:

	perl Makefile.PL
	make
	make install

To run filterIndels.pl without error, you need to have muscle installed and in your PATH.  If it's not in your path, you can supply the muscle executable with full path to the -y option.  You can install CFF without installing muscle.  If you want to run filterIndels.pl, you can install muscle at a later time.

To run getReals.pl with the -f option (implying candidates should be filtered for chimeras) without error, you need to have uchime installed and in your PATH.  If it's not in your path, you can supply the uchime executable with full path to the -y option.  You can install CFF without installing uchime.  If you want to run getReals.pl with -f, you can install uchime at a later time.

EXAMPLE
-------

Some test data has been provided, and used in the following examples.

EXAMPLE 1 (run these commands):

	cd samples
	tcsh run_example1.tcsh

Example 1 will run these commands:

mergeSeqs.pl     Caporaso_FASTA/L6S2?_19???.fna -f example1.glib --outdir Caporaso_FASTA_out -o .lib -p ''
neighbors.pl     Caporaso_FASTA_out/example1.glib -o .nbrs
errorRates.pl    Caporaso_FASTA_out/example1.glib -n Caporaso_FASTA_out/example1.glib.nbrs -z 2 -o .erates
nZeros.pl        Caporaso_FASTA_out/L6S2?_19???.fna.lib -n Caporaso_FASTA_out/example1.glib.nbrs -r Caporaso_FASTA_out/example1.glib.erates -o .n0s
getCandidates.pl Caporaso_FASTA_out/L6S2?_19???.fna.lib.n0s -o .cands -f .rejects -h 30
getReals.pl      Caporaso_FASTA_out/L6S2?_19???.fna.lib.n0s.cands -d 'Caporaso_FASTA_out/L6S2?_19???.fna.lib' -f Caporaso_FASTA_out/example1.glib -k 2
filterIndels.pl  Caporaso_FASTA_out/example1.glib.reals -o .filt -f .indels

Note that in the first command (mergeSeqs.pl), we turn off abundance value parsing from the fasta deflines by supplying -p ''.  While the command will work without -p '', it will generate an error because the default value for -p expects to see abundance patterns on deflines in the form "size=#;".  If you never intend to save abundance values on your deflines, you may supply --save-as-default -p '' to save the setting.

Example 2 (run these commands):

	cd samples
	tcsh run_example2.tcsh

Example 2 will run pretty much the same commands, only it starts with fastq files instead of fasta and it uses usearch to quality-filter & de-replicate the sequences.

CONTACT
-------

To report bugs, request features, submit patches, etc., visit:

https://github.com/hepcat72/CFF/issues

Other correspondence can be sent to either of the authors below directly.

AUTHORS
-------

Robert W. Leach  
Bioinformatics Group  
Lewis Seigler Institute for Integrative Genomics  
Princeton University  
Princeton, NJ

Mikhail Tikhonov  
Lewis Seigler Institute for Integrative Genomics  
Princeton University  
Princeton, NJ

TROUBLESHOOTING
---------------

Using --verbose can sometimes help identify a problem.  Each script also comes with a debug mode.  Supply the --debug flag to help figure out issues.  Debug flags can be submitted multiple times, or with an integer parameter to increase the amount of debug output.  This will add line numbers to errors and warnings as well as print ongoing status messages and in some cases, calculation formulas.

KNOWN ISSUES
------------

none

CHANGELOG
---------

Changes are tracked via git and can be viewed on github for each individual script.
