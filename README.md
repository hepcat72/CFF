CFF README
==========
Created: 2/12/2014
Copyright 2014


DESCRIPTION
-----------

This analysis package, called 'CFF' (Cluster Free Filtering), provides a way to filter metagenomic data for likely sequencing errors.  The pipeline is a 7-step process:

	1. mergeSeqs.pl     merges sequence files with new unique IDs
	2. neighbors.pl     generates a list of hamming distance 1 seqs
	3. errorRates.pl    generates a Z-Score histogram (see -h)
	4. errorRates.pl    generates error rate estimates (see -z)
	5. nZeros.pl        generates expected error abundances
	6. getCandidates.pl filters sequences by N/N0 threshold
	7. filterIndels.pl  filters less abundant sequences with indels

Use these scripts when you have metagenomic samples of ungapped, aligned, & same-sized sequences, to help give you an idea which sequences might be real and which are the result of PCR substitution errors.  The method employed in the filtering does not make use of clustering, but rather exploits low sequencing error rates to gauge the amount of errors one would expect (N0) after observing safely-called errors from all hamming distance 1 neighbor sequences.  The N0 expected error abundances are used to compare with actual abundance and separated using a simple threshold (N/N0).  I.e. If the actual abundance is much larger than the abundance you would expect to see if a sequence is the result of PCR substitution errors, then you would expect the sequence to be real (i.e. exist in the original sample).  The pipeline is designed to filter for errors that are the result of PCR substitutions.  However, and additional post-filtering step has been provided to remove lesser abundant sequences that only differ from more abundant sequences by indels.

Helpful definitions:

        Candidate:         A sequence that is presumed to not be the
	                   result of a PCR substitution error and is
	                   believed to exist in the original biological
	                   sample.
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

Other than this README and the LICENSE, documentation is in the scripts themselves and can be viewed by supplying --help (or --advanced-help).  Usage can be viewed by simply running the scripts without any parameters.

DOWNLOAD
--------

https://github.com/hepcat72/CFF/archive/master.zip

INSTALL
-------

Dependencies:

	muscle - required by filterIndels.pl
	http://www.drive5.com/muscle/downloads.htm

Install the above dependency (a sequence alignment tool) and make sure muscle is in your PATH, then install CFF.  You may install muscle afterwards, but you will see an error.

In a terminal window, cd into the CFF directory and run the following commands:

	perl Makefile.PL
	make
	make install

To run filterIndels, you need to have muscle installed and in your PATH.  If it's not in your path, you can supply the muscle executable with full path to the -y option.  You can install CFF without installing muscle.  If you want to run filterIndels.pl, you can install muscle at a later time.

EXAMPLE
-------

Test data has been provided, so it has been included in this example run.  Note that the example does not use mergeSeqs.pl.  Running mergeSeqs.pl is not required if you already have a sequence file with unique identifiers.

	neighbors.pl -i sample/6_1.drp.fna -o .nbrs

	errorRates.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.nbrs -h .hst

(at this point, you may plot the histogram data in sample/6_1.drp.fna.hst to select a z score cutoff.  We'll use 3.5 in the next step.)

	errorRates.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.nbrs -z 3.5 -o .err

	nZeros.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.nbrs -r sample/6_1.drp.fna.err -o .n0

	getCandidates.pl -i sample/6_1.drp.fna.n0 -o .cands

	filterIndels.pl -i sample/6_1.drp.fna.n0.cands -o .filt

For usage and additional information, each command will generate a usage message if run with no arguments.  Each command will also give details on file formats and additional information when run with the --help flag.

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

Each script comes with a debug mode.  Supply the --debug flag to help figure out issues.  Debug flags can be submitted multiple times, or with an integer parameter to increase the debug verbosity.  This will add line numbers to errors and warnings as well as print ongoing status messages and in some cases, calculation formulas.

KNOWN ISSUES
------------

none

CHANGELOG
---------

Changes are tracked via git and can be viewed on github for each individual script.
