hamming1 README
Created: 2/12/2014
Copyright 2014


MANIFEST
--------

errorRates.pl
neighbors.pl
nZeros.pl
README.md - This file

DESCRIPTION
-----------

This package, called 'hamming1' represents a 4-step process:

	1. neighbors.pl  generates a hamming distance 1 neighbors file
	2. errorRates.pl generates a Z-Score histogram (see -h)
	3. errorRates.pl generates error rate estimates (see -z)
	4. nZeros.pl     generates expected error abundances

Use these scripts when you have a metagenomic sample of ungapped, aligned, & same-sized sequences, to help give you an idea which sequences might be real and which are the result of PCR substitution errors.  The N0 expected error abundances output can be compared to actual abundance to infer whether the sequence is real.  I.e. If the actual abundance is much larger than the abundance you would expect to see if a sequence is the result of PCR substitution errors, then you would expect the sequence to be real (i.e. exist in the original sample).

The starting sequence file must have abundance values on the deflines.  In other words, all sequences must have already been made to be unique and the number of occurrences of any particular sequence has been recorded on its defline.

LICENSE
-------

Read LICENSE

DOCUMENTATION
-------------

Other than this README and the LICENSE, documentation is in the scripts themselves and can be viewed by supplying --help or --advanced-help.  Usage can be viewed by simply running the scripts without any parameters.

DOWNLOAD
--------

https://github.com/hepcat72/hamming1/

INSTALL
-------

The scripts are ready to use, although if you have a non-default path to your perl installation, you may need to change the first line of each file to reflect the non-standard path.  There are no dependencies outside of the default perl modules.  It has been tested with perl version 5.16.2.

EXAMPLE
-------

Test data has been provided, so it has been included in this example run:

	cd hamming1

	head test/input.fa

>6_1_5;ee=0.053;size=114430;
TACGTAGGGTGCGAGCGTTAATCGGAATTACTGGGCGTAAAGCGAGCGCAGACGGTTACTTAAGCAGGATGTGAAATCCC
CGGGCTCAACCTGGGAACTGCGTTCTGAACTGGGTGACTAGAGTGTGTCA
>6_1_16;ee=0.038;size=61240;
TACGGAGGGTGCGAGCGTTAATCGGAATAACTGGGCGTAAAGGGCACGCAGGCGGTGACTTAAGTGAGGTGTGAAAGCCC
CGGGCTTAACCTGGGAATTGCATTTCATACTGGGTCGCTAGAGTACTTTA
>6_1_1;ee=0.049;size=57755;
TACGTATGTCACAAGCGTTATCCGGATTTATTGGGCGTAAAGCGCGTCTAGGTGGTTATGTAAGTCTGATGTGAAAATGC
AGGGCTCAACTCTGTATTGCGTTGGAAACTGCATGACTAGAGTACTGGAG
>6_1_6;ee=0.053;size=20749;

	./neighbors.pl -i /mypath/input.fa -o .neb

	./errorRates.pl -i /mypath/input.fa -n /mypath/input.fa.neb -h .hst

(at this point, you may plot the histogram data in /mypath/input.fa.hst to select a z score cutoff.  We'll use 3.5 in the next step.)

	./errorRates.pl -i /mypath/input.fa -n /mypath/input.fa.neb -z 3.5 -o .err

	./nZeros.pl -i /mypath/input.fa -n /mypath/input.fa.neb -r /mypath/input.fa.err -o .n0

You will end up with output that looks like this:

	head input.fa.n0

#nZeros.pl Version 1.7
# Created: 2/18/2014
# Last modified: Thu Feb 27 16:10:27 2014
#Thu Feb 27 16:14:08 2014
#/usr/bin/perl nZeros.pl -i /mypath/input.fa -n /mypath/input.fa.neb -r /mypath/input.fa.err -o .n0
>6_1_5;ee=0.053;size=114430;N0=0;
TACGTAGGGTGCGAGCGTTAATCGGAATTACTGGGCGTAAAGCGAGCGCAGACGGTTACTTAAGCAGGATGTGAAATCCCCGGGCTCAACCTGGGAACTGCGTTCTGAACTGGGTGACTAGAGTGTGTCA
>6_1_16;ee=0.038;size=61240;N0=0;
TACGGAGGGTGCGAGCGTTAATCGGAATAACTGGGCGTAAAGGGCACGCAGGCGGTGACTTAAGTGAGGTGTGAAAGCCCCGGGCTTAACCTGGGAATTGCATTTCATACTGGGTCGCTAGAGTACTTTA
>6_1_1;ee=0.049;size=57755;N0=0;

CONTACT
-------

To report bugs, request features, submit patches, etc., visit:

https://github.com/hepcat72/hamming1/issues

Other correspondence can be sent to either of the authors below directly.

AUTHORS
-------

Robert W. Leach
Bioinformatics Group
Lewis Seigler Institute for Integrative Genomics
Princeton University
Princeton, NJ
rleach@genomics.princeton.edu

Mikhail Tikhonov
Lewis Seigler Institute for Integrative Genomics
Princeton University
Princeton, NJ
tikhonov@princeton.edu

TROUBLESHOOTING
---------------

Each script comes with a debug mode.  Supply the --debug flag to help figure out issues.  Debug flags can be submitted multiple times, or with an integer parameter to increase the debug verbosity.  This will add line numbers to errors and warnings as well as print ongoing status messages and in some cases, calculation formulas.

KNOWN ISSUES
------------

1. hamming1 only addresses PCR substitutions and does not handle indels or other types of mutations or errors.

CHANGELOG
---------

Changes are tracked via git and can be viewed on github for each individual script.