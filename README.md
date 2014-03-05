hamming1 README
===============
Created: 2/12/2014
Copyright 2014


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

Read LICENSE.md

DOCUMENTATION
-------------

Other than this README and the LICENSE, documentation is in the scripts themselves and can be viewed by supplying --help or --advanced-help.  Usage can be viewed by simply running the scripts without any parameters.

DOWNLOAD
--------

https://github.com/hepcat72/hamming1/

INSTALL
-------

	perl Makefile.PL
	make
	make install

EXAMPLE
-------

Test data has been provided, so it has been included in this example run:

	cd hamming1

	./neighbors.pl -i sample/6_1.drp.fna -o .neb

	./errorRates.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.neb -h .hst

(at this point, you may plot the histogram data in sample/6_1.drp.fna.hst to select a z score cutoff.  We'll use 3.5 in the next step.)

	./errorRates.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.neb -z 3.5 -o .err

	./nZeros.pl -i sample/6_1.drp.fna -n sample/6_1.drp.fna.neb -r sample/6_1.drp.fna.err -o .n0

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

Mikhail Tikhonov  
Lewis Seigler Institute for Integrative Genomics  
Princeton University  
Princeton, NJ

TROUBLESHOOTING
---------------

Each script comes with a debug mode.  Supply the --debug flag to help figure out issues.  Debug flags can be submitted multiple times, or with an integer parameter to increase the debug verbosity.  This will add line numbers to errors and warnings as well as print ongoing status messages and in some cases, calculation formulas.

KNOWN ISSUES
------------

1. hamming1 only addresses PCR substitutions and does not handle indels or other types of mutations or errors.

CHANGELOG
---------

Changes are tracked via git and can be viewed on github for each individual script.
