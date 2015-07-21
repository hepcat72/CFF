# CFF README

Updated: 2/20/2015<BR>
Copyright 2015

## DESCRIPTION

The Cluster-Free Filtering package (CFF) is a fast denoiser designed for analyzing multi-sample metagenomic datasets from "tag sequencing" experiments (e.g. 16S). Its purpose is to identify and eliminate sequences generated by substitution errors during PCR or sequencing. The algorithm does not cluster similar sequences together, which allows analyzing moderate-abundance community members with sub-OTU (single-nucleotide) resolution. The intended application is for analyzing subtle compositional differences between broadly similar communities, e.g. a location series of similar microbial habitats or a time series sampling the same habitat.

This version works best with Illumina data. The package will also work with 454 (or Ion Torrent) data, with the caveat that frequent indels can decrease sensitivity and produce low abundance errors among the output "reals". Though we don't provide a mechanism to statistically handle indels, filterIndels.pl, if run before the pipeline in --pre-filter-mode, can discard & sum abundances of lesser-abundant sequences differing only by 454-like homopolymer indels, and is extremely fast compared non-homopolymer indel filtering.

The final sequence output of this pipeline can be converted to a format which can be used in the Qiime analysis package.  See qiime.org for more information.

The benefit of this approach is the ability to find very similar sequences which behave differently in terms of abundance in a series of samples (e.g. a time-series).  Toward that end, CFF provides a means by which to identify pairs of sequences that have high sequence identity and dynamically dissimilar abundances.

This README file provides basic installation/usage information. For a thorough introduction to the cluster-free filtering approach, including the motivation, algorithm, limitations, and examples of application, see [1]. For detailed usage information of individual scripts, see built-in documentation.


## GETTING STARTED

This section walks you through what you need to do to go from scratch to first results.

1. First, download CFF and extract the zip file by double-clicking it:

[https://github.com/hepcat72/CFF/archive/master.zip](https://github.com/hepcat72/CFF/archive/master.zip)

2. Second, if you do not have usearch version 7 installed, obtain a download link here:

[http://www.drive5.com/usearch/download.html](http://www.drive5.com/usearch/download.html)

Note: Be sure to get **usearch version 7.0.1090**.  The new version 8 is currently incompatible.  Also note, the script will install muscle as well.  See the "INSTALL" section for other parameters or for information on skipping the muscle install.

3. Open a terminal window and `cd` into the CFF directory (e.g. `cd ~/Downloads/CFF-master`), then run the following commands for your system.  Note, the install script will install both usearch and muscle.  Both installs can be skipped by running `./install.tcsh skip skip` instead of the given command.

##### Mac OS X:

    ./install.tcsh "your_usearch_download_url"

If you already have usearch version 7 installed, you may replace the download link with the path to your usearch executable.

##### Linux:

    sudo ./install_linux.sh
    ./install.tcsh "your_usearch_download_url"

The linux install script installs some dependencies of the install script if they are not present on your system.

The install script will tell you upon completion whether the installation succeeded and passed its diagnostic tests or whether installation failed.

4. Finally, to complete the install and access the CFF commands, open a new terminal window.

If you run into any problems, refer to the "INSTALL" section below.

5. [OPTIONAL] If you wish, you may manually test your CFF installation in a new terminal window by running the following command inside the test directory of the CFF installation (e.g. `cd ~/Downloads/CFF-master/test`):

    tcsh run_test.tcsh

If any of the tests failed, try running example 3 to inspect any error messages which might indicate what the problem is:

    cd samples
    tcsh run_example3.tcsh

Example 3 demonstrates the value of the interestingPairs.pl script.  The samples are taken from a male and a female subject (see [1]), at 3 time points separated by 3 months, with a 7th "unknown" sample taken 2 months afterward.  The analysis shows that the dominant strain in the female samples and the dominant strain in the male samples (sequences lib_1 and lib_2) differ by only one nucleotide. Their strongly negative dynamical similarity is indicative of strongly anti-correlated distribution across samples.  And indeed, one of these sequences is dominant in the male subject but absent in the female, and vice versa for the other sequence, as you can see in the abundance table summary file (located in Caporaso_GutSamples_out/4_reals_table/), where one should find the rows corresponding to lib_1 and lib_2 and observe that sequence lib_1 is strongly present in samples (M3_Apr09, M3_Jan09, and M3_Oct08), and sequence lib_2 in (F4_Apr09, F4_Jan09, and F4_Oct08).  The seventh "unknown" sample (column 1_335 in the abundance table summary file) is confidently identified as coming from the male subject, because it has the male-subject-specific strain.  And indeed, this sample was taken from the male subject. This illustrates the utility of our method, stressing, in particular, that you don't even necessarily need a lot of samples to see meaningful features at the sub-OTU level.  Any other cluster-based method would have equated the two sequences differing by a single base.

If you encounter any problems during the example run, refer to "INSTALL" below.


## THE PIPELINE

The pipeline is primarily a 7-step process whose goal is to find sequences that are as error-free as possible. There are also 3 additional steps at the end to: filter insertions & deletions, convert the output to be compatible with qiime version 1.3.0 (see qiime.org), and a follow-up analysis designed to find pairs of similar sequences that behave differently in a population. It can be run with a single command (run_CFF_on_FastA.tcsh or run_CFF_on_FastQ.tcsh), but each script component is also available:

    1.  mergeSeqs.pl        trims, dereplicates, and renames sequences in samples
    2.  neighbors.pl        generates a list of Hamming distance 1 sequences
    3.  errorRates.pl       generates a Z-Score histogram (see -h)
    4.  errorRates.pl       estimates substitution error rate (see -z)
    5.  nZeros.pl           calculates null model abundance (N0) for each sequence
    6.  getCandidates.pl    filters sequences by N/N0 threshold
    7.  getReals.pl         filters chimeric candidates and generates an abundance table
    8.  filterIndels.pl     (optional) identifies sequences differing by indels
    9.  cff2qiime.pl        (optional) convert sequence output to qiime input files
    10. interestingPairs.pl (optional) identify similar sequences that behave differently

These scripts assume that, since they were sequenced from specific PCR amplicons, each sequence starts at the same place. In other words, they are by nature, generally aligned. CFF will work with any PCR-amplified tag sequencing dataset (such as 16S), but is NOT applicable to shotgun metagenomics where sequences need to be aligned to match each other.

Note that filterIndels.pl has options specific to 454 sequence data described in both its usage and help output.

Here's briefly, how the pipeline works. First, sequences are renamed so that each sequence has the same name across samples. Next, the most abundant sequences are used to estimate (substitution) error rates. The estimated error rates are then used to calculate each sequence's expected abundance (N0) if it was the result of substitution errors only. Sequences whose observed abundance significantly exceeds this predicted null-model abundance are "nominated" as candidate reals. Candidates that receive enough nominations across samples are retained as real. In this manner even low-abundance sequences are recorded, as long as they could be confidently identified as present above background in a sufficient number of other samples; see [1].

The pipeline is designed to filter for errors that are the result of sequencing or PCR substitutions.  However, an additional pre-/post-filtering step has been provided to identify possible indels. This is an optional post-filtering step when applied to Illumina data, but is suggested as a pre-filtering step when applied to 454 data, where indel errors are commonly generated during sequencing.

When running run_CFF_on_FastQ, be sure to select the correct fastq_ascii offset (either 33 or 64). This is a parameter to usearch. A detailed explanation of this parameter can be found on the usearch website at [http://drive5.com/usearch/manual/fastq_params.html](http://drive5.com/usearch/manual/fastq_params.html).

##### Helpful definitions:

    Candidate:         A sequence in an individual sample that is suspected to not be the
                       result of a PCR substitution error and may exist in the original
                       biological sample.
    Real:              A candidate sequence that was found in an arbitrary minimum
                       threshold number of samples (and may have been optionally screened
                       for chimerism), is believed to exist in the original biological
                       sample, and to not be the result of a PCR substitution error.
    Fake:              A sequence that is believed to be the result of a PCR substitution
                       error and is believed to not exist in the original biological
                       sample.
    Indel:             A DNA insertion or deletion.
    Mother sequence:   A theoretical real sequence from which erroneous sequences are
                       derived.  Each sequence in column 1 of the neighbors file is
                       referred to as a mother sequence when comparing it to its
                       neighbors on that row.
    N0 ("N zero"):     The abundance of a sequence that would be expected if it is the
                       result of a PCR substitution error.
    N/N0:              A.k.a. the "abundance/N0" fraction.  This is the abundance of a
                       sequence divided by the expected abundance if the sequence is a
                       fake sequence (i.e. the result of a PCR substitution error).
    Neighbor:          A sequence that differs by 1 substitution from a mother sequence.
                       Also referred to as a "1st neighbor" or a "hamming distance 1
                       neighbor".
    Reverse Spillover: An error that reverses a previous error to be correct by chance,
                       or an error that causes a real sequence to turn into a different
                       real sequence.
    Z Score:           During the estimation of PCR substitution error rates, a Z Score is
                       calculated for each neighbor. We want all the neighbors used to
                       estimate the error rate to confidently be the result of an actual
                       error, and the Z score is a means to do this so that our estimation
                       is accurate. It is computed as:

                           Z = (An - Amu) / Astddev

                       where "An" is the abundance of a particular neighbor, "Amu" is the
                       average neighbor abundance, and "Astddev" is the standard deviation
                       of neighbor abundance.
                       A histogram of all Z-scores can be useful when selecting a cutoff,
                       but is a subjective decision, left to be interpreted by the user.
                       Once a Z-score cutoff is supplied, it is used to skip neighbor
                       sequences which might be at least partially, the result of a real
                       sequence.
    Dynamical          A Pearson correlation of the abundances of 2 sequences across a
    Similarity         series of samples, normalized by their maximum possible correlation 
                      (cmax, computed as the correlation of the sequence with the higher
                       average abundance across samples, to a Poisson-downsampled version
                       of itself)

Note, each script is designed to handle multiple input files.  No other scripting is required to run the individual scripts.  No shell loops are required.


## LICENSE

Read [LICENSE](./LICENSE)


## DOCUMENTATION

Other than this README and the [LICENSE](./LICENSE), documentation is in the scripts themselves and can be viewed by supplying --help (or --help --extended).  Usage can be viewed by simply running the scripts without any parameters.


## INSTALL

Follow the installation procedure under "**GETTING STARTED**".  If you have any trouble, refer to the notes below. If you are installing on a Qiime 1.3 Virtual Box, see the QIIME subsection below.

### QUICK INSTALL

The install script will install both usearch and muscle third-party executables, however a personal download link for usearch must be obtained by the user.  Obtain a download link for usearch version 7.0.1090 here:

#### Obtain your personal usearch download URL:

[http://www.drive5.com/usearch/download.html](http://www.drive5.com/usearch/download.html)

Note: Be sure to get **usearch version 7.0.1090**.  The new version 8 is currently incompatible.  The URL will look something like this:

http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000

#### Mac OS X:
##### User-level/local install:

    ./install.tcsh "http://personal_usearch_download_url"

##### Root-level/system install:

    sudo ./install.tcsh "http://personal_usearch_download_url"

#### Linux:
##### User-level/local install:

    sudo ./install_linux.sh
    ./install.tcsh "http://personal_usearch_download_url"

Note, the `sudo ./install_linux.sh` requires root access, but may not be necessary.  If you do not have root access, just skip that command.  If you run into problems, find someone with root access to run the sudo command.

##### Root-level/system install:

    sudo ./install_linux.sh
    sudo ./install.tcsh "http://personal_usearch_download_url"

The usage for the install.tcsh script includes 2 options:

**USAGE 1**: install.tcsh "**USEARCH**"

**USAGE 2**: install.tcsh "**USEARCH**" "**MUSCLE**"

Where "**USEARCH**" and "**MUSCLE**" can be:
 1. A download URL of a gzipped tarball or executable
 2. A tarball or executable
 3. "skip" or "ignore"

Examples:

    install.tcsh http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000
    install.tcsh usearch7.0.1090_i86osx32
    install.tcsh usearch muscle3.8.31_i86darwin64.tar
    install.tcsh skip http://www.drive5.com/muscle/downloads3.8.31/muscle3.8.31_i86linux64.tar.gz

usearch and muscle are third-party applications not maintained by CFF.

The install_linux.sh script installs some dependencies of the install.tcsh script if they are not on your system, specifically: tcsh, curl, and perl's ExtUtils::MakeMaker module.

The install.tcsh script does a few things of note:

1. It will create a "soft link" named "usearch" that points to the usearch version 7 executable.  "usearch" is the default command used by the CFF pipeline.  If you have trouble, make sure that usearch version 7 has precedence in your PATH by running `usearch -version`.
2. It will create a "soft link" named "muscle" that points to the muscle executable.
3. On Mac OS X, if the developer tools are not installed, it will automatically ask you if you would like to install XCode, and then walk you through that process.
4. It installs cpanminus and local::lib to aid in the install of the dependent perl modules.

### UPDATE

To update your copy of this package, simply cd into the CFF directory in a terminal window and run the following commands.

    perl Makefile.PL
    make
    sudo make install

You can skip installing the dependencies such as usearch, muscle, and the required perl modules, however note any discrepancies when you run `perl Makefile.PL` and install anything that is missing or below the required version number.

Note that each perl script in the package has its own version number.  If you include --header as an option when running commands, output files will have a header with run information, including the version of the script that was used to generate the output.  Rest assured that all the elements of the pipeline will skip the headers when processing the files (except for the output of cff2qiime.pl, as qiime does not recognize the headers).


### ADVANCED INSTALL

Install dependencies and make sure they are in your path (e.g. run `which muscle`). Follow the instructions on their respective download pages:

**muscle - alignment tool required by filterIndels.pl**

[http://www.drive5.com/muscle/downloads.htm](http://www.drive5.com/muscle/downloads.htm)

**usearch (v7) - sequence search tool required by getReals.pl & interestingPairs.pl**

[http://www.drive5.com/usearch/download.html](http://www.drive5.com/usearch/download.html)

Note: Be sure to get **usearch version 7.0.1090**. The new version 8 is currently incompatible.

Install perl module dependencies. You may have to answer a series of questions to set up cpan at first, but the default options are usually fine. Run this command to start CPAN:

    sudo env FTP_PASSIVE=1 PERL_MM_USE_DEFAULT=1 perl -MCPAN -e shell

Enter your system password, then at the cpan prompt, install these dependent modules using these commands:

    install Getopt::Long
    install File::Glob
    install IPC::Open3
    install IO::Select
    install IO::Pipe::Producer
    install Sys::Info
    install Sys::MemInfo
    install File::Which
    install Math::Random

Now we're ready to install CFF.  In a terminal window, cd into the CFF directory and run the following commands:

    perl Makefile.PL
    make
    sudo make install

##### USEARCH
The newest version of usearch (version 8.x, release in 2015) is different enough to make it incompatible with the current version of CFF. If you already have version 8 installed, please refer to the [version 7 notes](http://www.drive5.com/usearch/manual/quick_usearch7.html), particularly, the section on [installing multiple versions](http://www.drive5.com/usearch/manual/multiple_versions.html).

To keep usearch version 8 primary in your path and only use version 7 with CFF, run these two commands in a terminal window and edit your run_CFF_on_FastQ.tcsh script (replacing the usearch path with the location of your usearch version 7 executable):

    getReals.pl --save-as-default -y /path/to/usearch_version_7
    interestingPairs.pl --save-as-default -y /path/to/usearch_version_7

The replace this line in `run_CFF_on_FastQ.tcsh` (run `which run_CFF_on_FastQ.tcsh` to find where it is installed):

    setenv USEARCH   usearch

with:

    setenv USEARCH   /path/to/usearch_version_7

This will implicitly add the usearch 7 path to every call of these scripts. The current default user-set options can be viewed at the bottom of each script's usage output (i.e. run each script without any options to see your user defaults).

##### PERL
The perl path at the top of each script will be changed to match the version of perl that was used when running Makefile.PL. This can be changed at any time by rerunning these commands. The perl path used when running the cpan command is also important

##### PERL MODULES
In some cases, a module install may fail with an error. Some of these cases can be overcome by simply forcing the module to install. At the cpan prompt:

    force install File::Glob

Sometimes, this may initiate an install of some standard perl modules. If this happens, selecting all default options is usually good enough - just keep hitting return. If this doesn't work, you may need to find the module under `~/.cpan/build` and follow the installation instructions found there.

You may already have some of the dependent perl modules, but some might be too old, so note the version numbers. Here is the required version information for each of the modules.

    Getopt::Long          v2.38   REQUIRED for all scripts
    File::Glob            v1.17   REQUIRED for all scripts
    IPC::Open3            v1.12   REQUIRED for filterIndels.pl only
    IO::Select            v1.21   REQUIRED for filterIndels.pl & interestingPairs.pl only
    IO::Pipe::Producer    v2.0    REQUIRED for filterIndels.pl & interestingPairs.pl only
    Sys::Info             v0.78   REQUIRED for filterIndels.pl & interestingPairs.pl only
    Sys::MemInfo          v0.91   REQUIRED for filterIndels.pl only
    Math::Random          v0.71   REQUIRED for interestingPairs.pl only
    File::Which           v1.09   OPTIONAL All scripts will use `which` without it

##### QIIME 1.3 VIRTUAL BOX INSTALL NOTES

If you are installing CFF on the Qiime Virtual Box (version 1.3), note that the Virtual Box that comes with the installation does not have the tcsh shell used for the pipeline scripts (run_CFF_on_FastA.tcsh and run_CFF_on_FastQ.tcsh). If you were to run one of these scripts, you would see an error like this:

    bash: run_CFF_on_FastQ.tcsh: /bin/tcsh: bad interpreter: No such file or directory

Installing tcsh is very simple. Open a terminal and run this command:

    sudo apt-get install tcsh

The Qiime Virtual Box comes with muscle version 3.6 pre-installed, but not usearch. When downloading, be sure to get the version compiled for your system, paying particular attention to 32 versus 64 bit processors.

The preferred perl path in the Qiime Virtual Box is `/usr/local/bin/perl`. It has a bunch of the perl module dependencies already installed. `/usr/bin/perl` exists, but is a lesser perl version and results in warnings and difficulty installing File::Glob. Be sure to use `/usr/local/bin/perl` when running cpan and Makefile.PL.

###### filterIndels.pl
You can install CFF without installing muscle.  If you want to run filterIndels.pl, you can install muscle at a later time.  Muscle is not required if running in --pre-filter-mode or --homopolymer-mode. Otherwise you need to have muscle installed and in your PATH (or you can supply the muscle executable with full path using the -y option).

###### getReals.pl
You can install CFF without installing usearch.  If you want to run getReals.pl with -f, you can install usearch at a later time.  To run with the -f option (implying candidates should be filtered for chimeras) without error, uchime is required (a part of the usearch package).  If it's not in your path, you can supply the usearch executable with full path using the -y option.

###### interestingPairs.pl
You can install CFF without installing usearch.  If you want to run interestingPairs.pl, you can install usearch at a later time.  To run without error, usearch is required. If it's not in your path, you can supply the usearch executable with full path using the -y option.


## EXAMPLES

In [1], we discuss the application of CFF to analyze a time series of 16S samples from a longitudinal study performed by Caporaso et al. [2]. As test data, we provide a subset of samples from that study. The same 10 samples are provided in FastA and FastQ format to illustrate the two uses of the CFF pipeline (using raw FastQ and already quality-filtered FastA data). For a detailed description of the test data, see samples/testDataDescription.txt.

##### Example 1

Run these commands:

    cd samples
    tcsh run_example1.tcsh

Example 1 is roughly equivalent to running the following commands (the difference is that when you run the example script, the output files will be neatly organized in a number of subdirectories so you can follow the flow of logic more easily).  Note, $outdir = "Caporaso_FASTA_out".

    mergeSeqs.pl        Caporaso_FASTA/L6S2?_19???.fna -f example1.glib --outdir $outdir -o .lib -p '' -b 130
    neighbors.pl        $outdir/example1.glib -o .nbrs
    errorRates.pl       $outdir/example1.glib -n $outdir/example1.glib.nbrs -z 2 -o .erates
    nZeros.pl           $outdir/L6S2?_19???.fna.lib -n $outdir/example1.glib.nbrs -r $outdir/example1.glib.erates -o .n0s
    getCandidates.pl    $outdir/L6S2?_19???.fna.lib.n0s -o .cands -f .rejects -h 30
    getReals.pl         $outdir/L6S2?_19???.fna.lib.n0s.cands -d '$outdir/L6S2?_19???.fna.lib' -f $outdir/example1.glib -k 2
    filterIndels.pl     $outdir/example1.glib.reals -o .filt -f .indels
    cff2qiime.pl        $outdir/example1.glib.reals.filt -s $outdir/example1.glib.reals.smry
    interestingPairs.pl $outdir/example1.glib -s $outdir/example1.glib.smry -o example1.glib.pairs

Note that in the first command (mergeSeqs.pl), we turn off abundance value parsing from the fasta deflines by supplying -p ''.  While the command will work without -p '', it will generate an error because the default value expects to see abundance patterns on deflines in the form "size=#;".  If you never intend to save abundance values on your deflines, you may supply --save-as-default -p '' to save the setting.

##### Example 2

Run these commands:

    cd samples
    tcsh run_example2.tcsh

Example 2 runs CFF on FastQ test data. This executes pretty much the same commands, only it starts by performing minimal quality filtering using USEARCH (see [1]).

##### Example 3

Run these commands:

    cd samples
    tcsh run_example3.tcsh

Example 3 runs CFF on the gut samples test data. It runs the same analysis as example 2, but serves to demonstrate the value of the interestingPairs.pl script.  Please refer to the interpretation of the results at the end of the GETTING STARTED section above.


## REFERENCES

1. [Tikhonov M, Leach RW, Wingreen NS. (2014) Interpreting 16S data without clustering to achieve sub-OTU resolution. ISME J. 117.] (http://dx.doi.org/10.1038/ismej.2014.117 "Tikhonov et al., 2014")

2. [Caporaso JG, Lauber CL, Costello EK, et al. (2011) Moving pictures of the human microbiome. Genome Biol. 12(5):R50.] (http://dx.doi.org/10.1186/gb-2011-12-5-r50 "Caporaso et al., 2011")


## CONTACT

For technical support (bugs, feature requests, error messages, script functionality, portability...):

[https://github.com/hepcat72/CFF/issues](https://github.com/hepcat72/CFF/issues)

For science-related questions (usefulness of CFF for your application, limitations of error model, other sequencing platforms...):

tikhonov@fas.harvard.edu


## AUTHORS

Robert W. Leach<BR>
Bioinformatics Group<BR>
Lewis-Sigler Institute for Integrative Genomics<BR>
Princeton University<BR>
Princeton, NJ

Mikhail Tikhonov<BR>
School of Engineering and Applied Sciences<BR>
Harvard University<BR>
Boston, MA


## TROUBLESHOOTING

Using --verbose can sometimes help identify a problem.  Each script also comes with a --debug mode that can help to figure out issues.  Debug flags can be submitted multiple times, or with an integer parameter to increase the amount of debug output.  This will add line numbers to errors and warnings as well as print ongoing status messages and in some cases, calculation formulas.

Questions relating to specific options or general questions can frequently be addressed by running the individual perl scripts without options or with the --help flag.

Users are encouraged to submit bug reports at [https://github.com/hepcat72/CFF/issues](https://github.com/hepcat72/CFF/issues).  Please include copied & pasted errors, a description of the expected behavior, a description of how to reproduce the error, and the output of these commands:

    uname -a
    echo $SHELL
    muscle -version
    usearch -version

For errors from specific scripts (e.g. getReals.pl), please also include the output of the specific script's --version flag, e.g.:

    getReals.pl --version

###### Missing sequence warnings
Generally, warnings will not interrupt execution, but may indicate unintended behavior, such as if you run the wrong files together or in the wrong respective order (which is most likely to cause a problem in the getReals.pl script where the files supplied with the -i flag and the -n flag are expected to be in the same respective order, e.g. this improper file ordering will cause errors about missing sequence IDs: `-i "1.lib.n0s.cands 2.lib.n0s.cands ... 10.lib.n0s.cands" -n "10.lib.n0s 1.lib.n0s 2.lib.n0s ..."`).

###### Argument list too long
Running the run_CFF* scripts on lots of files can sometimes bring about the "Argument list too long" error. This issue has been mitigated compared to previous versions of CFF. If you are running a run_CFF* script prior to version 1.3 or if your run_CFF* script does not have a version number at the top, see the UPDATE section above toi get the latest version to potentially get around this issue.

If you are still encountering this error, there are a number of ways to get around it. The best way is to supply your file arguments to the script using a quoted glob pattern E.g.:

    tcsh run_CFF_on_FastQ.tcsh 130 33 myanalysis "*.fq"

Note the __"*.fq"__, _with quotes_. While file lists without quotes, or supplied via mechanisms such as:

    tcsh run_CFF_on_FastQ.tcsh 130 33 myanalysis `cat my_file_list.txt`

are possible, there is a system-imposed character limit on the total combined length of the file names. The limit is affected by the size of the data stored in your environment and in any shell variables. The run_CFF* scripts try to catch potential situations where this error is likely to be encountered (during the execution of the getReals.pl script), but this check may be unreliable or mis-calculated for some systems. If it causes problems, refer to the notes in the run_CFF* scripts above the section containing the "CHECKMAX" variable.

Other ways to get around this error include: shortening your input file names, checking for and clearing out any large amounts of data stored in your environment, or running CFF on a system with a larger command-line length limit (run `getconf ARG_MAX` to find your system's upper limit).

See the INSTALL section for more notes on potential pitfalls.


## CHANGELOG

Changes are tracked via git and can be viewed on github for each individual script.
