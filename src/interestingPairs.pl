#!/usr/bin/perl
#Generated using perl_script_template.pl

#USAGE: Run with no options to get usage or with --extended for more details

my $software_version_number = '1.3';
my $created_on_date         = '9/10/2014';

#Robert W. Leach
#Princeton University
#Carl Icahn Laboratory
#Lewis Sigler Institute for Integrative Genomics
#Bioinformatics Group
#Room 133A
#Princeton, NJ 08544
#rleach@genomics.princeton.edu
#Copyright 2014

##
## Initialize Run
##

use warnings;
use strict;
use Getopt::Long qw(GetOptionsFromArray :config no_auto_abbrev);
use File::Glob ':glob';

#Global variables used in subs
my $preserve_args  = [@ARGV];
my $defaults_dir   = (sglob('~/.rpst'))[0];
my @user_defaults  = getUserDefaults(1);

#Global variables used in subs
my $default_stub   = 'STDIN';
my $header         = 0;
my $error_limit    = 5;
my $help           = 0;
my $extended       = 0;
my $version        = 0;
my $overwrite      = 0;
my $skip_existing  = 0;
my $dry_run        = 0;
my $verbose        = 0;
my $quiet          = 0;
my $DEBUG          = 0;
my $force          = 0;
my $use_as_default = 0;
my $aggregate_mode = 1;
my $compound_mode  = 0;

#usearch ublast variables
my $usearch_col_str   = 'query+target+id+ql+qlo+qhi+tl+tlo+thi+evalue';
my($query_col,          #These column numbers are set automatically based on
   $subject_col,        #the usearch_col_str value.
   $identity_col,
   $query_len_col,
   $query_start_col,
   $query_stop_col,
   $subject_len_col,
   $subject_start_col,
   $subject_stop_col,
   $evalue_col);

#Global variables used in main
my $output_files      = [];
my $outfile_suffix    = '';
my $input_files       = [];
my $abunds_files      = [];
my $outdirs           = [];
#my $min_mean_abund    = 10;
my($min_mean_abund);
my $max_dynsim        = 0.8;
my $min_seqsim        = 0.9;
my $filetype          = 'auto';
my $abundance_pattern = 'size=(\d+);';
my $seq_id_pattern    = '^\s*[>\@]\s*([^;]+)';
my $sigdig            = 3;
my $default_tmpdir    = './';
my $tmpdir            = exists($ENV{TMPDIR}) ? $ENV{TMPDIR} :
  (exists($ENV{TMP}) && -e $ENV{TMP} && -d $ENV{TMP} ? $ENV{TMP} :
   (exists($ENV{TEMP}) && -e $ENV{TEMP} && -d $ENV{TMP} ? $ENV{TEMP} :
    (-e '/tmp' && -d '/tmp' ? '/tmp' : $default_tmpdir)));
my $tmp_suffix        = '.tmp.fna';
my $usearch           = 'usearch';
my $processes         = 0;
my $usearch_opts      = '';
my $append_usearch    = 0;
my $append_abunds     = 0;

#Command line parameters
my $GetOptHash =
  {'i|seq-file=s'           => sub {push(@$input_files,    #REQUIRED unless <>
					 [sglob($_[1])])}, #        is supplied
   '<>'                     => sub {checkFileOpt($_[0],1);
				    push(@$input_files,    #REQUIRED unless -i
					 [sglob($_[0])])}, #        is supplied
   's|abunds-file=s'        => sub {push(@$abunds_files,   #OPTIONAL [none]
					 [sglob($_[1])])},
   'o|outfile=s'            => sub {push(@$output_files,   #OPTIONAL [none]
					 [sglob($_[1])])},
   'outdir=s'               => sub {push(@$outdirs,        #OPTIONAL [none]
					 [sglob($_[1])])},
   'a|min-mean-abundance=s' => \$min_mean_abund,           #OPTIONAL [1% total/
                                                           #       num_samples]
   'x|max-dynamical-' .                                    #OPTIONAL [0.8]
   'similarity=s'           => \$max_dynsim,
   'n|min-seq-similarity=s' => \$min_seqsim,               #OPTIONAL [0.9]
   't|sequence-filetype=s'  => \$filetype,                 #OPTIONAL [auto]
				                           #(fasta,fastq,auto)
   'p|abundance-pattern=s'  => \$abundance_pattern,        #OPTIONAL
                                                           #     [size=(\d+);]
   'q|seq-id-pattern=s'     => \$seq_id_pattern,           #OPTIONAL [^\s*[>\@
                                                           #      ]\s*([^;]+)]
   'significant-digits=s'   => \$sigdig,                   #OPTIONAL [3]
   'tmpdir=s'               => \$tmpdir,                   #OPTIONAL [(env)]
   'tmp-suffix=s'           => \$tmp_suffix,               #OPTIONAL [.tmp.fna]
   'parallel-processes=i'   => \$processes,                #OPTIONAL [1/core]
   'y|usearch-exe=s'        => \$usearch,                  #OPTIONAL [usearch]
   'usearch-col-str=s'      => \$usearch_col_str,          #OPTIONAL [query+..]
   'usearch-opts-str=s'     => \$usearch_opts,             #OPTIONAL [none]
   'append-usearch!'        => \$append_usearch,           #OPTIONAL [Off]
   'append-abunds!'         => \$append_abunds,            #OPTIONAL [Off]
   'overwrite!'             => \$overwrite,                #OPTIONAL [Off]
   'skip-existing!'         => \$skip_existing,            #OPTIONAL [Off]
   'force:+'                => \$force,                    #OPTIONAL [Off]
   'verbose:+'              => \$verbose,                  #OPTIONAL [Off]
   'quiet'                  => \$quiet,                    #OPTIONAL [Off]
   'debug:+'                => \$DEBUG,                    #OPTIONAL [Off]
   'help'                   => \$help,                     #OPTIONAL [Off]
   'extended'               => \$extended,                 #OPTIONAL [Off]
   'version'                => \$version,                  #OPTIONAL [Off]
   'header!'                => \$header,                   #OPTIONAL [Off]
   'error-type-limit=i'     => \$error_limit,              #OPTIONAL [5]
   'dry-run'                => \$dry_run,                  #OPTIONAL [Off]
   'save-as-default'        => \$use_as_default,           #OPTIONAL [Off]
   'aggregate-mode!'        => \$aggregate_mode,           #OPTIONAL [On]
   'join-conflicts!'        => \$compound_mode,            #OPTIONAL [Off]
  };

#Set user-saved defaults
GetOptionsFromArray([@user_defaults],%$GetOptHash) if(scalar(@user_defaults));

#Get the input options & catch any errors in option parsing
if(!GetOptions(%$GetOptHash))
  {
    #Try to guess which arguments GetOptions is complaining about
    my @possibly_bad = grep {!(-e $_)} map {@$_} @$input_files;

    error('Getopt::Long::GetOptions reported an error while parsing the ',
	  'command line arguments.  The warning should be above.  Please ',
	  'correct the offending argument(s) and try again.');
    usage(1);
    quit(-2);
  }

##
## Validate Options
##

#Process & validate the default options (supply whether there will be outfiles)
processDefaultOptions(scalar(@$output_files));

#Require input file(s)
if(scalar(@$input_files) == 0 && isStandardInputFromTerminal())
  {
    error('No input detected.');
    usage(1);
    quit(-7);
  }

if(scalar(@$input_files) == 0)
  {
    error('No input sequence files (-i) detected.');
    usage(1);
    quit(-7);
  }

if(#There are no abundance files
   scalar(@$abunds_files) == 0 &&
   ((#All input file sets contain only 1 file
     scalar(grep {scalar(@$_) > 1} @$input_files) == 0 &&
     #There are fewer than 6 input file sets
     scalar(@$input_files) < 6) ||
    (#There's at least 1 input file set with more than 1 file
     scalar(grep {scalar(@$_) > 1} @$input_files) &&
     #There's at least 1 input file set with fewer than 6 files
     scalar(grep {scalar(@$_) < 6} @$input_files)
    )))
  {
    error('Not enough abundance data supplied (-s).  You must either provide ',
	  'a file of sample sequence abundances (-s) or a series of sample ',
	  'sequence files (-i) with abundances on the deflines (see -p).  At ',
	  'least 6 samples in either case are required (per output file or ',
	  'set of input files (provided by multiple -i flags)) in order to ',
	  'calculate dynamical similarity.  Use --force to try anyway.');
    usage(1);
    quit(1);
  }

#The number of output files and abundance trace files must be the same if they
#are both supplied.
if(scalar(@$output_files) && scalar(@$abunds_files) &&
   (scalar(@$output_files) != scalar(@$abunds_files) ||
    scalar(grep {scalar(@{$output_files->[$_]}) !=
		   scalar(@{$abunds_files->[$_]})} (0..$#{$output_files}))))
  {
    #Allow there to be 1 abunds file (per output file set)
    if(!(scalar(@$abunds_files) == 1 && scalar(@{$abunds_files->[0]}) == 1) ||
       (scalar(@$output_files) == scalar(@$abunds_files) &&
	scalar(grep {scalar(@$_) > 1} @$abunds_files)))
      {
	error("The number of abundance trace files (-s) must be the same as ",
	      "the number of output files (-o) or 1.");
	quit(2);
      }
  }

if(defined($min_mean_abund) &&
   ($min_mean_abund < 0 || $min_mean_abund !~ /^(\d+\.?\d*|\d*\.?\d+)$/))
  {
    error("Invalid minimum mean abundance (-a): [$min_mean_abund].  Must be ",
	  "an unsigned number without an exponent.");
    quit(3);
  }

if($max_dynsim < 0 || $max_dynsim > 2 ||
   $max_dynsim !~ /^(\d+\.?\d*|\d*\.?\d+)$/)
  {
    error("Invalid maximum dynamic similarity (-x): [$max_dynsim].  Must be ",
	  "an unsigned number between 0 and 2 (inclusive) without an ",
	  "exponent.");
    quit(4);
  }

if($min_seqsim < 0 || $min_seqsim > 1 ||
   $min_seqsim !~ /^(\d+\.?\d*|\d*\.?\d+)$/)
  {
    error("Invalid minimum sequence similarity (-n): [$min_seqsim].  Must be ",
	  "an unsigned number between 0 and 1 (inclusive) without an ",
	  "exponent.");
    quit(5);
  }

#The tempdir is invalid
if(!defined($tmpdir) || $tmpdir eq '' || !(-e $tmpdir))
  {
    if(-e $default_tmpdir)
      {
	verbose("The temporary directory",
		(defined($tmpdir) ? ": [$tmpdir]" : '')," is invalid.  Using ",
		"default: [$default_tmpdir] along with temporary suffix ",
		"[$tmp_suffix] which will be applied to -i files.  ",
		"Use --tmpdir and --tmp-suffix to change these values.")
	  if($verbose > 1 || ($DEBUG && $verbose));
	$tmpdir = $default_tmpdir;
      }
    else
      {
	error("The temporary directory",
	      (defined($tmpdir) ? ": [$tmpdir]" : '')," is invalid.  Use ",
	      "--tmpdir and --tmp-suffix to manage temporary data (necessary ",
	      "for running ublast).");
	quit(6);
      }
  }

if($filetype =~ /fasta/i)
  {$filetype = 'fasta'}
elsif($filetype =~ /fastq/i)
  {$filetype = 'fastq'}
elsif($filetype =~ /auto/)
  {$filetype = 'auto'}
else
  {
    error("Unrecognized file type: [$filetype].  Must be 'fasta', 'fastq', ",
	  "or 'auto'.");
    quit(7);
  }

if($abundance_pattern =~ /^\s*$/)
  {
    error("-p cannot be an empty string.");
    quit(8);
  }
elsif($abundance_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$abundance_pattern = '(' . $abundance_pattern . ')'}

if($seq_id_pattern ne '' &&
   $seq_id_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$seq_id_pattern = '(' . $seq_id_pattern . ')'}

#Require an output file if an outdir has been supplied
if(scalar(@$outdirs) && scalar(@$output_files) == 0)
  {
    error("An outfile (-o) is required if an output directory (--outdir) is ",
	  "supplied.");
    quit(-8);
  }

#Make sure uchime is properly installed if library files have been supplied
$usearch = getUsearchExe($usearch);
if(incompatible($usearch))
  {
    error("Required executables and/or modules missing.  Unable to proceed.");
    quit(9);
  }

if($processes <= 0)
  {$processes = getNumCores()}

if($usearch_col_str !~ /(\A|\+)query(\Z|\+)/  ||
   $usearch_col_str !~ /(\A|\+)target(\Z|\+)/ ||
   $usearch_col_str !~ /(\A|\+)id(\Z|\+)/     ||
   $usearch_col_str !~ /(\A|\+)ql(\Z|\+)/     ||
   $usearch_col_str !~ /(\A|\+)qlo(\Z|\+)/    ||
   $usearch_col_str !~ /(\A|\+)qhi(\Z|\+)/    ||
   $usearch_col_str !~ /(\A|\+)tl(\Z|\+)/     ||
   $usearch_col_str !~ /(\A|\+)tlo(\Z|\+)/    ||
   $usearch_col_str !~ /(\A|\+)thi(\Z|\+)/    ||
   $usearch_col_str !~ /(\A|\+)evalue(\Z|\+)/)
  {
    error("Not all of the required usearch ublast output columns have been ",
	  "specified (-userfields).  The required fields are (in no ",
	  "particular order): [query+target+id+ql+qlo+qhi+tl+tlo+thi+",
	  "evalue].");
    quit(10);
  }

my $col_num = 0;
foreach my $col_name (split(/\+/,$usearch_col_str,-1))
  {
    $col_num++;
    if($col_name =~ /^query$/i)
      {$query_col = $col_num}
    elsif($col_name =~ /^target$/i)
      {$subject_col = $col_num}
    elsif($col_name =~ /^id$/i)
      {$identity_col = $col_num}
    elsif($col_name =~ /^ql$/i)
      {$query_len_col = $col_num}
    elsif($col_name =~ /^qlo$/i)
      {$query_start_col = $col_num}
    elsif($col_name =~ /^qhi$/i)
      {$query_stop_col = $col_num}
    elsif($col_name =~ /^tl$/i)
      {$subject_len_col = $col_num}
    elsif($col_name =~ /^tlo$/i)
      {$subject_start_col = $col_num}
    elsif($col_name =~ /^thi$/i)
      {$subject_stop_col = $col_num}
    elsif($col_name =~ /^evalue$/i)
      {$evalue_col = $col_num}
  }

my $db_manual     = 0;
my $evalue_manual = 0;
my $accel_manual  = 0;
my $quiet_manual  = 0;
if($usearch_opts ne '')
  {
    my $bad_uopts = 0;
    if($usearch_opts =~ /-ublast\b/i)
      {
	error("Manual setting of usearch's -ublast option via ",
	      "--usearch-opts-str is not supported.  Please use this ",
	      "script's -i option instead and remove -ublast from your ",
	      "string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-db\b/i)
      {$db_manual = 1}
    if($usearch_opts =~ /-userout\b/i)
      {
	error("Manual setting of usearch's -userout option via ",
	      "--usearch-opts-str is not supported.  Please remove -userout ",
	      "from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-evalue\b/i)
      {$evalue_manual = 1}
    if($usearch_opts =~ /-id\b/i)
      {
	error("Manual setting of usearch's -id option via ",
	      "--usearch-opts-str is not supported.  Please use this ",
	      "script's -n option instead and remove -id ",
	      "from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-accel\b/i)
      {$accel_manual = 1}
    if($usearch_opts =~ /-strand\b/i)
      {
	error("Manual setting of usearch's -strand option via ",
	      "--usearch-opts-str is not supported.  All sequences are ",
	      "assumed to be aligned and all hits are assumed to only be ",
	      "relevant on the forward/plus strand.  Please remove -strand ",
	      "from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-threads\b/i)
      {
	error("Manual setting of usearch's -threads option via ",
	      "--usearch-opts-str is not supported.  Please use this ",
	      "script's --parallel-processes option instead and remove ",
	      "-threads from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-userfields\b/i)
      {
	error("Manual setting of usearch's -userfields option via ",
	      "--usearch-opts-str is not supported.  Please use this ",
	      "script's --usearch-col-str option instead and remove ",
	      "-userfields from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-query_cov\b/i)
      {
	error("Manual setting of usearch's -query_cov option via ",
	      "--usearch-opts-str is not supported.  This script uses the ",
	      "minimum identity to manipulate the coverage options to force ",
	      "every alignment to the query length and adjust the identity ",
	      "value returned by usearch.  Please remove -query_cov ",
	      "from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-target_cov\b/i)
      {
	error("Manual setting of usearch's -target_cov option via ",
	      "--usearch-opts-str is not supported.  This script uses the ",
	      "minimum identity to manipulate the coverage options to force ",
	      "every alignment to the query length and adjust the identity ",
	      "value returned by usearch.  Please remove -target_cov ",
	      "from your string: [$usearch_opts] and try again.");
	$bad_uopts = 1;
      }
    if($usearch_opts =~ /-quiet\b/i)
      {$quiet_manual = 1}
    quit(11) if($bad_uopts);
  }

##
## Prepare Input/Output Files
##

#Get input & output files in corresponding sets.  E.g.:
#
#    -i '1 2 3' -d 'a b c'                     #Command line
#    $input_files = [[1,2,3]];
#    $other_files = [[a,b,c]];
#    $input_file_sets = [[1,a],[2,b],[3,c]];   #Resulting sets
#
my($input_file_sets,   #getFileSets(3DinfileArray,2DsuffixArray,2DoutdirArray)
   $output_file_sets) = getFileSets(#I'm supplying output files here to be
				    #combined with outdirs and checked for
				    #existence.  They will have an empty string
				    #appended as a suffix
				    [$input_files,
				     $abunds_files,
				     $output_files],

				    #There are never output files for each
				    #input file, thus the first suffix is undef
				    #The other files are fully named outfiles,
				    #so the suffix is an empty string (if
				    #output files were supplied - otherwise
				    #undef
				    [[],
				     [],
				     (scalar(@$output_files) ? [''] : [])],

				    $outdirs,

				    #Aggregate mode = false,false,true
				    [[],
				     [],
				     (scalar(@$output_files) ? [1] : [])]);

#Create the output directories
mkdirs(@$outdirs);

my $seq_hash    = {}; #->{output_file}->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
my $abunds_hash = {}; #->{abunds_file}->{ID}=[#,#,#,...]
my $file_cnt    = {};
my $seq_check   = {};
my($input_file,$abunds_file);

#For each set of corresponding input files
foreach my $set_num (0..$#$input_file_sets)
  {
    $input_file      =    $input_file_sets->[$set_num]->[0];
    $abunds_file     =    $input_file_sets->[$set_num]->[1];
    #I don't need to retrieve the output file names from input_file_sets
    #because they will not have the outdir prepended to them.  Instead, I will
    #get those from output_file_sets.
    my($output_file) = @{$output_file_sets->[$set_num]->[2]};
    $output_file = 'STDOUT' unless(defined($output_file));

    next if($dry_run);

    if(!exists($file_cnt->{$output_file}))
      {$file_cnt->{$output_file} = 0}

    my $seq_recs = getCheckAllSeqRecs($input_file);

    if(scalar(@$seq_recs) == 0)
      {
	error("Could not find any sequences in [$input_file].  Skipping.");
	next;
      }

    #If abundance files were supplied, retrieve the abundance values
    if(defined($abunds_file))
      {
	if(!exists($abunds_hash->{$abunds_file}))
	  {$abunds_hash->{$abunds_file} = getAbundanceTraces($abunds_file)}
	if(scalar($abunds_hash->{$abunds_file}) == 0 ||
	   (scalar($abunds_hash->{$abunds_file}) == 1 &&
	    exists($abunds_hash->{$abunds_file}->{_HEADER_})))
	  {
	    error("Skipping sequence file [$input_file] because there was no ",
		  "abundance data parsed from abundance file [$abunds_file].");
	    next;
	  }
      }

    #For each sequence
    foreach my $seq_rec (@$seq_recs)
      {
	my $id = getID($seq_rec);

	#If there's an abundance file, merge the abundance values
	if(defined($abunds_file))
	  {
	    #It's OK to have more sequences than abundance traces.  We're going
	    #to limit the analysis to only those traces found in the abundance
	    #trace file - which is expected to be the summary file from
	    #getReals.pl.
	    #if(!exists($abunds_hash->{$abunds_file}->{$id}))
	    #  {
	    #	error("Sequence ID [$id] was not found in the abundance ",
	    #	      "trace file [$abunds_file].  Skipping.");
	    #	next;
	    #  }

	    if(exists($abunds_hash->{$abunds_file}->{$id}))
	      {
		#This will be used for error checking later
		$seq_check->{$output_file}->{IDS}->{$id}                = 1;
		$seq_check->{$output_file}->{SEQFILES}->{$input_file}   = 1;
		$seq_check->{$output_file}->{ABUNDFILE}->{$abunds_file} = 1;

		#Save/update the sequence/abundance data
		$seq_hash->{$output_file}->{$id}->{SEQ}      = $seq_rec->[1];
		$seq_hash->{$output_file}->{$id}->{ABUNDS}   =
		  $abunds_hash->{$abunds_file}->{$id}->{ABUNDS};
		$seq_hash->{$output_file}->{$id}->{ABUNDSUM} =
		  $abunds_hash->{$abunds_file}->{$id}->{ABUNDSUM};
	      }
	    #Skip any sequences from the sequence file that are not in the
	    #abundance file, since this comes from getReals and represents a
	    #subset of sequences
	  }
	#Otherwise, we're parsing out the abundances from sample files and
	#using everything
	else
	  {
	    my $abund = getAbund($seq_rec);

	    #We are pushing abundances onto each sequence's abundance array as
	    #we encounter them file by file.  Some files might have 0
	    #occurrences of a particular sequence, so when that happens, we
	    #need to fill in 0s for all those prior files in which a particular
	    #sequence did not occur
	    if(exists($seq_hash->{$output_file}) &&
	       exists($seq_hash->{$output_file}->{$id}))
	      {
		my $array_diff = $file_cnt->{$output_file} -
		  scalar(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}});
		if($array_diff)
		  {push(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}},
			((0) x $array_diff))}
		push(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}},$abund);
	      }
	    else
	      {
		if($file_cnt->{$output_file})
		  {$seq_hash->{$output_file}->{$id}->{ABUNDS} =
		     [(0) x $file_cnt->{$output_file}]}
		push(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}},$abund);
	      }
	    $seq_hash->{$output_file}->{$id}->{SEQ} = $seq_rec->[1];
	    $seq_hash->{$output_file}->{$id}->{ABUNDSUM} += $abund;
	  }
      }

    $file_cnt->{$output_file}++;
    debug("FILE COUNT FOR OUTFILE: [$output_file]: [",
	  "$file_cnt->{$output_file}].") if($DEBUG > 1);
  }

if(scalar(grep {$file_cnt->{$_}} keys(%$file_cnt)) == 0)
  {
    error("Unable to parse any of the sequence files.");
    quit(12);
  }

#Now filter everything for mean abundance.  Of the ones that pass, the
#abundances of some will not be quite filled in yet, so top them off with
#zeroes.
foreach my $output_file (keys(%$seq_hash))
  {
    my($abunds_file,$num_samples);
    #If abundance files were supplied, make sure everything is OK
    if(scalar(@$abunds_files))
      {
	#There should only be 1 abundance file for every output file
	if(scalar(keys(%{$seq_check->{$output_file}->{ABUNDFILE}})) != 1)
	  {
	    error("There should only be one abundance file for every output ",
		  "file, but found: [",
		  join(',',keys(%{$seq_check->{$output_file}->{ABUNDFILE}})),
		  "].  If you see this error, please contact the author.");
	    next;
	  }
	$abunds_file = (keys(%{$seq_check->{$output_file}->{ABUNDFILE}}))[0];

	#Get the number of samples from an arbitrary ID
	foreach my $id (keys(%{$seq_hash->{$output_file}}))
	  {
	    $num_samples =
	      scalar(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}});
	    last;
	  }

	#Gather any IDs that were missing from the sequence file(s)
	my $missing_ids = [];
	foreach my $id (grep {$_ ne '_HEADER_'}
			keys(%{$abunds_hash->{$abunds_file}}))
	  {if(!exists($seq_check->{$output_file}->{IDS}->{$id}))
	     {push(@$missing_ids,$id)}}

	#Report missing IDs and skip this analysis
	if(scalar(@$missing_ids))
	  {
	    my $report_ids = [@$missing_ids];
	    $report_ids = [@{$missing_ids}[0..8],'...']
	      if(scalar(@$missing_ids) > 10);
	    my $report_files =
	      [keys(%{$seq_check->{$output_file}->{SEQFILES}})];
	    $report_files = [@{$report_files}[0..1],'...']
	      if(scalar(@$missing_ids) > 3);
	    error("[",scalar(@$missing_ids),"] IDs found in the abundance ",
		  "trace file [$abunds_file] were not found in any of the ",
		  "associated sequence files [",join(',',@$report_files),
		  "].  Missing IDs: [",join(',',@$report_ids),"].  Skipping.");
	    next;
	  }
      }
    else
      {$num_samples = $file_cnt->{$output_file}}

    #Determine the minimum mean abundance
    my $total_abund = 0;
    $total_abund += $seq_hash->{$output_file}->{$_}->{ABUNDSUM}
      foreach(keys(%{$seq_hash->{$output_file}}));
    my $local_min_mean_abund = (defined($min_mean_abund) ? $min_mean_abund :
			        int($total_abund * 0.01 / $num_samples));

    verbose("Total abundance of all sequences/samples = [$total_abund].\n",
	    "Minimum mean abundance: [$local_min_mean_abund].");

    #Enforce the minimum mean abundance
    foreach my $id (keys(%{$seq_hash->{$output_file}}))
      {
	debug("AVERAGE ABUNDANCE OF [$id]: ((",
	      "$seq_hash->{$output_file}->{$id}->{ABUNDSUM} / ",
	      "$num_samples) < $local_min_mean_abund).") if($DEBUG > 1);
	if(($seq_hash->{$output_file}->{$id}->{ABUNDSUM} / $num_samples) <
	   $local_min_mean_abund)
	  {
	    delete($seq_hash->{$output_file}->{$id});
	    next;
	  }

	#If we've parsed the abundances from the sequence files (i.e. there is
	#no abundance file), we need to top off any missing abundances after
	#the last sequence file was read
	if(!defined($abunds_file))
	  {
	    my $array_diff = $file_cnt->{$output_file} -
	      scalar(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}});
	    if($array_diff)
	      {push(@{$seq_hash->{$output_file}->{$id}->{ABUNDS}},
		    ((0) x $array_diff))}
	  }
      }

    #If there are too few IDs left after enforcing min mean abundance
    if(scalar(keys(%{$seq_hash->{$output_file}})) < 2)
      {
	error((scalar(keys(%{$seq_hash->{$output_file}})) == 1 ?
	       "Only 1 sequence" : "None of the sequences"),
	      " had the minimum mean abundance across all the samples (-n): ",
	      "[$local_min_mean_abund].  Please make sure that your ",
	      "abundance values are on the sequence deflines (see -p) or ",
	      "lower the minimum mean abundance (see -n).");
	cleanTmpFiles();
	quit(13);
      }

    #Retrieve similar sequence pairs
    my $seq_pair_hash = getSimilarSeqPairs($seq_hash->{$output_file},
					   $min_seqsim);

    #Clean up the temporary files created for usearch
    cleanTmpFiles();

    debug("Similar sequence pairs ([",scalar(keys(%$seq_pair_hash)),
	  "] of them):\n",
	  map {my $p=$_;"$p\t" .
		 join("\n$p\t",
		      map {"$_\t" .
			     join("\t",@{$seq_pair_hash->{$p}->{$_}->{HIT}})}
		      keys(%{$seq_pair_hash->{$p}})) .
			"\n"}
	  keys(%$seq_pair_hash)) if($DEBUG);

    #Further filter the similar sequence pairs by abundance dissimilarity
    my $abund_pair_hash = getDissimilarAbundPairs($seq_pair_hash,
						  $seq_hash->{$output_file},
						  $max_dynsim);

    #Open the output file
    if($output_file ne 'STDOUT')
      {openOut(*PAIRS,$output_file)}

    reportPairs($abund_pair_hash,
		$seq_pair_hash,
		$seq_hash->{$output_file},
	        (defined($abunds_file) ? $abunds_hash->{$abunds_file} : {}),
	        undef,
	        undef,
	        $local_min_mean_abund);

    #Close the output file
    if($output_file ne 'STDOUT')
      {closeOut(*PAIRS)}
  }

printRunReport();

##
## End Main
##

























































































BEGIN
  {
    #This allows us to track runtime warnings about undefined variables, etc.
    local $SIG{__WARN__} = sub {my $err = $_[0];chomp($err);
				warning("Runtime warning: [$err].")};
  }

##
## Subroutines
##

##
## Subroutine that prints formatted verbose messages.  Specifying a 1 as the
## first argument prints the message in overwrite mode (meaning subsequence
## verbose, error, warning, or debug messages will overwrite the message
## printed here.  However, specifying a hard return as the first character will
## override the status of the last line printed and keep it.  Global variables
## keep track of print length so that previous lines can be cleanly
## overwritten.
##
sub verbose
  {
    return(0) unless($verbose);

    #Read in the first argument and determine whether it's part of the message
    #or a value for the overwrite flag
    my $overwrite_flag = $_[0];

    #If a flag was supplied as the first parameter (indicated by a 0 or 1 and
    #more than 1 parameter sent in)
    if(scalar(@_) > 1 && ($overwrite_flag eq '0' || $overwrite_flag eq '1'))
      {shift(@_)}
    else
      {$overwrite_flag = 0}

#    #Ignore the overwrite flag if STDOUT will be mixed in
#    $overwrite_flag = 0 if(isStandardOutputToTerminal());

    #Read in the message
    my $verbose_message = join('',grep {defined($_)} @_);

    $overwrite_flag = 1 if(!$overwrite_flag && $verbose_message =~ /\r/);

    #Initialize globals if not done already
    $main::last_verbose_size  = 0 if(!defined($main::last_verbose_size));
    $main::last_verbose_state = 0 if(!defined($main::last_verbose_state));
    $main::verbose_warning    = 0 if(!defined($main::verbose_warning));

    #Determine the message length
    my($verbose_length);
    if($overwrite_flag)
      {
	$verbose_message =~ s/\r$//;
	if(!$main::verbose_warning && $verbose_message =~ /\n|\t/)
	  {
	    warning('Hard returns and tabs cause overwrite mode to not work ',
		    'properly.');
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    #If this message is not going to be over-written (i.e. we will be printing
    #a \n after this verbose message), we can reset verbose_length to 0 which
    #will cause $main::last_verbose_size to be 0 the next time this is called
    if(!$overwrite_flag)
      {$verbose_length = 0}
    #If there were \r's in the verbose message submitted (after the last \n)
    #Calculate the verbose length as the largest \r-split string
    elsif($verbose_message =~ /\r[^\n]*$/)
      {
	my $tmp_message = $verbose_message;
	$tmp_message =~ s/.*\n//;
	($verbose_length) = sort {$b <=> $a} map {length($_)}
	  split(/\r/,$tmp_message);
      }
    #Otherwise, the verbose_length is the size of the string after the last \n
    elsif($verbose_message =~ /([^\n]*)$/)
      {$verbose_length = length($1)}

    #If the buffer is not being flushed, the verbose output doesn't start with
    #a \n, and output is to the terminal, make sure we don't over-write any
    #STDOUT output
    #NOTE: This will not clean up verbose output over which STDOUT was written.
    #It will only ensure verbose output does not over-write STDOUT output
    #NOTE: This will also break up STDOUT output that would otherwise be on one
    #line, but it's better than over-writing STDOUT output.  If STDOUT is going
    #to the terminal, it's best to turn verbose off.
    if(!$| && $verbose_message !~ /^\n/ && isStandardOutputToTerminal())
      {
	#The number of characters since the last flush (i.e. since the last \n)
	#is the current cursor position minus the cursor position after the
	#last flush (thwarted if user prints \r's in STDOUT)
	#NOTE:
	#  tell(STDOUT) = current cursor position
	#  sysseek(STDOUT,0,1) = cursor position after last flush (or undef)
	my $num_chars = sysseek(STDOUT,0,1);
	if(defined($num_chars))
	  {$num_chars = tell(STDOUT) - $num_chars}
	else
	  {$num_chars = 0}

	#If there have been characters printed since the last \n, prepend a \n
	#to the verbose message so that we do not over-write the user's STDOUT
	#output
	if($num_chars > 0)
	  {$verbose_message = "\n$verbose_message"}
      }

    #Overwrite the previous verbose message by appending spaces just before the
    #first hard return in the verbose message IF THE VERBOSE MESSAGE DOESN'T
    #BEGIN WITH A HARD RETURN.  However note that the length stored as the
    #last_verbose_size is the length of the last line printed in this message.
    if($verbose_message =~ /^([^\n]*)/ && $main::last_verbose_state &&
       $verbose_message !~ /^\n/)
      {
	my $append = ' ' x ($main::last_verbose_size - length($1));
	unless($verbose_message =~ s/\n/$append\n/)
	  {$verbose_message .= $append}
      }

    #If you don't want to overwrite the last verbose message in a series of
    #overwritten verbose messages, you can begin your verbose message with a
    #hard return.  This tells verbose() to not overwrite the last line that was
    #printed in overwrite mode.

    #Print the message to standard error
    print STDERR ($verbose_message,
		  ($overwrite_flag ? "\r" : "\n"));

    #Record the state
    $main::last_verbose_size  = $verbose_length;
    $main::last_verbose_state = $overwrite_flag;

    #Return success
    return(0);
  }

sub verboseOverMe
  {verbose(1,@_)}

##
## Subroutine that prints errors with a leading program identifier containing a
## trace route back to main to see where all the subroutine calls were from,
## the line number of each call, an error number, and the name of the script
## which generated the error (in case scripts are called via a system call).
## Globals used defined in main: error_limit, quiet, verbose
## Globals used defined in here: error_hash, error_number
## Globals used defined in subs: last_verbose_state, last_verbose_size
##
sub error
  {
    return(0) if($quiet);

    #Gather and concatenate the error message and split on hard returns
    my @error_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@error_message,'') unless(scalar(@error_message));
    pop(@error_message) if(scalar(@error_message) > 1 &&
			   $error_message[-1] !~ /\S/);

    $main::error_number++;
    my $leader_string = "ERROR$main::error_number:";

    #Assign the values from the calling subroutines/main
    my(@caller_info,$line_num,$caller_string,$stack_level,$script);

    #Build a trace-back string.  This will be used for tracking the number of
    #each type of error as well as embedding into the error message in debug
    #mode.
    $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    @caller_info = caller(0);
    $line_num = $caller_info[2];
    $caller_string = '';
    $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    if($DEBUG)
      {$leader_string .= "$script:$caller_string"}

    $leader_string .= ' ';
    my $leader_length = length($leader_string);

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Clean up any previous verboseOverMe output that may be longer than the
    #first line of the error message, put leader string at the beginning of
    #each line of the message, and indent each subsequent line by the length
    #of the leader string
    my $error_string = $leader_string . shift(@error_message) .
      ($verbose && defined($main::last_verbose_state) &&
       $main::last_verbose_state ?
       ' ' x ($main::last_verbose_size - $error_length) : '') . "\n";
    foreach my $line (@error_message)
      {$error_string .= (' ' x $leader_length) . $line . "\n"}

    #If the global error hash does not yet exist, store the first example of
    #this error type
    if(!defined($main::error_hash) ||
       !exists($main::error_hash->{$caller_string}))
      {
	$main::error_hash->{$caller_string}->{EXAMPLE}    = $error_string;
	$main::error_hash->{$caller_string}->{EXAMPLENUM} =
	  $main::error_number;

	$main::error_hash->{$caller_string}->{EXAMPLE} =~ s/\n */ /g;
	$main::error_hash->{$caller_string}->{EXAMPLE} =~ s/ $//g;
	$main::error_hash->{$caller_string}->{EXAMPLE} =~ s/^(.{100}).+/$1.../;
      }

    #Increment the count for this error type
    $main::error_hash->{$caller_string}->{NUM}++;

    #Print the error unless it is over the limit for its type
    if($error_limit == 0 ||
       $main::error_hash->{$caller_string}->{NUM} <= $error_limit)
      {
	print STDERR ($error_string);

	#Let the user know if we're going to start suppressing errors of this
	#type
	if($error_limit &&
	   $main::error_hash->{$caller_string}->{NUM} == $error_limit)
	  {print STDERR ($leader_string,"NOTE: Further errors of this type ",
			 "will be suppressed.\n$leader_string",
			 "Set --error-type-limit to 0 to turn off error ",
			 "suppression\n")}
      }

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size  = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }

##
## Subroutine that prints warnings with a leader string containing a warning
## number
##
## Globals used defined in main: error_limit, quiet, verbose
## Globals used defined in here: warning_hash, warning_number
## Globals used defined in subs: last_verbose_state, last_verbose_size
##
sub warning
  {
    return(0) if($quiet);

    $main::warning_number++;

    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@warning_message,'') unless(scalar(@warning_message));
    pop(@warning_message) if(scalar(@warning_message) > 1 &&
			     $warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number:";

    #Assign the values from the calling subroutines/main
    my(@caller_info,$line_num,$caller_string,$stack_level,$script);

    #Build a trace-back string.  This will be used for tracking the number of
    #each type of warning as well as embedding into the warning message in
    #debug mode.
    $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    @caller_info = caller(0);
    $line_num = $caller_info[2];
    $caller_string = '';
    $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    if($DEBUG)
      {$leader_string .= "$script:$caller_string"}

    $leader_string   .= ' ';
    my $leader_length = length($leader_string);

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Clean up any previous verboseOverMe output that may be longer than the
    #first line of the warning message, put leader string at the beginning of
    #each line of the message and indent each subsequent line by the length
    #of the leader string
    my $warning_string =
      $leader_string . shift(@warning_message) .
	($verbose && defined($main::last_verbose_state) &&
	 $main::last_verbose_state ?
	 ' ' x ($main::last_verbose_size - $warning_length) : '') .
	   "\n";
    foreach my $line (@warning_message)
      {$warning_string .= (' ' x $leader_length) . $line . "\n"}

    #If the global warning hash does not yet exist, store the first example of
    #this warning type
    if(!defined($main::warning_hash) ||
       !exists($main::warning_hash->{$caller_string}))
      {
	$main::warning_hash->{$caller_string}->{EXAMPLE}    = $warning_string;
	$main::warning_hash->{$caller_string}->{EXAMPLENUM} =
	  $main::warning_number;

	$main::warning_hash->{$caller_string}->{EXAMPLE} =~ s/\n */ /g;
	$main::warning_hash->{$caller_string}->{EXAMPLE} =~ s/ $//g;
	$main::warning_hash->{$caller_string}->{EXAMPLE} =~
	  s/^(.{100}).+/$1.../;
      }

    #Increment the count for this warning type
    $main::warning_hash->{$caller_string}->{NUM}++;

    #Print the warning unless it is over the limit for its type
    if($error_limit == 0 ||
       $main::warning_hash->{$caller_string}->{NUM} <= $error_limit)
      {
	print STDERR ($warning_string);

	#Let the user know if we're going to start suppressing warnings of this
	#type
	if($error_limit &&
	   $main::warning_hash->{$caller_string}->{NUM} == $error_limit)
	  {print STDERR ($leader_string,"NOTE: Further warnings of this ",
			 "type will be suppressed.\n$leader_string",
			 "Set --error-type-limit to 0 to turn off error ",
			 "suppression\n")}
      }

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size  = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }

##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer) to keep track of
## buffered lines from multiple file handles.
##
sub getLine
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle}->{FILE} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) > 0)
	  {
	    return(@{$main::infile_line_buffer->{$file_handle}->{FILE}},
		   map
		   {
		     #If carriage returns were substituted and we haven't
		     #already issued a carriage return warning for this file
		     #handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!exists($main::infile_line_buffer->{$file_handle}
				->{WARNED}))
		       {
			 $main::infile_line_buffer->{$file_handle}->{WARNED}
			   = 1;
			 warning('Carriage returns were found in your file ',
				 'and replaced with hard returns.');
		       }
		     split(/(?<=\n)/,$_);
		   } <$file_handle>);
	  }
	
	#Otherwise return everything else
	return(map
	       {
		 #If carriage returns were substituted and we haven't already
		 #issued a carriage return warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !exists($main::infile_line_buffer->{$file_handle}
			    ->{WARNED}))
		   {
		     $main::infile_line_buffer->{$file_handle}->{WARNED}
		       = 1;
		     warning('Carriage returns were found in your file ',
			     'and replaced with hard returns.');
		   }
		 split(/(?<=\n)/,$_);
	       } <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) == 0)
      {
	my $line = <$file_handle>;
	#The following is to deal with files that have the eof character at the
	#end of the last line.  I may not have it completely right yet.
	if(defined($line))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	      {
		$main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
		warning('Carriage returns were found in your file and ',
			'replaced with hard returns.');
	      }
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	      split(/(?<=\n)/,$line);
	  }
	else
	  {@{$main::infile_line_buffer->{$file_handle}->{FILE}} = ($line)}
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }

##
## This subroutine allows the user to print debug messages containing the line
## of code where the debug print came from and a debug number.  Debug prints
## will only be printed (to STDERR) if the debug option is supplied on the
## command line.
##
sub debug
  {
    return(0) unless($DEBUG);

    $main::debug_number++;

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split(/\n/,join('',grep {defined($_)} @_));
    push(@debug_message,'') unless(scalar(@debug_message));
    pop(@debug_message) if(scalar(@debug_message) > 1 &&
			   $debug_message[-1] !~ /\S/);

    my $leader_string = "DEBUG$main::debug_number:";

    #Assign the values from the calling subroutines/main
    my(@caller_info,$line_num,$caller_string,$stack_level,$script);

    #Build a trace-back string.
    $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    @caller_info = caller(0);
    $line_num = $caller_info[2];
    $caller_string = '';
    $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
#    $caller_string .= "MAIN(LINE$line_num): ";

    #I just want to know the sub or place in main we were in when the debug was
    #called (unless we're in the 'quit' sub)
    if($caller_string =~ /^quit/)
      {$caller_string = "MAIN(LINE$line_num): ";}
    else
      {$caller_string =~ s/:.*/:/}

    $leader_string .= $caller_string;

    #Figure out the length of the first line of the error
    my $debug_length = length(($debug_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $debug_message[0]);

    #Put location information at the beginning of each line of the message
    print STDERR ($leader_string,
		  shift(@debug_message),
		  ($verbose &&
		   defined($main::last_verbose_state) &&
		   $main::last_verbose_state ?
		   ' ' x ($main::last_verbose_size - $debug_length) : ''),
		  "\n");
    my $leader_length = length($leader_string);
    foreach my $line (@debug_message)
      {print STDERR (' ' x $leader_length,
		     $line,
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }

##
## This sub marks the time (which it pushes onto an array) and in scalar
## context returns the time since the last mark by default or supplied mark
## (optional) In array context, if an index is not supplied, the time between
## all marks is returned.
## A mark is not made if a mark index is supplied
## Uses a global time_marks array reference
##
sub markTime
  {
    #Record the time
    my $time = time();

    #Set a global array variable if not already set to contain (as the first
    #element) the time the program started (NOTE: "$^T" is a perl variable that
    #contains the start time of the script)
    $main::time_marks = [$^T] if(!defined($main::time_marks));

    #Read in the time mark index or set the default value
    my $mark_index = (defined($_[0]) ? $_[0] : -1);  #Optional Default: -1

    #Error check the time mark index sent in
    if($mark_index > (scalar(@$main::time_marks) - 1))
      {
	error('Supplied time mark index is larger than the size of the ',
	      "time_marks array.\nThe last mark will be set.");
	$mark_index = -1;
      }

    #Calculate the time since the time recorded at the time mark index
    my $time_since_mark = $time - $main::time_marks->[$mark_index];

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

    #If called in array context, return time between all marks
    if(!defined($_[0]) && wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_] - $main::time_marks->[$_ - 1]}
		  (1..(scalar(@$main::time_marks) - 1)))}
	else
	  {return(())}
      }

    #Return the time since the time recorded at the supplied time mark index
    return($time_since_mark);
  }

##
## This subroutine reconstructs the command entered on the command line
## (excluding standard input and output redirects).  The intended use for this
## subroutine is for when a user wants the output to contain the input command
## parameters in order to keep track of what parameters go with which output
## files.
##
#Globals used: $preserve_args
sub getCommand
  {
    my $perl_path_flag = $_[0];
    my $no_defaults    = $_[1];
    my($command);
    my @return_args = ();

    #Determine the script name
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Put quotes around any parameters containing un-escaped spaces or asterisks
    my $arguments = [@$preserve_args];
    foreach my $arg (@$arguments)
      {if($arg =~ /(?<!\\)[\s\*]/ || $arg =~ /^<|>|\|(?!\|)/ || $arg eq '' ||
	  $arg =~ /[\{\}\[\]\(\)]/)
	 {$arg = "'" . $arg . "'"}}

    #Determine the perl path used (dependent on the `which` unix built-in)
    if($perl_path_flag)
      {
	$command = `which $^X`;
	push(@return_args,$command);
	chomp($command);
	$command .= ' ';
      }

    #Build the original command
    $command .= join(' ',($0,@$arguments));
    push(@return_args,($0,@$arguments));

    #Add any default flags that were previously saved
    my @default_options = getUserDefaults();
    if(!$no_defaults && scalar(@default_options))
      {
	$command .= ' [USER DEFAULTS ADDED: ';
	$command .= join(' ',@default_options);
	$command .= ']';
	push(@return_args,@default_options);
      }

    return(wantarray ? @return_args : $command);
  }

##
## This subroutine checks for files with spaces in the name before doing a glob
## (which breaks up the single file name improperly even if the spaces are
## escaped).  The purpose is to allow the user to enter input files using
## double quotes and un-escaped spaces as is expected to work with many
## programs which accept individual files as opposed to sets of files.  If the
## user wants to enter multiple files, it is assumed that space delimiting will
## prompt the user to realize they need to escape the spaces in the file names.
## This version works with a mix of unescaped and escaped spaces, as well as
## glob characters.  It will also split non-files on unescaped spaces as well.
##
sub sglob
  {
    my $command_line_string = $_[0];
    unless(defined($command_line_string))
      {
	warning("Undefined command line string encountered.");
	return($command_line_string);
      }

    my $home = '';
    if(exists($ENV{HOME}) && -e $ENV{HOME})
      {$home = $ENV{HOME}}

    #Note, when bsd_glob gets a string with a glob character it can't expand,
    #it drops the string entirely.  Those strings are returned with the glob
    #characters so the surrounding script can report an error.
    return(map {my @x = bsd_glob($_);my $v = $_;
		scalar(@x) &&               #An existing file or ~ expansion
		  scalar(@x) == scalar(grep {-e $_ ||
					       ($home &&
						$v =~ /~/ &&
						m%$home%)} @x) ? @x : $_}
	   split(/(?<!\\)\s+/,$command_line_string));
  }

#Globals used: $software_version_number, $created_on_date
sub getVersion
  {
    my $version_message   = '';
    my $template_version  = 3.2;
    my $script            = $0;
    $script               =~ s/^.*\/([^\/]+)$/$1/;
    my $lmd               = localtime((stat($0))[9]);

    if(!defined($software_version_number) || $software_version_number !~ /\S/)
      {
	warning("Software version number variable unset/missing.");
	$software_version_number = 'unknown';
      }

    if((!defined($created_on_date) || $created_on_date eq 'DATE HERE') &&
       $0 !~ /perl_script_template\.pl$/)
      {
	warning("Created-on-date global variable unset/missing.");
	$created_on_date = 'UNKNOWN';
      }

    #Create version string
    $version_message  = '#' . join("\n#",
				   ("$script Version $software_version_number",
				    " Created: $created_on_date",
				    " Last modified: $lmd"));

    #Add template version
    $version_message .= "\n#" .
      join("\n#",
	   ('Generated using perl_script_template.pl ' .
	    "Version $template_version",
	    ' Created: 5/8/2006',
	    ' Author:  Robert W. Leach',
	    ' Contact: rleach@genomics.princeton.edu',
	    ' Company: Princeton University',
	    ' Copyright 2014'));

    return($version_message);
  }

#This subroutine is a check to see if input is user-entered via a TTY (result
#is non-zero) or directed in (result is zero)
sub isStandardInputFromTerminal
  {return(-t STDIN || eof(STDIN))}

#This subroutine is a check to see if prints are going to a TTY.  Note,
#explicit prints to STDOUT when another output handle is selected are not
#considered and may defeat this subroutine.
sub isStandardOutputToTerminal
  {return(-t STDOUT && select() eq 'main::STDOUT')}

#This subroutine exits the current process.  Note, you must clean up after
#yourself before calling this.  Does not exit if $force is true.  Takes the
#error number to supply to exit().
sub quit
  {
    my $errno = $_[0];

    if(!defined($errno))
      {$errno = -1}
    elsif($errno !~ /^[+\-]?\d+$/)
      {
	error("Invalid argument: [$errno].  Only integers are accepted.  Use ",
	      "error() or warn() to supply a message, then call quit() with ",
	      "an error number.");
	$errno = -1;
      }

    debug("Exit status: [$errno].");

    #Exit if there were no errors or we are not in force mode or (we are in
    #force mode and the error is -1 (meaning an overwrite situation))
    exit($errno) if($errno == 0 || !$force || ($force && $errno == -1));
  }

#Globals used: $quiet, $verbose
sub printRunReport
  {
    my $local_verbose = $_[0];

    #Return if quiet or there's nothing to report
    return(0) if($quiet || (!$verbose && !$DEBUG &&
		            !defined($main::error_number) &&
		            !defined($main::warning_number)));

    #Report the number of errors, warnings, and debugs on STDERR
    print STDERR ("\n",'Done.  EXIT STATUS: [',
		  'ERRORS: ',
		  ($main::error_number ? $main::error_number : 0),' ',
		  'WARNINGS: ',
		  ($main::warning_number ? $main::warning_number : 0),
		  ($DEBUG ?
		   ' DEBUGS: ' .
		   ($main::debug_number ? $main::debug_number : 0) : ''),' ',
		  'TIME: ',scalar(markTime(0)),"s]");

    #Print an extended report if requested or there was an error or warning
    if($verbose || $local_verbose ||
       defined($main::error_number) ||
       defined($main::warning_number))
      {
	if($main::error_number || $main::warning_number)
	  {print STDERR " SUMMARY:\n"}
	else
	  {print STDERR "\n"}

	#If there were errors
	if($main::error_number)
	  {
	    foreach my $err_type
	      (sort {$main::error_hash->{$a}->{EXAMPLENUM} <=>
		       $main::error_hash->{$b}->{EXAMPLENUM}}
	       keys(%$main::error_hash))
	      {print STDERR ("\t",$main::error_hash->{$err_type}->{NUM},
			     " ERROR",
			     ($main::error_hash->{$err_type}->{NUM} > 1 ?
			      'S' : '')," LIKE: [",
			     $main::error_hash->{$err_type}->{EXAMPLE},"]\n")}
	  }

	#If there were warnings
	if($main::warning_number)
	  {
	    foreach my $warn_type
	      (sort {$main::warning_hash->{$a}->{EXAMPLENUM} <=>
		       $main::warning_hash->{$b}->{EXAMPLENUM}}
	       keys(%$main::warning_hash))
	      {print STDERR ("\t",$main::warning_hash->{$warn_type}->{NUM},
			     " WARNING",
			     ($main::warning_hash->{$warn_type}->{NUM} > 1 ?
			      'S' : '')," LIKE: [",
			     $main::warning_hash->{$warn_type}->{EXAMPLE},
			     "]\n")}
	  }

        if(defined($main::error_number) || defined($main::warning_number))
          {print STDERR ("\tScroll up to inspect full errors/warnings ",
		         "in-place.\n")}
      }
    else
      {print STDERR "\n"}
  }


#This subroutine takes multiple "types" of "sets of input files" in a 3D array
#and returns an array of combination arrays where a combination contains 1 file
#of each type.  The best way to explain the associations is by example.  Here
#are example input file associations without output suffixes or directories.
#Each type is a 2D array contained in the outer type array:

#Example 1:
#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#input files of type 3: [[x,y]]
#resulting associations: [[1,4,x],[2,5,x],[3,6,x],[a,d,y],[b,e,y],[c,f,y]]
#Example 2:
#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#input files of type 3: [[x,y,z]]
#resulting associations: [[1,4,x],[2,5,y],[3,6,z],[a,d,x],[b,e,y],[c,f,z]]
#Example 3:
#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#input files of type 3: [[x],[y]]
#resulting associations: [[1,4,x],[2,5,x],[3,6,x],[a,d,y],[b,e,y],[c,f,y]]
#Example 4:
#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#input files of type 3: [[x],[y],[z]]
#resulting associations: [[1,4,x],[2,5,y],[3,6,z],[a,d,x],[b,e,y],[c,f,z]]
#Example 5:
#input files of type 1: [[1,a],[2,b],[3,c]]
#input files of type 2: [[4,d],[5,e],[6,f]]
#input files of type 3: [[x],[y],[z]]
#resulting associations: [[1,4,x],[2,5,y],[3,6,z],[a,d,x],[b,e,y],[c,f,z]]
#Example 6:
#input files of type 1: [[1],[2]]
#input files of type 2: [[a]]
#resulting associations: [[1,a],[2,a]]

#If you submit a 2D array or 1D array, or even a single string, the subroutine
#will wrap it up into a 3D array for processing.  Note that a 1D array mixed
#with 2D arrays will prompt the subroutine to guess which way to associate that
#series of files in the 1D array(s) with the rest.
#The dimensions of the 2D arrays are treated differently if they are the same
#as when they are different.  First, the subroutine will attempt to match array
#dimensions by transposing (and if a dimension is 1, it will copy elements to
#fill it up to match).  For example, the subroutine detects that the second
#dimension in this example matches, so it will copy the 1D array:

#From this:
#input files of type 1: [[1,2],[a,b]]
#input files of type 2: [[4,5],[d,e]]
#input files of type 3: [[x,y]]       #[x,y] will be copied to match dimensions
#To this:
#input files of type 1: [[1,2],[a,b]]
#input files of type 2: [[4,5],[d,e]]
#input files of type 3: [[x,y],[x,y]]
#resulting associations: [[1,4,x],[2,5,y],[a,d,x],[b,e,y]]

#There are also 2 other optional inputs for creating the second return value
#(an array of output file stubs/names associated with each input file).  The
#two optional inputs are a 1D array of outfile suffixes and a 2D array of
#output directories.

#Associations between output directories will be made in the same way as
#between different input file types.  For example, when suffixes are provided
#for type 1:

#Example 1:
#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#outfile suffixes: [.txt,.tab]
#resulting input file associations: [[1,4],[2,5],[3,6],[a,d],[b,e],[c,f]]
#resulting outfile names:  [[1.txt,4.tab],[2.txt,5.tab],[3.txt,6.tab],
#                           [a.txt,d.tab],[b.txt,e.tab],[c.txt,f.tab]]

#Output directories are associated with combinations of files as if the output
#directory 2D array was another file type.  However, the most common expected
#usage is that all output will go to a single directory, so here's an example
#where only the first input file type generates an output file and all output
#goes to a single output directory:

#input files of type 1: [[1,2,3],[a,b,c]]
#input files of type 2: [[4,5,6],[d,e,f]]
#outfile suffixes: [.txt]
#output directories: [[out]]
#resulting input file associations: [[1,4],[2,5],[3,6],[a,d],[b,e],[c,f]]
#resulting outfile names:  [[1.txt,undef],[2.txt,undef],[3.txt,undef],
#                           [a.txt,undef],[b.txt,undef],[c.txt,undef]]

#Note that this subroutine also detects input on standard input and treats it
#as an input of the same type as the first array in the file types array passed
#in.  If there is only one input file in that array, it will be considered to
#be a file name "stub" to be used to append outfile suffixes.

#Globals used: $overwrite, $skip_existing, $aggregate_mode, $compound_mode
sub getFileSets
  {
    my $file_types_array = copyArray($_[0]); #Copied to not change in main
                                  #A 3D array where the outer array specifies
                                  #file type (e.g. all files supplied by
                                  #instances of -i), the next array specifies
                                  #a specific instance of an option/flag (e.g.
                                  #the first instance of -i on the command
                                  #line) and the inner-most array contains the
                                  #arguments to that instance of that flag.
    my $outfile_suffixes = copyArray($_[1]); #Copied to not change in main
                                  #OPTIONAL: An array (2D) no larger than
                                  #file_types_array's outer array (multiple
                                  #suffixes per input file type).  The order of
                                  #the suffix types must correspond to the
                                  #order of the input file types.  I.e. the
                                  #outer array of the file_types_array must
                                  #have the same corresponding order of type
                                  #elements (though it may contain fewer
                                  #elements if (e.g.) only 1 type of input file
                                  #has output files).  E.g. If the first type
                                  #in the file_types_array is files submitted
                                  #with -i, the first suffix will be appended
                                  #to files of type 1.  Note that if suffixes
                                  #are provided, any type without a suffix will
                                  #not be present in the returned outfile array
                                  #(there will be an undefined value as a
                                  #placeholder).  If no suffixes are provided,
                                  #the returned outfile array will contain
                                  #outfile stubs for every input file type to
                                  #which you must append your own suffix.
    my $outdir_array     = copyArray($_[2]); #Copied to not change in main
                                  #OPTIONAL: A 2D array of output directories.
                                  #The dimensions of this array must either be
                                  #1x1, 1xN, or NxM where N or NxM must
                                  #correspond to the dimensions of one of the
                                  #input file types.  See notes above for an
                                  #example.  Every input file combination will
                                  #output to a single output directory.  Also
                                  #note that if suffixes are provided, any type
                                  #without a suffix will not be present in the
                                  #returned outfile array.  If no suffixes are
                                  #provided, the returned outfile array will
                                  #contain outfile stubs to which you must
                                  #append your own suffix.
    my $aggregate_suf_mode = defined($_[3]) ? ($_[3] ? $_[3] : 0) :
      (defined($aggregate_mode) ? $aggregate_mode : 0); #OPTIONAL [On] Allow
                                  #duplicate suffixed-outfiles/stubs between
                                  #combinations of input files - the coder must
                                  #keep track of aggregate output file names &
                                  #not over-write them with output from a new
                                  #combo of input files.  What this means is
                                  #that each combo returned will allow the same
                                  #outfile stub to be returned multiple times,
                                  #so the coder must open it for writing the
                                  #first time only, and each subsequent time,
                                  #must open in append mode.  This argument may
                                  #be a scalar, or be a 2D array that has the
                                  #same or lesser dimensions as the outfile
                                  #suffixes array.  If turned off, and
                                  #duplicates exist, the outfile names will be
                                  #compounded (as long as compound conflicts is
                                  #true, otherwise, it will quit the script
                                  #with an error).
    my $compound_conflicts = defined($_[4]) ? ($_[4] ? $_[4] : 0) :
      (defined($compound_mode) ? $compound_mode : 0); #OPTIONAL [Off] Allow
                                  #predicted file name conflicts to be made
                                  #unique by compounding the input file names
                                  #(only controls the case where multiple input
                                  #files of the same type reside in different
                                  #directories, have the same name, and are
                                  #outputting to the same directory (i.e.
                                  #outdir_array is not empty).  If left off, a
                                  #conflict will cause the script to exit.  If
                                  #turned on and conflicts exist, all outfile
                                  #names will be compounded (before
                                  #aggregate_suf_mode=0 compounds).
    my $outfile_stub = defined($default_stub) ? $default_stub : 'STDIN';

    eval {use Data::Dumper;1} if($DEBUG < 0);

    debug("Num initial arguments: [",scalar(@_),"].") if($DEBUG < -99);

    debug("Initial size of file types array: [",scalar(@$file_types_array),
	  "].") if($DEBUG < -99);

    ##
    ## Error check/fix the file_types_array (a 3D array of strings)
    ##
    if(ref($file_types_array) ne 'ARRAY')
      {
	#Allow them to submit scalars of everything
	if(ref(\$file_types_array) eq 'SCALAR')
	  {$file_types_array = [[[$file_types_array]]]}
	else
	  {
	    error("Expected an array for the first argument, but got a [",
		  ref($file_types_array),"].");
	    quit(-9);
	  }
      }
    elsif(scalar(grep {ref($_) ne 'ARRAY'} @$file_types_array))
      {
	my @errors = map {ref(\$_)} grep {ref($_) ne 'ARRAY'}
	  @$file_types_array;
	#Allow them to have submitted an array of scalars
	if(scalar(@errors) == scalar(@$file_types_array) &&
	   scalar(@errors) == scalar(grep {$_ eq 'SCALAR'} @errors))
	  {$file_types_array = [[$file_types_array]]}
	else
	  {
	    @errors = map {ref($_)} grep {ref($_) ne 'ARRAY'}
	      @$file_types_array;
	    error("Expected an array of arrays for the first argument, but ",
		  "got an array of [",join(',',@errors),"].");
	    quit(-10);
	  }
      }
    elsif(scalar(grep {my @x=@$_;scalar(grep {ref($_) ne 'ARRAY'} @x)}
		 @$file_types_array))
      {
	#Look for SCALARs
	my @errors = map {my @x=@$_;map {ref(\$_)} @x}
	  grep {my @x=@$_;scalar(grep {ref($_) ne 'ARRAY'} @x)}
	    @$file_types_array;
	debug("ERRORS ARRAY: [",join(',',@errors),"].") if($DEBUG < -99);
	#Allow them to have submitted an array of arrays of scalars
	if(scalar(@errors) == scalar(map {@$_} @$file_types_array) &&
	   scalar(@errors) == scalar(grep {$_ eq 'SCALAR'} @errors))
	  {$file_types_array = [$file_types_array]}
	else
	  {
	    #Reset the errors because I'm not looking for SCALARs anymore
	    @errors = map {my @x=@$_;'[' .
			     join('],[',
				  map {ref($_) eq '' ? 'SCALAR' : ref($_)} @x)
			       . ']'}
	      @$file_types_array;
	    error("Expected an array of arrays of arrays for the first ",
		  "argument, but got an array of arrays of [",
		  join(',',@errors),"].");
	    quit(-11);
	  }
      }
    elsif(scalar(grep {my @x = @$_;
		       scalar(grep {my @y = @$_;
				    scalar(grep {ref(\$_) ne 'SCALAR'}
					   @y)} @x)} @$file_types_array))
      {
	my @errors = map {my @x = @$_;map {my @y = @$_;map {ref($_)} @y} @x}
	  grep {my @x = @$_;
		scalar(grep {my @y = @$_;
			     scalar(grep {ref(\$_) ne 'SCALAR'} @y)} @x)}
	    @$file_types_array;
	error("Expected an array of arrays of arrays of scalars for the ",
	      "first argument, but got an array of arrays of [",
	      join(',',@errors),"].");
	quit(-12);
      }

    debug("Size of file types array after input check/fix: [",
	  scalar(@$file_types_array),"].") if($DEBUG < -99);

    ##
    ## Error-check/fix the outfile_suffixes array (a 2D array of strings)
    ##
    my $suffix_provided = [map {0} @$file_types_array];
    if(defined($outfile_suffixes))
      {
	if(ref($outfile_suffixes) ne 'ARRAY')
	  {
	    #Allow them to submit scalars of everything
	    if(!defined($outfile_suffixes) ||
	       ref(\$outfile_suffixes) eq 'SCALAR')
	      {
		$suffix_provided->[0] = 1;
		$outfile_suffixes = [[$outfile_suffixes]];
	      }
	    else
	      {
		error("Expected an array for the second argument, but got a [",
		      ref($outfile_suffixes),"].");
		quit(-28);
	      }
	  }
	elsif(scalar(grep {!defined($_) || ref($_) ne 'ARRAY'}
		     @$outfile_suffixes))
	  {
	    my @errors = map {defined($_) ? ref(\$_) : $_}
	      grep {!defined($_) || ref($_) ne 'ARRAY'} @$outfile_suffixes;
	    #Allow them to have submitted an array of scalars
	    if(scalar(@errors) == scalar(@$outfile_suffixes) &&
	       scalar(@errors) == scalar(grep {!defined($_) || $_ eq 'SCALAR'}
					 @errors))
	      {$outfile_suffixes = [$outfile_suffixes]}
	    else
	      {
		@errors = map {ref($_)} grep {ref($_) ne 'ARRAY'}
		  @$outfile_suffixes;
		error("Expected an array of arrays for the second argument, ",
		      "but got an array of [",join(',',@errors),"].");
		quit(-29);
	      }
	  }
	elsif(scalar(grep {my @x=@$_;scalar(grep {ref(\$_) ne 'SCALAR'} @x)}
		     @$outfile_suffixes))
	  {
	    #Reset the errors because I'm not looking for SCALARs anymore
	    my @errors = map {my @x=@$_;map {ref($_)} @x}
	      grep {my @x=@$_;scalar(grep {ref($_) ne 'ARRAY'} @x)}
		@$outfile_suffixes;
	    error("Expected an array of arrays of scalars for the second ",
		  "argument, but got an array of arrays of [",
		  join(',',@errors),"].");
	    quit(-30);
	  }

	foreach my $suffix_index (0..$#{$outfile_suffixes})
	  {$suffix_provided->[$suffix_index] =
	     defined($outfile_suffixes->[$suffix_index]) &&
	       scalar(@{$outfile_suffixes->[$suffix_index]})}
      }

    ##
    ## Error-check/fix the outdir_array (a 2D array of strings)
    ##
    my $outdirs_provided = 0;
    if(defined($outdir_array) && scalar(@$outdir_array))
      {
	#Error check the outdir array to make sure it's a 2D array of strings
	if(ref($outdir_array) ne 'ARRAY')
	  {
	    #Allow them to submit scalars of everything
	    if(ref(\$outdir_array) eq 'SCALAR')
	      {
		$outdirs_provided = 1;
		$outdir_array     = [[$outdir_array]];
	      }
	    else
	      {
		error("Expected an array for the third argument, but got a [",
		      ref($outdir_array),"].");
		quit(-14);
	      }
	  }
	elsif(scalar(grep {ref($_) ne 'ARRAY'} @$outdir_array))
	  {
	    my @errors = map {ref(\$_)} grep {ref($_) ne 'ARRAY'}
	      @$outdir_array;
	    #Allow them to have submitted an array of scalars
	    if(scalar(@errors) == scalar(@$outdir_array) &&
	       scalar(@errors) == scalar(grep {$_ eq 'SCALAR'} @errors))
	      {
		$outdirs_provided = 1;
		$outdir_array = [$outdir_array];
	      }
	    else
	      {
		@errors = map {ref($_)} grep {ref($_) ne 'ARRAY'}
		  @$outdir_array;
		error("Expected an array of arrays for the third argument, ",
		      "but got an array of [",join(',',@errors),"].");
		quit(-15);
	      }
	  }
	elsif(scalar(grep {my @x=@$_;scalar(grep {ref(\$_) ne 'SCALAR'} @x)}
		     @$outdir_array))
	  {
	    #Look for SCALARs
	    my @errors = map {my @x=@$_;map {ref($_)} @x}
	      grep {my @x=@$_;scalar(grep {ref(\$_) ne 'SCALAR'} @x)}
		@$outdir_array;
	    error("Expected an array of arrays of scalars for the third ",
		  "argument, but got an array of arrays of [",
		  join(',',@errors),"].");
	    quit(-16);
	  }
	else
	  {$outdirs_provided = 1}

	#If any outdirs are empty strings, error out & quit
	my $empties_exist = scalar(grep {my @x=@$_;scalar(grep {$_ eq ''} @x)}
				   @$outdir_array);
	if($empties_exist)
	  {
	    error("Output directories may not be empty strings.");
	    quit(-27);
	  }
      }

    debug("First aggregate suf mode: ",Dumper($aggregate_suf_mode))
      if($DEBUG < -99);

    ##
    ## Error-check/fix the aggregate_suf_mode (a 2D array of strings)
    ##
    if(ref($aggregate_suf_mode) ne 'ARRAY')
      {
	#Allow them to submit scalars of everything
	if(ref(\$aggregate_suf_mode) eq 'SCALAR')
	  {$aggregate_suf_mode = [[$aggregate_suf_mode]]}
	else
	  {
	    error("Expected an array for the fourth argument, but got a [",
		  ref($aggregate_suf_mode),"].");
	    quit(-28);
	  }
      }
    elsif(scalar(grep {!defined($_) || ref($_) ne 'ARRAY'}
		 @$aggregate_suf_mode))
      {
	my @errors = map {defined($_) ? ref(\$_) : $_}
	  grep {!defined($_) || ref($_) ne 'ARRAY'} @$aggregate_suf_mode;
	#Allow them to have submitted an array of scalars
	if(scalar(@errors) == scalar(@$aggregate_suf_mode) &&
	   scalar(@errors) == scalar(grep {!defined($_) || $_ eq 'SCALAR'}
				     @errors))
	  {$aggregate_suf_mode = [$aggregate_suf_mode]}
	else
	  {
	    @errors = map {ref($_)} grep {ref($_) ne 'ARRAY'}
	      @$aggregate_suf_mode;
	    error("Expected an array of arrays for the second argument, ",
		  "but got an array of [",join(',',@errors),"].");
	    quit(-29);
	  }
      }
    elsif(scalar(grep {my @x=@$_;scalar(grep {ref(\$_) ne 'SCALAR'} @x)}
		 @$aggregate_suf_mode))
      {
	#Reset the errors because I'm not looking for SCALARs anymore
	my @errors = map {my @x=@$_;map {ref($_)} @x}
	  grep {my @x=@$_;scalar(grep {ref($_) ne 'ARRAY'} @x)}
	    @$aggregate_suf_mode;
	error("Expected an array of arrays of scalars for the second ",
	      "argument, but got an array of arrays of [",
	      join(',',@errors),"].");
	quit(-30);
      }

    debug("Contents of file types array before adding dash file: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    debug("Aggregate suf mode after manipulation: ",
	  Dumper($aggregate_suf_mode)) if($DEBUG < -99);

    ##
    ## If standard input is present, ensure it's in the file_types_array
    ##
    if(!isStandardInputFromTerminal())
      {
	#The first element of the file types array is specifically the type of
	#input file that can be provided via STDIN.  However, a user may
	#explicitly supply a dash on the command line to have the STDIN go to a
	#different parameter instead of the default
	debug("file_types_array->[0] is [",
	      (defined($file_types_array->[0]) ? 'defined' : 'undefined'),"].")
	  if($DEBUG < -99);

	if(!defined($file_types_array->[0]))
	  {$file_types_array->[0] = []}

	my $input_files = $file_types_array->[0];
	my $num_input_files = scalar(grep {$_ ne '-'} map {@$_} @$input_files);
	my $dash_was_explicit =
	  scalar(grep {my $t=$_;scalar(grep {my $e=$_;
					     scalar(grep {$_ eq '-'} @$e)}
				       @$t)} @$file_types_array);
	my $type_index_of_dash = 0;
	if($dash_was_explicit)
	  {$type_index_of_dash =
	     (scalar(grep {my $t=$_;scalar(grep {my $e=$_;
						 scalar(grep {$_ eq '-'} @$e)}
					   @{$file_types_array->[$t]})}
		     (0..$#{$file_types_array})))[0]}

	debug("There are $num_input_files input files.") if($DEBUG < -99);
	debug("Outfile stub: $outfile_stub.") if($DEBUG < -99);

	#If there's only one input file detected, the dash for STDIN was not
	#explicitly provided, and an outfile suffix has been provided, use that
	#input file as a stub for the output file name construction
	if($num_input_files == 1 && !$dash_was_explicit &&
	   defined($outfile_suffixes) && scalar(@$outfile_suffixes) &&
	   defined($outfile_suffixes->[0]))
	  {
	    $outfile_stub = (grep {$_ ne '-'} map {@$_} @$input_files)[0];

	    #Unless the dash was explicitly supplied as a separate file, treat
	    #the input file as a stub only (not as an actual input file
	    @$input_files = ();
	    $num_input_files = 0;

	    #If the stub contains a directory path AND outdirs were supplied
	    if($outfile_stub =~ m%/% &&
	       defined($outdir_array) &&
	       #Assume the outdir is good if
	       ((ref($outdir_array) eq 'ARRAY' && scalar(@$outdir_array)) ||
		ref(\$outdir_array) eq 'SCALAR'))
	      {
		error("You cannot use --outdir and embed a directory path in ",
		      "the outfile stub (-i with a single argument when ",
		      "redirecting standard input in).  Please use one or ",
		      "the other.");
		quit(-13);
	      }
	  }
	#If standard input has been redirected in (which is true because we're
	#here) and an outfule_suffix has been defined for the type of files
	#that the dash is in or will be in, inform the user about the name of
	#the outfile using the default stub for STDIN
	elsif(defined($outfile_suffixes) &&
	      scalar(@$outfile_suffixes) > $type_index_of_dash &&
	      defined($outfile_suffixes->[$type_index_of_dash]))
	  {verbose("Input on STDIN will be referred to as [$outfile_stub].")}

	debug("Outfile stub: $outfile_stub.") if($DEBUG < -99);

	#Unless the dash was supplied explicitly by the user, push it on
	unless($dash_was_explicit)
	  {
	    debug("Pushing on the dash file to the other $num_input_files ",
		  "files.") if($DEBUG < -99);
	    debug("input_files is ",(defined($input_files) ? '' : 'un'),
		  "defined, is of type [",ref($input_files),
		  "], and contains [",
		  (defined($input_files) ?
		   scalar(@$input_files) : 'undefined'),"] items.")
	      if($DEBUG < -99);

	    debug(($input_files eq $file_types_array->[0] ?
		   'input_files still references the first element in the ' .
		   'file types array' : 'input_files has gotten overwritten'))
	      if($DEBUG < -99);

	    #Create a new 1st input file set with it as the only file member
	    unshift(@$input_files,['-']);

	    debug(($input_files eq $file_types_array->[0] ?
		   'input_files still references the first element in the ' .
		   'file types array' : 'input_files has gotten overwritten'))
	      if($DEBUG < -99);
	  }
      }

    debug("Contents of file types array after adding dash file: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    ##
    ## Error-check/fix the file_types_array with the outfile_suffixes array
    ##
    if(scalar(@$file_types_array) < scalar(@$outfile_suffixes))
      {
	error("More outfile suffixes (",scalar(@$outfile_suffixes),"): [",
	      join(',',map {defined($_) ? $_ : 'undef'} @$outfile_suffixes),
	      "] than file types [",scalar(@$file_types_array),"].");
	quit(-30);
      }
    #Elsif the sizes are different, top off the outfile suffixes with undefs
    elsif(scalar(@$file_types_array) > scalar(@$outfile_suffixes))
      {while(scalar(@$file_types_array) > scalar(@$outfile_suffixes))
	 {push(@$outfile_suffixes,undef)}}

    ##
    ## Error-check/fix aggregate_suf_mode array with the outfile_suffixes array
    ##
    #Make sure that the aggregate_suf_mode 2D array has the same dimensions as
    #the outfile_suffixes array - assuming any missing values that are defined
    #in the suffixes array default to 0 unless the subarray is size 1.
    #If a subarray is missing and the original first subarray was size 1,
    #default to its value, otherwise default to 0.  E.g. a suffix array such as
    #[[a,b,c][d,e][undef]] and aggregate_suf_mode of [[1]] will generate a new
    #aggregate_suf_mode array of: [[1,1,1][1,1][undef]].  Or: [[1][0]] ->
    #[[1,1,1][0,0][undef]].  Or: [[1,0][0]] -> [[1,0,0][0,0][undef]]  Or:
    #[[1,0][1]] -> [[1,0,0][1,1][undef]]
    if(scalar(@$aggregate_suf_mode) > scalar(@$outfile_suffixes))
      {
	error("Aggregate suffix mode array is out of bounds.  Must have as ",
	      "many or fewer members as the outfile suffixes array.");
	quit(-31);
      }
    my $global_agg_mode = (scalar(@$aggregate_suf_mode) == 1 &&
			   scalar(@{$aggregate_suf_mode->[0]}) == 1) ? 1 : 0;
    #Create sub-arrays as needed, but don't make them inadvertently bigger
    while(scalar(@$aggregate_suf_mode) < scalar(@$outfile_suffixes))
      {
	#Determine what the next index will be
	my $suff_array_index = scalar(@$aggregate_suf_mode);
	push(@$aggregate_suf_mode,
	     (defined($outfile_suffixes->[$suff_array_index]) ?
	      (scalar(@{$outfile_suffixes->[$suff_array_index]}) ?
	       [$global_agg_mode] : []) : undef));
      }
    foreach my $suff_array_index (0..$#{$outfile_suffixes})
      {
	next unless(defined($outfile_suffixes->[$suff_array_index]));
	#Make sure it's not bigger than the suffixes subarray
	if(scalar(@{$aggregate_suf_mode->[$suff_array_index]}) >
	   scalar(@{$outfile_suffixes->[$suff_array_index]}))
	  {
	    error("Aggregate suffix mode sub-array at index ",
		  "[$suff_array_index] is out of bounds.  Must have as ",
		  "many or fewer members as the outfile suffixes array.");
	    quit(-32);
	  }
	while(scalar(@{$aggregate_suf_mode->[$suff_array_index]}) <
	      scalar(@{$outfile_suffixes->[$suff_array_index]}))
	  {
	    push(@{$aggregate_suf_mode->[$suff_array_index]},
		 (scalar(@{$aggregate_suf_mode->[$suff_array_index]}) ?
		  $aggregate_suf_mode->[0] : $global_agg_mode));
	  }
      }

    ##
    ## Special case (probably unnecessary now with upgrades in 6/2014)
    ##
    my $one_type_mode = 0;
    #If there's only 1 input file type and (no outdirs or 1 outdir), merge all
    #the sub-arrays
    if(scalar(@$file_types_array) == 1 &&
       (!$outdirs_provided || (scalar(@$outdir_array) == 1 &&
			       scalar(@{$outdir_array->[0]}) == 1)))
      {
	$one_type_mode = 1;
	debug("Only 1 type of file was submitted, so the array is being ",
	      "preemptively flattened.") if($DEBUG < -99);

	my @merged_array = ();
	foreach my $row_array (@{$file_types_array->[0]})
	  {push(@merged_array,@$row_array)}
	$file_types_array->[0] = [[@merged_array]];
      }

    debug("Contents of file types array after merging sub-arrays: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    debug("OUTDIR ARRAY DEFINED?: [",defined($outdir_array),"] SIZE: [",
	  (defined($outdir_array) ? scalar(@$outdir_array) : '0'),"].")
      if($DEBUG < -99);

    ##
    ## Prepare to treat outdirs the same as infiles
    ##
    #If output directories were supplied, push them onto the file_types_array
    #so that they will be error-checked and modified in the same way below.
    if($outdirs_provided)
      {push(@$file_types_array,$outdir_array)}

    debug("Contents of file types array after adding outdirs: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    ##
    ## Prepare to error-check file/dir array dimensions
    ##
    my $twods_exist = scalar(grep {my @x = @$_;
			      scalar(@x) > 1 &&
				scalar(grep {scalar(@$_) > 1} @x)}
			     @$file_types_array);
    debug("2D? = $twods_exist") if($DEBUG < -99);

    #Determine the maximum dimensions of any 2D file arrays
    my $max_num_rows = (#Sort on descending size so we can grab the largest one
			sort {$b <=> $a}
			#Convert the sub-arrays to their sizes
			map {scalar(@$_)}
			#Grep for arrays larger than 1 with subarrays larger
			#than 1
			grep {my @x = @$_;
			      !$twods_exist ||
				(scalar(@x) > 1 &&
				 scalar(grep {scalar(@$_) > 1} @x))}
			@$file_types_array)[0];

    my $max_num_cols = (#Sort on descending size so we can grab the largest one
			sort {$b <=> $a}
			#Convert the sub-arrays to their sizes
			map {my @x = @$_;(sort {$b <=> $a}
					  map {scalar(@$_)} @x)[0]}
			#Grep for arrays larger than 1 with subarrays larger
			#than 1
			grep {my @x = @$_;
			      !$twods_exist ||
				(scalar(@x) > 1 &&
				 scalar(grep {scalar(@$_) > 1} @x))}
			@$file_types_array)[0];

    debug("Max number of rows and columns in 2D arrays: [$max_num_rows,",
	  "$max_num_cols].") if($DEBUG < -99);

    debug("Size of file types array: [",scalar(@$file_types_array),"].")
      if($DEBUG < -99);

    debug("Contents of file types array before check/transpose: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    ##
    ## Error-check/transpose file/dir array dimensions
    ##
    #Error check to make sure that all file type arrays are either the two
    #dimensions determined above or a 1D array equal in size to either of the
    #dimensions
    my $row_inconsistencies = 0;
    my $col_inconsistencies = 0;
    my $twod_col_inconsistencies = 0;
    my @dimensionalities    = (); #Keep track for checking outfile stubs later
    foreach my $file_type_array (@$file_types_array)
      {
	my @subarrays = @$file_type_array;

	#If it's a 2D array (as opposed to just 1 col or row), look for
	#inconsistencies in the dimensions of the array
	if(scalar(scalar(@subarrays) > 1 &&
		  scalar(grep {scalar(@$_) > 1} @subarrays)))
	  {
	    push(@dimensionalities,2);

	    #If the dimensions are not the same as the max
	    if(scalar(@subarrays) != $max_num_rows)
	      {
		debug("Row inconsistencies in 2D arrays found")
		  if($DEBUG < -99);
		$row_inconsistencies++;
	      }
	    elsif(scalar(grep {scalar(@$_) != $max_num_cols} @subarrays))
	      {
		debug("Col inconsistencies in 2D arrays found")
		  if($DEBUG < -99);
		$col_inconsistencies++;
		$twod_col_inconsistencies++;
	      }
	  }
	else #It's a 1D array (i.e. just 1 col or row)
	  {
	    push(@dimensionalities,1);

	    #If there's only 1 row
	    if(scalar(@subarrays) == 1)
	      {
		debug("There's only 1 row of size ",
		      scalar(@{$subarrays[0]}),". Max cols: [$max_num_cols]. ",
		      "Max rows: [$max_num_rows]")
		  if($DEBUG < -99);
		if(#$twods_exist &&
		   !$one_type_mode &&
		   scalar(@{$subarrays[0]}) != $max_num_rows &&
		   scalar(@{$subarrays[0]}) != $max_num_cols &&
		   scalar(@{$subarrays[0]}) > 1)
		  {
		    debug("Col inconsistencies in 1D arrays found (size: ",
			  scalar(@{$subarrays[0]}),")")
		      if($DEBUG < -99);
		    $col_inconsistencies++;
		  }
		#If the 1D array needs to be transposed because it's a 1 row
		#array and its size matches the number of rows, transpose it
		elsif(#$twods_exist &&
		      !$one_type_mode &&
		      $max_num_rows != $max_num_cols &&
		      scalar(@{$subarrays[0]}) == $max_num_rows)
		  {@$file_type_array = transpose(\@subarrays)}
	      }
	    #Else if there's only 1 col
	    elsif(scalar(@subarrays) == scalar(grep {scalar(@$_) == 1}
					       @subarrays))
	      {
		debug("There's only 1 col of size ",scalar(@subarrays),
		      "\nThe max number of columns is $max_num_cols")
		  if($DEBUG < -99);
		if(#$twods_exist &&
		   !$one_type_mode &&
		   scalar(@subarrays) != $max_num_rows &&
		   scalar(@subarrays) != $max_num_cols &&
		   scalar(@subarrays) > 1)
		  {
		    debug("Row inconsistencies in 1D arrays found (size: ",
			  scalar(@subarrays),")")
		      if($DEBUG < -99);
		    $row_inconsistencies++;
		  }
		#If the 1D array needs to be transposed because it's a 1 col
		#array and its size matches the number of cols, transpose it
		elsif(#$twods_exist &&
		      !$one_type_mode &&
		      $max_num_rows != $max_num_cols &&
		      scalar(@subarrays) == $max_num_cols)
		  {@$file_type_array = transpose(\@subarrays)}
	      }
	    else #There must be 0 cols
	      {
		debug("Col inconsistencies in 0D arrays found")
		  if($DEBUG < -99);
		$col_inconsistencies++;
	      }

	    debug("This should be array references: [",
		  join(',',@$file_type_array),"].") if($DEBUG < -100);
	  }
      }

    debug("Contents of file types array after check/transpose: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    #Note that if the user has supplied a duplicate input file that creates an
    #output file of the same name, there are two possible outcomes.  If in
    #aggregate mode, it is assumed that they want to generate duplicate
    #appended output.  If not in aggregate mode, the script will either quit
    #with an error about conflicting outfile names or compound the the input
    #file names and re-check for uniqueness (and quit if the compounded names
    #are not unique).

    ##
    ## Create sets/combos (handling the default stub and prepending outdirs)
    ##
    my($infile_sets_array,$outfiles_sets_array,$stub_sets_array);
    if(defined($outdir_array) && scalar(@$outdir_array))
      {
	debug("outdir array has [",scalar(@$outdir_array),"] members.")
	  if($DEBUG < -99);

	my $unique_out_check      = {};
	my $nonunique_found       = 0;
	my $tmp_infile_sets_array = getMatchedSets($file_types_array);

	foreach my $infile_set (@$tmp_infile_sets_array)
	  {
	    debug("Infile set with dirname: [",
		  join(',',map {defined($_) ? $_ : 'undef'} @$infile_set),"].")
	      if($DEBUG < -99);

	    my $stub_set = [];
	    my $dirname = $infile_set->[-1];
	    #For every file (except the last one (which is an output directory)
	    foreach my $file (@{$infile_set}[0..($#{$infile_set} - 1)])
	      {
		my $stub = $file;
		if(defined($stub))
		  {
		    #Us the default outfile stub if this is a redirect
		    $stub = $outfile_stub if($stub eq '-');

		    #Eliminate any path strings from the file name
		    $stub =~ s/.*\///;

		    #Prepend the outdir path
		    my $new_outfile_stub = $dirname .
		      ($dirname =~ /\/$/ ? '' : '/') . $stub;

		    debug("Prepending directory $new_outfile_stub using [",
			  "$file].") if($DEBUG < -100);

		    push(@$stub_set,$new_outfile_stub);

		    $unique_out_check->{$new_outfile_stub}->{$file}++;

		    #Check for conflicting output file names from multiple
		    #different input files that will overwrite one another
		    #(the same output file from the same input file is OK -
		    #we'll assume they won't open it more than once
		    if(scalar(keys(%{$unique_out_check->{$new_outfile_stub}}))
		       > 1)
		      {$nonunique_found = 1}
		  }
		else
		  {push(@$stub_set,$stub)}
	      }
	    push(@$infile_sets_array,
		 [@{$infile_set}[0..($#{$infile_set} - 1)]]);
	    push(@$stub_sets_array,$stub_set);
	  }

	if($nonunique_found)
	  {
	    error('The following output file name stubs were created by ',
		  'multiple input file names.  Their output files will be ',
		  'overwritten when used.  Please make sure each like-named ',
		  'input file (from a different source directory) outputs to ',
		  'a different output directory or that the input file names ',
		  'are not the same.  Offending file stub conflicts: [',
		  join(',',map {"stub $_ is generated by [" .
				  join(',',
				       keys(%{$unique_out_check->{$_}})) .
					 "]"}
		       (grep {scalar(keys(%{$unique_out_check->{$_}})) > 1}
			keys(%$unique_out_check))),'].');
	    quit(-1);
	  }
      }
    else
      {
	$infile_sets_array = getMatchedSets($file_types_array);
	$stub_sets_array   = copyArray($infile_sets_array);

	#Replace any dashes with the outfile stub
	foreach my $stub_set (@$stub_sets_array)
	  {foreach my $stub (@$stub_set)
	     {$stub = $outfile_stub if(defined($stub) && $stub eq '-')}}
      }

    debug("Stubs before making them unique: ",Dumper($stub_sets_array))
      if($DEBUG < 0);

    #makeCheckOutputs returns an outfiles_sets_array and stub_sets_array that
    #have been confirmed to not overwrite each other or existing files.  It
    #quits the script if it finds a conflict.  It uses the aggregate suf mode
    #variable to know when to compound file names to avoid potential
    #overwrites, but either compounds or quits with an error based on the value
    #of compound_conflicts (true or false).
    ($outfiles_sets_array,
     $stub_sets_array) = makeCheckOutputs($stub_sets_array,
					  $outfile_suffixes,
					  $aggregate_suf_mode,
					  $compound_conflicts);

    debug("Stubs after making them unique: ",Dumper($stub_sets_array))
      if($DEBUG < 0);
    debug("Outfiles from the stubs: ",Dumper($outfiles_sets_array))
      if($DEBUG < 0);

    debug("Processing input file sets: [(",
	  join('),(',(map {my $a = $_;join(',',map {defined($_) ? $_ : 'undef'}
					   @$a)} @$infile_sets_array)),
	  ")] and output stubs: [(",
	  join('),(',
               (map {my $a = $_;
                     join(',',map {my $b = $_;defined($b) ? '[' .
                                     join('],[',map {defined($_) ?
                                                       ($_ eq '' ?
                                                        'EMPTY-STRING' : $_) :
                                                         'undef'} @$b) .
                                              ']' : 'undef'}
		@$a)} @$outfiles_sets_array)),")].")
      if($DEBUG < 0);

    return($infile_sets_array,$outfiles_sets_array,$stub_sets_array);
  }

#This subroutine transposes a 2D array (i.e. it swaps rows with columns).
#Assumes argument is a 2D array.  If the number of columns is not the same from
#row to row, it fills in missing elements with an empty string.
sub transpose
  {
    my $twod_array    = $_[0];
    debug("Transposing: [(",
	  join('),(',map {join(',',@$_)} @$twod_array),")].") if($DEBUG < -99);
    my $transposition = [];
    my $last_row = scalar(@$twod_array) - 1;
    my $last_col = (sort {$b <=> $a} map {scalar(@$_)} @$twod_array)[0] - 1;
    debug("Last row: $last_row, Last col: $last_col.") if($DEBUG < -99);
    foreach my $col (0..$last_col)
      {push(@$transposition,
	    [map {$#{$twod_array->[$_]} >= $col ?
		    $twod_array->[$_]->[$col] : ''}
	     (0..$last_row)])}
    debug("Transposed: [(",
	  join('),(',map {join(',',@$_)} @$transposition),")].")
      if($DEBUG < -99);
    return(wantarray ? @$transposition : $transposition);
  }

#This subroutine takes an array of file names and an outfile suffix and returns
#any file names that already exist in the file system
sub getExistingOutfiles
  {
    my $outfile_stubs_for_input_files = $_[0];
    my $outfile_suffix                = scalar(@_) >= 2 ? $_[1] : ''; #OPTIONAL
                                        #undef means there won't be outfiles
                                        #Empty string means that the files in
                                        #$_[0] are already outfile names
    my $existing_outfiles             = [];

    #Check to make sure previously generated output files won't be over-written
    #Note, this does not account for output redirected on the command line.
    #Also, outfile stubs are checked for future overwrite conflicts in
    #getFileSets (i.e. separate files slated for output with the same name)

    #For each output file *stub*, see if the expected outfile exists
    foreach my $outfile_stub (grep {defined($_)}
			      @$outfile_stubs_for_input_files)
      {if(-e "$outfile_stub$outfile_suffix")
	 {push(@$existing_outfiles,"$outfile_stub$outfile_suffix")}}

    return(wantarray ? @$existing_outfiles : $existing_outfiles);
  }

#This subroutine takes a 1D or 2D array of output directories and creates them
#(Only works on the last directory in a path.)  Returns non-zero if successful
#Globals used: $overwrite, $dry_run, $use_as_default
sub mkdirs
  {
    my @dirs       = @_;
    my $status     = 1;
    my @unwritable = ();

    #Create the output directories
    if(scalar(@dirs))
      {
	foreach my $dir_set (@dirs)
	  {
	    if(ref($dir_set) eq 'ARRAY')
	      {
		foreach my $dir (@$dir_set)
		  {
		    if(-e $dir)
		      {
			if(!$use_as_default && !(-w $dir))
			  {push(@unwritable,$dir)}
			elsif(!$use_as_default && $overwrite)
			  {warning('The --overwrite flag will not empty or ',
				   'delete existing output directories.  If ',
				   'you wish to delete existing output ',
				   'directories, you must do it manually.')}
		      }
		    elsif($use_as_default || !$dry_run)
		      {
			my $tmp_status = mkdir($dir);
			$status = $tmp_status if($tmp_status);
		      }
		    else
		      {
			my $encompassing_dir = $dir;
			$encompassing_dir =~ s%/$%%;
			$encompassing_dir =~ s/[^\/]+$//;
			$encompassing_dir = '.'
			  unless($encompassing_dir =~ /./);

			if(!(-w $encompassing_dir))
			  {error("Unable to create directory: [$dir].  ",
				 "Encompassing directory is not writable.")}
			else
			  {verbose("[$dir] Directory created.")}
		      }
		  }
	      }
	    else
	      {
		my $dir = $dir_set;
		if(-e $dir)
		  {
		    if(!$use_as_default && !(-w $dir))
		      {push(@unwritable,$dir)}
		    elsif(!$use_as_default && $overwrite)
		      {warning('The --overwrite flag will not empty or ',
			       'delete existing output directories.  If ',
			       'you wish to delete existing output ',
			       'directories, you must do it manually.')}
		  }
		elsif($use_as_default || !$dry_run)
		  {
		    my $tmp_status = mkdir($dir);
		    $status = $tmp_status if($tmp_status);
		  }
		else
		  {
		    my $encompassing_dir = $dir;
		    $encompassing_dir =~ s%/$%%;
		    $encompassing_dir =~ s/[^\/]+$//;
		    $encompassing_dir = '.'
		      unless($encompassing_dir =~ /./);

		    if(!(-w $encompassing_dir))
		      {error("Unable to create directory: [$dir].  ",
			     "Encompassing directory is not writable.")}
		    else
		      {verbose("[$dir] Directory created.")}
		  }
	      }
	  }

	if(scalar(@unwritable))
	  {
	    error("These output directories do not have write permission: [",
		  join(',',@unwritable),
		  "].  Please change the permissions to proceed.");
	    quit(-18) unless($use_as_default);
	  }
      }

    return($status);
  }

#This subroutine checks for existing output files
#Globals used: $overwrite, $skip_existing
sub checkFile
  {
    my $output_file    = defined($_[0]) ? $_[0] : return(1);
    my $input_file_set = $_[1]; #Optional - Used for verbose/error messages
    my $local_quiet    = scalar(@_) > 2 && defined($_[2]) ? $_[2] : 0;
    my $quit           = scalar(@_) > 3 && defined($_[3]) ? $_[3] : 1;
    my $status         = 1;

    if(-e $output_file)
      {
	debug("Output file: [$output_file] exists.");

	if($skip_existing)
	  {
	    verbose("[$output_file] Output file exists.  Skipping",
		    (defined($input_file_set) ?
                     (" input file(s): [",join(',',@$input_file_set),"]") :
                     ''),".") unless($local_quiet);
	    $status = 0;
	  }
	elsif(!$overwrite)
	  {
	    error("[$output_file] Output file exists.  Unable to ",
		  "proceed.  ",
                  (defined($input_file_set) ?
                   ("Encountered while processing input file(s): [",
		    join(',',grep {defined($_)} @$input_file_set),
		    "].  ") : ''),
                  "This may have been caused by multiple input files ",
		  "writing to one output file because there were not ",
		  "existing output files when this script started.  If any ",
		  "input files are writing to the same output file, you ",
		  "should have seen a warning about this above.  Otherwise, ",
		  "you may have multiple versions of this script running ",
		  "simultaneously.  Please check your input files and ",
		  "outfile suffixes to fix any conflicts or supply the ",
		  "--skip-existing or --overwrite.")
	      unless($local_quiet);
	    quit(-1) if($quit);
	    $status = 0;
	  }
      }
    else
      {debug("Output file: [$output_file] does not exist yet.")}

    return($status);
  }

#Uses globals: $main::open_handles, $dry_run
sub openOut
  {
    my $file_handle  = $_[0];
    my $output_file  = $_[1];
    my $select       = (scalar(@_) >= 3 && defined($_[2]) ? $_[2] : 1);
    my $local_quiet  = (scalar(@_) >= 4 && defined($_[3]) ? $_[3] : 0);
    my $local_header = (scalar(@_) >= 5 && defined($_[4]) ? $_[4] : 1);
    my $status       = 1;

    debug("Output file is ",(defined($output_file) ? '' : 'NOT '),
          "defined",(defined($output_file) ? " and is of type [" .
		     (ref($output_file) eq '' ? 'SCALAR' : ref($output_file)) .
		     "]." : ''));

    #If there was no output file, assume user is outputting to STDOUT
    if(!defined($output_file) || $file_handle eq *STDOUT)
      {
        select(STDOUT) if($select);

        #If this is the first time encountering the STDOUT open
        if(!defined($main::open_handles) ||
           !exists($main::open_handles->{*STDOUT}))
          {
            verbose('[STDOUT] Opened for all output.') unless($local_quiet);

            #Store info. about the run as a comment at the top of the output
            #file if STDOUT has been redirected to a file
            if(!isStandardOutputToTerminal() && $local_header)
              {print(getHeader())}
          }

	$main::open_handles->{*STDOUT}->{FILE}  = 'STDOUT';
	$main::open_handles->{*STDOUT}->{QUIET} = $local_quiet;
      }
    #else if the output file fails the check
    elsif(!checkFile($output_file,undef,0,0))
      {$status = 0}
    #Else if opening the output file fails
    elsif(!$dry_run && !open($file_handle,">$output_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$output_file].\n",$!)
          unless($local_quiet);
	$status = 0;
      }
    else
      {
	$main::open_handles->{$file_handle}->{FILE}  = $output_file;
	$main::open_handles->{$file_handle}->{QUIET} = $local_quiet;

	if($dry_run)
	  {
	    my $encompassing_dir = $output_file;
	    $encompassing_dir =~ s/[^\/]+$//;
	    $encompassing_dir =~ s%/%%;
	    $encompassing_dir = '.' unless($encompassing_dir =~ /./);

	    if(-e $output_file && !(-w $output_file))
	      {error("Output file exists and is not writable: ",
		     "[$output_file].") unless($local_quiet)}
	    elsif(-e $encompassing_dir && !(-w $encompassing_dir))
	      {error("Encompassing directory of output file: ",
		     "[$output_file] exists and is not writable.")
                 unless($local_quiet)}
	    else
	      {verbose("[$output_file] Opened output file.")
                 unless($local_quiet)}

	    #This cleans up the global hashes of file handles
	    closeOut($file_handle);

	    return($status);
	  }

	verbose("[$output_file] Opened output file.") unless($local_quiet);

	#Store info about the run as a comment at the top of the output
	print $file_handle (getHeader()) if($local_header);

	#Select the output file handle
	select($file_handle) if($select);
      }

    return($status);
  }

#Globals used: $main::open_handles
sub closeOut
  {
    my $file_handle = $_[0];
    my $file_name   = scalar(@_) < 2 ? '' : $_[1]; #Optional filename to test
                                                   #whether the file was
                                                   #opened in the first place
    my $status = 1;

    #If we're printing to STDOUT, don't close - just issue a checkpoint message
    if(!defined($file_name) || $file_handle eq *STDOUT)
      {
	verbose("[STDOUT] Output checkpoint.  ",
		"Time taken: [",scalar(markTime()),' Seconds].');
      }
    elsif(tell($file_handle) == -1)
      {
	error("File handle submitted was not open.");
	$status = 0;

	if(exists($main::open_handles->{$file_handle}))
	  {
	    warning("Untracking previously closed (or unopened) file handle.");
	    delete($main::open_handles->{$file_handle});
	  }
      }
    else
      {
	if(!$dry_run)
	  {
	    #Select standard out
	    select(STDOUT);

	    #Close the output file handle
	    close($file_handle);
	  }

	verbose("[$main::open_handles->{$file_handle}->{FILE}] Output file ",
		"done.  Time taken: [",scalar(markTime()),' Seconds].')
	  unless($main::open_handles->{$file_handle}->{QUIET});

	delete($main::open_handles->{$file_handle});
      }

    return($status);
  }

#Globals used: $main::open_handles, $force
sub openIn
  {
    my $file_handle = $_[0];
    my $input_file  = $_[1];
    my $local_quiet = defined($_[2]) ? $_[2] : 0;
    my $status      = 1;     #Returns true if successful or $force > 1

    if(!defined($input_file))
      {
	error("Invalid input file submitted.  File name undefined.");
	$status = 0;
      }
    else
      {
	#Open the input file
	if(!open($file_handle,$input_file))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open input file: [$input_file].  $!");
	    $status = 0 if($force < 2);
	  }
	else
	  {
	    verbose('[',($input_file eq '-' ? $default_stub : $input_file),
		    '] Opened input file.') unless($local_quiet);

	    $main::open_handles->{$file_handle}->{FILE}  = $input_file;
	    $main::open_handles->{$file_handle}->{QUIET} = $local_quiet;

	    closeIn($file_handle) if($dry_run);
	  }
      }

    return($status);
  }

#Globals used: $main::open_handles
sub closeIn
  {
    my $file_handle = $_[0];

    #Close the input file handle
    close($file_handle);

    verbose('[',($main::open_handles->{$file_handle}->{FILE} eq '-' ?
		 $default_stub : $main::open_handles->{$file_handle}->{FILE}),
	    '] Input file done.  Time taken: [',scalar(markTime()),
	    ' Seconds].') unless($main::open_handles->{$file_handle}->{QUIET});

    delete($main::open_handles->{$file_handle});
  }

#Note: Creates a surrounding reference to the submitted array if called in
#scalar context and there are more than 1 elements in the parameter array
sub copyArray
  {
    if(scalar(grep {ref(\$_) ne 'SCALAR' && ref($_) ne 'ARRAY'} @_))
      {
	error("Invalid argument - not an array of scalars.");
	quit(-19);
      }
    my(@copy);
    foreach my $elem (@_)
      {push(@copy,(defined($elem) && ref($elem) eq 'ARRAY' ?
		   [copyArray(@$elem)] : $elem))}
    debug("Returning array copy of [",
	  join(',',map {defined($_) ? $_ : 'undef'} @copy),
	  "].") if($DEBUG < -99);
    return(wantarray ? @copy : (scalar(@copy) > 1 ? [@copy] : $copy[0]));
  }

#Globals used: $defaults_dir
sub getUserDefaults
  {
    my $remove_quotes = defined($_[0]) ? $_[0] : 0;
    my $script        = $0;
    $script           =~ s/^.*\/([^\/]+)$/$1/;
    my $defaults_file = $defaults_dir . "/$script";
    my $return_array  = [];

    if(open(DFLTS,$defaults_file))
      {
	@$return_array = map {chomp;if($remove_quotes){s/^['"]//;s/["']$//}$_}
	  <DFLTS>;
	close(DFLTS);
      }
    elsif(-e $defaults_file)
      {error("Unable to open user defaults file: [$defaults_file].  $!")}

    debug("Returning array: [@$return_array].");

    return(wantarray ? @$return_array : $return_array);
  }

#Globals used: $defaults_dir
sub saveUserDefaults
  {
    my $argv   = $_[0]; #OPTIONAL
    my $status = 1;

    return($status) unless($use_as_default);

    my $orig_defaults = getUserDefaults();

    #Grab defaults from getCommand, because it re-adds quotes & other niceties
    if(!defined($argv))
      {
	$argv = [getCommand(0,1)];
	#Remove the script name
	shift(@$argv);
      }

    my $script        = $0;
    $script           =~ s/^.*\/([^\/]+)$/$1/;
    my $defaults_file = $defaults_dir . "/$script";

    my $save_argv = [grep {$_ ne '--save-as-default'} @$argv];

    debug("Defaults dir: [$defaults_dir].") if($DEBUG > 99);

    #If the defaults directory does not exist and mkdirs returns an error
    if(!(-e $defaults_dir) && !mkdirs($defaults_dir))
      {
	error("Unable to create defaults directory: [$defaults_dir].  $!");
	$status = 0;
      }
    else
      {
	if(open(DFLTS,">$defaults_file"))
	  {
	    print DFLTS (join("\n",@$save_argv));
	    close(DFLTS);
	  }
	else
	  {
	    error("Unable to write to defaults file: [$defaults_file].  $!");
	    $status = 0;
	  }
      }

    if($status)
      {print("Old user defaults: [",join(' ',@$orig_defaults),"].\n",
	     "New user defaults: [",join(' ',getUserDefaults()),"].\n")}
  }

#Globals used: $header, $extended
sub getHeader
  {
    my $local_extended = scalar(@_) && defined($_[0]) ? $_[0] : $extended;

    return('') unless($header);

    if(!$local_extended && !defined($main::header_str))
      {
	my $version_str = getVersion();
	$version_str =~ s/\n(?!#|\z)/\n#/sg;
	$main::header_str = "$version_str\n" .
	  '#' . scalar(localtime($^T)) . "\n" .
	    '#' . scalar(getCommand(1)) . "\n\n";
      }
    elsif($local_extended && !defined($main::header_str_ext))
      {
	my $version_str = getVersion();
	$version_str =~ s/\n(?!#|\z)/\n#/sg;
	$main::header_str_ext = "$version_str\n" .
	  '#' . scalar(localtime($^T)) . "\n" .
	    '#' . scalar(getCommand(1)) . "\n\n";
      }

    return($local_extended ? $main::header_str_ext : $main::header_str);
  }

#This sub takes an array reference (which should initially point to an empty
#array) and a reference to an array containing a series of numbers indicating
#the number of available items to choose from for each position.  It returns,
#in order, an array (the size of the second argument (pool_sizes array))
#containing an as-yet unseen combination of values where each value is selected
#from 1 to the pool size at that position.  E.g. If the pool_sizes array is
#[2,3,1], the combos will be ([1,1,1],[1,2,1],[1,3,1],[2,1,1],[2,2,1],[2,3,1])
#on each subsequent call.  Returns undef when all combos have been generated
sub GetNextIndepCombo
  {
    #Read in parameters
    my $combo      = $_[0];  #An Array of numbers
    my $pool_sizes = $_[1];  #An Array of numbers indicating the range for each
                             #position in $combo

    if(ref($combo) ne 'ARRAY' ||
       scalar(grep {/\D/} @$combo))
      {
	print STDERR ("ERROR:ordered_digit_increment.pl:GetNextIndepCombo:",
		      "The first argument must be an array reference to an ",
		      "array of integers.\n");
	return(0);
      }
    elsif(ref($pool_sizes) ne 'ARRAY' ||
	  scalar(grep {/\D/} @$pool_sizes))
      {
	print STDERR ("ERROR:ordered_digit_increment.pl:GetNextIndepCombo:",
		      "The second argument must be an array reference to an ",
		      "array of integers.\n");
	return(0);
      }

    my $set_size   = scalar(@$pool_sizes);

    #Initialize the combination if it's empty (first one) or if the set size
    #has changed since the last combo
    if(scalar(@$combo) == 0 || scalar(@$combo) != $set_size)
      {
	#Empty the combo
	@$combo = ();
	#Fill it with zeroes
        @$combo = (split('','0' x $set_size));
	#Return true
        return(1);
      }

    my $cur_index = $#{$combo};

    #Increment the last number of the combination if it is below the pool size
    #(minus 1 because we start from zero) and return true
    if($combo->[$cur_index] < ($pool_sizes->[$cur_index] - 1))
      {
        $combo->[$cur_index]++;
        return(1);
      }

    #While the current number (starting from the end of the combo and going
    #down) is at the limit and we're not at the beginning of the combination
    while($combo->[$cur_index] == ($pool_sizes->[$cur_index] - 1) &&
	  $cur_index >= 0)
      {
	#Decrement the current number index
        $cur_index--;
      }

    #If we've gone past the beginning of the combo array
    if($cur_index < 0)
      {
	@$combo = ();
	#Return false
	return(0);
      }

    #Increment the last number out of the above loop
    $combo->[$cur_index]++;

    #For every number in the combination after the one above
    foreach(($cur_index+1)..$#{$combo})
      {
	#Set its value equal to 0
	$combo->[$_] = 0;
      }

    #Return true
    return(1);
  }

#This subroutine returns 2 arrays.  It creates an array of output file names
#and an array of output file stubs from the input file names and the output
#directories (in case the coder wants to handle output file name construction
#on their own).  It checks all the future output files for possible overwrite
#conflicts and checks for existing output files.  It quits if it finds a
#conflict.  It uses the aggregate_modes array to determine whether a conflict
#is actually a conflict or just should be appended to when encountered.  If
#compound_conflicts is true, it tries to avoid conflicting non-aggregating
#output files by joining the input file names with delimiting dots (in the
#order supplied in the stubs array).  It smartly compounds with a single file
#name if that file name is unique, otherwise, it joins all file names.
sub makeCheckOutputs
  {
    my $stub_sets          = copyArray($_[0]);#REQUIRED 2D array of stub combos
    my $suffixes           = $_[1]; #OPTIONAL (Requires $_[2])
    my $aggregate_modes    = $_[2]; #OPTIONAL (Requires $_[1])
    my $compound_conflicts = defined($_[3]) ? $_[3] : 0;
    my $index_uniq         = [map {{}} @{$stub_sets->[0]}]; #Array of hashes
    my $is_index_unique    = [map {1} @{$stub_sets->[0]}];
    my $delim              = '.';

    debug("Called.") if($DEBUG < 0);

    #Build the is_index_unique array
    foreach my $stub_set (@$stub_sets)
      {
	foreach my $type_index (0..$#{$stub_set})
	  {
	    if($#{$index_uniq} < $type_index)
	      {
		error("Critical internal error: type index too big.");
		quit(-25);
	      }
	    #Only interested in stubs with defined values
	    if(defined($stub_set->[$type_index]))
	      {
		if(exists($index_uniq->[$type_index]
			  ->{$stub_set->[$type_index]}))
		  {
		    $is_index_unique->[$type_index] = 0;
		    debug("Index [$type_index] is not unique")
		      if($DEBUG < -99);
		  }
		$index_uniq->[$type_index]->{$stub_set->[$type_index]} = 1;
	      }
	    else
	      {$is_index_unique->[$type_index] = 0}
	  }
      }

    #Find the first unique index with defined values if one exists.
    #We'll use it to hopefully make other stubs unique
    my($first_unique_index);
    foreach my $index (0..$#{$is_index_unique})
      {
	if($is_index_unique->[$index])
	  {
	    $first_unique_index = $index;
	    debug("Unique index: [$index].") if($DEBUG < 0);
	    last;
	  }
      }

    my $outfiles_sets = [];    #This will be the returned 3D outfiles array
    my $unique_hash   = {};    #This will be the check for outfile uniqueness
                               #$unique_hash->{$outfile}->{$type}->{$aggmode}++
                               #Quit if any file has multiple types or
                               #$unique_hash->{$outfile}->{$type}->{0} > 1

    #For each stub set
    foreach my $stub_set (@$stub_sets)
      {
	push(@$outfiles_sets,[]);
	my $saved_stub_set = copyArray($stub_set);

	#For each file-type/stub index
	foreach my $type_index (0..$#{$stub_set})
	  {
	    push(@{$outfiles_sets->[-1]},[]);

	    debug("Index $type_index is ",($is_index_unique->[$type_index] ?
					   '' : 'not '),"unique.")
	      if($DEBUG < 0);

	    my $name          = $stub_set->[$type_index];
	    my $compound_name = $stub_set->[$type_index];

	    #If aggregate modes is not defined and we're compounding conflicts
	    #OR aggregate mode is set for this type index, there exists a non-
	    #aggregate mode AND the stubs at this index are not unique AND this
	    #stub is defined, compound the name
	    if(((!defined($aggregate_modes) && $compound_conflicts) ||
		($compound_conflicts &&
		 defined($aggregate_modes->[$type_index]) &&
		 scalar(grep {!$_} @{$aggregate_modes->[$type_index]}))) &&
	       !$is_index_unique->[$type_index] &&
	       defined($stub_set->[$type_index]))
	      {
		debug("Creating compund name for index $type_index.")
		  if($DEBUG < 0);
		my $stub = $stub_set->[$type_index];
		$stub =~ s/.*\///;
		my $dir = $stub_set->[$type_index];
		unless($dir =~ s/^(.*\/).*/$1/)
		  {$dir = ''}

		if(defined($first_unique_index))
		  {
		    my $unique_name = $stub_set->[$first_unique_index];
		    $unique_name =~ s/.*\///;

		    #Compound the stub with the unique one in index order

		    #For backward compatibility, change stub in-place
		    if(!defined($aggregate_modes))
		      {$stub_set->[$type_index] = $dir .
			 ($type_index < $first_unique_index ?
			  $stub . $delim . $unique_name :
			  $unique_name . $delim . $stub)}

		    $compound_name = $dir .
		      ($type_index < $first_unique_index ?
		       $stub . $delim . $unique_name :
		       $unique_name . $delim . $stub);
		  }
		else
		  {
		    #Don't worry if not enough files exist to create a unique
		    #compound name.  Uniqueness is checked after compounding.

		    my $tmp_stub = $stub_set->[0];
		    $tmp_stub =~ s/(.*\/).*/$1/;

		    #For backward compatibility, change stub in-place
		    if(!defined($aggregate_modes))
		      {$stub_set->[$type_index] =
			 $tmp_stub . join($delim,
					  map {s/.*\///;$_} grep {defined($_)}
					  @$saved_stub_set)}

		    $compound_name =
		      $tmp_stub . join($delim,
				       map {s/.*\///;$_} grep {defined($_)}
				       @$saved_stub_set);
		  }
		debug("New stub: [$compound_name].") if($DEBUG < 0);
	      }

	    debug("Creating file names.") if($DEBUG < 0);
	    #Create the file names using the suffixes and compound name (though
	    #note that the compound name might not be compounded)
	    #If the stub is defined
	    if(defined($stub_set->[$type_index]))
	      {
		#If suffixes is defined & there are suffixes for this index
		if(defined($suffixes) && $#{$suffixes} >= $type_index &&
		   defined($suffixes->[$type_index]) &&
		   scalar(@{$suffixes->[$type_index]}))
		  {
		    my $cnt = 0;
		    #For each suffix available for this file type
		    foreach my $suffix (@{$suffixes->[$type_index]})
		      {
			if(!defined($suffix))
			  {
			    push(@{$outfiles_sets->[-1]->[$type_index]},undef);
			    $cnt++;
			    next;
			  }

			#Concatenate the possibly compounded stub and suffix to
			#the new stub set
			push(@{$outfiles_sets->[-1]->[$type_index]},
			     ($aggregate_modes->[$type_index]->[$cnt] ?
			      $name . $suffix : $compound_name . $suffix));

			$unique_hash
			  ->{$outfiles_sets->[-1]->[$type_index]->[-1]}
			    ->{$type_index}
			      ->{$aggregate_modes->[$type_index]->[$cnt]}++;

			$cnt++;
		      }
		  }
	      }
	    else
	      {
		if(defined($suffixes->[$type_index]))
		  {
		    #The stub is added to the new stub set unchanged
		    #For each suffix available for this file type
		    foreach my $suffix (@{$suffixes->[$type_index]})
		      {push(@{$outfiles_sets->[-1]->[$type_index]},undef)}
		  }
	      }
	  }
      }

    #Let's make sure that the suffixes for each type are unique
    if(defined($suffixes))
      {
	debug("Checking suffixes.") if($DEBUG < 0);
	my $unique_suffs = {};   #$unique_suffs->{$type}->{$suffix}++
	                         #Quit if $unique_suffs->{$type}->{$suffix} > 1
	my $dupe_suffs   = {};
	foreach my $type_index (0..$#{$suffixes})
	  {
	    foreach my $suffix (grep {defined($_)} @{$suffixes->[$type_index]})
	      {
		$unique_suffs->{$type_index}->{$suffix}++;
		if($unique_suffs->{$type_index}->{$suffix} > 1)
		  {$dupe_suffs->{$type_index + 1}->{$suffix} = 1}
	      }
	  }

	if(scalar(keys(%$dupe_suffs)))
	  {
	    my @report_errs = map {my $k = $_;"$k:" .
				     join(",$k:",keys(%{$dupe_suffs->{$k}}))}
	      keys(%$dupe_suffs);
	    @report_errs = (@report_errs[0..8],'...')
	      if(scalar(@report_errs) > 10);
	    error("The following input file types have duplicate output file ",
		  "suffixes (type:suffix): [",join(',',@report_errs),"].  ",
		  "This is a predicted overwrite situation that can only be ",
		  "surpassed by providing unique suffixes for each file type ",
		  "or by using --force combined with either --overwrite or ",
		  "--skip-existing.");
	    quit(-33);
	  }
      }

    #Now we shall check for uniqueness using unique_hash and the suffixes
    #$unique_hash->{$outfile}->{$type}->{$aggmode}++
    #Quit if any file has multiple types or
    #$unique_hash->{$outfile}->{$type}->{0} > 1
    if(#The unique hash is populated
       scalar(keys(%$unique_hash)) &&
       #If any file has multiple types
       scalar(grep {scalar(keys(%$_)) > 1} values(%$unique_hash)))
      {
	my @report_errs = grep {scalar(keys(%{$unique_hash->{$_}})) > 1}
	  keys(%$unique_hash);
	@report_errs = (@report_errs[0..8],'...')
	  if(scalar(@report_errs) > 10);
	error("The following output files have conflicting file names from ",
	      "different input file types: [",join(',',@report_errs),"].  ",
	      "Please make sure the corresponding similarly named input ",
	      "files output to different directories.  This error may be ",
	      "circumvented by --force and either --overwrite or ",
	      "--skip-existing, but it is heavily discouraged - only use for ",
	      "testing.");
	quit(-34);
      }
    #Quit if $unique_hash->{$outfile}->{$type}->{0} > 1
    elsif(#The unique hash is populated
	  scalar(keys(%$unique_hash)) &&
	  #There exist non-aggregate modes
	  scalar(grep {!$_} map {@$_} grep {defined($_)} @$aggregate_modes) &&
	  #There exists an output filename duplicate
	  scalar(grep {$_ > 1} map {values(%$_)} grep {exists($_->{0})}
		 map {values(%$_)} values(%$unique_hash)))
      {
	my @report_errs =
	  grep {my $k = $_;scalar(grep {exists($_->{0}) && $_->{0} > 0}
				  values(%{$unique_hash->{$k}}))}
	    keys(%$unique_hash);
	@report_errs = (@report_errs[0..8],'...')
	  if(scalar(@report_errs) > 10);
	error("Output file name conflict(s) detected: [",
	      join(',',@report_errs),"].  Aggregate output mode is not ",
	      "turned on for these files.  There must be a different output ",
	      "file name for each combination of input files.  Please check ",
	      "your input files for duplicates.  This error may be ",
	      "circumvented by --force and either --overwrite or ",
	      "--skip-existing, but it is heavily discouraged - only use for ",
	      "testing.");
	quit(-35);
      }
    #Quit if any of the outfiles created already exist
    else
      {
	my(%exist);
	foreach my $outfile_arrays_combo (@$outfiles_sets)
	  {foreach my $outfile_array (@$outfile_arrays_combo)
	     {foreach my $outfile (@$outfile_array)
		{checkFile($_,undef,1,0) || $exist{$_}++}}}

	if(scalar(keys(%exist)))
	  {
	    my $report_exist = [scalar(keys(%exist)) > 10 ?
				((keys(%exist))[0..8],'...') : keys(%exist)];
	    error("Output files exist: [",join(',',@$report_exist),
		  "].  Use --overwrite or --skip-existing to continue.");
	    quit(-30);
	  }
      }

    debug("Unique hash: ",Dumper($unique_hash),"\nAggregate modes: ",
	  Dumper($aggregate_modes))
      if($DEBUG < 0);
    debug("Returning outfiles: ",Dumper($outfiles_sets)) if($DEBUG < -99);

    #While edited the stubs that were sent in in the scope where the call was
    #made, however we're also going to return those stubs concatenated with
    #file suffixes sent in [1:M relationship].  (If no suffixes were provided,
    #this will essentially be the same as the stubs, only with subarrays
    #inserted in.
    return($outfiles_sets,$stub_sets);
  }

sub getMatchedSets
  {
    my $array = $_[0]; #3D array

    debug("getMatchedSets called") if($DEBUG < -99);

    #First, create a list of hashes that contain the effective and actual
    #dimension size and the file type index, as well as the 2D array of file
    #names themselves.  The number of rows is the actual and effective first
    #dimension size and the the number of columns is the second dimension size.
    #The number of columns may be variable.  If the number of columns is the
    #same for every row, the effective and actual second dimension size is the
    #same.  If they are different, the actual second dimension size is a series
    #of number of columns for each row and the effective dimension size is as
    #follows: If The numbers of columns across all rows is either 1 or N, the
    #effective second dimension size is N, else it is the series of numbers of
    #columns across each row (a comma-delimited string).  If an array is empty
    #or contains undef, it will be treated as 1x1.

    #When the effective second dimension size is variable, but only contains
    #sizes of 1 & N, then the first element of each row is copied until all
    #rows have N columns.

    #Create an array of hashes that store the dimension sizes and 2D array data
    my $type_container = [];
    #For each file type index
    foreach my $type_index (0..$#{$array})
      {
	#If any rows are empty, create a column containing a single undef
	#member
	foreach my $row (@{$array->[$type_index]})
	  {push(@$row,undef) if(scalar(@$row) == 0)}
	if(scalar(@{$array->[$type_index]}) == 0)
	  {push(@{$array->[$type_index]},[undef])}

	my $first_dim_size = scalar(@{$array->[$type_index]});

	#A hash tracking the number of second dimension sizes
	my $sd_hash = {};

	#A list of the second dimension sizes
	my $second_dim_sizes = [map {my $s=scalar(@$_);$sd_hash->{$s}=1;$s}
				@{$array->[$type_index]}];

	#Ignore second dimension sizes of 1 in determining the effective second
	#dimension size
	delete($sd_hash->{1});

	#Grab the first second dimension size (or it's 1 if none are left)
	my $first_sd_size =
	  scalar(keys(%$sd_hash)) == 0 ? 1 : (keys(%$sd_hash))[0];

	#The effective second dimension size is the first one from above if
	#there's only 1 of them, otherwise it's variable and stored as a comma-
	#delimited string.  Note, if it's a mix of 1 & some dimension N, the
	#effective second dimension size is N.
	my($effective_sd_size);
	if(scalar(keys(%$sd_hash)) == 1 || scalar(keys(%$sd_hash)) == 0)
	  {$effective_sd_size = $first_sd_size}
	else
	  {$effective_sd_size = join(',',@$second_dim_sizes)}

	debug("Type [$type_index] is $first_dim_size x $effective_sd_size or ",
	      "[$first_dim_size] x [@$second_dim_sizes]") if($DEBUG < -99);

	#Change each 2D file array into a hash which stores its type index,
	#actual row size(s), effective row sizes, actual column sizes,
	#effective column sizes, and the actual 2D array of file names
	push(@$type_container,
	     {AF   => [$first_dim_size],       #Actual first dimension sizes
	      AS   => $second_dim_sizes,       #Actual second dimension sizes
	      EF   => $first_dim_size,         #Effective first dimension size
	      ES   => $effective_sd_size,      #Effective second dimension size
	      TYPE => $type_index,             #Type of files contained
	      DATA => scalar(copyArray($array->[$type_index]))});
	                                       #2D array of file names
      }

    #Next, we transpose any arrays based on the following criteria.  Assume FxS
    #is the effective first by second dimension sizes and that the type
    #container array is ordered by precedence(/type).  The first array will not
    #be transposed to start off and will be added to a new synced group.  For
    #each remaining array, if effective dimensions match an existing synced
    #group (in order), it is added to that synced group.  If it matches none,
    #it is the first member of a new synced group.  A group's dimensions match
    #if they are exactly the same, if they are reversed but exactly the same,
    #or 1 dimension is size 1 and the other dimension is not size 1 and matches
    #either F or S.  If a matching array's dimensions are reversed (i.e. F1xS1
    #= S2xF2 or (F1=1 and S1!=1 and S1=F2) or (S1=1 and F1!=1 and F1=S2)) and
    #it can be transposed, transpose it, else if a matching array's dimensions
    #are reversed and all the members of the synced group can be transposed,
    #transpose the members of the synced group.  Then the current array is
    #added to it.  Otherwise, the array is added as the first member of a new
    #synced group.  If the second dimension is a mix of sizes 1 & N only and N
    #matches F or S in the synced group, the 1 member is duplicated to match
    #the other dimension (F or S).

    my $synced_groups = [{EF    => $type_container->[0]->{EF},
			  ES    => $type_container->[0]->{ES},
			  AF    => [@{$type_container->[0]->{AF}}],
			  AS    => [@{$type_container->[0]->{AS}}],
			  GROUP => [$type_container->[0]]  #This is a hash like
			                                   #in the type_
			                                   #container array
			 }];

    eval {use Data::Dumper;1} if($DEBUG < 0);

    debug("Initial group with default candidate: ",
	  Dumper($synced_groups->[-1]->{GROUP}),"There are [",
	  scalar(@$type_container),"] types total.") if($DEBUG < -99);

    #For every type_hash_index in the type container except the first one
    foreach my $type_hash_index (1..$#{$type_container})
      {
	my $type_hash = $type_container->[$type_hash_index];

	my $found_match = 0;

	my $candidate_ef = $type_hash->{EF};
	my $candidate_es = $type_hash->{ES};
	my $candidate_af = $type_hash->{AF};
	my $candidate_as = $type_hash->{AS};

	debug("candidate_ef $candidate_ef candidate_es $candidate_es ",
	      "candidate_af @$candidate_af candidate_as @$candidate_as")
	  if($DEBUG < -99);

	foreach my $group_hash (@$synced_groups)
	  {
	    my $group_ef = $group_hash->{EF};
	    my $group_es = $group_hash->{ES};
	    my $group_af = $group_hash->{AF};
	    my $group_as = $group_hash->{AS};

	    debug("group_ef $group_ef group_es $group_es group_af @$group_af ",
		  "group_as @$group_as") if($DEBUG < -99);

	    #If the candidate and group match (each explained in-line below)
	    if(#Either candidate or group is 1x1 (always a match)
	       ($candidate_ef eq '1' && $candidate_es eq '1') ||
	       ($group_ef     eq '1' && $group_es eq '1') ||

	       #Exact or reverse exact match
	       ($candidate_ef eq $group_ef && $candidate_es eq $group_es) ||
	       ($candidate_ef eq $group_es && $candidate_es eq $group_ef) ||

	       #candidate_ef is 1 and candidate_es is not 1 but matches either
	       ($candidate_ef eq '1' && $candidate_es ne '1' &&
		($candidate_es eq $group_es || $candidate_es eq $group_ef)) ||

	       #candidate_es is 1 and candidate_ef is not 1 but matches either
	       ($candidate_es eq '1' && $candidate_ef ne '1' &&
		($candidate_ef eq $group_es || $candidate_ef eq $group_ef)) ||

	       #group_ef is 1 and group_es is not 1 but matches either
	       ($group_ef eq '1' && $group_es ne '1' &&
		($group_es eq $candidate_es || $group_es eq $candidate_ef)) ||

	       #group_es is 1 and group_ef is not 1 but matches either
	       ($group_es eq '1' && $group_ef ne '1' &&
		($group_ef eq $candidate_es || $group_ef eq $candidate_ef)) ||

	       #First dimensions match exactly and each second dimension is
	       #either an exact corresponding match or one of them is a 1
	       ($candidate_ef eq $group_ef &&
		scalar(grep {$group_as->[$_] == $candidate_as->[$_] ||
			       $group_as->[$_] == 1 ||
				 $candidate_as->[$_] == 1}
		       (0..($group_ef - 1))) == $group_ef))
	      {
		$found_match = 1;

		#If the candidate's dimensions are not the same, the group's
		#dimensions are not the same, and the candidate's dimensions
		#are reversed relative to the group, we need to transpose
		#either the candidate or the group.
		if(#Neither the candidate nor group is a square
		   $candidate_ef ne $candidate_es && $group_ef ne $group_es &&
		   #Either the candidate or group is not variable dimension
		   ($candidate_es !~ /,/ || $group_es !~ /,/) &&
		   #The matching dimension is opposite & not size 1
		   (($candidate_ef eq $group_es && $group_es ne '1') ||
		    ($candidate_es eq $group_ef && $group_ef ne '1')))
		  {
		    #We need to transpose either the candidate or group

		    #If the candidate can be transposed
		    if($candidate_es !~ /,/)
		      {
			#Assuming the number of columns varies between 1 & M,
			#fill up the rows of size 1 to match M before
			#transposing. (Won't hurt if they don't)
			foreach my $row (@{$type_hash->{DATA}})
			  {while(scalar(@$row) < $candidate_es)
			     {push(@$row,$row->[0])}}

			debug("Transposing candidate.") if($DEBUG < -99);
			@{$type_hash->{DATA}} = transpose($type_hash->{DATA});
			my $tmp = $candidate_ef;
			$candidate_ef = $type_hash->{EF} = $candidate_es;
			$candidate_es = $type_hash->{ES} = $tmp;
			$candidate_af = $type_hash->{AF} =
			  [scalar(@{$type_hash->{DATA}})];
			$candidate_as = $type_hash->{AS} =
			  [map {scalar(@$_)} @{$type_hash->{DATA}}];
		      }
		    #Else if the group can be transposed
		    elsif($group_es !~ /,/)
		      {
			debug("Transposing group.") if($DEBUG < -99);
			#For every member of the group (which is a type hash)
			foreach my $member_type_hash (@{$group_hash->{GROUP}})
			  {
			    #Assuming the number of columns varies between 1 &
			    #M, fill up the rows of size 1 to match M before
			    #transposing. (Won't hurt if they don't)
			    foreach my $row (@{$member_type_hash->{DATA}})
			      {while(scalar(@$row) < $group_es)
				 {push(@$row,$row->[0])}}

			    @{$member_type_hash->{DATA}} =
			      transpose($member_type_hash->{DATA});

			    #Update the type hash's metadata
			    my $tmp = $member_type_hash->{EF};
			    $member_type_hash->{EF} = $member_type_hash->{ES};
			    $member_type_hash->{ES} = $tmp;
			    $member_type_hash->{AF} =
			      [scalar(@{$member_type_hash->{DATA}})];
			    $member_type_hash->{AS} =
			      [map {scalar(@$_)} @{$member_type_hash->{DATA}}];
			  }

			#Update the group metadata (using the first member of
			#the group)
			my $tmp = $group_ef;
			$group_ef = $group_hash->{EF} = $group_es;
			$group_es = $group_hash->{ES} = $tmp;
			$group_af = $group_hash->{AF} =
			  [scalar(@{$group_hash->{GROUP}->[0]->{DATA}})];
			$group_as = $group_hash->{AS} =
			  [map {scalar(@$_)}
			   @{$group_hash->{GROUP}->[0]->{DATA}}];
		      }
		    else
		      {
			error("Critical internal error: Transpose not ",
			      "possible.  This should not be possible.");
			quit(-26);
		      }
		  }

		#Anything that needed transposed has now been transposed, so
		#now we need to even things up by filling any 1-dimensional
		#arrays to match their 2D matches.

		debug("Add rows if(candidate_ef eq '1' && group_ef ne '1' && ",
		      "group_es ne '1'): if($candidate_ef eq '1' && ",
		      "$group_ef ne '1' && $group_es ne '1')")
		  if($DEBUG < -99);

		#If we need to add any rows to the candidate
		if($candidate_ef eq '1' && $group_ef ne '1')
		  {
		    debug("Adding rows to candidate.") if($DEBUG < -99);
		    foreach(2..$group_ef)
		      {push(@{$type_hash->{DATA}},
			    [@{copyArray($type_hash->{DATA}->[0])}])}

		    #Update the metadata
		    $candidate_ef = $type_hash->{EF} = $group_ef;
		    #The effective second dimension size did not change
		    $candidate_af = $type_hash->{AF} = [$group_ef];
		    $candidate_as = $type_hash->{AS} =
		      [map {scalar(@$_)} @{$type_hash->{DATA}}];
		  }

		debug("Add columns if(candidate_es eq '1' && group_es ne ",
		      "'1': if($candidate_es eq '1' && $group_es ne '1')")
		  if($DEBUG < -99);

		#If we need to add any columns to the candidate
		my $col_change = 0;
		foreach my $i (0..$#{$group_as})
		  {
		    my $num_cols = $group_as->[$i];
		    my $row = $type_hash->{DATA}->[$i];
		    while(scalar(@$row) < $num_cols)
		      {
			$col_change = 1;
			push(@$row,$row->[0]);
		      }
		  }
		if($col_change)
		  {
		    debug("Added columns to candidate.") if($DEBUG < -99);
		    #Update the metadata
		    #The effective first dimension size did not change
		    $candidate_es = $type_hash->{ES} = $group_es;
		    #The actual first dimension size did not change
		    $candidate_as = $type_hash->{AS} =
		      [map {scalar(@$_)} @{$type_hash->{DATA}}];
		  }
		#If we need to add any rows to the group
		if($group_ef eq '1' && $candidate_ef ne '1')
		  {
		    debug("Adding rows to group.") if($DEBUG < -99);
		    foreach my $member_type_hash (@{$group_hash->{GROUP}})
		      {
			#Copy the first row up to the effective first
			#dimension size of the candidate
			foreach(2..$candidate_ef)
			  {push(@{$member_type_hash->{DATA}},
				[@{copyArray($member_type_hash->{DATA}
					     ->[0])}])}

			#Update the member metadata
			$member_type_hash->{EF} = $candidate_ef;
			#Effective second dimension size did not change
			$member_type_hash->{AF} = [$candidate_ef];
			$member_type_hash->{AS} =
			  [map {scalar(@$_)} @{$member_type_hash->{DATA}}];
		      }

		    #Update the group metadata
		    $group_ef = $group_hash->{EF} = $candidate_ef;
		    #Effective second dimension size did not change
		    $group_af = $group_hash->{AF} =
		      [scalar(@{$group_hash->{GROUP}->[0]->{DATA}})];
		    #The actual second dimension size could be different if the
		    #candidate has a variable second dimension size
		    $group_as = $group_hash->{AS} =
		      [map {scalar(@$_)} @{$group_hash->{GROUP}->[0]->{DATA}}];
		  }

		#If we need to add any columns to the group
		$col_change = 0;
		foreach my $member_type_hash (@{$group_hash->{GROUP}})
		  {
		    foreach my $i (0..$#{$candidate_as})
		      {
			my $num_cols = $candidate_as->[$i];
			my $row = $member_type_hash->{DATA}->[$i];
			while(scalar(@$row) < $num_cols)
			  {
			    $col_change = 1;
			    push(@$row,$row->[0]);
			  }
		      }

		    if($col_change)
		      {
			#Update the member metadata
			#The effective first dimension size did not change
			$member_type_hash->{ES} = $candidate_es;
			#The actual first dimension size did not change
			$member_type_hash->{AS} =
			  [map {scalar(@$_)} @{$member_type_hash->{DATA}}];
		      }
		    else #Assume everything in a group is same dimensioned
		      {last}
		  }

		if($col_change)
		  {
		    debug("Added columns to group.") if($DEBUG < -99);
		    #Update the metadata
		    #The effective first dimension size did not change
		    $group_es = $group_hash->{ES} = $candidate_es;
		    #The actual first dimension size did not change
		    $group_as = $group_hash->{AS} =
		      [map {scalar(@$_)} @{$group_hash->{GROUP}->[0]->{DATA}}];
		  }

		#Put this candidate in the synced group
		push(@{$group_hash->{GROUP}},$type_hash);

		debug("Group after adding candidate: ",
		      Dumper($group_hash->{GROUP})) if($DEBUG < -99);

		#We stop when we find a match so that we don't put this
		#candidate in multiple synced groups
		last;
	      }
	  }

	unless($found_match)
	  {
	    #Create a new synced group
	    push(@$synced_groups,{EF    => $candidate_ef,
				  ES    => $candidate_es,
				  AF    => $candidate_af,
				  AS    => $candidate_as,
				  GROUP => [$type_hash]});

	    debug("New group after adding candidate: ",
		  Dumper($synced_groups->[-1]->{GROUP})) if($DEBUG < -99);
	  }
      }

    debug("Synced groups contains [",Dumper($synced_groups),"].")
      if($DEBUG < -99);

    #Now I have a set of synced groups, meaning every hash in the group has the
    #same dimensions described by the group's metadata.  However, I don't need
    #that metadata anymore, so I can condense the groups into 1 big array of
    #type hashes and all I need from those is the TYPE (the first index into
    #$array) and the DATA (The 2D array of files).

    #Each group has F * S paired combos.  In order to generate all possible
    #combinations, I need to string them along in a 1 dimensional array

    my $flattened_groups = []; #Each member is an unfinished combo (a hash with
                               #2 keys: TYPE & ITEM, both scalars

    foreach my $synced_group (@$synced_groups)
      {
	push(@$flattened_groups,[]);
	foreach my $row_index (0..($synced_group->{EF} - 1))
	  {
	    foreach my $col_index (0..($synced_group->{AS}->[$row_index] - 1))
	      {
		my $unfinished_combo = [];

		#foreach type hash in GROUP, add the item at row/col index to a
		#combo
		foreach my $type_hash (@{$synced_group->{GROUP}})
		  {
		    debug("ITEM should be type '': [",
			  ref($type_hash->{DATA}->[$row_index]->[$col_index]),
			  "].")
		      if($DEBUG < -100);

		    push(@$unfinished_combo,
			 {TYPE => $type_hash->{TYPE},
			  ITEM => $type_hash->{DATA}->[$row_index]
			  ->[$col_index]});
		  }

		push(@{$flattened_groups->[-1]},$unfinished_combo);
	      }
	  }
      }

    debug("Flattened groups contains: [",Dumper($flattened_groups),"].")
      if($DEBUG < -99);

    my $combos = [];
    my $combo  = [];
    while(GetNextIndepCombo($combo,
			    [map {scalar(@$_)} @$flattened_groups]))
      {
	#The index of combo items corresponds to the index of flattened_groups
	#The values of combo correspond to the index into the array member of
	#flattened_groups

	#Construct this combo from the unfinished combos
	my $finished_combo = [];
	foreach my $outer_index (0..$#{$combo})
	  {
	    my $inner_index = $combo->[$outer_index];

	    push(@$finished_combo,
		 @{$flattened_groups->[$outer_index]->[$inner_index]});
	  }

	#Check the finished combo to see that it contains 1 file of each type
	my $check = {map {$_ => 0} (0..$#{$array})};
	my $unknown = {};
	foreach my $type_index (map {$_->{TYPE}} @$finished_combo)
	  {
	    if(exists($check->{$type_index}))
	      {$check->{$type_index}++}
	    else
	      {$unknown->{$type_index}++}
	  }
	my @too_many = grep {$check->{$_} > 1} keys(%$check);
	if(scalar(@too_many))
	  {
	    error("Critical Internal error: Bad Option Combo.  These option ",
		  "types had more than 1 value: [",join(',',@too_many),
		  "].  Use --force to include all combos by attempting to ",
		  "repair it.");

	    next unless($force);

	    #If they force it, try to repair by eliminating extra ones
	    my $fixed_fin_combo = [];
	    my $done = {};
	    foreach my $hash (@$finished_combo)
	      {
		next if(exists($done->{$hash->{TYPE}}));
		$done->{$hash->{TYPE}} = 1;
		push(@$fixed_fin_combo,$hash);
	      }
	    @$finished_combo = @${fixed_fin_combo};
	  }
	my @missing = grep {$check->{$_} == 0} keys(%$check);
	if(scalar(@missing))
	  {
	    error("Critical Internal error: Bad Option Combo.  These option ",
		  "types were missing: [",join(',',@missing),
		  "].  Use --force to include all combos by attempting to ",
		  "repair it.");

	    next unless($force);

	    #If they force it, try to repair by adding undefs
	    foreach my $type_index (@missing)
	      {push(@$finished_combo,{TYPE => $type_index,ITEM => undef})}
	  }
	if(scalar(keys(%$unknown)))
	  {
	    error("Critical Internal error: Bad Option Combo.  These option ",
		  "types are unknown: [",join(',',keys(%$unknown)),
		  "].  Use --force to include all combos by attempting to ",
		  "repair it.");

	    next unless($force);

	    #If they force it, try to repair by eliminating unknowns
	    my $fixed_fin_combo = [];
	    foreach my $hash (@$finished_combo)
	      {push(@$fixed_fin_combo,$hash)
		 unless(exists($unknown->{$hash->{TYPE}}))}
	    @$finished_combo = @${fixed_fin_combo};
	  }

	#Save the combo to return it
	push(@$combos,
	     [map {$_->{ITEM}}
	      sort {$a->{TYPE} <=> $b->{TYPE}} @$finished_combo]);
      }

    debug("getMatchedSets returning combos: [",Dumper($combos),"].")
      if($DEBUG < -99);

    return(wantarray ? @$combos : $combos);
  }

#This subroutine is spcifically for use with the '<>' Getopt::Long operator
#(which catches flagless options) when used to capture files.  Since all
#unknown options go here, this sub watches for values that do not exist as
#files and begin with a dash followed by a non-number (to be rather forgiving
#of stub usages).  It just issues a warning, but if in strict mode, it will
#call quit.
sub checkFileOpt
  {
    my $alleged_file = $_[0];
    my $strict       = defined($_[1]) ? $_[1] : 0;
    if($alleged_file =~ /^-\D/ && !(-e $alleged_file))
      {
	if($strict)
	  {
	    error("Unknown option: [$alleged_file].");
	    quit(-31);
	  }
	else
	  {warning("Potentially unknown option assumed to be a file name: ",
		   "[$alleged_file].")}
      }
  }

sub processDefaultOptions
  {
    my $outfiles_defined = $_[0];

    #Print the usage if there are no non-user-default arguments (or it's just
    #the extended flag) and no files directed or piped in
    if((scalar(@$preserve_args) == 0 ||
	(scalar(@$preserve_args) == 1 &&
	 $preserve_args->[0] eq '--extended')) &&
       isStandardInputFromTerminal())
      {
	usage(0);
	quit(0);
      }

    #Error-check for mutually exclusive flags supplied together
    if(scalar(grep {$_} ($use_as_default,$help,$version)) > 1)
      {
	error("--help, --version & --save-as-default are mutually exclusive.");
	quit(-3);
      }

    #If the user has asked for help, call the help subroutine & quit
    if($help)
      {
	help($extended);
	quit(0);
      }

    #If the user has asked for the software version, print it & quit
    if($version)
      {
	print(getVersion(),"\n");
	quit(0);
      }

    #If the user has asked to save the options, save them & quit
    if($use_as_default)
      {
	saveUserDefaults() && quit(0);
	quit(-4);
      }

    #Check validity of verbosity options
    if($quiet && ($verbose || $DEBUG))
      {
	$quiet = 0;
	error('--quiet is mutually exclusive with both --verbose & --debug.');
	quit(-5);
      }

    #Check validity of existing outfile options
    if($skip_existing && $overwrite)
      {
	error('--overwrite & --skip-existing are mutually exclusive.');
	quit(-6);
      }

    #Warn users when they turn on verbose and output is to the terminal
    #(implied by no outfile suffix & no redirect out) that verbose messages may
    #be messy
    if($verbose && !$outfiles_defined && isStandardOutputToTerminal())
      {warning('You have enabled --verbose, but appear to be outputting to ',
	       'the terminal.  Verbose messages may interfere with ',
	       'formatting of terminal output making it difficult to read.  ',
	       'You may want to either turn verbose off, redirect output to ',
	       'a file, or supply output files by other means.')}

    if($dry_run)
      {
	#It only makes sense to do a dry run in either verbose or debug mode
	$verbose = 1 unless($verbose || $DEBUG);
	verbose('Starting dry run.');
      }

    verbose('Run conditions: ',scalar(getCommand(1)));
    verbose("Verbose level:  [$verbose].");
    verbose('Header:         [on].')           if($header);
    verbose("Debug level:    [$DEBUG].")       if($DEBUG);
    verbose("Force level:    [$force].")       if($force);
    verbose('Overwrite mode: [on].')           if($overwrite);
    verbose('Skip mode:      [on].')           if($skip_existing);
    verbose("Dry run level:  [$dry_run].")     if($dry_run);
    verbose("Aggregate mode: [off].")          if(!$aggregate_mode);
    verbose("Join conflicts: [on].")           if($compound_mode);
    verbose("Error level:    [$error_limit].") if($error_limit != 5);
  }

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script   = $0;
    my $advanced = $_[0];
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #$software_version_number  - global
    #$created_on_date          - global
    $created_on_date = 'UNKNOWN' if($created_on_date eq 'DATE HERE');

    #Print a description of this program
    print << "end_print";

$script version $software_version_number
Copyright 2014
Robert W. Leach
Created: $created_on_date
Last Modified: $lmd
Princeton University
Carl Icahn Laboratory
Lewis Sigler Institute for Integrative Genomics
Bioinformatics Group
Room 133A
Princeton, NJ 08544
rleach\@genomics.princeton.edu

* WHAT IS THIS: This script identifies pairs of sequences among multiple
                samples that meet 3 supplied criteria: minimum average
                abundance (default: 10), maximum dynamical similarity (default:
                0.8), and minimum sequence similarity/identity (default: 0.9).
                Pairs of sequences that are very similar but exhibit different
                behavior (in terms of abundance in a population, e.g. in a
                time-series) are output.

                This script will parse abundance values from a summary
                abundance file (see -s) of "real" sequences (as is produced by
                getReals.pl using its -s option) and retrieve their sequences
                from a single global library sequence file (see -i).  Note that
                the IDs parsed from both file types must match.

                This script represents the last optional step of an 8 step
                process in the package called 'cff' (cluster free filtering).
                Please refer to the README for general information about the
                package.

* SEQUENCE FORMAT: Fasta or fastq format file containing a set of unique,
  (-i)             ungapped, aligned, and same-sized sequences and with a
                   unique identifier followed by an abundance value on the
                   defline.  Default defline format looks like this example:

                   >lib_6;size=1002;

                   Any defline format can be used as long as it is consistent,
                   both pieces of information are present, and the identifier
                   is the first unbroken string after the defline character.
                   The unique ID may contain the abundance value.  Use -p to
                   extract the abundance values from deflines not in the above
                   format.

* ABUNDANCE FORMAT: Tab-delimited text file where each row represents a
  (-s)              different sequence and each column represents a different
                    sample.  The values in the table are abundances of each
                    sequence/sample combination.  A commented (#) header line
                    will be used if --header and --append-abunds are supplied.
                    Example:

#ID   	5_77	5_78	5_79	5_80	5_81	5_82	5_83	5_84	5_85	5_86
lib_73	0   	0   	0   	0   	0   	5   	4   	13  	21  	5   
lib_7 	32  	24  	143 	104 	196 	57  	444 	401 	118 	235 
lib_75	3   	17  	3   	11  	0   	10  	2   	0   	1   	0   
lib_58	12  	17  	11  	11  	4   	11  	4   	0   	4   	2   
lib_14	125 	82  	86  	38  	28  	94  	65  	30  	71  	36  
lib_28	36  	22  	14  	23  	5   	60  	14  	4   	34  	11  
lib_65	15  	2   	1   	2   	12  	9   	2   	6   	10  	2   

* OUTPUT FORMAT: Tab-delimited text format.  Rows are sorted by ascending
  (-o)           absolute abundance correlation value.  usearch output can be
                 appended by supplying --append-usearch.  Per-sample abundance
                 values of each of a pair can be appended by supplying
                 --appeand-abunds.  Example of default output (with --header):

                 #Greater-Abundance-ID	Lesser-Abundance-ID	Dynamical-Similarity	Sequence-Similarity	Greater-Total-Abundance	Lesser-Total-Abundance
                 lib_1	lib_2	-0.751	0.992	21950	15451
                 lib_1	lib_3	0.587	0.939	21950	11816
                 lib_2	lib_3	-0.00162	0.931	15451	11816
                 lib_1	lib_4	0.395	0.946	21950	9574
                 lib_3	lib_4	-0.233	0.946	11816	9574
                 lib_2	lib_4	-0.457	0.939	15451	9574

                 Output is sorted by decreasing Lesser-Total-Abundance,
                 decreasing Sequence-Similarity, decreasing Greater-Total-
                 Abundance, increasing Dynamical-Similarity, then the IDs
                 themselves.  Note that Dynamical Similarity should be
                 considered a binary value.  Either the abundance traces are
                 similar (and thus filtered out) or they are dissimilar.

end_print

    if($advanced)
      {
	my $header          = getHeader(0);
	my $extended_header = getHeader(1);

	print << "end_print";
* HEADER FORMAT: If --header is supplied and STANDARD output is not going to
                 the terminal, a header that is commented using the '#'
                 character (i.e. each line of the header will begin with '#').
                 The format of the standard header looks like this:

$header

                 And here is the format of the extended header:

$extended_header

* OVERWRITE PROTECTION: This script prevents the over-writing of files (unless
                        --overwrite is provided).  A check is performed for
                        pre-existing files before any output is generated.  It
                        will even check if future output files will be over-
                        written in case two input files from different
                        directories have the same name and a common --outdir.
                        Furthermore, before output starts to a given file, a
                        last-second check is performed in case another program
                        or script instance is competing for the same output
                        file.  If such a case is encountered, an error will be
                        generated and the file will always be skipped.

* ADVANCED FILE I/O FEATURES:

Sets of input files, each with different output files/directories can be
supplied.  Supply each file set with an additional -i (or --seq-file) flag.
Wrap each set of files in quotes and separate them with spaces.

Each output file/directory must be supplied in the same relative order so that
each associated input file set can be output into a different directory.  If
the number of files in each set is the same, you can supply all output files or
directories as a single set instead of each having a separate flag for each
one.

Examples:

  $0 -i 'a b c' --outfile '1' -i 'd e f' --outfile '2'

    Resulting file sets: a,b,c output to 1 and d,e,f output to 2

  $0 -i 'a b c' -i 'd e f' --outfile '1 2 3'

    Resulting file sets: a,d > 1; b,e > 2; c,f > 3

If the number of files per set is the same as the number of directories in 1
set are the same, this is what will happen:

  $0 -i 'a b' -i 'd e' --outfile '1 2'

    Resulting file sets: a,d > 1; b,e > 2

NOT this: 'a,b > 1; d,e > 2'  To do this, you must supply the --outfile flag
for each set, like this:

  $0 -i 'a b' -i 'd e' --outfile '1' --outfile '2'

Other examples:

  $0 -i 'a b c' -i 'd e f' --outfile '1 2'

    Result: a,b,c > 1; d,e,f > 2

  $0 -i 'a b c' --outfile '1 2 3' -i 'd e f' --outfile '4 5 6'

    Result: a > 1; b > 2; c > 3; d > 4; e > 5; f > 6

Abundance trace input files are associated in the same manner as the output
files above.  Basically, if the number of files or sets of files match, they
will be automatically associated in the order in which they were provided on
the command line.

end_print
      }

    return(0);
  }

##
## This subroutine prints a usage statement in long or short form depending on
## whether "no descriptions" is true.
##
#Globals used: $extended
sub usage
  {
    my $error_mode     = $_[0];
    my $local_extended = scalar(@_) > 1 && defined($_[1]) ? $_[1] : $extended;

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Grab the first version of each option from the global GetOptHash
    my $options =
      ($error_mode ? '[' .
       join('] [',
	    grep {$_ ne '-i'}           #Remove REQUIRED params
	    map {my $key=$_;            #Save the key
		 $key=~s/\|.*//;        #Remove other versions
		 $key=~s/(\!|=.|:.)$//; #Remove trailing getopt stuff
		 $key = (length($key) > 1 ? '--' : '-') . $key;} #Add dashes
	    grep {$_ ne '<>'}           #Remove the no-flag parameters
	    keys(%$GetOptHash)) .
       ']' : '[...]');

    print("\n$script -i \"input file(s)\" $options\n",
	  (!$local_extended ? '' :
	   "$script $options < input_file\n"),
	  "\n");

    if($error_mode)
      {print("Run with no options for usage.\n")}
    else
      {
	if(!$local_extended)
	  {
	    print << 'end_print';
     -s                   REQUIRED Summary abundance file(s).  See --help for
                                   file format.
     -i                   REQUIRED Input sequence file(s).  See --help for file
                                   format.
     -o                   OPTIONAL [stdout] Output file.
     -a                   OPTIONAL [10] The minimum average abundance at or
                                   above which a sequence will be considered.
     -x                   OPTIONAL [0.8] Maximum dynamical similarity of the
                                   abundances of two sequences required to
                                   output a pair.
     -n                   OPTIONAL [0.9] Minimum sequence identity of two
                                   sequences required to output a pair.
     --verbose            OPTIONAL Verbose mode.
     --quiet              OPTIONAL Quiet mode.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version.
     --save-as-default    OPTIONAL Save the command line arguments.
     --help               OPTIONAL Print general info and file formats.
     --extended           OPTIONAL Print extended usage/help/version/header.

Run with just --extended for a more descriptive usage & additional options.
end_print
	  }
	else #Advanced options/extended usage output
	  {
	    print << 'end_print';
     -s,--abunds-file    *REQUIRED Input file of abundances of "real" sequences
                                   in multiple samples.  This is the file you
                                   would get from running getReals.pl with the
                                   -s option.  There must be either 1 total
                                   abundance files or 1 abundance file for
                                   every outfile (-o).
                                   *If you wish to interrogate all sequences,
                                   regardless of whether they are real or not
                                   and you supply multiple sample sequence
                                   files with abundances on the deflines to -i,
                                   -s is not required.
     -i,--seq-file        REQUIRED Input fasta or fastq sequence file(s).  This
                                   may be a global library sequence file or a
                                   series of sample sequence files.  In either
                                   case, the union of all sequences are used.
                                   Space separated, globs OK (e.g. -i "*.fna
                                   [A-Z].{?,??}.fa").  See --extended --help
                                   for file format and advanced usage examples.
                                   *No flag required.
     -o,--outfile         OPTIONAL [stdout] Output file.  Will not overwrite
                                   without --overwrite.  Default behavior
                                   prints output to standard out.  See
                                   --extended --help for output file format and
                                   advanced usage examples.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Default output
                                   directory is the current directory.
                                   Creates directories specified, but not
                                   recursively.  Also see --extended --help for
                                   advanced usage examples.
     -a,                  OPTIONAL [see footnote*] The minimum average
     --min-mean-abundance          abundance at or above which a sequence will
                                   be considered.  See -x and -n for further
                                   criteria.  A value of 0 means use all
                                   sequences.
                                   *The default value is a percentage of the
                                   total abundance of all sequences across all
                                   samples (divided by the number of saples),
                                   but when this option is used, the minimum
                                   should be expressed in raw abundance of an
                                   individual sequence per sample.  To see the
                                   default calculated minimum, use --verbose
                                   mode.
     -x,                  OPTIONAL [0.8] Maximum dynamical similarity of the
     --max-dynamical-              abundances across all samples of two
       similarity                  sequences in order for the pair to be
                                   output.  See --help for information on how
                                   dynamical similarity is calculated.
     -n,                  OPTIONAL [0.9] Minimum sequence identity of two
     --min-seq-similarity          sequences in order for the pair to be
                                   output.  Sequence similarity is determined
                                   using usearch's ublast capability.
     -t,                  OPTIONAL [auto](fasta,fastq,auto) Input file (-i)
     --sequence-filetype           type.  Using a value other than auto will
                                   make file reading faster.  "auto" cannot be
                                   used when redirecting a file in.
     -p,--abundance-      OPTIONAL [size=(\\d+);] A perl regular expression
        pattern                    used to extract the abundance value from the
                                   fasta/fastq defline of the input sequence
                                   file.  The abundance value pattern must be
                                   surrounded by parenthases.  If no
                                   parenthases are provided, the entire pattern
                                   will be assumed to be the abundance value.
                                   Abundance values are captured this way to
                                   allow flexibility of file format.  Note, the
                                   entire defline, without the trailing hard
                                   return but including the '>' or '\@'
                                   character is available to match against.
     -q,--seq-id-pattern  OPTIONAL [^\\s*[>\\\@]\\s*([^;]+)] A perl regular
                                   expression to extract seq IDs from deflines.
                                   The ID pattern must be surrounded by
                                   parenthases.  If no parenthases are
                                   provided, the entire pattern is assumed to
                                   be the ID.  If the pattern does not match,
                                   a default pattern that uses the first string
                                   up to the first white space will be used.
                                   Sequence IDs are captured this way to
                                   allow flexibility of file format.  Note, the
                                   entire defline, without the trailing hard
                                   return but including the '>' or '\@'
                                   character is available to match against.
     --significant-digits OPTIONAL [3] Number of significant digits to report
                                   in the output files and verbose output.  0
                                   means all digits are significant.
     --tmpdir             OPTIONAL [env*] The directory to use for temporary
                                   intermediate files used by usearch.
                                   *Default values is retrieved from an
                                   environment variable.
     --tmp-suffix         OPTIONAL [.tmp.fna] Suffix appended to temporary file
                                   names to make them easily identifiable.
     --parallel-processes OPTIONAL [1/core*] Specify the number of threads to
                                   use when running usearch.
                                   *By default, this script will set this value
                                   to the number of cores on your machine.
                                   Even when set to 1, this script will always
                                   perform & process alignments in a child
                                   process.  This number is a maximum.  You can
                                   run more processes than the available cores,
                                   but note that you may hit some system
                                   limitations.  Must be greater than 0.
     -y,--usearch-exe     OPTIONAL [usearch] The command used to call usearch
                                   to determine sequence identity.  See
                                   --usearch-opts-str to fine-tune the running
                                   of usearch.
     --usearch-col-str    OPTIONAL [query+target+id+ql+qlo+qhi+tl+tlo+thi+
                                   evalue] This is the string passed to
                                   usearch's -userfields option.  All the
                                   values in the default string are required,
                                   but the order may be changed and fields may
                                   be added.  The entire string must be
                                   supplied, not just added fields.
     --usearch-opts-str   OPTIONAL [none] usearch is run with a series of
                                   default options, some of which may be
                                   changed using --usearch-opts-str.  Note that
                                   certain usearch options are controlled by
                                   other options in this script.  In
                                   particular, -n controls these 3 usearch
                                   options: -id, -query_cov, and -target_cov.
                                   Alignments returned by usearch based on
                                   these options are extended to the full query
                                   length and the identity is tweaked.  Hits to
                                   self are filtered out.  Other options linked
                                   to usearch options: --usearch-col-str
                                   controls -userfields and --parallel-
                                   processes controls -threads.  -ublast and
                                   -db take a file that is created based on -i
                                   and -s.  These usearch options have defaults
                                   that cannot be changed: -userout (which
                                   defaults to a temporary file) and -strand
                                   (plus).  These options can be changed using
                                   --usearch-opts-str: -accel (0.6), -evalue
                                   (10), and -db.  None of the default options
                                   can be removed, though any option may be
                                   added.  Here is an example of a usearch
                                   command line created by this script:

                                   usearch -ublast 1234567.tmp.fna
                                     -db 12345671.tmp.fna
                                     -userout 12345672.tmp.fna -evalue 10
                                     -id 0.8 -accel 0.6 -strand plus
                                     -threads 4 -userfields query+target+id+ql\
                                     +qlo+qhi+tl+tlo+thi+evalue -query_cov 0.8
                                     -target_cov 0.8 -quiet

     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --quiet              OPTIONAL Quiet mode.
     --overwrite          OPTIONAL Overwrite existing output files.  By
                                   default, existing output files will not be
                                   over-written.  See also --skip-existing.
     --skip-existing      OPTIONAL Skip existing output files.
     --force              OPTIONAL Prevent script-exit upon critical error and
                                   continue processing.  Supply twice to
                                   additionally prevent skipping the processing
                                   of input files that cause errors.  Use this
                                   option with extreme caution.  This option
                                   will not over-ride over-write protection.
                                   See also --overwrite or --skip-existing.
     --append-usearch,    OPTIONAL [Off] Append usearch output columns to
     --no-append-usearch           --outfile's default columns.  See
                                   --usearch-col-str to add usearch columns.
                                   If supplied with --header, the header line
                                   will include the --usearch-col-str column
                                   names.
     --append-abunds,     OPTIONAL [Off] Append abundance values of both
     --no-append-abunds            sequences to --outfile's default columns.
                                   If supplied with --header, the header line
                                   will show the starting column of each of the
                                   pair's abundance values.
     --header,--noheader  OPTIONAL [On] Print commented script version, date,
                                   and command line call to each output file.
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
                                   Values less than 0 debug the template code
                                   that was used to create this script.
     --error-type-limit   OPTIONAL [5] Limits each type of error/warning to
                                   this number of outputs.  Intended to
                                   declutter output.  Note, a summary of
                                   warning/error types is printed when the
                                   script finishes, if one occurred or if in
                                   verbose mode.  0 = no limit.  See also
                                   --quiet.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info.  Includes template
                                   version with --extended.
     --save-as-default    OPTIONAL Save the command line arguments.  Saved
                                   defaults are printed at the bottom of this
                                   usage output and used in every subsequent
                                   call of this script.  Supplying this flag
                                   replaces current defaults with all options
                                   that are provided with this flag.  Values
                                   are stored in [$defaults_dir].
     --aggregate-mode,    OPTIONAL [On] Allows multiple input file combinations
     --no-aggregate-mode           to generate output that is  accumulated in a
                                   common output file.  This is only used when
                                   -o is provided.  When off, if
                                   --join-file-conflicts is provided, output
                                   file names are manipulated to attempt to
                                   avoid future overwrites.  If a conflict
                                   cannot be avoided, exits with an error
                                   before file processing begins.
     --join-conflicts     OPTIONAL [Off] Attempt to avoid future overwrite
     --no-join-conflicts           situations by joining different types of
                                   input filenames together using a delimiting
                                   dot ('.').  Ordering of the joined names
                                   cannot be changed.  Use --verbose to
                                   observe output file names.  This option only
                                   applies when there are multiuple types of
                                   input files (more than just -i) and
                                   aggregate mode is off.
     --extended           OPTIONAL Print extended usage/help/version/header.
                                   Supply alone for extended usage.  Includes
                                   extended version in output file headers.
                                   Incompatible with --noheader.  See --help &
                                   --version.
     --help               OPTIONAL Print general info and file format
                                   descriptions.  Includes advanced usage
                                   examples with --extended.
end_print
	  }

	my @user_defaults = getUserDefaults();
	print(scalar(@user_defaults) ?
	      "Current user defaults: [@user_defaults].\n" :
	      "No user defaults set.\n");
      }

    return(0);
  }

#Uses globals: $seq_id_pattern, $abundance_pattern, $homopolymers_only
#Returns sequence records as an array of arrays and adds the original order
#number to the end of each record in a hash keyed on "ORDER".
sub getCheckAllSeqRecs
  {
    my $input_file     = $_[0];
    my $seq_recs       = [];
    my $seq_hash       = {};

    openIn(*INPUT,$input_file) || return($seq_recs);

    my $verbose_freq      = 100;
    my $cnt               = 0;
    my $skip              = 0;
    my($rec,$len);

    #For each line in the current input file
    while($rec = getNextSeqRec(*INPUT))
      {
	$cnt++;
	verboseOverMe("[$input_file] Reading record: [$cnt].")
	  unless($cnt % $verbose_freq);

	my($def,$seq) = @$rec;
	my $id        = '';

	if($def =~ /\s*[\@\>]\s*(\S+)/)
	  {
	    my $default_id = $1;

	    if($seq_id_pattern ne '' && $def =~ /$seq_id_pattern/)
	      {$id = $1}
	    else
	      {
		$id = $default_id;
		warning("Unable to parse seqID from defline: [$def] ",
			"in file: [$input_file] using pattern: ",
			"[$seq_id_pattern].  Please either fix the ",
			"defline or use a different pattern to extract ",
			"the seqID value.  Using default ID: [$id].  ",
			"Use \"-q ''\" to to avoid this warning.")
		  if($seq_id_pattern ne '');
	      }
	  }
	else
	  {
	    error("Could not parse defline in record [$cnt] of file ",
		  "[$input_file]: [$def].  Please edit the file to contain ",
		  "IDs and abundance values on the deflines.  Skipping this ",
		  "file.");
	    $skip = 1;
	    last;
	  }

	if(!defined($len))
	  {$len = length($seq)}
	elsif($len != length($seq))
	  {
	    error("Sequences in sequence file: [$input_file] are required to ",
		  "be the same length.  Sequence [$id] in record [$cnt] is ",
		  "length: [",length($seq),"] which is different from the ",
		  "previous sequence(s): [$len].  Please edit the file to ",
		  "truncate the sequences to the same length.  Skipping ",
		  "file.");
	    $skip = 1;
	    last;
	  }

	if(exists($seq_hash->{$id}))
	  {warning("Sequence ID: [$id] found multiple times in input file: ",
		   "[$input_file].")}

	my $abund = 1;
	if($def =~ /$abundance_pattern/)
	  {
	    my $matched_abund = $1;

	    if($matched_abund =~ /^\d+$/)
	      {$abund = $matched_abund}
	    else
	      {warning("Unable to parse abundance from defline: [$def] ",
		       "using pattern: [$abundance_pattern].  Please ",
		       "either fix the defline or use a different ",
		       "pattern to extract the abundance value.  Using ",
		       "default abundance: [$abund].")}
	  }
	else
	  {warning("Could not parse defline [$def].  Assuming abundance 1.")}

	$seq_hash->{$id} = 1;

	#ORDER is an integer to use to sort to get back the original file order
	#ID is the ID parsed from the defline
	#ABUND is the sequence abundance parsed from the defline
	#NOMONOS is the sequence after extracting all homopolymer repeats
	#NOMONOC is a string of ascii characters indicating homopolymer lengths
	#NOMONOL is the sequence length after homopolymer repeat extraction
	push(@$seq_recs,[$def,$seq,{ID      => $id,
				    ABUND   => $abund}]);
      }

    closeIn(*INPUT);

    return($skip ?
	   (wantarray ? () : []) :
	   (wantarray ? (@$seq_recs) : $seq_recs));
  }

#Copied from DNAstiffness.pl on 2/12/2014 so as to be independent -Rob
sub getNextFastaRec
  {
    my $handle    = $_[0];      #File handle or file name
    my $no_format = $_[1];

    if(exists($main::{FASTABUFFER}) && exists($main::{FASTABUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTABUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTABUFFER}->{$handle}});
		@{$main::{FASTABUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTABUFFER}->{$handle}}));
	  }
	elsif(eof($handle))
	  {return(undef)}
      }

    my $parent_id_check = {};
    my $first_loop = 0;
    my $line_num = 0;
    my $line     = '';
    my $defline  = '';
    my($seq);

    #For each line in the current input file
    while(getLine($handle))
      {
	$line_num = $.;
	$line = $_;

	next if($line !~ /\S/ || $line =~ /^\s*#/);
	if($line =~ />/)
	  {
	    if($defline)
	      {
		my $solidseq =
		  ($no_format ? $seq :
		   formatSequence($seq));
		chomp($solidseq);
		chomp($defline);

		push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);
	      }
	    $defline = $line;

	    my $tmp_id = $defline;
	    $tmp_id =~ s/^\s*>\s*//;
	    $tmp_id =~ s/\s.*//;
	    if($tmp_id eq '')
	      {warning("No Defline ID on line: [$line_num] of current file.  ",
		       " Universal coordinates will be used if some were ",
		       "supplied either via command line arguments of via ",
		       "coordinate file with no parent sequence ID.")}
	    elsif(exists($parent_id_check->{$tmp_id}))
	      {
		error("Two sequences found with the same ID on the ",
		      "defline: [$tmp_id] in current fasta file.  The same ",
		      "pairs of coordinates will be used for each sequence.");
	      }

	    undef($seq);
	  }
	elsif($line =~ /^([^\t]+?) *\t\s*(.*)/)
	  {
	    $defline = $1;
	    $seq     = $2;

	    my $solidseq =
	      ($no_format ? $seq :
	       formatSequence($seq));
	    chomp($solidseq);
	    chomp($defline);

	    push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);

	    undef($seq);
	  }
	else
	  {$seq .= $line}
      }

    #Handle the last sequence (if there were any sequences)
    if(defined($seq))
      {
	my $solidseq =
	  ($no_format ? $seq :
	   formatSequence($seq));
	chomp($solidseq);
	chomp($defline);

	push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);
      }

    #Return the first sequence (if sequence was parsed)
    if(exists($main::{FASTABUFFER}) && exists($main::{FASTABUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTABUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTABUFFER}->{$handle}});
		@{$main::{FASTABUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTABUFFER}->{$handle}}));
	  }
	else
	  {return(undef)}
      }
    else
      {return(undef)}
  }

#Copied from DNAstiffness.pl on 2/12/2014 so as to be independent -Rob
sub formatSequence
  {
    #1. Read in the parameters.
    my $sequence          = $_[0];
    my $chars_per_line    = $_[1];
    my $coords_left_flag  = $_[2];
    my $coords_right_flag = $_[3];
    my $start_coord       = $_[4];
    my $coords_asc_flag   = $_[5];
    my $coord_upr_bound   = $_[6];
    my $uppercase_flag    = $_[7];
    my $print_flag        = $_[8];
    my $nucleotide_flag   = $_[9];

    my($formatted_sequence,
       $sub_string,
       $sub_sequence,
       $coord,
       $max_num_coord_digits,
       $line_size_left,
       $lead_spaces,
       $line);
    my $coord_separator = '  ';
    my $tmp_sequence = $sequence;
    $tmp_sequence =~ s/\s+//g;
    $tmp_sequence =~ s/<[^>]*>//g;
    my $seq_len = length($tmp_sequence);

    #2. Error check the parameters and set default values if unsupplied.
    my $default_chars_per_line    = ''; #Infinity
    my $default_coords_left_flag  = 0;
    my $default_coords_right_flag = 0;
    my $default_start_coord       = (!defined($coords_asc_flag) ||
				     $coords_asc_flag ? 1 : $seq_len);
    my $default_coords_asc_flag   = 1;
    my $default_coord_upr_bound   = undef();  #infinity (going past 1 produces
    my $default_uppercase_flag    = undef();  #          negative numbers)
    my $default_print_flag        = 0;

    if(!defined($chars_per_line) || $chars_per_line !~ /^\d+$/)
      {
        if(defined($chars_per_line) &&
	   $chars_per_line !~ /^\d+$/ && $chars_per_line =~ /./)
	  {print("WARNING:seq-lib.pl:formatSequence: Invalid ",
	         "chars_per_line: [$chars_per_line] - using default: ",
		 "[$default_chars_per_line]<BR>\n")}
        #end if(chars_per_line !~ /^\d+$/)
	$chars_per_line = $default_chars_per_line;
      }
    elsif(!$chars_per_line)
      {$chars_per_line = ''}
    #end if(!defined($chars_per_line) || $chars_per_line !~ /^\d+$/)
    if(!defined($coords_left_flag))
      {$coords_left_flag = $default_coords_left_flag}
    #end if(!defined(coords_left_flag))
    if(!defined($coords_right_flag))
      {$coords_right_flag = $default_coords_right_flag}
    #end if(!defined(coords_right_flag))
    if(!defined($start_coord) || $start_coord !~ /^\-?\d+$/)
      {
        if(defined($start_coord) &&
           ($coords_left_flag || $coords_right_flag))
          {print("WARNING:formatSequence.pl:formatSequence: Invalid ",
                 "start_coord: [$start_coord] - using default: ",
                 "[$default_start_coord]\n")}
        #end if($start_coord !~ /^\d+$/)
        $start_coord = $default_start_coord;
      }
    #end if(!defined($start_coord) || $start_coord !~ /^\d+$/)
    if(!defined($coords_asc_flag))
      {$coords_asc_flag = $default_coords_asc_flag}
    #end if(!defined(coords_right_flag))
    if(defined($coord_upr_bound) && $coord_upr_bound !~ /^\d+$/)
      {undef($coord_upr_bound)}
    if(!defined($print_flag))
      {$print_flag = $default_print_flag}
    #end if(!defined($print_flag))

    if(defined($coord_upr_bound) && $start_coord < 1)
      {$start_coord = $coord_upr_bound + $start_coord}
    elsif($start_coord < 1)
      {$start_coord--}
    elsif(defined($coord_upr_bound) && $start_coord > $coord_upr_bound)
      {$start_coord -= $coord_upr_bound}

    #3. Initialize the variables used for formatting.  (See the DATASTRUCTURES
    #   section.)
    if($coords_asc_flag)
      {
        if(defined($coord_upr_bound) &&
           ($seq_len + $start_coord) > $coord_upr_bound)
          {$max_num_coord_digits = length($coord_upr_bound)}
        else
          {$max_num_coord_digits = length($seq_len + $start_coord - 1)}

        $coord = $start_coord - 1;
      }
    else
      {
        if(defined($coord_upr_bound) && ($start_coord - $seq_len + 1) < 1)
          {$max_num_coord_digits = length($coord_upr_bound)}
        elsif(!defined($coord_upr_bound) &&
              length($start_coord - $seq_len - 1) > length($start_coord))
          {$max_num_coord_digits = length($start_coord - $seq_len - 1)}
        else
          {$max_num_coord_digits = length($start_coord)}

        $coord = $start_coord + 1;
      }
    $line_size_left = $chars_per_line;
    $lead_spaces    = $max_num_coord_digits - length($start_coord);

    #5. Add the first coordinate with spacing if coords_left_flag is true.
    $line = ' ' x $lead_spaces . $start_coord . $coord_separator
      if($coords_left_flag);

    #6. Foreach sub_string in the sequence where sub_string is either a
    #   sub_sequence or an HTML tag.
    foreach $sub_string (split(/(?=<)|(?<=>)/,$sequence))
      {
        #6.1 If the substring is an HTML tag
        if($sub_string =~ /^</)
          #6.1.1 Add it to the current line of the formatted_sequence
          {$line .= $sub_string}
        #end if(sub_string =~ /^</)
        #6.2 Else
        else
          {
            $sub_string =~ s/\s+//g;

	    if($nucleotide_flag)
	      {
		my(@errors);
		(@errors) = ($sub_string =~ /([^ATGCBDHVRYKMSWNX])/ig);
		$sub_string =~ s/([^ATGCBDHVRYKMSWNX])//ig;
		if(scalar(@errors))
		  {print STDERR ("WARNING:formatSequence.pl:formatSequence:",
				 scalar(@errors),
				 " bad nucleotide characters were ",
				 "filtered out of your sequence: [",
				 join('',@errors),
				 "].\n")}
	      }

            #6.2.1 If the sequence is to be uppercased
            if(defined($uppercase_flag) && $uppercase_flag)
              #6.2.1.1 Uppercase the sub-string
              {$sub_string = uc($sub_string)}
            #end if(defined($uppercase_flag) && $uppercase_flag)
            #6.2.2 Else if the sequence is to be lowercased
            elsif(defined($uppercase_flag) && !$uppercase_flag)
              #6.2.2.1 Lowercase the sub-string
              {$sub_string = lc($sub_string)}
            #end elsif(defined($uppercase_flag) && !$uppercase_flag)

            #6.2.3 While we can grab enough sequence to fill the rest of a line
            while($sub_string =~ /(.{1,$line_size_left})/g)
              {
                $sub_sequence = $1;
                #6.2.3.1 Add the grabbed sequence to the current line of the
                #        formatted sequence
                $line .= $sub_sequence;
                #6.2.3.2 Increment the current coord by the amount of sequence
                #        grabbed
                my $prev_coord = $coord;
                if($coords_asc_flag)
                  {
                    $coord += length($sub_sequence);
                    if(defined($coord_upr_bound)      &&
                       $prev_coord <= $coord_upr_bound &&
                       $coord > $coord_upr_bound)
                      {$coord -= $coord_upr_bound}
                  }
                else
                  {
                    $coord -= length($sub_sequence);
                    if(defined($coord_upr_bound) &&
                       $prev_coord >= 1 && $coord < 1)
                      {$coord = $coord_upr_bound + $coord - 1}
                    elsif($prev_coord >= 1 && $coord < 1)
                      {$coord--}
                  }
                #6.2.3.3 If the length of the current sequence grabbed
                #        completes a line
                if($line_size_left eq '' ||
		   length($sub_sequence) == $line_size_left)
                  {
                    $lead_spaces = $max_num_coord_digits - length($coord);
                    #6.2.3.3.1 Conditionally add coordinates based on the
                    #          coords flags
                    $line .= $coord_separator . ' ' x $lead_spaces . $coord
                      if($coords_right_flag);

                    #6.2.3.3.2 Add a hard return to the current line of the
                    #          formatted sequence
                    $line .= "\n";

                    #6.2.3.3.3 Add the current line to the formatted_sequence
                    $formatted_sequence .= $line;
                    #6.2.3.3.4 Print the current line if the print_flag is true
                    print $line if($print_flag);

                    #6.2.3.3.5 Start the next line
                    $lead_spaces = $max_num_coord_digits - length($coord+1);
                    $line = '';
                    $line = ' ' x $lead_spaces
                          . ($coords_asc_flag ? ($coord+1) : ($coord-1))
                          . $coord_separator
                      if($coords_left_flag);

                    #6.2.3.3.6 Reset the line_size_left (length of remaining
                    #          sequence per line) to chars_per_line
                    $line_size_left = $chars_per_line;
                  }
                #end if(length($sub_sequence) == $line_size_left)
                #6.2.3.4 Else
                else
                  #6.2.3.4.1 Decrement line_size_left (length of remaining
                  #          sequence per line) by the amount of sequence
                  #          grabbed
                  {$line_size_left -= length($sub_sequence)}
                #end 6.2.3.4 Else
              }
            #end while($sub_string =~ /(.{1,$line_size_left})/g)
          }
        #end 6.2 Else
      }
    #end foreach $sub_string (split(/(?=<)|(?<=>)/,$sequence))
    #7. Add the last coodinate with enough leadin white-space to be lined up
    #   with the rest coordinates if the coords_right_flag is true
    $lead_spaces = $max_num_coord_digits - length($coord);
    $line .= ' ' x $line_size_left . $coord_separator . ' ' x $lead_spaces
          . $coord
      if($coords_right_flag && $line_size_left != $chars_per_line);
    $line =~ s/^\s*\d+$coord_separator\s*$// if($coords_left_flag);

    #8. Add the ending PRE tag to the last line of the formatted sequence
    $line =~ s/\n+$/\n/s;

    #9. Add the last line to the formatted_sequence
    $formatted_sequence .= $line;
    #10. Print the last line if the print_flag is true
    print "$line\n" if($print_flag);

    if($coord < 1 && ($coords_left_flag || $coords_right_flag))
      {print("WARNING: The sequence straddles the origin.  Coordinates are ",
             "inaccurate.")}

    #11. Return the formatted_sequence
    return $formatted_sequence;
  }

#Merged the above getNextFastaRec subroutine with the code from convertSeq.pl
#on 2/12/2014
sub getNextFastqRec
  {
    my $handle    = $_[0];      #File handle or file name
    my $no_format = $_[1];

    if(exists($main::{FASTQBUFFER}) && exists($main::{FASTQBUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTQBUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTQBUFFER}->{$handle}});
		@{$main::{FASTQBUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTQBUFFER}->{$handle}}));
	  }
	elsif(eof($handle))
	  {return(undef)}
      }

    my $parent_id_check  = {};
    my $first_loop       = 0;
    my $line_num         = 0;
    my $line             = '';
    my $defline          = '';
    my $seq              = '';
    my $qual             = '';
    my $getting_sequence = 0;
    my $comment_buffer   = '';

    #For each line in the current input file
    while(getLine($handle))
      {
	$line_num = $.;
	$line = $_;

	next if($line !~ /\S/ || $line =~ /^\s*#/);

	#If this is the defline, or the quality length is the same as the seq
	if(length($qual) >= length($seq) && /^\s*\@[^\n\r]*/)
	  {
	    if($defline ne '' || $seq ne '' || $qual ne '')
	      {
		my $solidseq =
		  ($no_format ? $seq :
		   formatSequence($seq));
		$qual =~ s/[\s\r\n\t]+//g unless($no_format);
		chomp($solidseq);
		chomp($qual);
		chomp($defline);

		push(@{$main::{FASTQBUFFER}->{$handle}},
		     [$defline,$solidseq,$qual]);
	      }
	    $defline = $line;

	    my $tmp_id = $defline;
	    $tmp_id =~ s/^\s*\@\s*//;
	    $tmp_id =~ s/\s.*//;

	    if($tmp_id eq '')
	      {warning("No Defline ID on line: [$line_num] of current file.")}
	    elsif(exists($parent_id_check->{$tmp_id}))
	      {error("Two sequences found with the same ID on the ",
		     "defline: [$tmp_id] in current fastq file.")}

	    $seq  = '';
	    $qual = '';
	    $getting_sequence = 1;
	  }
	elsif($getting_sequence && /^\s*\+[^\n\r]*/)
	  {$getting_sequence = 0}
	elsif($getting_sequence)
	  {
	    s/\s+//g;
	    if(/^[A-Za-z\n\.~]*$/)
	      {$seq .= $_}
	    else
	      {
		error("Expected a sequence character string, but ",
		      "got: [$_].  Appending anyway.");
		$seq .= $_;
	      }
	  }
	elsif($seq =~ /./)
	  {
	    s/\s+//g;
	    if(/^[\!-\~]*$/)
	      {$qual .= $_}
	    else
	      {
		error("Expected a quality character string, but ",
		      "got: [$_].  Appending anyway.");
		$qual .= $_;
	      }
	  }
	#else must be a comment, ignore it
      }

    #Handle the last sequence (if there were any sequences)
    if($seq ne '')
      {
	my $solidseq =
	  ($no_format ? $seq :
	   formatSequence($seq));
	$qual =~ s/[\s\r\n\t]+//g unless($no_format);
	chomp($solidseq);
	chomp($defline);
	chomp($qual);

	push(@{$main::{FASTQBUFFER}->{$handle}},[$defline,$solidseq,$qual]);
      }

    #Return the first sequence (if sequence was parsed)
    if(exists($main::{FASTQBUFFER}) && exists($main::{FASTQBUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTQBUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTQBUFFER}->{$handle}});
		@{$main::{FASTQBUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTQBUFFER}->{$handle}}));
	  }
	else
	  {return(undef)}
      }
    else
      {return(undef)}
  }

#Uses global variables: lastfiletype, filetype, & $input_file
sub getNextSeqRec
  {
    if(!defined($main::lastfiletype) || $filetype ne 'auto')
      {
	if($filetype eq 'fasta')
	  {$main::getnextsub = \&getNextFastaRec}
	elsif($filetype eq 'fastq')
	  {$main::getnextsub = \&getNextFastqRec}
      }
    elsif(defined($main::lastfiletype) &&
	  exists($main::lastfiletype->{$input_file}))
      {
	if($main::lastfiletype->{$input_file} eq 'fasta')
	  {$main::getnextsub = \&getNextFastaRec}
	elsif($main::lastfiletype->{$input_file} eq 'fastq')
	  {$main::getnextsub = \&getNextFastqRec}
      }

    if($filetype eq 'auto' &&
       (!defined($main::lastfiletype) ||
	!exists($main::lastfiletype->{$input_file})))
      {
	if($input_file eq '-')
	  {
	    error("`-t auto` cannot be used when the input file is supplied ",
		  "on standard input.  Please supply the exact file type.");
	    quit(14);
	  }

	if(!-e $input_file || $input_file =~ / /)
	  {
	    error("`-t auto` cannot be used when the input file does not ",
		  "exist or has a space in its name.  Please supply the ",
		  "exact file type.");
	    quit(15);
	  }

	my $num_fastq_defs =
	  `head -n 50 $input_file | grep -c -E '^[\@\+]'`;
	debug("System output from: [",
	      qq(head -n 50 $input_file | grep -c -E '^[\@\+]'),
	      "]:\n$num_fastq_defs") if($DEBUG > 1);
	$num_fastq_defs =~ s/^\D+//;
	$num_fastq_defs =~ s/\D.*//;

	if($num_fastq_defs > 0)
	  {
	    $main::getnextsub = \&getNextFastqRec;
	    $main::lastfiletype->{$input_file} = 'fastq';
	  }
	else
	  {
	    my $num_fasta_defs =
	      `head -n 50 $input_file | grep -c -E '^>'`;
	    $num_fasta_defs =~ s/^\D+//;
	    $num_fasta_defs =~ s/\D.*//;

	    if($num_fasta_defs > 0)
	      {
		$main::getnextsub = \&getNextFastaRec;
		$main::lastfiletype->{$input_file} = 'fasta';
	      }
	    else
	      {
		if(!defined($main::lastfiletype) ||
		   !exists($main::lastfiletype->{$input_file}))
		  {
		    error("Unable to determine file type.  Skipping file ",
			  "[$input_file].");
		    return(undef);
		  }
		warning("Unable to determine file type.  Defaulting to ",
			"[$main::lastfiletype->{$input_file}].");
		if($main::lastfiletype->{$input_file} eq 'fasta')
		  {$main::getnextsub = \&getNextFastaRec}
		else
		  {$main::getnextsub = \&getNextFastqRec}
	      }
	  }
      }

    return($main::getnextsub->(@_));
  }

#Globals used: $abundance_pattern
sub getAbund
  {
    my $rec   = $_[0];
    my $abund = 1;
    if(scalar(@$rec) >= 3 && defined($rec->[2]) &&
       ref($rec->[2]) eq 'HASH' && exists($rec->[2]->{ABUND}))
      {$abund = $rec->[2]->{ABUND}}
    elsif($rec->[0] =~ /$abundance_pattern/)
      {
	my $matched_abund = $1;

	if($matched_abund =~ /^\d+$/)
	  {$abund = $matched_abund}
	else
	  {
	    warning("Unable to parse abundance from defline: [$rec->[0]] ",
		    "using pattern: [$abundance_pattern].  Please ",
		    "either fix the defline or use a different ",
		    "pattern to extract the abundance value.  Using ",
		    "default abundance: [1].");
	  }
	$rec->[2]->{ABUND} = $abund;
      }
    else
      {
	warning("Could not parse defline [$rec->[0]].  Assuming abundance 1.");
	$rec->[2]->{ABUND} = $abund;
      }

    return($abund);
  }

sub getSize
  {
    my $rec  = $_[0];
    my $size = '';

    if(scalar(@$rec) >= 3 && defined($rec->[2]) &&
       ref($rec->[2]) eq 'HASH' && exists($rec->[2]->{SIZE}))
      {$size = $rec->[2]->{SIZE}}
    else
      {
	$size = length($rec->[1]);
	$rec->[2]->{SIZE} = $size;
      }

    return($size);
  }

#Globals used: $seq_id_pattern
sub getID
  {
    my $rec = $_[0];
    my $id = '';

    if(ref($rec) ne 'ARRAY')
      {
	error("First argument must be an array reference.");
	return($id);
      }

    if(scalar(@$rec) >= 3 && defined($rec->[2]) &&
       ref($rec->[2]) eq 'HASH' && exists($rec->[2]->{ID}))
      {$id = $rec->[2]->{ID}}
    elsif($rec->[0] =~ /\s*[\@\>]\s*(\S+)/)
      {
	my $default_id = $1;

	if($seq_id_pattern ne '' && $rec->[0] =~ /$seq_id_pattern/)
	  {$id = $1}
	else
	  {
	    $id = $default_id;
	    warning("Unable to parse seqID from defline: [$rec->[0]] ",
		    "using pattern: [$seq_id_pattern].  Please ",
		    "either fix the defline or use a different ",
		    "pattern to extract the seqID value.  Using ",
		    "default ID: [$default_id].  Use \"-q ''\" to to ",
		    "avoid this warning.") if($seq_id_pattern ne '');
	  }
	$rec->[2]->{ID} = $id;
      }
    else
      {
	warning("Could not parse defline [$rec->[0]].  Using entire ",
		"defline.");
	$id = $rec->[0];
	$rec->[2]->{ID} = $id;
      }

    return($id);
  }

sub incompatible
  {
    my $usearch = $_[0];
    my $exe = $usearch;
    $exe =~ s/ .*//;

    if(!defined($usearch) || $usearch eq '' || !-e $exe || !-x $exe)
      {
	error("The usearch executable [$exe] appears to either not be in ",
	      "your path, not exist, not have execute permissions, or you ",
	      "have not created a symbolic link named 'usearch' to the full ",
	      "name of the executable with version number.  If you have not ",
	      "installed usearch, you can find it here: http://www.drive5.com",
	      "/usearch/download.html");
	return(1);
      }

    my $version = `$exe -version`;
    chomp($version);
    if($version =~ /usearch v(\S+)/)
      {
	my $vnum = $1;
	my $confirmed = [3,8,31];
	my $vnums = [split(/\.|_/,$vnum,-1)];
	my $ok = 1;
	my $i = 0;
	for($i = 0;$i < scalar(@$vnums) && $i < scalar(@$confirmed);$i++)
	  {
	    if($vnums->[$i] != $confirmed->[$i])
	      {
		if($vnums->[$i] < $confirmed->[$i])
		  {$ok = 0}
		else
		  {$ok = 1}
		last;
	      }
	  }
	warning("This script was tested with usearch version ",
		"7.0.1090_i86osx32.  Your version appears to be [$vnum], ",
		"thus it may not work properly.") unless($ok);
      }
    else
      {warning("This script was tested with usearch version ",
	       "7.0.1090_i86osx32.  It may not work properly with the ",
	       "version you are using.")}

    unless(eval('use IO::Pipe::Producer;1;') ||
	   eval('use local::lib;use IO::Pipe::Producer;1;'))
      {
	error('Perl Module [IO::Pipe::Producer] not found.');
	return(1);
      }
    unless(eval('use IO::Select;1;') ||
	   eval('use local::lib;use IO::Select;1;'))
      {
	error('Perl Module [IO::Select] not found.');
	return(1);
      }
    unless(eval('use Math::Random qw(:all);1;') ||
	   eval('use local::lib;use Math::Random qw(:all);1;'))
      {
	error('Perl Module [Math::Random] not found.');
	return(1);
      }
    unless(eval("use Statistics::Distributions;1;") ||
	   eval("use local::lib;use Statistics::Distributions;1;"))
      {
	error('Perl Module [Statistics::Distributions] not found.');
	return(1);
      }

    return(0);
  }

sub getUsearchExe
  {
    my $usearch   = $_[0];
    my $sent_exe = $usearch;
    $sent_exe    =~ s/ .*//;
    my $exe      = '';

    if(eval("use File::Which;1;") ||
       eval("use local::lib;use File::Which;1;"))
      {$exe = which($sent_exe)}
    else
      {
	verbose("File::Which not found, switching to backup method.");
	$exe = `which $sent_exe`;
	chomp($exe);
	if($exe =~ /which: Command not found./ || $exe !~ /\S/)
	  {
	    error("System command 'which' does not appear to exist.  Please ",
		  "install the perl module File::Which.");
	    $exe = '';
	  }
	elsif($exe =~ /not found/i)
	  {$exe = ''}
      }

    return($exe);
  }

#Determine the number of cores on this machine.
sub getNumCores
  {
    my $num_cores = 1;
    unless(eval('use Sys::Info;1;') ||
	   eval('use local::lib;use Sys::Info;1;'))
      {
	warning("Perl module [Sys::Info] not found.  Use --parallel-",
		"processes to to use more than 1 core for this script.");
	return($num_cores);
      }
    my $info   = Sys::Info->new;
    my $cpu    = $info->device(CPU => ());
    $num_cores = $cpu->count;
    debug("Cores: [$num_cores].") if($DEBUG > 1);
    return($num_cores);
  }

#Returns: 2D array of hash keys (i.e. sequence IDs)
sub getDividedQueries
  {
    my $seq_hash      = $_[0]; #$seq_hash->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
    my $num_divisions = $_[1];

    if($num_divisions <= 0 || scalar(keys(%$seq_hash)) == 0)
      {return([[keys(%$seq_hash)]])}

    my $groupings  = [];
    my $group_num  = 0;
    my $group_size = int(scalar(keys(%$seq_hash)) / $num_divisions);
    $group_size++ if(scalar(keys(%$seq_hash)) / $num_divisions != $group_size);

    foreach my $id (keys(%$seq_hash))
      {
	$group_num++ if(scalar(@{$groupings->[$group_num]}) >= $group_size);
	push(@{$groupings->[$group_num]},$id);
      }

    return($groupings);
  }

#Enforces significant digits to number of $places supplied (unlimited if 0)
#Trims leading zeros
#Trims trailing zeros after the decimal
#Pads with zeros after the decimal up to $places digits if $pad is true
#Pads with zeros up to $pad digits if $places is 0
#Does not touch exponents
#Prepends a 1 to exponent values if it does not begin with a number
sub sigdig
  {
    my $num     = $_[0];
    my $places  = $_[1];

    #Pad with 0's by adding decimal places up to $places significant digits, or
    #if $places is 0, up to the pad value (note there can still be numbers in
    #this case with more significant digits than specified by the pad value)
    my $pad     = defined($_[2]) ? ($places > 0 && $_[2] ? $places : $_[2]) :
      0;

    my $new_num = '';

    if(!defined($places) || $places !~ /\d/ || $places =~ /\D/ || $places < 0)
      {
	error("An invalid number of significant digits was specified.");
	return($num);
      }
    #0 means all are significant digits
    elsif($places == 0)
      {
	$new_num = $num;
	my $cur_sig_dig = getsigdig($new_num);
	if($pad > $cur_sig_dig)
	  {
	    #If there's an exponent
	    if($new_num =~ /^([+\-]?[0-9\.]+)(e[+\-]?[0-9\.]+)$/i)
	      {
		my $pree = $1;
		my $e    = $2;
		return(sigdig($pree,$places,$pad).$e);
	      }
	    #If there's an exponent and no preceding number
	    elsif($new_num =~ /^([+\-]?)e[+\-]?[0-9\.]+$/i)
	      {
		my $sign = $1;
		my $e    = $2;
		return(sigdig($sign.'1',$places,$pad).$e);
	      }
	    #Pad the number
	    else
	      {
		if($new_num !~ /\./)
		  {$new_num .= '.'}
		$new_num .= '0' x ($pad - $cur_sig_dig);
		return($new_num);
	      }
	  }

	return($new_num);
      }

    #If there's an exponent with a preceding number
    if($num =~ /^([+\-]?[0-9\.]*)(e[+\-]?[0-9\.]+)$/i)
      {
	my $pree = $1;
	my $e    = $2;
	$pree = $pree . '1' if($pree !~ /\d/);
	return(sigdig($pree,$places,$pad).$e);
      }
    elsif($num =~ /[^0-9\.+\-]/ || $num =~ /\..*\./ || $num =~ /.[+\-]/)
      {
	error("Invalid number format: [$num].");
	return($num);
      }
    elsif($num == 0)
      {
	if($pad)
	  {
	    $new_num = '0.';
	    if($new_num !~ /\./)
	      {$new_num .= '.'}
	    $new_num .= '0' x $pad;
	    return($new_num);
	  }
	return(0);
      }

    my $first_real   = 0;
    my $num_added    = 0;
    my $decimal_seen = 0;
    my $last_digit   = 0;
    my $sign         = '';
    foreach my $digit (unpack("(A)*",$num))
      {
        if($digit =~ /\+|-/)
          {
            $sign = $digit;
            next;
          }
        if($digit =~ /[1-9]/)
          {$first_real = 1}
        elsif($digit eq '.')
          {
            $decimal_seen = 1;
            if($new_num eq '')
              {$new_num = '0'}
            if($num_added < $places)
              {$new_num .= '.'}
            elsif($num_added > $places)
              {last}
            next;
          }

        if($first_real)
          {
            if($num_added < $places)
              {
                $new_num .= $digit;
                $num_added++;
              }
            elsif($num_added == $places)
              {
                if($digit >= 5)
                  {
                    #This gets rid of the decimal
                    my $tmp_num = join("",split(/\D*/,$new_num)) + 1;
                    if($new_num =~ /\.(\d*)$/)
                      {
                        my $len = length($1);
                        unless($tmp_num =~ s/(?=\d{$len}\Z)/./)
                          {
                            if($new_num =~ /^(0\.0+)/)
                              {$tmp_num = "$1$tmp_num"}
                          }
                      }
                    $new_num = $tmp_num;
                  }

		#If we haven't gotten to the end of the whole number yet
                if(!$decimal_seen)
                  {
                    $new_num .= '0';
                    $num_added++;
                  }
                else
                  {last}
              }
            elsif(!$decimal_seen)
              {
                $new_num .= '0';
                $num_added++;
              }
            else
              {last}
            $last_digit = $digit;
          }
        elsif($decimal_seen)
          {$new_num .= '0'}
      }

    if($pad)
      {
	my $cur_sig_dig = getsigdig($new_num);
	if($pad > $cur_sig_dig && $new_num !~ /\./)
	  {$new_num .= '.'}
	$new_num .= '0' x ($pad - $cur_sig_dig);
      }
    #Trim the trailing zeros
    elsif($new_num =~ /\./i)
      {
	$new_num =~ s/0+$//;
	$new_num =~ s/\.$//;
      }

    return("$sign$new_num");
  }

sub getSeqFile
  {
    my $seq_hash = $_[0]; #seq_hash->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
    my $use_keys = defined($_[1]) ? $_[1] : [keys(%$seq_hash)];
    my $tmp_file = getTempFilename();

    verbose("Creating temporary files for running usearch.") if($verbose > 1);

    checkFile($tmp_file)          || return('');
    openOut(*TMP,$tmp_file,1,1,0) || return('');
    print(map {">$_\n$seq_hash->{$_}->{SEQ}\n"}
	  grep {exists($seq_hash->{$_}) ? 1 :
		  error("Key: [$_] not found in sequence hash.") && 0}
	  @$use_keys);
    closeOut(*TMP);

    if(!(-e $tmp_file))
      {
	error("Sequence file creation failed - file [$tmp_file] was not ",
	      "created.");
	return(undef);
      }
    elsif(-z $tmp_file)
      {
	error("Sequence file creation failed - file [$tmp_file] is empty.");
	return(undef);
      }

    return($tmp_file);
  }

#Globals used: $tmpdir, $tmp_suffix
sub getTempFilename
  {
    my $local_name_stub  = defined($_[0]) ? $_[0] : time();
    my $local_tmpdir     = (defined($_[1]) ? $_[1] :
			    (defined($tmpdir) ? $tmpdir : ''));
    my $local_tmp_suffix = (defined($_[2]) ? $_[2] :
			    (defined($tmp_suffix) ? $tmp_suffix : ''));
    my $tmp_filename     = $local_name_stub;
    $main::tmpfiles      = {} if(!defined($main::tmpfiles));

    if(defined($local_tmpdir) && $local_tmpdir ne '')
      {
	if(!(-e $local_tmpdir))
	  {
	    error("Temporary directory: [$local_tmpdir] does not exist.");
	    return(undef);
	  }
	elsif(!(-d $local_tmpdir))
	  {
	    error("Temporary directory: [$local_tmpdir] is not a directory.");
	    return(undef);
	  }

	$local_name_stub =~ s/.*\///;
	$tmp_filename =
	  ($local_tmpdir =~ m%/$% ? $local_tmpdir : "$local_tmpdir/") .
	    $local_name_stub;
      }

    my $differentiator = '';

    while(-e "$tmp_filename$differentiator$local_tmp_suffix")
      {$differentiator++}

    $tmp_filename .= $differentiator . $local_tmp_suffix;

    $main::tmpfiles->{$tmp_filename}++;

    return($tmp_filename);
  }

sub cleanTmpFiles
  {
    return() unless(defined($main::tmpfiles));
    foreach my $tmpfile (keys(%$main::tmpfiles))
      {unlink($tmpfile)}
  }

#Globals used: $tmpdir
sub getUDB
  {
    my $seq_file    = $_[0];
    my $tmp_dir     = $tmpdir;
    my $usearch_exe = $usearch;

    if(!defined($seq_file))
      {return(undef)}

    #Create a temporary database file *name*
    my $udb_file = getTempFilename();

    verbose("Creating usearch sequence database.");

    my $cmd =
      "$usearch_exe -makeudb_ublast '$seq_file' -output '$udb_file' -quiet";

    if($verbose > 1)
      {verbose($cmd)}
    else
      {debug($cmd)}

    #Create the database file
    `$usearch_exe -makeudb_ublast '$seq_file' -output '$udb_file' -quiet`;

    if(!(-e $udb_file))
      {
	error("Usearch database creation failed - database file does not ",
	      "exist.");
	return(undef);
      }
    elsif(-z $udb_file)
      {
	error("Usearch database creation failed - database file is empty.");
	return(undef);
      }

    return($udb_file);
  }

sub newHitIsBetter
  {
    my $new_hit = $_[0];
    my $old_hit = $_[1];

    my $new_expect = getHitExpect($new_hit);
    my $old_expect = getHitExpect($old_hit);
    my $new_length = getHitLength($new_hit);
    my $old_length = getHitLength($old_hit);

    if(!defined($new_expect) || !defined($old_expect) ||
       ($new_expect == $old_expect &&
	(!defined($new_length) || !defined($old_length))))
      {
	error("Unable to find hit data in comparison: [",join(',',@$new_hit),
	      "] versus [",join(',',@$old_hit),"].  Returning false.");
	return(0);
      }

    if($new_expect =~ /^e/i)
      {$new_expect = "1$new_expect"}
    if($old_expect =~ /^e/i)
      {$old_expect = "1$old_expect"}

    return($new_expect == $old_expect ?
	   $new_length > $old_length :
	   $new_expect < $old_expect);
  }

#Globals used: $evalue_col
sub getHitExpect
  {
    my $hit_array = $_[0];
    if(scalar(@$hit_array) < $evalue_col)
      {return(undef)}
    return($hit_array->[$evalue_col - 1]);
  }

#Globals used: $query_start, $query_stop
sub getHitLength
  {
    my $hit_array = $_[0];
    if(scalar(@$hit_array) < $query_start_col ||
       scalar(@$hit_array) < $query_stop_col)
      {return(undef)}
    return($hit_array->[$query_stop_col  - 1] -
	   $hit_array->[$query_start_col - 1] + 1);
  }

sub getJobProgress
  {
    my $job_prog_hash = $_[0]; #->{job_num} = {PERCENT=>#,JOBQUERIES=>#}
    my $total_queries = $_[1];

    if($total_queries < 1)
      {
	error("Bad total queries: [$total_queries].");
	return(0);
      }

    my $sum = 0;
    foreach my $job_num (keys(%$job_prog_hash))
      {$sum += $job_prog_hash->{$job_num}->{PERCENT} / 100 *
	 $job_prog_hash->{$job_num}->{JOBQUERIES}}

    if($DEBUG && $sum/$total_queries > 1.2)
      {warning("Job progress calculation seems to be way too high: ",
	       "[$sum/$total_queries].  Number of completed queries should ",
	       "not be higher than the total number of queries.")}

    #Don't let it be higher than 100 due to math inaccuracy
    if($sum > $total_queries)
      {return(100)}

    #Force 2 decimal places
    return(int($sum / $total_queries * 10000) / 100)
  }

#This subroutine performs the usearch ublast.  It uses the requested minimum
#identity to set the coverage requirements, assuming that sequences should
#completely align from end-to-end.  Any returned hits are expanded to full
#length and the identity is tweaked.  Results are filtered using the tweaked
#identity.  It assumes that the requested identity will be high and thus sets a
#static liberal expect value to balance speed and accuracy based on testing
#results.  It sets the coverage requirement based on the min identity.
#Globals used: $usearch, $processes, $usearch_col_str, $usearch_opts,
#$db_manual, $evalue_manual, $accel_manual, $quiet_manual
sub getSimilarSeqPairs
  {
    my $seq_hash       = $_[0]; #seq_hash->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
    my $min_identity   = $_[1];
    my $usearch_params = defined($usearch_opts) ? $usearch_opts : '';
    my $usearch_exe    = $usearch;
    my $expect         = 10;
    my $accel          = 0.6;
    my $strand         = 'plus';
    my $query_cov      = $min_identity;
    my $target_cov     = $min_identity;
    my $fields         = $usearch_col_str;
    my $seq_pair_hash  = {};

    verbose("Determining sequence-similar pairs.");

    my $query_file = getSeqFile($seq_hash);
    if(!defined($query_file))
      {return($seq_pair_hash)}

    my($udb);
    unless($db_manual)
      {
	$udb = getUDB($query_file);
	if(!defined($udb))
	  {return($seq_pair_hash)}
      }

    my $outfile = getTempFilename();

    eval('use IO::Pipe::Producer;1;') ||
      eval('use local::lib;use IO::Pipe::Producer;1;') ||
	error('Perl Module [IO::Pipe::Producer] not found.') ||
	  quit(16);
    eval('use IO::Select;1;') ||
      eval('use local::lib;use IO::Select;1;') ||
	error('Perl Module [IO::Select] not found.') ||
	  quit(17);

    my $obj = new IO::Pipe::Producer();

    my $usearch_command = $usearch_exe .
      " -ublast $query_file" .
	($db_manual ? '' : " -db $udb") .
	  " -userout $outfile" .
	    ($evalue_manual ? '' : " -evalue $expect") .
	      " -id $min_identity" .
		($accel_manual ? '' : " -accel $accel") .
		  " -strand $strand" .
		    " -threads $processes" .
		      " -userfields $fields" .
			" -query_cov $query_cov -target_cov $target_cov" .
			  ($usearch_params =~ /\S/ ? " $usearch_params" : '') .
			    ($verbose || $quiet_manual ? '' : ' -quiet');

    if($verbose > 1)
      {verbose("COMMAND: ",$usearch_command)}
    else
      {debug("COMMAND: ",$usearch_command)}

    #Run the usearch command
    my($stdout,$stderr) =
      $obj->getSystemProducer($usearch_command);

    debug("Usearch ublast started");

    my $status_msg = '';

    #Use a Select opject to determine which handles are ready to be read
    my $sel = new IO::Select;
    $sel->add($stderr,$stdout);

    #While we have a file handle with output
    while(my @fhs = $sel->can_read())
      {
	#For each file handle with output
	foreach my $fh (@fhs)
	  {
	    my $line;
	    if($fh eq $stderr)
	      {
		#Doing this to get ongoing status of usearch
		local $/ = "\r";
		$line = <$fh>;
	      }
	    elsif($fh eq $stdout)
	      {
		local $/ = "\n";
		$line = <$fh>;
	      }
	    else
	      {
		error("Unrecognized file handle.");
		next;
	      }

	    #If we hit the end of the file, remove the handle & continue
	    unless(defined($line))
	      {
		$sel->remove($fh);
		next;
	      }

	    #Put output on the correct stream based on the current handle
	    if($fh eq $stdout)
	      {
		if($verbose > 1)
		  {
		    verbose($line);
		    verboseOverMe($status_msg);
		  }
	      }
	    elsif($fh eq $stderr)
	      {
		##
		## Skip all these header lines except the last one which
		## shows progress
		##

		#usearch v7.0.1090_i86osx32, 4.0Gb RAM (17.2Gb total), 8...
		#(C) Copyright 2013 Robert C. Edgar, all rights reserved.
		#http://drive5.com/usearch
		#
		#Licensed to: rleach@princeton.edu
		#
		#00:00 5.0Mb Reading /Users/rleach/PROJECT/DENOISE/20140...
		#00:00  21Mb Database loaded
		#^C:37  28Mb    3.3% Searching, 100.0% matched

		$line =~ s/[\r\n]//g;
		if($line !~ /^usearch\sv|drive5\.com|Copyright|^\s*$|
			     ^Licensed to|Reading |Database loaded/x)
		  {
		    $status_msg = $line;
		    verboseOverMe($status_msg);
		  }
		elsif($verbose > 1)
		  {
		    verbose($line);
		    verboseOverMe($status_msg);
		  }
	      }
	    else
	      {error("Unable to determine file handle.")}
	  }
      }

    close($stdout);
    close($stderr);

    debug("Usearch ublast done");

    #Now we can filter & print the ublast output
    openIn(*IN,$outfile,1);
    while(my $line = getLine(*IN))
      {
	chomp($line);
	if($line =~ /\t/)
	  {
	    my $data       = [split(/\t/,$line,-1)];
	    my $query_id   = $data->[$query_col - 1];
	    my $subject_id = $data->[$subject_col - 1];

	    debug("Evaluating query/subject [$query_id/$subject_id] hit: [",
		  join(' ',@$data),"].") if($DEBUG > 5);

	    #Skip hits to self
	    next if($query_id eq $subject_id);

	    debug("Not hit to self.") if($DEBUG > 5);

	    #Always order the IDs by decreasing total abundance, then
	    #increasing ascii ID so that reciprocal duplicates are merged
	    #This is done here because the 2 IDs involved in the hit may
	    #have been in different query sets and thus separate child
	    #processes.
	    my($first,$second) =
	      sort {$seq_hash->{$b}->{ABUNDSUM} <=>
		      $seq_hash->{$a}->{ABUNDSUM} || $a cmp $b}
		($query_id,$subject_id);

	    my $identity = getUblastIdentity($data,$seq_hash);

	    #getUblastIdentity emits an error if $identity is not defined
	    next if(!defined($identity));

	    debug("Indentity is defined.") if($DEBUG > 5);

	    if($line =~ /\S/ &&
	       $identity >= $min_identity &&
	       (!exists($seq_pair_hash->{$first}) ||
		!exists($seq_pair_hash->{$first}->{$second}) ||
		!defined($seq_pair_hash->{$first}->{$second}) ||
		ref($seq_pair_hash->{$first}->{$second}->{HIT}) ne 'ARRAY' ||
		scalar(@{$seq_pair_hash->{$first}->{$second}->{HIT}}) == 0 ||
		newHitIsBetter($data,
			       $seq_pair_hash->{$first}->{$second}->{HIT})))
	      {
		debug("Hit is good.") if($DEBUG > 5);

		$seq_pair_hash->{$first}->{$second}->{HIT}      = $data;
		$seq_pair_hash->{$first}->{$second}->{IDENTITY} = $identity;
	      }

	    debug("Hit eval. done.") if($DEBUG > 5);
	  }
	elsif($line !~ /^usearch|^\s*$/)
	  {error("Could not parse usearch output line: [$line].")}
      }
    closeIn(*IN);

    if(scalar(keys(%{$seq_pair_hash})) == 0)
      {error("No sequence pairs were found to meet the minimum sequence ",
	     "identity cutoff.  You may need to either reduce your identity ",
	     "cutoff (see -n/--min-seq-similarity) or adjust some of the ",
	     "usearch parameters (see --usearch-opts-str), particularly the ",
	     "-evalue parameter.")}

    return($seq_pair_hash);
  }

#Globals used: $query_col,$subject_col,$identity_col,$query_len_col,
#              $query_start_col,$query_stop_col,$subject_len_col,
#              $subject_start_col,$subject_stop_col
sub getUblastIdentity
  {
    my $data     = $_[0];
    my $seq_hash = $_[1];

    if(ref($data) ne 'ARRAY' || scalar(@$data) == 0)
      {
	error("Invalid data array sent in.");
	return(undef);
      }

    my $max_col =
      (sort {$a <=> $b} grep {defined($_) && $_ > 0}
       ($query_col,$subject_col,$identity_col,$query_len_col,$query_start_col,
	$query_stop_col,$subject_len_col,$subject_start_col,
	$subject_stop_col))[0];

    if(scalar(@$data) < $max_col)
      {
	error("Supplied array doesn't have enough columns: [",scalar(@$data),
	      "] instead of the required [$max_col].  Skipping hit.");
	return(undef);
      }

    my $iden   = $data->[$identity_col      - 1] / 100;
    my $q      = $data->[$query_col         - 1];
    my $qlen   = $data->[$query_len_col     - 1];
    my $qstart = $data->[$query_start_col   - 1];
    my $qstop  = $data->[$query_stop_col    - 1];

    if($qstart == 1 && $qstop == $qlen)
      {return($iden)}

    my $t      = $data->[$subject_col       - 1];
    my $tlen   = $data->[$subject_len_col   - 1];
    my $tstart = $data->[$subject_start_col - 1];
    my $tstop  = $data->[$subject_stop_col  - 1];

    if(!exists($seq_hash->{$q}))
      {
	error("ID: [$q] could not be found in the sequence hash.");
	return(undef);
      }
    elsif(!exists($seq_hash->{$t}))
      {
	error("ID: [$t] could not be found in the sequence hash.");
	return(undef);
      }

    #I will base the length matched on the query
    my($new_qstart,$new_qstop,$new_tstart,$new_tstop) =
      extendToQueryLength($qlen,$tlen,$qstart,$qstop,'+',$tstart,$tstop,'+');

    my $total_iden = $iden * ($qstop - $qstart + 1) / $qlen;

    my $left_iden = 0;
    my $left_len  = $qstart - $new_qstart;
    if($left_len)
      {
	my $left_qstr = substr($seq_hash->{$q}->{SEQ},
			       ($new_qstart - 1),
			       $left_len);
	my $left_tstr = substr($seq_hash->{$t}->{SEQ},
			       ($new_tstart - 1),
			       $left_len);
	$left_iden = getIdentity($left_qstr,$left_tstr);

	$total_iden += $left_iden * $left_len / $qlen;
      }

    my $right_iden = 0;
    my $right_len  = $new_qstop - $qstop;
    if($right_len)
      {
	my $right_qstr = substr($seq_hash->{$q}->{SEQ},
				$qstop,
				$right_len);
	my $right_tstr = substr($seq_hash->{$t}->{SEQ},
				$tstop,
				$right_len);
	$right_iden = getIdentity($right_qstr,$right_tstr);

	$total_iden += $right_iden * $right_len / $qlen;
      }

    if($total_iden > 1)
      {error("Bad Identity: [$total_iden] from right side.  Hit: [",
	     join(',',@$data),"].")}

    if($iden != $total_iden)
      {debug("Changing identity of [$q] vs. [$t] hit from [$iden] to ",
	     "[$total_iden].  Left Iden: [$left_iden] left length: ",
	     "[$left_len].  Right Iden: [$right_iden] Right length: ",
	     "[$right_len].  Length of hit: [$qstop - $qstart + 1]  ",
	     "Total length: [$qlen]");}

    return($total_iden);
  }

#Copied getMismatches from motifBruteSearch.pl and edited it on 9/17/2014
sub getIdentity
  {
    my $seq1          = $_[0];
    my $seq2          = $_[1];
    my $len1          = length($seq1);
    my $len2          = length($seq2);
    my $mismatches    = 0;
    my $ambig_matches = 0;

    if($len1 != $len2)
      {
	error("Sequences sent in are not the same length [$len1 != $len2].");
	return(0);
      }

    if($len1 == 0)
      {return(0)}

    #We're going to compare the shortest common length anyway
    my $short_len = $len1 < $len2 ? $len1 : $len2;

    foreach my $p (0..($short_len - 1))
      {
	my $c1 = substr($seq1,$p,1);
	my $c2 = substr($seq2,$p,1);

	my $code = iupacMatch($c1,$c2);

	if($code < 0)
	  {$mismatches++}
	elsif($code == 0)
	  {$ambig_matches++}
      }

    return(($len1 - $mismatches) / $len1);
  }

sub iupacMatch
  {
    my $c1 = uc($_[0]);
    my $c2 = uc($_[1]);

    if($c1 =~ /[ATGC]/ && $c1 eq $c2)               #Discrete match
      {return(1)}
    elsif($c1 eq $c2 || $c1 eq 'N' || $c2 eq 'N')   #Easy ambig match
      {return(0)}
    elsif($c1 =~ /[ATGC]/ && $c2 =~ /[ATGC]/)       #Discrete mismatch
      {return(-1)}

    #One or both here below is an ambiguous
    #character (and neither is N).  The following
    #should take care of all potential ambiguous
    #matches.  There's probably a more efficient
    #way to do this...
    elsif(($c1 eq 'A'      && $c2 =~ /[DHVRMW]/) || #One is non-ambig
	  ($c1 eq 'T'      && $c2 =~ /[BDHYKW]/) ||
	  ($c1 eq 'G'      && $c2 =~ /[BDVRKS]/) ||
	  ($c1 eq 'C'      && $c2 =~ /[BHVYMS]/) ||
	  ($c2 eq 'A'      && $c1 =~ /[DHVRMW]/) ||
	  ($c2 eq 'T'      && $c1 =~ /[BDHYKW]/) ||
	  ($c2 eq 'G'      && $c1 =~ /[BDVRKS]/) ||
	  ($c2 eq 'C'      && $c1 =~ /[BHVYMS]/) ||

	  ($c1 =~ /[BDHV]/ && $c2 !~ /[ATGC]/)   || #1 is BDHV and not ATGC
	  ($c2 =~ /[BDHV]/ && $c1 !~ /[ATGC]/)   || #(BDHV match all ambigs)

	  ($c1 =~ /[RY]/   && $c2 =~ /[KMSW]/)   || #1 is RY (1 being SW or KM
	  ($c2 =~ /[RY]/   && $c1 =~ /[KMSW]/)   || #is partially addressed)

	  ($c1 =~ /[SW]/   && $c2 =~ /[KM]/)     || #One is S or W (addresses
	  ($c2 =~ /[SW]/   && $c1 =~ /[KM]/))       #remainder of above)
      {return(0)}

    #Else it's a discrete mismatch involving ambiguous characters (eg S and W)

    return(-1);
  }

sub extendToQueryLength
  {
    my $query_len      = $_[0];
    my $subject_len    = $_[1];
    my $query_start    = $_[2];
    my $query_stop     = $_[3];
    my $query_strand   = $_[4];
    my $subject_start  = $_[5];
    my $subject_stop   = $_[6];
    my $subject_strand = $_[7];

    #If there was no query length supplied, set it to the max coord
    $query_len = ($query_len ? $query_len :
		  ($query_start > $query_stop ?
		   $query_start : $query_stop));
    $subject_len = ($subject_len ? $subject_len :
		    ($subject_start > $subject_stop ?
		     $subject_start : $subject_stop));

    #Error check
    if($query_len < $query_start || $query_len < $query_stop)
      {
	error("Query coordinates larger than query length.");
	return(undef,undef,undef,undef);
      }
    if($subject_len < $subject_start || $subject_len < $subject_stop)
      {
	error("Subject coordinates larger than subject length.");
	return(undef,undef,undef,undef);
      }

    my $start_diff      = 0;
    my $stop_diff       = 0;
    my $new_query_start = 0;
    my $new_query_stop  = 0;
    if($query_start < $query_stop)
      {
	debug("1: New query stop: $new_query_stop New subject stop: undef")
	  if($DEBUG > 3);
	$start_diff      = $query_start - 1;
	$stop_diff       = $query_len   - $query_stop;
	$new_query_start = 1;
	$new_query_stop  = $query_len;
      }
    else
      {
	debug("2: New query stop: $new_query_stop New subject stop: undef")
	  if($DEBUG > 3);
	$start_diff      = $query_len  - $query_start;
	$stop_diff       = $query_stop - 1;
	$new_query_start = $query_len;
	$new_query_stop  = 1;
      }

    my $new_subject_start = 0;
    my $new_subject_stop  = 0;
    if($subject_start < $subject_stop)
      {
	debug("3: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$new_subject_start = $subject_start - $start_diff;
	$new_subject_stop  = $subject_stop  + $stop_diff;
      }
    else
      {
	debug("4: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$new_subject_start = $subject_start + $start_diff;
	$new_subject_stop  = $subject_stop  - $stop_diff;
      }

    debug("Query/subject start/stop from [$query_start,$query_stop,",
	  "$subject_start,$subject_stop] to [$new_query_start,",
	  "$new_query_stop,$new_subject_start,$new_subject_stop] before ",
	  "adjustment.") if($DEBUG > 3);

    my $adjust = 0;
    #Check to see if we've exceeded the bounds of the subject sequence
    if($new_subject_start < 1)
      {
	debug("5: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$adjust = 1 - $new_subject_start;
	$new_subject_start = 1;
      }
    elsif($new_subject_start > $subject_len)
      {
	debug("6: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$adjust = $subject_len - $new_subject_start;
	$new_subject_start = $subject_len;
      }

    if($adjust)
      {
	if($query_start < $query_stop)
	  {$new_query_start += $adjust}
	else
	  {$new_query_start -= $adjust}
	debug("7: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
      }

    $adjust = 0;
    if($new_subject_stop < 1)
      {
	debug("8: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$adjust = 1 - $new_subject_stop;
	$new_subject_stop = 1;
      }
    elsif($new_subject_stop > $subject_len)
      {
	debug("9: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
	$adjust = $subject_len - $new_subject_stop;
	$new_subject_stop = $subject_len;
      }

    if($adjust)
      {
	if($query_start < $query_stop)
	  {$new_query_stop += $adjust}
	else
	  {$new_query_stop -= $adjust}
	debug("10: New query stop: $new_query_stop New subject stop: ",
	      "$new_subject_stop") if($DEBUG > 3);
      }

    debug("Changing query/subject start/stop from [$query_start,$query_stop,",
	  "$subject_start,$subject_stop] to [$new_query_start,",
	  "$new_query_stop,$new_subject_start,$new_subject_stop].")
      if($DEBUG > 3);

    return($new_query_start,  $new_query_stop,
	   $new_subject_start,$new_subject_stop);
  }

sub dynamicalSimilarityCorrelation
  {
    my $greater_abundances = $_[0];
    my $lesser_abundances  = $_[1];
    my $local_quiet        = $_[2];

    my $greater_sum = 0;
    $greater_sum += $_ foreach(@$greater_abundances);
    my $greater_average = $greater_sum / scalar(@$greater_abundances);

    my $lesser_sum = 0;
    $lesser_sum += $_ foreach(@$lesser_abundances);
    my $lesser_average = $lesser_sum / scalar(@$lesser_abundances);

    my $mock_lesser_abundances =
      [map {$_ * $lesser_average / $greater_average} @$greater_abundances];

    #Create 2D arrays of sampled abundances (higher abundances being more
    #likely to be selected than lower ones)
    my $greater_resampled_abundances = [];
    my $lesser_resampled_abundances  = [];

    foreach(0..9)
      {
        push(@$greater_resampled_abundances,
	     poissonResample($greater_abundances));
        push(@$lesser_resampled_abundances,
	     poissonResample($mock_lesser_abundances));
      }

    my $correlation_sum = 0;

    foreach my $i (0..9)
      {
        #Do not add anything if the resampling is all 0s
        foreach my $j (0..9)
          {
	    my($r,$p) = getPearson($greater_resampled_abundances->[$i],
				   $lesser_resampled_abundances->[$j],
				   $local_quiet);
	    $correlation_sum += $r if(defined($r));
          }
      }

    my $correlation_max = $correlation_sum / 100;

    my $dynamical_similarity = 0;
    if($correlation_max)
      {
        my($r,$p) = getPearson($greater_abundances,
			       $lesser_abundances,$local_quiet);
        $dynamical_similarity = $r / $correlation_max;
      }

    return($dynamical_similarity);
  }

sub poissonResample
  {
    eval('use Math::Random qw(:all);1;') ||
      eval('use local::lib;use Math::Random qw(:all);1;') ||
	error("Perl module [Math::Random] not found.") ||
	  quit(18);
    my $not_all_zeroes = [map {random_poisson(1, $_)} @{$_[0]}];
    while(!scalar(grep {$_} @$not_all_zeroes))
      {$not_all_zeroes = [map {random_poisson(1, $_)} @{$_[0]}]}
    return($not_all_zeroes);
  }

sub getPearson
  {
    my $targets       = [grep {$_ ne ''} @{$_[0]}];
    my $non_targets   = [grep {$_ ne ''} @{$_[1]}];
    my $local_quiet   = $_[2];
    my($corr_coef,$p_val);

    debug("Called with Targets Values: [@$targets] and Non-Target Values: ",
	  "[@$non_targets].") if($DEBUG > 99);

    if(scalar(@$targets) != scalar(@$non_targets))
      {
	error("The number of targets [",scalar(@$targets),
	      "] and non-targets [",scalar(@$non_targets),
	      "] must be the same.");
      }
    elsif(scalar(@$targets) < 6 && !$force)
      {
	warning("Too little data ([",scalar(@$targets),"] target values and [",
		scalar(@$non_targets),"] non-target values) to calculate ",
		"correlation statistics.  Need at least 6.");
      }
    else
      {
	eval("use Statistics::Distributions;1;") ||
	  eval("use local::lib;use Statistics::Distributions;1;") ||
	    error("Perl module [Statistics::Distributions] not found.") ||
	      quit(19);

	my $adjusted_targets     = [];
	my $adjusted_non_targets = [];

	my $lowest_val = (sort {$a <=> $b} (@$targets,@$non_targets))[0];

	if($lowest_val < 0)
	  {
	    my $add = 0;
	    if(scalar(grep {$_ == $lowest_val} @$targets) ==
	       scalar(@$targets) ||
		 scalar(grep {$_ == $lowest_val} @$non_targets) ==
		   scalar(@$non_targets))
	      {$add = 0.5}
	    @$adjusted_targets = map {$_ - $lowest_val + $add} @$targets;
	    @$adjusted_non_targets = map {$_ - $lowest_val + $add}
	      @$non_targets;
	  }
	elsif(scalar(grep {$_ > 0} @$targets) == 0 ||
	      scalar(grep {$_ > 0} @$non_targets) == 0)
	  {
	    @$adjusted_targets = map {$_ + 0.5} @$targets;
	    @$adjusted_non_targets = map {$_ + 0.5} @$non_targets;
	  }
	else
	  {
	    @$adjusted_targets = @$targets;
	    @$adjusted_non_targets = @$non_targets;
	  }

	#Calculate all the sums needed to calculate the correlation coef.
	#http://www.socialresearchmethods.net/kb/statcorr.php
	my $num_samples        = scalar(@$targets);
	my $targ_sum           = 0;
	my $nontarg_sum        = 0;
	my $mult_sum           = 0;
	my $targ_square_sum    = 0;
	my $nontarg_square_sum = 0;
	foreach my $i (0..$#$adjusted_targets)
	  {
	    $targ_sum           += $adjusted_targets->[$i];
	    $nontarg_sum        += $adjusted_non_targets->[$i];
	    $mult_sum           += ($adjusted_targets->[$i] *
				    $adjusted_non_targets->[$i]);
	    $targ_square_sum    += $adjusted_targets->[$i]**2;
	    $nontarg_square_sum += $adjusted_non_targets->[$i]**2;
	  }

	my $denom_sqr = ($num_samples * $targ_square_sum - $targ_sum**2) *
	  ($num_samples * $nontarg_square_sum - $nontarg_sum**2);
	if($denom_sqr < 0)
	  {
	    warning("Got a negative number for the denominator: ",
		    "[(($num_samples * $mult_sum) - ($targ_sum * ",
		    "$nontarg_sum)) / (($num_samples * $targ_square_sum - ",
		    "$targ_sum**2) * ($num_samples * $nontarg_square_sum - ",
		    "$nontarg_sum**2))] using equation: [((num_samples * ",
		    "mult_sum) - (targ_sum * nontarg_sum)) / ((num_samples * ",
		    "targ_square_sum - targ_sum**2) * (num_samples * ",
		    "nontarg_square_sum - nontarg_sum**2))] of the pearson ",
		    "correlation coefficient calculation from target values: ",
		    "[",join(', ',@$targets),"] and non-target values: [",
		    join(', ',@$non_targets),"].") unless($local_quiet);
	    return(undef,undef);
	  }
	elsif($denom_sqr == 0)
	  {
	    warning("Got a 0 for the denominator: [(($num_samples * ",
		    "$mult_sum) - ($targ_sum * $nontarg_sum)) / ",
		    "(($num_samples * $targ_square_sum - $targ_sum**2) * ",
		    "($num_samples * $nontarg_square_sum - ",
		    "$nontarg_sum**2))] using equation: [((num_samples * ",
		    "mult_sum) - (targ_sum * nontarg_sum)) / ((num_samples * ",
		    "targ_square_sum - targ_sum**2) * (num_samples * ",
		    "nontarg_square_sum - nontarg_sum**2))] of the pearson ",
		    "correlation coefficient calculation from target values: ",
		    "[",join(', ',@$targets),"] and non-target values: [",
		    join(', ',@$non_targets),"].") unless($local_quiet);
	    return(undef,undef);
	  }

	$corr_coef = (($num_samples * $mult_sum) - ($targ_sum * $nontarg_sum))
	  / sqrt($denom_sqr);

#Commented out for now - we're not reporting P-Val (yet)

#	my $degrees_of_freedom = $num_samples - 2;
#
#	#Check the denominator
#	if(abs($corr_coef) >= 1)
#	  {
#	    warning("Got a 0 (or less) for the p-value's denominator: ",
#		    "[sqrt($degrees_of_freedom / (1 - $corr_coef**2))] using ",
#		    "equation: [sqrt(degrees_of_freedom / (1 -",
#		    " corr_coef**2))] of the student t value calculation ",
#		    "from target values: [",join(', ',@$targets),"] and non-",
#		    "target values: [",join(', ',@$non_targets),"].")
#              unless($local_quiet);
#	    return((abs($corr_coef) == 1 ? $corr_coef : undef),undef);
#	  }
#
#	my $student_t = sqrt($degrees_of_freedom / (1 - $corr_coef**2));
#
#	$p_val = Statistics::Distributions::tprob($degrees_of_freedom,
#						  $student_t);
      }

    return($corr_coef,$p_val);
  }

#Assumes that the key order in the seq_pairs_hash is in order of decreasing
#abundance
#Returns abund_pair_hash: ->{ID1}->{ID2} = {hash of info about pair}
sub getDissimilarAbundPairs
  {
    my $seq_pair_hash   = $_[0]; #$seq_pair_hash ->{ID1}->{ID2}->{HIT} = [info]
    my $seq_hash        = $_[1]; #$seq_hash->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
    my $max_dynsim      = $_[2];
    my $abund_pair_hash = {};

    return($abund_pair_hash) if(scalar(keys(%$seq_pair_hash)) == 0);

    foreach my $greater_id (keys(%$seq_pair_hash))
      {
	foreach my $lesser_id (keys(%{$seq_pair_hash->{$greater_id}}))
	  {
	    my $dyn_sim =
	      dynamicalSimilarityCorrelation($seq_hash->{$greater_id}
					     ->{ABUNDS},
					     $seq_hash->{$lesser_id}
					     ->{ABUNDS});
	    if(!defined($dyn_sim))
	      {
		error("Could not determine dynamical similarity.");
		next;
	      }

	    if($dyn_sim <= $max_dynsim)
	      {
		debug("$greater_id\t$lesser_id\t$dyn_sim");
		$abund_pair_hash->{$greater_id}->{$lesser_id} = $dyn_sim;
	      }
	  }
      }

    if(scalar(keys(%$abund_pair_hash)) == 0)
      {error("No dissimilar abundance pairs were found to meet the maximum ",
	     "dynamical similarity cutoff (-x/--max-dynamical-similarity: ",
	     "[$max_dynsim]).  Raise this cutoff and try again.")}

    return($abund_pair_hash);
  }

#Globals used: $header, $sigdig, $usearch_col_str, $min_mean_abund,
#$max_dynsim, $min_seqsim
sub reportPairs
  {
    my $abund_pair_hash      = $_[0];
    my $seq_pair_hash        = $_[1];
    my $seq_hash             = $_[2]; #->{ID}={ABUNDS=[],SEQ=str,ABUNDSUM}
    my $abunds_hash          = $_[3];
    my $local_usearch        = (defined($_[4]) ? $_[4] :
				(defined($append_usearch) ?
				 $append_usearch : 0));
    my $local_abunds         = (defined($_[5]) ? $_[5] :
				(defined($append_abunds) ?
				 $append_abunds : 0));
    my $local_min_mean_abund = $_[6];

    my $abund_cols = 0;
    my @pairs = ();
    foreach my $greater_id (keys(%$abund_pair_hash))
      {
	foreach my $lesser_id (keys(%{$abund_pair_hash->{$greater_id}}))
	  {
	    push(@pairs,[$greater_id,
			 $lesser_id,
			 putZero(sigdig($abund_pair_hash->{$greater_id}
					->{$lesser_id},$sigdig)),
			 putZero(sigdig($seq_pair_hash->{$greater_id}
					->{$lesser_id}->{IDENTITY},$sigdig)),
			 $seq_hash->{$greater_id}->{ABUNDSUM},
			 $seq_hash->{$lesser_id}->{ABUNDSUM}]);

	    if($local_usearch)
	      {push(@{$pairs[-1]},
		    @{$seq_pair_hash->{$greater_id}->{$lesser_id}->{HIT}})}

	    if($local_abunds)
	      {
		$abund_cols = scalar(@{$seq_hash->{$greater_id}->{ABUNDS}});
		push(@{$pairs[-1]},@{$seq_hash->{$greater_id}->{ABUNDS}});
		push(@{$pairs[-1]},@{$seq_hash->{$lesser_id}->{ABUNDS}});
	      }

	    if($DEBUG)
	      {debug(join("\t",($greater_id,
				$lesser_id,
				sigdig($abund_pair_hash->{$greater_id}
				       ->{$lesser_id},$sigdig),
				sigdig($seq_pair_hash->{$greater_id}
				       ->{$lesser_id}->{IDENTITY},$sigdig),
				@{$seq_pair_hash->{$greater_id}->{$lesser_id}
				    ->{HIT}},
				@{$seq_hash->{$greater_id}->{ABUNDS}},
				@{$seq_hash->{$lesser_id}->{ABUNDS}})))}
	  }
      }

    if($header)
      {
	print("#",join("\t",('Greater-Abundance-ID',
			     'Lesser-Abundance-ID',
			     'Dynamical-Similarity',
			     'Sequence-Similarity',
			     'Greater-Total-Abundance',
			     'Lesser-Total-Abundance')));

	if($local_usearch)
	  {print("\tusearch[",join("\t",split(/\+/,$usearch_col_str)),']')}

	if($local_abunds)
	  {
	    $abund_cols--;
	    print("\tGreater-Sample-Abundances[",
		  join("\t",(exists($abunds_hash->{_HEADER_}) ?
			     @{$abunds_hash->{_HEADER_}} : (1..$abund_cols))),
		  "]\tLesser-Sample-Abundances[",
		  join("\t",(exists($abunds_hash->{_HEADER_}) ?
			     @{$abunds_hash->{_HEADER_}} : (1..$abund_cols))),
		  ']');
	  }

	print("\n");
      }

    if(scalar(@pairs))
      {print(join("\n",
		  map {join("\t",@$_)}
		  sort {#Order by...
		    #Descending abundance of the lesser abundant seq
		    $b->[5] <=> $a->[5] ||
		      #descending identity
		      $b->[3] <=> $a->[3] ||
			#Descending abundance of the greater abundant seq
			$b->[4] <=> $a->[4] ||
			  #Ascending dynamical similarity
			  $a->[2] <=> $b->[2] ||
			    #greater ID
			    $a->[0] cmp $b->[0] ||
			      #lesser ID
			      $a->[1] cmp $b->[1]} @pairs),
	     "\n")}
    else
      {warning("No pairs that meet all the cutoffs were found.  Please ",
	       "adjust the cutoffs and re-try.  Current cutoff thresholds: ",
	       "-a/--min-mean-abundance: [$local_min_mean_abund], ",
	       "-x/--max-dynamical-similarity: [$max_dynsim], ",
	       "-n/--min-seq-similarity: [$min_seqsim].")}
  }

#Globals used: $force, $header
sub getAbundanceTraces
  {
    my $abunds_file = $_[0];
    my $abunds_hash = {};
    my $line_num    = 0;
    my $num_cols    = 0;
    my @bad_rows    = ();

    openIn(*ABUNDS,$abunds_file);
    while(my $line = getLine(*ABUNDS))
      {
	$line_num++;
	verboseOverMe("Reading abundance file line [$line_num].");

	chomp($line);

	if($header && $line =~ /^\s*#/ && $line =~ /\t/)
	  {
	    my $col_headers = [split(/ *\t */,$line,-1)];
	    shift(@$col_headers);

	    #If this line has more tabs or we haven't set a header yet
	    if((exists($abunds_hash->{_HEADER_}) &&
		scalar(@{$abunds_hash->{_HEADER_}}) < scalar(@$col_headers)) ||
	       !exists($abunds_hash->{_HEADER_}))
	      {$abunds_hash->{_HEADER_} = $col_headers}
	  }

	next if($line =~ /^\s*$/ || $line =~ /^\s*#/);
	$line =~ s/^ +//;
	$line =~ s/ +$//;

	my $data = [map {/./ ? $_ : 0} split(/ *\t */,$line,-1)];
	my $id = shift(@$data);

	my @nonnums = grep {/\D/} @$data;
	if(scalar(@nonnums))
	  {
	    error("Non-numeric abundance value(s) found: [@nonnums] on line ",
		  "[$line_num] in abundance file (-s): [$abunds_file].");
	    closeIn(*ABUNDS);
	    return({});
	  }

	if($num_cols == 0)
	  {$num_cols = scalar(@$data)}
	elsif(scalar(@$data) != $num_cols)
	  {
	    if($force)
	      {$num_cols = scalar(@$data) if(scalar(@$data) > $num_cols)}
	    else
	      {push(@bad_rows,$line_num)}
	  }

	my $sum = 0;
	$sum += $_ foreach(@$data);

	$abunds_hash->{$id}->{ABUNDS}   = $data;
	$abunds_hash->{$id}->{ABUNDSUM} = $sum;
      }

    closeIn(*ABUNDS);

    if(scalar(@bad_rows))
      {
	error("The abundance trace file: [$abunds_file] has lines [",
	      join(',',@bad_rows),"] containing a different number of ",
	      "columns than the first line has [$num_cols].  Use --force to ",
	      "fill empty trailing columns with zeros up to match the line ",
	      "with the most columns.");
	return({});
      }

    if(!$force && $num_cols < 6)
      {
	if($force)
	  {warning("Not enough abundance columns in abundance file ",
		   "[$abunds_file].  Need at least 6 in order to calculate ",
		   "dynamical similarity.  Forcing.")}
	else
	  {
	    error("Not enough abundance columns in abundance file ",
		  "[$abunds_file].  Need at least 6 in order to calculate ",
		  "dynamical similarity.  Use --force to try anyway.");
	    return({});
	  }
      }

    #Fill the short rows with 0s up to the length of the row with the most
    #columns - this only happens if @bad_rows is empty, which is only empty if
    #--force is supplied and the number of columns is different in at least 1
    #row.
    if($force)
      {
	foreach my $id (keys(%{$abunds_hash}))
	  {
	    if(scalar(@{$abunds_hash->{$id}->{ABUNDS}}) != $num_cols)
	      {push(@{$abunds_hash->{$id}->{ABUNDS}},
		    ((0) x ($num_cols -
			    scalar(@{$abunds_hash->{$id}->{ABUNDS}}))))}
	  }
      }

    if(scalar(keys(%$abunds_hash)) == 0)
      {error("No abundance traces were successfully parsed from abundance ",
	     "file [$abunds_file].")}

    #If this line has more tabs or we haven't set a header yet
    if($header &&
       ((exists($abunds_hash->{_HEADER_}) &&
	 scalar(@{$abunds_hash->{_HEADER_}}) < $num_cols) ||
	!exists($abunds_hash->{_HEADER_})))
      {
	#If a header was parsed and we reasonably think it was supposed to be
	#headers for the columns (i.e. there was more than 1 column header)
	if(exists($abunds_hash->{_HEADER_}) &&
	   scalar(@{$abunds_hash->{_HEADER_}}) > 1)
	  {warning("Incorrect number of column headers [",
		   scalar(@{$abunds_hash->{_HEADER_}}),"] parsed from the ",
		   "abundance trace file [$abunds_file]: [",
		   join("\t",@{$abunds_hash->{_HEADER_}}),
		   "].  Generating sequential numeric headers.")}

	$abunds_hash->{_HEADER_} = [1..$num_cols];
      }

    if(scalar(keys(%$abunds_hash)) == 0 || (scalar(keys(%$abunds_hash)) == 1 &&
					    exists($abunds_hash->{_HEADER_})))
      {error("No abundances were parsed from abundance file [$abunds_file].")}

    return($abunds_hash);
  }

#This subroutine will prepend (or insert) a 0 to a number beginning with a '.'
#Does not work on exponent numbers (e.g. 1s^.5)
sub putZero
  {
    my $num = $_[0];
    if($num =~ /^-?\./)
      {$num =~ s/\./0./}
    return($num);
  }
