#!/usr/bin/perl
#Note: 'use warnings' is below instead of having -w above

#Generated using perl_script_template.pl 2.19
#Robert W. Leach
#Princeton University
#Carl Icahn Laboratory
#Lewis Sigler Institute for Integrative Genomics
#Bioinformatics Group
#Room 133A
#Princeton, NJ 08544
#rleach@genomics.princeton.edu
#Copyright 2014

#These variables (in main) are used by getVersion() and usage()
my $software_version_number = '1.4';
my $created_on_date         = '5/19/2014';

##
## Start Main
##

use warnings; #Same as using the -w, only just for code in this script
use strict;
use Getopt::Long qw(GetOptionsFromArray :config no_auto_abbrev);
use File::Glob ':glob';

#This will allow us to track runtime warnings about undefined variables, etc.
local $SIG{__WARN__} = sub {my $err = $_[0];chomp($err);
			    warning("Runtime warning: [$err].")};

#Declare & initialize variables.  Provide default values here.
my($fakes_suffix);
my $chime_aln_suffix  = '.chim-alns';
my $reals_suffix      = '.reals';
my $sum_suffix        = '.smry';
my $chime_suffix      = '.chim';
my $input_files       = [];
my $output_files      = [];
my $drp_files         = [];
my $library_files     = [];
my $outdirs           = [];
my $help              = 0;
my $extended          = 0;
my $version           = 0;
my $overwrite         = 0;
my $skip_existing     = 0;
my $header            = 0;
my $error_limit       = 50;
my $dry_run           = 0;
my $use_as_default    = 0;
my $defaults_dir      = (sglob('~/.rpst'))[0];
my $min_candidacies   = 1;
my $abundance_pattern = 'size=(\d+);';
my $filetype          = 'auto';
my $uchime            = 'usearch';
my $seq_id_pattern    = '^\s*[>\@]\s*([^;]+)';
my $default_tmpdir    = './';
my $tmpdir            = exists($ENV{TMPDIR}) ? $ENV{TMPDIR} :
  (exists($ENV{TMP}) && -e $ENV{TMP} && -d $ENV{TMP} ? $ENV{TMP} :
   (exists($ENV{TEMP}) && -e $ENV{TEMP} && -d $ENV{TMP} ? $ENV{TEMP} :
    (-e '/tmp' && -d '/tmp' ? '/tmp' : $default_tmpdir)));
my $tmp_suffix        = '.tmp.fna';

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args     = [@ARGV];  #Preserve the arguments for getCommand
my $num_explicit_args = scalar(@ARGV);
my $verbose           = 0;
my $quiet             = 0;
my $DEBUG             = 0;
my $force             = 0;
my @user_defaults     = getUserDefaults(1);

my $GetOptHash =
  {'i|candidate-seq-files=s' => sub {push(@$input_files,    #REQUIRED unless
					  [sglob($_[1])])}, # <> is supplied
   '<>'                      => sub {push(@$input_files,    #REQUIRED unless
					  [sglob($_[0])])}, # -i is supplied
   'd|dereplicated-files=s'  => sub {push(@$drp_files,      #REQUIRED
					  [sglob($_[1])])},
   'f|merged-seq-file|library-file=s'
                             => sub {push(@$library_files,  #OPTIONAL [none]
					  sglob($_[1]))},
   't|filetype=s'            => \$filetype,                 #OPTIONAL [auto]
				                            #(fasta,fastq,auto)
   'k|min-candidacies=s'     => \$min_candidacies,          #OPTIONAL [1]
   'o|outfile-stub=s'        => sub {push(@$output_files,   #OPTIONAL [stdout]
					  sglob($_[1]))},
   's|summary-suffix=s'      => \$sum_suffix,               #OPTIONAL [.smry]
   'r|reals-suffix=s'        => \$reals_suffix,             #OPTIONAL [.reals]
   'fakes-suffix=s'          => \$fakes_suffix,             #OPTIONAL [nooutpt]
   'chimes-suffix=s'         => \$chime_suffix,             #OPTIONAL [.chim]
   'chimes-aln-suffix=s'     => \$chime_aln_suffix,         #OPTIONAL
                                                            #       [.chim-aln]
   'q|seq-id-pattern=s'      => \$seq_id_pattern,           #OPTIONAL [^\s*[>
                                                            #    \@]\s*([^;]+)]
   'p|abundance-pattern=s'   => \$abundance_pattern,        #OPTIONAL
                                                            #     [size=(\d+);]
   'y|uchime-exe=s'          => \$uchime,                   #OPTIONAL [usearch
                                                            #   -uchime_denovo]
   'outdir=s'                => sub {push(@$outdirs,        #OPTIONAL [none]
					  [sglob($_[1])])},
   'tmpdir=s'                => \$tmpdir,                   #OPTIONAL [(env)]
   'tmp-suffix=s'            => \$tmp_suffix,               #OPTIONAL
                                                            #        [.tmp.fna]
   'overwrite!'              => \$overwrite,                #OPTIONAL [Off]
   'skip-existing!'          => \$skip_existing,            #OPTIONAL [Off]
   'force:+'                 => \$force,                    #OPTIONAL [Off]
   'verbose:+'               => \$verbose,                  #OPTIONAL [Off]
   'quiet'                   => \$quiet,                    #OPTIONAL [Off]
   'debug:+'                 => \$DEBUG,                    #OPTIONAL [Off]
   'help'                    => \$help,                     #OPTIONAL [Off]
   'extended'                => \$extended,                 #OPTIONAL [Off]
   'version'                 => \$version,                  #OPTIONAL [Off]
   'header!'                 => \$header,                   #OPTIONAL [On]
   'error-type-limit=s'      => \$error_limit,              #OPTIONAL [0]
   'dry-run'                 => \$dry_run,                  #OPTIONAL [Off]
   'use-as-default|save-as-default'                         #OPTIONAL [Off]
                             => \$use_as_default,
  };

#If the user has previously stored any defaults, get them
if(scalar(@user_defaults))
  {
    #This sets the default values before calling GetOptions so that the user
    #can explicitly change the defaults in any one run.  It also allows user-
    #defined default values to show in the usage output by setting before
    #calling usage().  Using a copy of the array because GetOptionsFromArray
    #alters it.
    GetOptionsFromArray([@user_defaults],%$GetOptHash);
  }

#Do not call usage if extended was saved as default and there's 1 arg (that may
#not be --extended), so set extended to 0, get the args, call usage (if called
#for), then restore.
my $saved_extended = $extended;
$extended = 0;

#Get the input options & catch any errors in option parsing
unless(GetOptions(%$GetOptHash))
  {
    #Try to guess which arguments GetOptions is complaining about
    my @possibly_bad = grep {!(-e $_)} map {@$_} @$input_files;

    error('Getopt::Long::GetOptions reported an error while parsing the ',
	  'command line arguments.  The error should be above.  Please ',
	  'correct the offending argument(s) and try again.');
    usage(1);
    quit(-2);
  }

#Print the usage if there are no arguments (or it's just the extended flag) and
#no files directed or piped in
if(($num_explicit_args == 0 || ($num_explicit_args == 1 && $extended)) &&
   isStandardInputFromTerminal())
  {
    usage(0,$extended);
    quit(0);
  }

#Restore the saved extended value if not explicitly set to true
$extended = $saved_extended if(!$extended);

#Error-check for mutually exclusive flags supplied together
if(scalar(grep {$_} ($use_as_default,$help,$version)) > 1)
  {
    error("Options [",join(',',grep {$_} ($use_as_default,$help,$version)),
	  "] are mutually exclusive.");
    quit(-3);
  }

#If the user specified that they would like to use the current options as
#default values, store them
if($use_as_default)
  {
    my $orig_defaults = getUserDefaults();
    if(saveUserDefaults())
      {
	print("Old user defaults: [",join(' ',@$orig_defaults),"].\n",
	      "New user defaults: [",join(' ',getUserDefaults()),"].\n");
	quit(0);
      }
    else
      {quit(-4)}
  }

print STDERR ("Starting dry run.\n") if($dry_run);

#Print the debug mode (it checks the value of the DEBUG global variable)
debug('Debug mode on.') if($DEBUG > 1);

#If the user has asked for help, call the help subroutine
if($help)
  {
    help($extended);
    quit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    print(getVersion($extended),"\n");
    quit(0);
  }

#Check validity of verbosity options
if($quiet && ($verbose || $DEBUG))
  {
    $quiet = 0;
    error('You cannot supply the quiet and (verbose or debug) flags ',
	  'together.');
    quit(-5);
  }

#Check validity of existing outfile options
if($skip_existing && $overwrite)
  {
    error('You cannot supply the --overwrite and --skip-existing flags ',
	  'together.');
    quit(-6);
  }

#Warn users when they turn on verbose and output is to the terminal
#(implied by no outfile suffix checked above) that verbose messages may be
#uncleanly overwritten
if($verbose && !defined($reals_suffix) && isStandardOutputToTerminal())
  {warning('You have enabled --verbose, but appear to possibly be ',
	   'outputting to the terminal.  Note that verbose messages can ',
	   'interfere with formatting of terminal output making it ',
	   'difficult to read.  You may want to either turn verbose off, ',
	   'redirect output to a file, or supply an outfile stub (-o).')}

#Make sure there is input
if(scalar(@$input_files) == 0 && isStandardInputFromTerminal())
  {
    error('No input files detected.');
    usage(1);
    quit(-7);
  }

#Make sure that an outfile suffix has been supplied if an outdir has been
#supplied
if(scalar(@$outdirs) && !defined($reals_suffix))
  {
    error("An outfile stub (-o) is required if an output directory ",
	  "(--outdir) is supplied.  Note, you may supply an empty string ",
	  "to name the output files the same as the input files.");
    quit(-8);
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
    quit(1);
  }

if($abundance_pattern =~ /^\s*$/)
  {
    error("Abundance pattern (-p) cannot be an empty string.");
    quit(2);
  }
elsif($abundance_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$abundance_pattern = '(' . $abundance_pattern . ')'}

if($min_candidacies !~ /^[1-9]\d*$/)
  {
    error("Invalid minimum candidacies value (-k): [$min_candidacies].  Must ",
	  "be an integer greater than 0.");
    quit(3);
  }

#If no outfile stubs were provided, there's at least one library file, and all
#the suffixes are either not defined or are not empty strings (meaning that the
#library file will not be overwritten - assumes that at least 1 of the suffixes
#is defined and that we will be outputting to a file instead of stdout),
#default the outfile stub to the library file(s)
if(scalar(@$output_files) == 0 && scalar(@$library_files) &&
  (!defined($reals_suffix)     || $reals_suffix     ne '') &&
  (!defined($fakes_suffix)     || $fakes_suffix     ne '') &&
  (!defined($chime_suffix)     || $chime_suffix     ne '') &&
  (!defined($chime_aln_suffix) || $chime_aln_suffix ne '') &&
  (!defined($sum_suffix)       || $sum_suffix       ne ''))
  {
    verbose("Using library file: [",join(',',@$library_files),
	    "] as the outfile stub.");
    $output_files = copyArray($library_files);
  }
elsif($DEBUG < -99)
  {debug('if(scalar(@$output_files) == 0 && scalar(@$library_files) && ',
	 '(!defined($reals_suffix)     || $reals_suffix     ne "") && ',
	 '(!defined($fakes_suffix)     || $fakes_suffix     ne "") && ',
	 '(!defined($chime_suffix)     || $chime_suffix     ne "") && ',
	 '(!defined($chime_aln_suffix) || $chime_aln_suffix ne "") && ',
	 '(!defined($sum_suffix)       || $sum_suffix       ne ""))',
	 "\nif(",scalar(@$output_files)," == 0 && ",scalar(@$library_files),
	 " && (!",defined($reals_suffix),"     || $reals_suffix     ne '') ",
	 "&& (!",defined($fakes_suffix),"     || ",
	 (defined($fakes_suffix) ? $fakes_suffix : 'undef'),"     ne '') && ",
	 "(!",defined($chime_suffix),"     || $chime_suffix     ne '') && (!",
	 defined($chime_aln_suffix)," || $chime_aln_suffix ne '') && (!",
	 defined($sum_suffix),"       || $sum_suffix       ne ''))")}

my $num_outfiles = (scalar(@$output_files) ? scalar(@$output_files) :
		    (isStandardOutputToTerminal() ? 0 : 1));

if(scalar(@$input_files) == 0)
  {
    error("-i is a required parameter.");
    quit(4);
  }

if(scalar(@$drp_files) == 0)
  {
    error("-d is a required parameter.");
    quit(5);
  }

if(scalar(@$input_files) != scalar(@$drp_files))
  {
    if(scalar(grep {scalar(@$_) == 1} @$input_files) ==
       scalar(@$input_files) && scalar(@$drp_files) == 1)
      {$input_files = [[map {@$_} @$input_files]]}
    else
      {
	error("-i was supplied [",scalar(@$input_files),"] times and -d was ",
	      "supplied [",scalar(@$drp_files),"] times.  The numbers must ",
	      "be the same.");
	quit(6);
      }
  }

my $bad_indexes = [map {$_++} grep {scalar(@{$input_files->[$_]}) !=
				      scalar(@{$drp_files->[$_]})}
		   (0..$#{$input_files})];
if(scalar(@$bad_indexes))
  {
    error("These numbered pairs of -i and -d have a different number of ",
	  "files: [",join(',',@$bad_indexes),"].  Each -i/-d bad pair has ",
	  "this many files: [",
	  join(',',(map {$_--;scalar(@${$input_files->[$_]}) . '/' .
			   scalar(@${$drp_files->[$_]})} @$bad_indexes)),
	  "].  The number of files to -i must be the same as to -d for each ",
	  "ordered occurrence of -i/-d.");
    quit(7);
  }

if(#There are output files
   scalar(@$output_files) &&
   (#There's only 1 -i
    (scalar(@$input_files) == 1 &&
     scalar(@$output_files) != 1) ||
    #There's multiple -i's but each contains a single file
    (scalar(@$input_files) != 1 &&
     scalar(grep {scalar(@$_) == 1} @$input_files) == scalar(@$input_files) &&
     scalar(@$output_files) != 1) ||
    #There's multiple -i's with multiple files
    (scalar(@$input_files) != 1 &&
     scalar(grep {scalar(@$_) == 1} @$input_files) != scalar(@$input_files) &&
     scalar(@$output_files) != scalar(@$input_files))))
  {
    error("The number of output file stubs (-o) must equal the number of ",
	  "times -i occurs on the command line.");
    quit(8);
  }

if(scalar(@$input_files) > 1 &&
   scalar(grep {scalar(@$_) == 1} @$input_files) != scalar(@$input_files) &&
   scalar(@$output_files) == 0)
  {
    error("Output file stubs (-o) are required if -i is supplied multiple ",
	  "times.");
    quit(8);
  }

if(#There are library files
   scalar(@$library_files) &&
   (#There's only 1 -i
    (scalar(@$input_files) == 1 &&
     scalar(@$library_files) != 1) ||
    #There's multiple -i's but each contains a single file
    (scalar(@$input_files) != 1 &&
     scalar(grep {scalar(@$_) == 1} @$input_files) == scalar(@$input_files) &&
     scalar(@$library_files) != 1) ||
    #There's multiple -i's with multiple files
    (scalar(@$input_files) != 1 &&
     scalar(grep {scalar(@$_) == 1} @$input_files) != scalar(@$input_files) &&
     scalar(@$library_files) != scalar(@$input_files))))
  {
    error("The number of library files (-f) must equal the number of times ",
	  "-i occurs on the command line.");
    quit(9);
  }

if(#There are library files
   scalar(@$library_files) &&
   #The tempdir is invalid
   (!defined($tmpdir) || $tmpdir eq '' || !(-e $tmpdir)))
  {
    if(-e $default_tmpdir)
      {
	verbose("The temporary directory",
		(defined($tmpdir) ? ": [$tmpdir]" : '')," is invalid.  Using ",
		"default: [$default_tmpdir] along with temporary suffix ",
		"[$tmp_suffix] which will be applied to -i and -f files.  ",
		"Use --tmpdir and --tmp-suffix to change these values.")
	  if($verbose > 1 || ($DEBUG && $verbose));
	$tmpdir = $default_tmpdir;
      }
    else
      {
	error("The temporary directory",
	      (defined($tmpdir) ? ": [$tmpdir]" : '')," is invalid.  Use ",
	      "--tmpdir and --tmp-suffix to manage temporary data (necessary ",
	      "for running uchime) or do not supply a library file.");
	quit(10);
      }
  }

if($min_candidacies !~ /^[1-9]\d*$/)
  {
    error("Invalid minimum candidacies (-k): [$min_candidacies].  Must be an ",
	  "integer greater than 0.");
    quit(12);
  }

#Make sure uchime is properly installed if library files have been supplied
if(scalar(@$library_files))
  {
    $uchime = getUchimeExe($uchime);
    quit(13) if(incompatible($uchime));
  }

if($seq_id_pattern ne '' &&
   $seq_id_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$seq_id_pattern = '(' . $seq_id_pattern . ')'}

#Get all the corresponding groups of files and output directories to process
my($input_file_sets,
   $output_file_sets) = getFileSets([(scalar(@$output_files) == 0 &&
				      $num_outfiles ?
				      [['STDOUT']] : [$output_files]),
				     $input_files,
				     $drp_files,
				     [$library_files],
				    ],

				    [[$reals_suffix,   #All correspond to each
				      $sum_suffix,     #of the output_files
				      $fakes_suffix,   #i.e. stubs
				      $chime_suffix,
				      $chime_aln_suffix],
				    ],

				    $outdirs);

#Look for existing output files generated by the input_files array
my @existing_outfiles = getExistingOutfiles($output_file_sets);
#If any of the expected output files already exist, quit with an error
if(scalar(@existing_outfiles) && !$overwrite && !$skip_existing)
  {
    error("Files exist: [",join(',',@existing_outfiles),"].\nUse --overwrite ",
	  'or --skip-existing to continue.');
    quit(-1);
  }

#Create the output directories
mkdirs(@$outdirs);

verbose('Run conditions: ',scalar(getCommand(1)));

#If output is going to STDOUT instead of output files with different extensions
#or if STDOUT was redirected, output run info once
verbose('[STDOUT] Opened for all output.') if(!defined($reals_suffix));

#Store info. about the run as a comment at the top of the output file if
#STDOUT has been redirected to a file
if(!isStandardOutputToTerminal() && $header)
  {print(getHeader($extended));}

my $hash      = {};
my $id_lookup = {};
my($input_file);
#For each set of input files associated by getFileSets
foreach my $set_num (0..$#$input_file_sets)
  {
    debug("Processing input file(s):\n\t",
	  join("\n\t",grep {defined($_)} @{$input_file_sets->[$set_num]})
	  ,"\nwith output file(s):\n\t",
	  join("\n\t",grep {defined($_)} @{$output_file_sets->[$set_num]}));

    #Not going to use the output file stub as-is.
    $input_file         = $input_file_sets->[$set_num]->[1];
    my $drp_file        = $input_file_sets->[$set_num]->[2];
    my $lib_file        = $input_file_sets->[$set_num]->[3];
    my($reals_file,$smry_file,$fakes_file,$chimes_file,$aln_file) =
      defined($output_file_sets->[$set_num]->[0]) ?
	split(/,/,$output_file_sets->[$set_num]->[0],-1) :
	  (undef,undef,undef,undef);
    if(!defined($reals_file))
      {$reals_file = 'STDOUT'}

    #Keep track of all the input files associated with this output file for the
    #table
    push(@{$hash->{$reals_file}->{SAMPLES}},$input_file);

    if(defined($lib_file) &&
       exists($hash->{$reals_file}) &&
       exists($hash->{$reals_file}->{LIBFILE}) &&
       $hash->{$reals_file}->{LIBFILE} ne $lib_file)
      {
	error("There should be only 1 library file for every reals file, ",
	      "however I have found multiple library files for reals file ",
	      "[$reals_file]: [$lib_file,$hash->{$reals_file}->{LIBFILE}].  ",
	      "Ignoring previous library files.");
      }
    $hash->{$reals_file}->{LIBFILE} = $lib_file;

    if(defined($smry_file) &&
       exists($hash->{$reals_file}) &&
       exists($hash->{$reals_file}->{SMRYFILE}) &&
       defined($hash->{$reals_file}->{SMRYFILE}) &&
       $hash->{$reals_file}->{SMRYFILE} ne $smry_file)
      {
	error("There should be only 1 summary file for every reals file, ",
	      "however I have found multiple summary files specified for ",
	      "reals file [$reals_file]: [$smry_file,",
	      "$hash->{$reals_file}->{SMRYFILE}].  Ignoring previous summary ",
	      "files.");
      }
    $hash->{$reals_file}->{SMRYFILE} = $smry_file;

    if(defined($fakes_file) &&
       exists($hash->{$reals_file}) &&
       exists($hash->{$reals_file}->{FAKESFILE}) &&
       defined($hash->{$reals_file}->{FAKESFILE}) &&
       $hash->{$reals_file}->{FAKESFILE} ne $fakes_file)
      {
	error("There should be only 1 fakes file for every reals file, ",
	      "however I have found multiple fakes files specified for ",
	      "reals file [$reals_file]: [$fakes_file,",
	      "$hash->{$reals_file}->{FAKESFILE}].  Ignoring previous fakes ",
	      "files.");
      }
    $hash->{$reals_file}->{FAKESFILE} = $fakes_file;

    if(defined($chimes_file) &&
       exists($hash->{$reals_file}) &&
       exists($hash->{$reals_file}->{CHIMESFILE}) &&
       $hash->{$reals_file}->{CHIMESFILE} ne $chimes_file)
      {
	error("There should be only 1 chimera file (--chimes-suffix) for ",
	      "every reals file, however I have found multiple chimera files ",
	      "specified for reals file [$reals_file]: [$chimes_file,",
	      "$hash->{$reals_file}->{CHIMESFILE}].  Ignoring previous ",
	      "chimera files.");
      }
    $hash->{$reals_file}->{CHIMESFILE} = $chimes_file;

    if(defined($aln_file) &&
       exists($hash->{$reals_file}) &&
       exists($hash->{$reals_file}->{ALNFILE}) &&
       $hash->{$reals_file}->{ALNFILE} ne $aln_file)
      {
	error("There should be only 1 chimera alignment file for every ",
	      "reals file, however I have found multiple chimera alignment ",
	      "files specified for reals file [$reals_file]: [$aln_file,",
	      "$hash->{$reals_file}->{ALNFILE}].  Ignoring previous chimera ",
	      "alignment files.");
      }
    $hash->{$reals_file}->{ALNFILE} = $aln_file;

    my $id_check = {};

    openIn(*DRPS,$drp_file) || $force > 1 || next;

    next if($dry_run);

    my $verbose_freq = 100;
    my $cnt          = 0;

    while(my $rec = getNextSeqRec(*DRPS))
      {
	my($def,$seq) = @$rec;
	$seq = uc($seq);
	my $id        = '';
	my $abundance = 1;
	$cnt++;

	if($def =~ /\s*[\@\>]\s*(\S+)/)
	  {
	    my $default_id = $1;

	    if($seq_id_pattern ne '' && $def =~ /$seq_id_pattern/)
	      {$id = $1}
	    else
	      {
		$id = $default_id;
		warning("Unable to parse seqID from defline: [$def] ",
			"in dereplicated file: [$drp_file] using pattern: ",
			"[$seq_id_pattern].  Please either fix the ",
			"defline or use a different pattern to extract ",
			"the seqID value.  Using default ID: [$id].  ",
			"Use \"-q ''\" to to avoid this warning.")
		  if($seq_id_pattern ne '');
	      }
	  }
	else
	  {
	    warning("Could not parse defline in record [$cnt] of file ",
		    "[$drp_file]: [$def].  Please edit the file to contain ",
		    "IDs.");
	  }

	if($def =~ /$abundance_pattern/)
	  {$abundance = $1}
	else
	  {
	    warning("Unable to parse abundance from defline: [$def] in ",
		    "record $cnt of dereplicated file: [$drp_file] using ",
		    "pattern: [$abundance_pattern].  Please either fix the ",
		    "defline or use a different pattern (-p) to extract the ",
		    "abundance value.  Assuming abundance is 1.");
	  }

	$id_check->{$id}++;

	$hash->{$reals_file}->{GLOBAL_ABUND}->{$seq} += $abundance;

	$hash->{$reals_file}->{DRPS}->{$seq}->{$input_file} =
	  {ID    => $id,
	   ABUND => $abundance,
	   REC   => $rec};
      }

    closeIn(*DRPS);

    my @ambigs = grep {$id_check->{$_} > 1} keys(%$id_check);
    if(scalar(@ambigs))
      {
	warning("Dereplicated file: [$drp_file] contains ambiguous IDs.  ",
		"These IDs were found on the indicated number of deflines: [",
		join(',',map {"$_:$id_check->{$_}"} @ambigs),"].  The ",
		"resulting output files might contain ambigous IDs.");
      }

    @ambigs   = ();
    $id_check = {};

    openIn(*CANDS,$input_file) || $force > 1 || next;

    $verbose_freq = 100;
    $cnt          = 0;
    my @missing   = ();

    while(my $rec = getNextSeqRec(*CANDS))
      {
	my($def,$seq) = @$rec;
	$seq = uc($seq);
	my $id        = '';
	$cnt++;

	if($def =~ /\s*[\@\>]\s*(\S+)/)
	  {
	    my $default_id = $1;

	    if($seq_id_pattern ne '' && $def =~ /$seq_id_pattern/)
	      {$id = $1}
	    else
	      {
		$id = $default_id;
		warning("Unable to parse seqID from defline: [$def] ",
			"in candidate file: [$input_file] using pattern: ",
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
		  "IDs and abundance values on the deflines.");
	  }

	if(!exists($hash->{$reals_file}->{DRPS}->{$seq}))
	  {push(@missing,$id)}

	$id_check->{$id}++;

	if(exists($id_lookup->{$reals_file}->{$seq}) &&
	   $id_lookup->{$reals_file}->{$seq}->{ID} ne $id)
	  {
	    error("The same sequence was found with different IDs.  ",
		  "Candidate file: ",
		  "[$id_lookup->{$reals_file}->{$seq}->{FL}] has ID: ",
		  "[$id_lookup->{$reals_file}->{$seq}->{ID}], but candidate ",
		  "file: [$input_file] has ID: [$id].  All IDs between all ",
		  "candidate and dereplicated files should be globally ",
		  "consistent.  Ignoring all previous conflicting IDs.");
	  }
	$id_lookup->{$reals_file}->{$seq}->{ID} = $id;
	$id_lookup->{$reals_file}->{$seq}->{FL} = $input_file;

	$hash->{$reals_file}->{CANDS}->{$seq}->{$input_file} = {ID  => $id,
								 REC => $rec};
      }

    closeIn(*CANDS);

    @ambigs = grep {$id_check->{$_} > 1} keys(%$id_check);
    if(scalar(@ambigs))
      {
	warning("Candidates file file: [$input_file] contains ambiguous ",
		"IDs.  These IDs were found on the indicated number of ",
		"deflines: [",join(',',map {"$_:$id_check->{$_}"} @ambigs),
		"].  The resulting table output table [$smry_file] will ",
		"contain ambigous IDs.");
      }

    if(scalar(@missing))
      {
	my @report_missing = scalar(@missing) > 10 ?
	  (@missing[0..8],'...') : @missing;
	error("The candidate sequences from candidate file [$input_file] ",
	      "with these IDs [",join(',',@report_missing),"] were missing ",
	      "in the dereplicated file [$drp_file].  Abundances may be ",
	      "missing from the output table in [$smry_file].");
      }
  }

foreach my $reals_file (keys(%$hash))
  {
    my $smry_file   = $hash->{$reals_file}->{SMRYFILE};
    my $fakes_file  = $hash->{$reals_file}->{FAKESFILE};
    my $chimes_file = $hash->{$reals_file}->{CHIMESFILE};
    my $aln_file    = $hash->{$reals_file}->{ALNFILE};
    my $lib_file    = $hash->{$reals_file}->{LIBFILE};
    my $chimeras    = {};

    #Determine K-filtered candidates
    my $k_real_seqs =
      [grep {scalar(keys(%{$hash->{$reals_file}->{CANDS}->{$_}})) >=
	       $min_candidacies}
       keys(%{$hash->{$reals_file}->{CANDS}})];
    my $fake_seqs =
      [grep {scalar(keys(%{$hash->{$reals_file}->{CANDS}->{$_}})) <
	       $min_candidacies}
       keys(%{$hash->{$reals_file}->{CANDS}})];
    my($real_seqs,$chime_seqs);

    #If there was a library file
    if(defined($lib_file) && $lib_file ne '')
      {
	#Prepare uchime to accept the library input
	($real_seqs,$chime_seqs) = getUchimeReals($lib_file,
						  $k_real_seqs,
						  $uchime,
						  $hash,
						  $id_lookup,
						  $aln_file,
						  $abundance_pattern,
						  $seq_id_pattern,
						  $tmpdir,
						  $tmp_suffix,
						  $reals_file);
      }
    else
      {
	$real_seqs  = $k_real_seqs;
	$chime_seqs = [];
      }

    next if($dry_run);

    #If there was a reals file
    if(defined($reals_file))
      {
	#Run-time double-check for existing output files.
	#This is how skip_existing and overwrite are checked.
	if($reals_file ne 'STDOUT')
	  {checkFile($reals_file) || next}

	openOut(*REAL,
		($reals_file ne 'STDOUT' ? $reals_file : '/dev/stdout'),
		1) || next;
      }

    print(map {(exists($id_lookup->{$reals_file}) &&
		exists($id_lookup->{$reals_file}->{$_}) ?
		">$id_lookup->{$reals_file}->{$_}->{ID};" : '>') . 'size='
		  . (exists($hash->{$reals_file}) &&
		     exists($hash->{$reals_file}->{GLOBAL_ABUND}) &&
		     exists($hash->{$reals_file}->{GLOBAL_ABUND}->{$_}) ?
		     $hash->{$reals_file}->{GLOBAL_ABUND}->{$_} : '1')
		    . ";\n$_\n"} @$real_seqs);

    closeOut(*REAL) if(defined($reals_file));

    #Print the abundance table
    if(defined($smry_file) && $smry_file ne '')
      {
	#Print the summary table file
	checkFile($smry_file) || next;
	openOut(*SMRY,$smry_file,1) || next;

	my $table_str =
	  "#ID\t" . join("\t",
			 sort {$a cmp $b}
			 map {my $f = $_;$f =~ s/.*\///;$f}
			 getVariablePrefixes(@{$hash->{$reals_file}
						 ->{SAMPLES}})) . "\n";

	foreach my $seq (@$real_seqs)
	  {
	    my $id = exists($id_lookup->{$reals_file}) &&
	      exists($id_lookup->{$reals_file}->{$seq}) ?
		$id_lookup->{$reals_file}->{$seq}->{ID} : 'ERR-ID-MISSING';
	    $table_str .= "$id";
	    foreach my $input_file (@{$hash->{$reals_file}->{SAMPLES}})
	      {
		$table_str .= "\t";
		if(exists($hash->{$reals_file}) &&
		   exists($hash->{$reals_file}->{DRPS}->{$seq}) &&
		   exists($hash->{$reals_file}->{DRPS}->{$seq}
			  ->{$input_file}) &&
		   $hash->{$reals_file}->{DRPS}->{$seq}->{$input_file}
		   ->{ABUND})
		  {$table_str .=
		     $hash->{$reals_file}->{DRPS}->{$seq}->{$input_file}
		       ->{ABUND}}
		else
		  {$table_str .= '0'}
	      }
	    $table_str .= "\n";
	  }

	print(straightenColumns($table_str));

	closeOut(*SMRY);
      }

    #If there was a fakes file
    if(defined($fakes_file) && $fakes_file ne '')
      {
	#Print the fakes fasta file
	checkFile($fakes_file) || next;
	openOut(*FAKE,$fakes_file,1) || next;

	print(map {(exists($id_lookup->{$reals_file}) &&
		    exists($id_lookup->{$reals_file}->{$_}) ?
		    ">$id_lookup->{$reals_file}->{$_}->{ID};" : '>') . 'size='
		     . (exists($hash->{$reals_file}) &&
			exists($hash->{$reals_file}->{GLOBAL_ABUND}) &&
			exists($hash->{$reals_file}->{GLOBAL_ABUND}->{$_}) ?
			$hash->{$reals_file}->{GLOBAL_ABUND}->{$_} : '1')
		       . ";\n$_\n"} @$fake_seqs);

	closeOut(*FAKE);
      }

    #If there was a chimes file
    if(defined($chimes_file) && $chimes_file ne '')
      {
	#Print the chimes fasta file
	checkFile($chimes_file) || next;
	openOut(*CHIME,$chimes_file,1) || next;

	print(map {(exists($id_lookup->{$reals_file}) &&
		    exists($id_lookup->{$reals_file}->{$_}) ?
		    ">$id_lookup->{$reals_file}->{$_}->{ID};" : '>') . 'size='
		     . (exists($hash->{$reals_file}) &&
			exists($hash->{$reals_file}->{GLOBAL_ABUND}) &&
			exists($hash->{$reals_file}->{GLOBAL_ABUND}->{$_}) ?
			$hash->{$reals_file}->{GLOBAL_ABUND}->{$_} : '1')
		       . ";\n$_\n"} @$chime_seqs);

	closeOut(*CHIME);
      }
  }

verbose("[STDOUT] Output done.") if(!defined($reals_suffix));

#Report the number of errors, warnings, and debugs on STDERR
printRunReport($verbose) if(!$quiet && ($verbose || $DEBUG ||
					defined($main::error_number) ||
					defined($main::warning_number)));

##
## End Main
##


























































































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
	($verbose_length) = sort {length($b) <=> length($a)}
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
    $caller_string .= "MAIN(LINE$line_num): ";
    $caller_string =~ s/:.*/:/;
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
## (optional) In array context, the time between all marks is always returned
## regardless of a supplied mark index
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
    if(wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_ - 1] - $main::time_marks->[$_]}
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

sub getVersion
  {
    my $full_version_flag = $_[0];
    my $template_version_number = '2.19';
    my $version_message = '';

    #$software_version_number  - global
    #$created_on_date          - global
    #$verbose                  - global

    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    if($created_on_date eq 'DATE HERE')
      {$created_on_date = 'UNKNOWN'}

    $version_message  = '#' . join("\n#",
				   ("$script Version $software_version_number",
				    " Created: $created_on_date",
				    " Last modified: $lmd"));

    if($full_version_flag)
      {
	$version_message .= "\n#" .
	  join("\n#",
	       ('Generated using perl_script_template.pl ' .
		"Version $template_version_number",
		' Created: 5/8/2006',
		' Author:  Robert W. Leach',
		' Contact: rleach@genomics.princeton.edu',
		' Company: Princeton University',
		' Copyright 2014'));
      }

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

sub printRunReport
  {
    my $extended = $_[0];

    return(0) if($quiet);

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

    #If the user wants the extended report
    if($extended)
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
      }
    else
      {print STDERR "\n"}

    if($main::error_number || $main::warning_number)
      {print STDERR ("\tScroll up to inspect full errors/warnings ",
		     "in-place.\n")}
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

#Globals used: $overwrite, $skip_existing
sub getFileSets
  {
    my $file_types_array = $_[0]; #A 3D array where the outer array specifies
                                  #file type (e.g. all files supplied by
                                  #instances of -i), the next array specifies
                                  #a specific instance of an option/flag (e.g.
                                  #the first instance of -i on the command
                                  #line) and the inner-most array contains the
                                  #arguments to that instance of that flag.
    my $outfile_suffixes = $_[1]; #OPTIONAL: An array (2D) no larger than
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
    my $outdir_array     = $_[2]; #OPTIONAL: A 2D array of output directories.
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
    my $outfile_stub = 'STDIN';

    debug("Num initial arguments: [",scalar(@_),"].") if($DEBUG < -99);

    debug("Initial size of file types array: [",scalar(@$file_types_array),
	  "].") if($DEBUG < -99);

    #Error check the file_types array to make sure it's a 3D array of strings
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

    #Error-check/fix the suffixes array
    my $suffix_provided = [map {0} @$file_types_array];
    if(defined($outfile_suffixes))
      {
	if(ref($outfile_suffixes) ne 'ARRAY')
	  {
	    #Allow them to submit scalars of everything
	    if(ref(\$outfile_suffixes) eq 'SCALAR')
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
	elsif(scalar(grep {ref($_) ne 'ARRAY'} @$outfile_suffixes))
	  {
	    my @errors = map {ref(\$_)} grep {ref($_) ne 'ARRAY'}
	      @$outfile_suffixes;
	    #Allow them to have submitted an array of scalars
	    if(scalar(@errors) == scalar(@$file_types_array) &&
	       scalar(@errors) == scalar(grep {$_ eq 'SCALAR'} @errors))
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

    #If output directories were supplied, error check them
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
		error("Expected an array for the second argument, but got a [",
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
		error("Expected an array of arrays for the second argument, ",
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
	    error("Expected an array of arrays of scalars for the second ",
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

    debug("Contents of file types array before adding dash file: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

    #If standard input has been redirected in
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

    #If the final size of the file types array is smaller than the suffixes
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

    #If output directories were supplied, push them onto the file_types_array
    #so that they will be error-checked and modified in the same way below.
    if($outdirs_provided)
      {push(@$file_types_array,$outdir_array)}

    debug("Contents of file types array after adding outdirs: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

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

    debug("Contents of modified file types array: [(",
	  join(')(',map {my $t=$_;'{' .
			   join('}{',map {my $e=$_;'[' . join('][',@$e) . ']'}
				@$t) . '}'} @$file_types_array),")].")
      if($DEBUG < -99);

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
		  join(',',@$file_type_array),"].") if($DEBUG < -99);
	  }
      }

    #New code to handle all combinations of files
    my($infile_sets_array,$outfiles_or_stubs);
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

		    debug("Prepended directory: [$new_outfile_stub] to ",
			  "infile: [$file].")
		      if($DEBUG < -99);

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
	    push(@$outfiles_or_stubs,$stub_set);
	  }

	if($nonunique_found)
	  {
	    makeStubsUnique($outfiles_or_stubs);

#	    error('The following output file name stubs were created by ',
#		  'multiple input file names and will be overwritten if ',
#		  'used.  Please make sure each similarly named input file ',
#		  'outputs to a different output directory or that the input ',
#		  'file names bare no similarity.  Offending file name ',
#		  'conflicts: [',
#		  join(',',map {"$_ is written to by [" .
#				  join(',',@{$unique_out_check->{$_}}) . "]"}
#		       (grep {scalar(@{$unique_out_check->{$_}}) > 1}
#			keys(%$unique_out_check))),'].');
#	    quit(-1);
	  }
      }
    else
      {
	$infile_sets_array = getMatchedSets($file_types_array);
	$outfiles_or_stubs = copyArray($infile_sets_array);

	#Replace any dahes with the outfile stub
	foreach my $stub_set (@$outfiles_or_stubs)
	  {foreach my $stub (@$stub_set)
	     {$stub = $outfile_stub if(defined($stub) && $stub eq '-')}}
      }

    #If there were any outfile suffixes provided, append them to the stubs and
    #make any stubs belonging to file types that have no defined suffix,
    #undefined, checking for existence along the way
    if(scalar(grep {$_} @$suffix_provided))
      {
	debug("Creating/checking outfile paths/names.");

	my(%exist);
	foreach my $stub_combo (@$outfiles_or_stubs)
	  {
	    foreach my $type_index (0..$#{$stub_combo})
	      {
		my $stub = $stub_combo->[$type_index];
		if(defined($outfile_suffixes->[$type_index]) &&
		   defined($stub) && $stub ne 'STDOUT')
		  {
		    foreach(map {$stub . $_}
			    grep {defined($_)} @{$outfile_suffixes
						   ->[$type_index]})
		      {checkFile($_,undef,1) || $exist{$_}++}
		    $stub_combo->[$type_index] =
		      join(',',
			   map {defined($_) ? $stub . $_ : ''}
			   @{$outfile_suffixes->[$type_index]});
		  }
		else
		  {$stub_combo->[$type_index] = undef}
	      }
	  }

	if(scalar(keys(%exist)))
	  {
	    my $report_exist = [scalar(keys(%exist)) > 10 ?
				((keys(%exist))[0..8],'...') : keys(%exist)];
	    error("Output files exist: [",join(',',@$report_exist),
		  "].  Use --overwrite or --skip-existing to continue.");
	    quit(-30);
	  }
      }
    #Otherwise, return all stubs

    debug("Processing input file sets: [(",
	  join('),(',(map {my $a = $_;join(',',map {defined($_) ? $_ : 'undef'}
					   @$a)} @$infile_sets_array)),
	  ")] and output stubs: [(",
	  join('),(',(map {my $a = $_;join(',',map {defined($_) ? $_ : 'undef'}
					   @$a)} @$outfiles_or_stubs)),")].")
      if($DEBUG < 0);

    return($infile_sets_array,$outfiles_or_stubs);
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
#Globals used: $overwrite,$dry_run
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
sub checkFile
  {
    my $output_file    = defined($_[0]) ? $_[0] : return(1);
    my $input_file_set = $_[1];
    my $local_quiet    = defined($_[2]) ? $_[2] : 0;
    my $quit           = defined($_[3]) ? $_[3] : 0;
    my $status         = 1;

    if(-e $output_file)
      {
	debug("Output file: [$output_file] exists.");

	if($skip_existing)
	  {
	    warning("[$output_file] Output file exists.  Skipping ",
		    "input file(s): [",join(',',@$input_file_set),"].");
	    $status = 0;
	  }
	elsif(!$overwrite)
	  {
	    error("[$output_file] Output file exists.  Unable to ",
		  "proceed.  Encountered while processing input file(s): [",
		  join(',',grep {defined($_)} @$input_file_set),
		  "].  This may have been caused by multiple input files ",
		  "writing to one output file because there were not ",
		  "existing output files when this script started.  If any ",
		  "input files are writing to the same output file, you ",
		  "should have seen a warning about this above.  Otherwise, ",
		  "you may have multiple versions of this script running ",
		  "simultaneously.  Please check your input files and ",
		  "outfile suffixes to fix any conflicts or supply the ",
		  "--skip-existing or --overwrite flag to proceed.")
	      unless($local_quiet);
	    quit(-1) if($quit);
	    $status = 0;
	  }
      }
    else
      {debug("Output file: [$output_file] does not exist yet.")}

    return($status);
  }

#Uses globals: $header,$main::open_handles,$dry_run,$extended
sub openOut
  {
    my $file_handle     = $_[0];
    my $output_file     = $_[1];
    #Select the output handle if third param is non-zero:
    my $select          = (scalar(@_) >= 3 && defined($_[2]) ? $_[2] : 0);
    my $header_override = defined($_[3]) ? $_[3] : 0;
    my $status          = 1;

    #Open the output file
    if(!$dry_run && !open($file_handle,">$output_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$output_file].\n",$!);
	$status = 0;
      }
    else
      {
	$main::open_handles->{$file_handle} = $output_file;

	if($dry_run)
	  {
	    my $encompassing_dir = $output_file;
	    $encompassing_dir =~ s/[^\/]+$//;
	    $encompassing_dir =~ s%/%%;
	    $encompassing_dir = '.' unless($encompassing_dir =~ /./);

	    if(-e $output_file && !(-w $output_file))
	      {error("Output file exists and is not writable: ",
		     "[$output_file].")}
	    elsif(-e $encompassing_dir && !(-w $encompassing_dir))
	      {error("Encompassing directory of output file: ",
		     "[$output_file] exists and is not writable.")}
	    else
	      {verbose("[$output_file] Opened output file.")}

	    #This cleans up the global hashes of file handles
	    closeOut($file_handle);

	    return($status);
	  }

	verbose("[$output_file] Opened output file.");

	if($select)
	  {
	    #Select the output file handle
	    select($file_handle);

	    #Store info about the run as a comment at the top of the output
	    print(getHeader($extended)) if($header && !$header_override);
	  }
	else
	  {
	    #Store info about the run as a comment at the top of the output
	    print $file_handle (getHeader($extended))
	      if($header && !$header_override);
	  }
      }

    return($status);
  }

#Globals used: $main::open_handles
sub closeOut
  {
    my $file_handle = $_[0];
    my $status = 1;

    if(tell($file_handle) == -1)
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

	verbose("[$main::open_handles->{$file_handle}] Output file done.  ",
		"Time taken: [",scalar(markTime()),' Seconds].');

	delete($main::open_handles->{$file_handle});
      }

    return($status);
  }

#Globals used: $main::open_handles
sub openIn
  {
    my $file_handle = $_[0];
    my $input_file  = $_[1];
    my $status      = 1;

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
	    $status = 0;
	  }
	else
	  {
	    verbose("[$input_file] Opened input file.");

	    $main::open_handles->{$file_handle} = $input_file;

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

    verbose("[$main::open_handles->{$file_handle}] Input file done.  ",
	    "Time taken: [",scalar(markTime()),' Seconds].');

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
    my $argv = $_[0]; #OPTIONAL

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
    my $status        = 1;

    my $save_argv = [grep {$_ ne '--use-as-default' &&
			     $_ ne '--save-as-default'} @$argv];

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

    return($status);
  }

sub getHeader
  {
    my $extended = $_[0];

    if(!$extended && !defined($main::header_str))
      {
	my $version_str = getVersion($extended);
	$version_str =~ s/\n(?!#|\z)/\n#/sg;
	$main::header_str = "$version_str\n" .
	  '#' . scalar(localtime($^T)) . "\n" .
	    '#' . scalar(getCommand(1)) . "\n\n";
      }
    elsif($extended && !defined($main::header_str_ext))
      {
	my $version_str = getVersion($extended);
	$version_str =~ s/\n(?!#|\z)/\n#/sg;
	$main::header_str_ext = "$version_str\n" .
	  '#' . scalar(localtime($^T)) . "\n" .
	    '#' . scalar(getCommand(1)) . "\n\n";
      }

    return($extended ? $main::header_str_ext : $main::header_str);
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

#This subroutine checks to see if stubs are unique.  If they are not, it
#compounds the name using the names of other input files.  Resulting names
#could still be non-unique if input files from different source directories
#have the same name and are being output to the same directory.  It edits the
#array pointed to by the submitted reference.
sub makeStubsUnique
  {
    my $stub_sets  = $_[0];
    my $index_uniq = [map {{}} @{$stub_sets->[0]}]; #Array of hashes
    my $is_index_unique = [map {1} @{$stub_sets->[0]}];
    my $delim = '.';

    debug("Called.") if($DEBUG < 0);

    foreach my $stub_set (@$stub_sets)
      {
	foreach my $type_index (0..$#{$stub_set})
	  {
	    if($#{$index_uniq} < $type_index)
	      {
		error("Critical internal error: type index too big.");
		quit(-25);
	      }
	    if(defined($stub_set->[$type_index]) &&
	       exists($index_uniq->[$type_index]->{$stub_set->[$type_index]}))
	      {$is_index_unique->[$type_index] = 0}
	    $index_uniq->[$type_index]->{$stub_set->[$type_index]} = 1;
	  }
      }

    #Find the first unique index if one exists - we'll use it to hopefully make
    #other stubs unique
    my($first_unique_index);
    foreach my $index (0..$#{$is_index_unique})
      {
	if($is_index_unique->[$index])
	  {
	    $first_unique_index = $index;
	    debug("Unique index: [$index].") if($DEBUG < 0);
	    last;
	  }
	if($index == 0)
	  {warning("The first set of input files has a potential over-write ",
		   "conflict.  Compounding output file stubs.  Use --verbose ",
		   "to see the output file names that were constructed.")}
      }

    foreach my $stub_set (@$stub_sets)
      {
	foreach my $type_index (0..$#{$stub_set})
	  {
	    debug("Index $type_index is ",($is_index_unique->[$type_index] ?
					   '' : 'not '),"unique.")
	      if($DEBUG < 0);

	    #Next if the stubs at this type index are already unique or if this
	    #stub is undefined
	    next if($is_index_unique->[$type_index] ||
		    !defined($stub_set->[$type_index]));

	    my $stub = $stub_set->[$type_index];
	    $stub =~ s/.*\///;

	    if(defined($first_unique_index))
	      {
		$stub_set->[$type_index] = $stub_set->[$first_unique_index] .
		  $delim . $stub;
	      }
	    else
	      {
		my $tmp_stub = $stub_set->[0];
		$tmp_stub =~ s/(.*\/).*/$1/;
		$stub_set->[$type_index] =
		  $tmp_stub . join($delim,
				   map {s/.*\///;$_} grep {defined($_)}
				   @$stub_set);
	      }
	    debug("New stub: [$stub_set->[$type_index]].") if($DEBUG < 0);
	  }
      }
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

    eval {use Data::Dumper;1} if($DEBUG);

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
		      if($DEBUG < -99);

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
    my $combo = [];
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

* WHAT IS THIS: This script takes a series of candidate files in fasta format
                (e.g. each from a different sample and produced by
                getCandidates.pl), the series of dereplicated fasta sequence
                files that were used to generate them (i.e. generated by
                mergeSeqs.pl), and an optional* global library file (-f)
                required for chimera identification; and generates 4 types of
                output files: a file of
                "real" sequences (see --reals-suffix), a tab-delimited summary
                file of abundances of all real sequences across all samples
                (see --summary-suffix), a file of chimeric sequences* (see
                --chimes-suffix), and a file of aligned chimeric sequences*
                (see --chimes-aln-suffix).
                *Chimeric output is produced only if a library file (-f) is
                provided.  Chimera filtering is done using uchime.  If found,
                chimeras are removed from the reals file.

                For more information on uchime, see the uchime documentation:
                http://drive5.com/uchime/uchime_download.html

* SEQUENCE FORMAT: Fasta or fastq format file containing a set of unique,
  -i,-d,-f         ungapped, aligned, and same-sized sequences and with a
                   unique identifier followed by an abundance value on the
                   defline.  Default defline format looks like this example:

                   >lib_6;size=1002;

                   Any defline format can be used as long as it is consistent,
                   both pieces of information are present, and the identifier
                   is the first unbroken string after the defline character.
                   The unique ID may contain the abundance value.  Use -p to
                   extract the abundance values from deflines not in the above
                   format.

* OUTPUT SEQUENCE FORMAT: Fasta format, the same as the input format,
  --reals-suffix,         untouched.  E.g.:
  --fakes-suffix,
  --chimes-suffix

>div_75;size=11;
TCCACGCCGTAAACGATGTCAACTAGCTGTTGGTCTTATGAATAAGATTAGTGGCGCAGCTAACGCGATAAGTTGACCGCCTGGGGAGTACGGTCGCAAGATTAAAAACTCAAAGGAATTGACGGGGGCCCGCACAAGCGGTGGAGCATGTGGTTTAATTCGATGCAACGCGAAGAACCTTACCTACCCTTGACATACAGTGG
>div_76;size=10;
TCCACGCCGTAAACGATGTCAACTAGCCGTTGGGCCCTTTATGGGTTTAGTGGCGCAGCTAACGCATTAAGTTGACCGCCTGGGGAGTACGGCCGCAAGGTTAAAACTCAAATGAATTGACGGGGGGCCCGCACAAGCGGTGGAGCATGTGGTTTAATTCGATGCAACGCGAAGAACCTTACCTACCCTTGACATCCAGAGAA

* OUTPUT ABUNDANCE FORMAT: Tab-delimited text file where each row represents a
  --summary-suffix(,-o)    different sequence and each column represents a
                           different sample.  The values in the table are
                           abundances of each sequence/sample combination.
                           Only real sequences are represented (those which
                           occurred in at least -k samples and passed chimera
                           filtering (if -f was provided).  Columns in the file
                           are filled in with spaces to make them easier to
                           read with a fixed-width font.  Note that file names
                           are used to form column headers with common
                           extensions and file paths removed.  Example:

#ID   	5_77	5_78	5_79	5_80	5_81	5_82	5_83	5_84	5_85	5_86
lib_73	0   	0   	0   	0   	0   	5   	4   	13  	21  	5   
lib_7 	32  	24  	143 	104 	196 	57  	444 	401 	118 	235 
lib_75	3   	17  	3   	11  	0   	10  	2   	0   	1   	0   
lib_58	12  	17  	11  	11  	4   	11  	4   	0   	4   	2   
lib_14	125 	82  	86  	38  	28  	94  	65  	30  	71  	36  
lib_28	36  	22  	14  	23  	5   	60  	14  	4   	34  	11  
lib_65	15  	2   	1   	2   	12  	9   	2   	6   	10  	2   


* OUTPUT ALIGNMENT FORMAT: See uchime documentation for the -uchimealns option.
  --chimes-aln-suffix      http://drive5.com/uchime/uchime_download.html

end_print

    if($advanced)
      {
	my $header          = getHeader();
	my $extended_header = getHeader(1);

	print << "end_print";
* HEADER FORMAT: If --header is supplied and STANDARD output is not going to
                 the terminal, every output file, including output to standard
                 out, will get a header that is commented using the '#'
                 character (i.e. each line of the header will begin with '#').
                 The format of the standard header looks like this:

$header

                 And here is the format of the --extended header:

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

Sets of input files, each with different output directories can be supplied.
Supply each file set with an additional -i (or --input-file) flag.  Wrap each
set of files in quotes and separate them with spaces.

Output directories (--outdir) can be supplied multiple times in the same order
so that each input file set can be output into a different directory.  If the
number of files in each set is the same, you can supply all output directories
as a single set instead of each having a separate --outdir flag.

Examples:

  $0 -i 'a b c' --outdir '1' -i 'd e f' --outdir '2'

    Resulting file sets: 1/a,b,c  2/d,e,f

  $0 -i 'a b c' -i 'd e f' --outdir '1 2 3'

    Resulting file sets: 1/a,d  2/b,e  3/c,f

If the number of files per set is the same as the number of directories in 1
set are the same, this is what will happen:

  $0 -i 'a b' -i 'd e' --outdir '1 2'

    Resulting file sets: 1/a,d  2/b,e

NOT this: 1/a,b 2/d,e  To do this, you must supply the --outdir flag for each
set, like this:

  $0 -i 'a b' -i 'd e' --outdir '1' --outdir '2'

Other examples:

  $0 -i 'a b c' -i 'd e f' --outdir '1 2'

    Result: 1/a,b,c  2/d,e,f

  $0 -i 'a b c' --outdir '1 2 3' -i 'd e f' --outdir '4 5 6'

    Result: 1/a  2/b  3/c  4/d  5/e  6/f

If this script was modified to handle multiple types of files that must be
processed together, the files which are associated with one another will be
associated in the same manner as the output directories above.  Basically, if
the number of files or sets of files match, they will be automatically
associated in the order in which they were provided on the command line.

end_print
      }

    return(0);
  }

##
## This subroutine prints a usage statement in long or short form depending on
## whether "no descriptions" is true.
##
sub usage
  {
    my $no_descriptions = $_[0];
    my $advanced_mode   = $_[1];

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Grab the first version of each option from the global GetOptHash
    my $options =
      ($no_descriptions ? '[' .
       join('] [',
	    grep {$_ ne '-i'}           #Remove REQUIRED params
	    map {my $key=$_;            #Save the key
		 $key=~s/\|.*//;        #Remove other versions
		 $key=~s/(\!|=.|:.)$//; #Remove trailing getopt stuff
		 $key = (length($key) > 1 ? '--' : '-') . $key;} #Add dashes
	    grep {$_ ne '<>'}           #Remove the no-flag parameters
	    keys(%$GetOptHash)) .
       ']' : '[...]');

    print("\n$script -i \"input file(s)\" [-o outfilestub] $options\n",
	  (!$advanced_mode ? '' :
	   "$script -i \"outfile_stub\" [-o outfilestub] $options < " .
	   "input_file\n"),"\n");

    if($no_descriptions)
      {print("Run with no options for usage.\n")}
    else
      {
	if(!$advanced_mode)
	  {
	    print << 'end_print';
     -i                   REQUIRED Space-separated candidate sequence files.
     -d                   REQUIRED Space-separated dereplicated sequence files.
     -f                   OPTIONAL Dereplicated sequence library file for
                                   chimera filtering.
     -o                   OPTIONAL [-f/stdout*] Stub for naming output files.
                                   *Supply --extended only for details.
     -k                   OPTIONAL [1] The minimum number of times a sequence
                                   must occur as a candidate across all -i
                                   files of a single set in order to be deemed
                                   real.  Must be greater than 0.
     --summary-suffix     OPTIONAL [.smry] Extension appended to the output
                                   stub (-o) for generating a table of "real"
                                   sequence abundances per sample.  This output
                                   goes to standard out if -o is not provided.
     --reals-suffix       OPTIONAL [.reals] Extension appended to the output
                                   stub (-o) for outputting "real" sequences.
     --fakes-suffix       OPTIONAL [no output] Extension appended to the output
                                   stub (-o) for outputting "fake" sequences
                                   (fewer candidate occurrences than -k).
     --chimes-suffix      OPTIONAL [.chim] Extension appended to the output
                                   stub (-o) for outputting chimeric sequences.
     --chimes-aln-suffix  OPTIONAL [.chim-aln] Extension appended to the output
                                   stub (-o) for outputting chimeric sequences.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Also see --help.
     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --quiet              OPTIONAL Quiet mode.
     --skip-existing      OPTIONAL Skip existing output files.
     --overwrite          OPTIONAL Overwrite existing output files.
     --force              OPTIONAL Ignore critical errors.
     --header             OPTIONAL Print commented script version, date, and
                                   command line call to each outfile.
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
     --error-type-limit   OPTIONAL [50] Limit number of outputs for each error/
                                   warning type.  0 = no limit.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info.
     --use-as-default     OPTIONAL Save the command line arguments.
     --help               OPTIONAL Print info and format descriptions.
     --extended           OPTIONAL Print extended usage/help/version/header
                                   when supplied with corresponding the flags.

Run with just --extended for a more descriptive usage & additional options.
end_print
	  }
	else #Advanced options/extended usage output
	  {
	    print << 'end_print';
     -i,                  REQUIRED Space-separated input candidate sequence
     --candidate-seq-file*         file(s) inside quotes (e.g. -i "*.txt
                                   *.text").  Expands standard bsd glob
                                   characters (e.g. '*', '?', etc.).  There
                                   must be the same number and order of files
                                   as provided to -d.  All IDs are expected to
                                   be unique and consistent with those in the
                                   -d and -f files.  When standard input
                                   detected, -o has been supplied, and -i is
                                   given only 1 value, it will be used as a
                                   file name stub.  See --extended --help for
                                   input file format and more advanced usage
                                   examples.  *No flag required.
     -d,                  REQUIRED Space-separated input dereplicated sequence
     --dereplicated-files          file(s) inside quotes (e.g. -i "*.txt
                                   *.text").  There must be the same number and
                                   order of files as provided to -i.  All IDs
                                   are expected to be unique and consistent
                                   with those in the -i and -f files.  Expands
                                   standard bsd glob characters (e.g. '*', '?',
                                   etc.).  See --extended --help for input file
                                   format and more advanced usage examples.
     -f,                  OPTIONAL Space-separated input dereplicated sequence
     --merged-seq-file,            library file for chimera filtering.  If a
     --library-file                chimera is found, it will be moved from the
                                   real category to the fake category (see
                                   --reals-suffix).  No chimera filtering will
                                   be performed if not supplied.  There should
                                   be 1 file for  every set of files provided
                                   by one -i flag.  All IDs are expected to be
                                   unique and consistent with those in the -i
                                   and -d files.  Every ID in the -i files must
                                   be present, though more IDs are allowed.
     -o,--outfile,        OPTIONAL [-f/stdout*] Output filename stub for naming
                                   output files.  See the suffix options below.
                                   There should be an outfile stub for every
                                   set of files provided by one -i flag.  If -f
                                   is provided and -o is not provided, the -f
                                   files will be used as stubs.  If neither are
                                   provided, the table output of
                                   --summary-suffix goes to standard out and
                                   all other suffix options are ignored.
     -k,--min-candidacies OPTIONAL [1] The minimum number of times a sequence
                                   must occur as a candidate across all -i
                                   files of a single set in order to be deemed
                                   real.  Must be greater than 0.
     -t,                  OPTIONAL [auto](fasta,fastq,auto) Input file (-i, -d,
     --sequence-filetype           -f) type.  Using this instead of auto will
                                   make file reading faster.  "auto" cannot be
                                   used when redirecting a file in.
     -p,                  OPTIONAL [size=(\\d+);] A perl regular expression
     --abundance-pattern           used to extract the abundance value from the
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
     -s,--summary-suffix  OPTIONAL [.smry] Extension appended to the output
                                   stub (-o) for outputting a table of "real"
                                   sequence abundances per sample.  Defaults to
                                   standard out if -o is not provided
                                   regardless of the extension provided here.
     -r,--reals-suffix    OPTIONAL [.reals] Outfile extension appended to the
                                   output file stubs (-o) to create a fasta
                                   output file of "real" sequences (not a
                                   chimeric sequence (if -f provided) and
                                   occurs as a candidate in >= -k samples).
                                   No such output will be generated if no
                                   suffix is supplied.  See --help for file
                                   format and advanced usage.  See --fakes-
                                   suffix and --chimes-suffix to also save
                                   "fakes" and chimeras in separate files.
                                   Requires -o.
     --fakes-suffix       OPTIONAL [no output] Extension appended to the output
                                   stub (-o) for outputting "fake" sequences
                                   (fewer candidate occurrences than -k).  May
                                   contain unidentified chimeras that did not
                                   pass the -k candidate threshold.  Requires
                                   -o.
     --chimes-suffix      OPTIONAL [.chim] Outfile extension appended to the
                                   output file stubs (-o) to create a fasta
                                   output file of sequences which passed the -k
                                   candidate threshold, but were identified by
                                   uchime as chimeric.  If not supplied,
                                   chimeras will not be culled from the "real"
                                   sequences.  See --help for file format and
                                   advanced usage.  See --reals-suffix to save
                                   real sequences in a separate file.
                                   Requires -o.
     --chimes-aln-suffix  OPTIONAL [.chim-aln] Extension appended to the output
                                   file stub (-o) for outputting chimeric
                                   sequences.  This is the output created by
                                   uchime, given the -uchimealns option.  Only
                                   used when -f is provided.
     -y,--uchime-exe      OPTIONAL [usearch -uchime_denovo] The command to use
                                   to call uchime.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Default output
                                   directory is the same as that containing
                                   each input file.  Relative paths will be
                                   relative to each individual input file.
                                   Creates directories specified, but not
                                   recursively.  Also see --extended --help for
                                   more advanced usage examples.
     --verbose            OPTIONAL Verbose mode/level (e.g. --verbose 2).
                                   Prints a run report with a summary of errors
                                   & warnings at the completion of the script.
     --quiet              OPTIONAL Quiet mode.
     --skip-existing      OPTIONAL Skip existing output files.
     --overwrite          OPTIONAL Overwrite existing output files.  By
                                   default, existing output files will not be
                                   over-written.  See also --skip-existing.
     --force              OPTIONAL Prevent script-exit upon critical error and
                                   continue processing.  Supply twice to
                                   additionally prevent skipping the processing
                                   of input files that cause errors.  Use this
                                   option with extreme caution.  This option
                                   will not over-ride over-write protection.
                                   See also --overwrite or --skip-existing.
     --header             OPTIONAL Print commented script version, date, and
                                   command line call to each output file.  If
                                   --extended is provided, information on the
                                   template used to create this script is
                                   printed as well.
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
     --error-type-limit   OPTIONAL [50] Limits each type of error/warning to
                                   this number of outputs.  Intended to
                                   declutter output.  Note, a summary of
                                   warning/error types is printed when the
                                   script finishes, if one occurred or if in
                                   verbose mode.  0 = no limit.  See also
                                   --quiet.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info.  Includes template
                                   version with --extended.
     --use-as-default,    OPTIONAL Save the command line arguments.  Saved
     --save-as-default             defaults are printed at the bottom of this
                                   usage output.  Supplying this flag replaces
                                   those defaults with all options that are
                                   provided with this flag.  Values are stored
                                   in ~/.rpst/.
     --help               OPTIONAL Print general info and file format
                                   descriptions.  Includes advanced usage
                                   examples with --extended.
     --extended           OPTIONAL Print extended usage/help/version/header.
                                   Supply alone for extended usage.  Includes
                                   extended version in output file headers.
                                   See --help, --header, and --version.

end_print
	  }

	my @user_defaults = getUserDefaults();
	print(scalar(@user_defaults) ?
	      "Current user defaults: [@user_defaults].\n" :
	      "No user defaults set.\n");
      }

    return(0);
  }

sub incompatible
  {
    my $uchime = $_[0];
    my $exe = $uchime;
    $exe =~ s/ .*//;

    if(!defined($uchime) || $uchime eq '' || !-e $exe || !-x $exe)
      {
	error("The uchime executable [$exe] appears to either not be in ",
	      "your path, not exist, not have execute permissions, or you ",
	      "have not created a symbolic link named 'usearch' to the full ",
	      "name of the executable with version number.  If you have not ",
	      "installed uchime, you can find it here: http://drive5.com/",
	      "uchime/uchime_download.html");
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

    return(0);
  }

sub getUchimeExe
  {
    my $uchime   = $_[0];
    my $sent_exe = $uchime;
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

#Uses globals: $uchime, $dry_run, $quiet
sub getUchimeReals
  {
    my $lib_file          = $_[0];
    my $seqs_array        = $_[1];
    my $uchime_exe        = defined($_[2]) ? $_[2] : $uchime;
    my $hash              = $_[3];
    my $id_lookup         = $_[4];
    my $aln_file          = $_[5];
    my $abundance_pattern = $_[6];
    my $seq_id_pattern    = $_[7];
    my $tmpdir            = $_[8];
    my $tmp_suffix        = $_[9];
    my $output_file       = $_[10];

    return($seqs_array,[]) if($dry_run);

    if(scalar(@$seqs_array) == 0)
      {return([],[])}

    #Assume all the sequences are the same length
    my $seqlen = length($seqs_array->[0]);

    #Create a temporary global candidates file
    my $stub = $lib_file;
    $stub =~ s/.*\///;
    $stub = ($tmpdir =~ m%/$% ? $tmpdir : "$tmpdir/") . $stub;
    my $gcand_file = $stub . '.' . time() . '.cands' . $tmp_suffix;
    my $tmp_out_file = $stub . '.' . time() . '.reals' . $tmp_suffix;

    verbose("\nCreating temporary files for running uchime.\n\n");

    checkFile($gcand_file)        || return($seqs_array,[]);
    openOut(*TMP,$gcand_file,1,1) || return($seqs_array,[]);
    print(map {(exists($id_lookup->{$output_file}) &&
		exists($id_lookup->{$output_file}->{$_}) ?
		">$id_lookup->{$output_file}->{$_}->{ID};" : '>') . 'size='
		  . (exists($hash->{$output_file}) &&
		     exists($hash->{$output_file}->{GLOBAL_ABUND}) ?
		     $hash->{$output_file}->{GLOBAL_ABUND} : '1')
		    . ";\n$_\n"} @$seqs_array);
    closeOut(*TMP);

    #Create the temporary library file (in case their format does not match)
    my $tmp_lib = $stub . '.' . time() . ".tmplib$tmp_suffix";
    #Don't need to check a temporary file
    openOut(*TMP,$tmp_lib,1,1) || return($seqs_array,[]);

    #Read in the library file
    openIn(*LIB,$lib_file) || $force > 1 || return($seqs_array,[]);

    my $verbose_freq = 100;
    my $cnt          = 0;
    my $id_check     = {};
    my $all_there    = {};

    while(my $rec = getNextSeqRec(*LIB))
      {
	my($def,$seq) = @$rec;
	$seq          = uc($seq);
	my $id        = '';
	my $abundance = 1;
	$cnt++;

	if($def =~ /\s*[\@\>]\s*(\S+)/)
	  {
	    my $default_id = $1;

	    if($seq_id_pattern ne '' && $def =~ /$seq_id_pattern/)
	      {$id = $1}
	    else
	      {
		$id = $default_id;
		warning("Unable to parse seqID from defline: [$def] ",
			"in library file: [$lib_file] using pattern: ",
			"[$seq_id_pattern].  Please either fix the ",
			"defline or use a different pattern to extract ",
			"the seqID value.  Using default ID: [$id].  ",
			"Use \"-q ''\" to to avoid this warning.")
		  if($seq_id_pattern ne '');
	      }
	  }
	else
	  {
	    warning("Could not parse defline in record [$cnt] of library ",
		    "file [$lib_file]: [$def].  Please edit the file to ",
		    "contain IDs.");
	  }

	if($def =~ /$abundance_pattern/)
	  {$abundance = $1}
	else
	  {
	    warning("Unable to parse abundance from defline: [$def] in ",
		    "record $cnt of library file: [$lib_file] using ",
		    "pattern: [$abundance_pattern].  Please either fix the ",
		    "defline or use a different pattern (-p) to extract the ",
		    "abundance value.  Assuming abundance is 1.");
	  }

	$id_check->{$id}++;
	$all_there->{$seq} = $id;

	print(">$id;size=$abundance\n$seq\n");
      }

    closeIn(*LIB);
    closeOut(*TMP);

    my @ambigs = grep {$id_check->{$_} > 1} keys(%$id_check);
    if(scalar(@ambigs))
      {
	warning("Library file: [$lib_file] contains ambiguous IDs.  ",
		"These IDs were found on the indicated number of deflines: [",
		join(',',map {"$_:$id_check->{$_}"} @ambigs),"].  The ",
		"resulting output table [$output_file] will contain ",
		"ambigous IDs.");
      }

    #Check that all the sequences of the candidates are present in the library
    #and that their IDs are the same
    foreach my $seq (@$seqs_array)
      {
	if(!exists($all_there->{$seq}))
	  {
	    error("The sequence with ID: ",
		  "[$id_lookup->{$output_file}->{$seq}->{ID}] was not found ",
		  "in the library file: [$lib_file]",
		  (exists($id_check->{$id_lookup->{$output_file}->{$seq}
				      ->{ID}}) ?
		   ", however (strangely) the ID appears to be present in " .
		   "the library, presumably with a different sequence." :
		   ' and the ID is missing too.'));
	  }
	elsif($all_there->{$seq} ne $id_lookup->{$output_file}->{$seq}->{ID})
	  {
	    error("A sequence with ID [$all_there->{$seq}] in library file ",
		  "[$lib_file] has a different ID [",
		  $id_lookup->{$output_file}->{$seq}->{ID},
		  "] in the candidate files.");
	  }
      }

    #Create a temporary file to hold the intermediate uchime output
    my $cands_with_global_abund = $stub . '.' . time() .
      ".globcands$tmp_suffix";

    #Create the optional alignment storage argument/file
    my $aln_arg = defined($aln_file) && $aln_file ne '' ?
      "-uchimealns $aln_file" : '';

    if($DEBUG < -99)
      {
	debug("Head of temporary candidates file $gcand_file:\n",
	      `head $gcand_file`)
      }

    my $uchime_command1 = "$uchime_exe -usearch_global $tmp_lib -db " .
      "$gcand_file -id 1.0 -idprefix $seqlen -matched " .
	"$cands_with_global_abund -strand plus -quiet" .
	  ($quiet ? ' 1> /dev/null 2> /dev/null' : '');
    my $uchime_command2 = "$uchime_exe -uchime_denovo " .
      "$cands_with_global_abund -minuniquesize 2 -nonchimeras $tmp_out_file " .
	"$aln_arg -quiet" . ($quiet ? ' 1> /dev/null 2> /dev/null' : '');

    #Identify the chimeras
    my $real_seqs = {};
    my $chimeras  = [];

    verbose("\nFirst uchime call:\n\n$uchime_command1\n");

    `$uchime_command1`;

    verbose("\nSecond uchime call:\n\n$uchime_command2\n\n");

    `$uchime_command2`;

    openIn(*UCHIME,$tmp_out_file);

    while(my $rec = getNextSeqRec(*UCHIME))
      {
	my($def,$seq) = @$rec;
	$seq = uc($seq);
	$real_seqs->{$seq} = 1;
      }

    closeIn(*UCHIME);

    foreach my $seq (@$seqs_array)
      {unless(exists($real_seqs->{$seq}))
	 {push(@$chimeras,$seq)}}

    unlink($tmp_lib);
    unlink($cands_with_global_abund);
    unlink($gcand_file);
    unlink($tmp_out_file);

    return([keys(%$real_seqs)],$chimeras);
  }

#Takes a tab-delimited string and inserts spaces before the tabs to even the
#column display
sub straightenColumns
  {
    my $str = $_[0];
    my $newstr = '';
    my @array = ();
    my @maxes = ();
    foreach my $line (split(/\n/,$str,-1))
      {
	push(@array,[split(/\t/,$line,-1)]);
	for(my $i = 0;$i < scalar(@{$array[-1]});$i++)
	  {
	    if((scalar(@maxes) - 1) < $i ||            #No max yet
	       length($array[-1]->[$i]) > $maxes[$i])  #current bigger than max
	      {$maxes[$i] = length($array[-1]->[$i])}
	  }
      }

    $newstr = join("\n",
		   map {
		     my $row = $_;
		     join("\t",
			  map {
			    $row->[$_] .
			      (' ' x ($maxes[$_] - length($row->[$_])))
			  }
			  (0..$#{$row}))
		   } @array);

    return($newstr);
  }

#Uses global variables: lastfiletype, filetype, & $input_file
sub getNextSeqRec
  {
    debug("Determining previous type");

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
	debug("Determining type");

	if($input_file eq '-')
	  {
	    error("`-t auto` cannot be used when the input file is supplied ",
		  "on standard input.  Please supply the exact file type.");
	    quit(2);
	  }

	if(!-e $input_file || $input_file =~ / /)
	  {
	    error("`-t auto` cannot be used when the input file ",
		  "[$input_file] does not exist or has a space in its name.  ",
		  "Please supply the exact file type.");
	    quit(3);
	  }

	my($num_fastq_defs);
	if(-e $input_file)
	  {
	    $num_fastq_defs =
	      `head -n 50 $input_file | grep -c -E '^[\@\+]'`;
	    debug("System output from: [",
		  qq(head -n 50 $input_file | grep -c -E '^[\@\+]'),
		  "]:\n$num_fastq_defs");
	    $num_fastq_defs =~ s/^\D+//;
	    $num_fastq_defs =~ s/\D.*//;
	  }
	else
	  {$num_fastq_defs = 0}

	if($num_fastq_defs > 0)
	  {
	    $main::getnextsub = \&getNextFastqRec;
	    $main::lastfiletype->{$input_file} = 'fastq';
	  }
	else
	  {
	    my($num_fasta_defs);
	    if(-e $input_file)
	      {
		$num_fasta_defs = `head -n 50 $input_file | grep -c -E '^>'`;

		debug("System output from: [",
		      qq(head -n 50 $input_file | grep -c -E '^>'),
		      "]:\n$num_fasta_defs");

		$num_fasta_defs =~ s/^\D+//;
		$num_fasta_defs =~ s/\D.*//;
	      }
	    else
	      {$num_fasta_defs = 0}

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
		    debug("Num fasta deflines: [$num_fasta_defs].");
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

    debug("Returning record");

    return($main::getnextsub->(@_));
  }

#Copied from DNAstiffness.pl on 2/12/2014 so as to be independent -Rob
sub getNextFastaRec
  {
#    my $self       = shift(@_);
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
#on 2/12/2014 - have not yet tested
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

sub getVariablePrefixes
  {
    my @strs          = ref($_[0]) eq 'ARRAY' ? @{$_[0]} : @_;
    my $smallest_size = length((sort {length($a) <=> length($b)} @strs)[0]);

    foreach my $size (map {$smallest_size - $_} (0..($smallest_size - 1)))
      {
	my $suffix_hash = {};
	foreach my $str (@strs)
	  {$suffix_hash->{substr($str,-$size)} = 1}
	if(scalar(keys(%$suffix_hash)) == 1)
	  {return(wantarray ? map {substr($_,0,-$size)} @strs :
		  [map {substr($_,0,-$size)} @strs])}
      }

    return(wantarray ? @strs : [@strs]);
  }
