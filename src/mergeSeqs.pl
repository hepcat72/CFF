#!/usr/bin/perl
#Note: 'use warnings' is below instead of having -w above

#Generated using perl_script_template.pl 2.14
#Robert W. Leach
#Princeton University
#Carl Icahn Laboratory
#Lewis Sigler Institute for Integratove Genomics
#Bioinformatics Group
#Room 133A
#Princeton, NJ 08544
#rleach@genomics.princeton.edu
#Copyright 2014

#These variables (in main) are used by getVersion() and usage()
my $software_version_number = '2.17';
my $created_on_date         = '3/26/2014';

##
## Start Main
##

use warnings; #Same as using the -w, only just for code in this script
use strict;
use Getopt::Long qw(GetOptionsFromArray);
use File::Glob ':glob';

#This will allow us to track runtime warnings about undefined variables, etc.
local $SIG{__WARN__} = sub {my $err = $_[0];chomp($err);
			    warning("Runtime warning: [$err].")};

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix,$tab_suffix); #Not defined so input can be overwritten
my $outfiles            = [];
my $outdirs             = [];
my $mapfiles            = [];
my $input_files         = [];
my $help                = 0;
my $extended            = 0;
my $version             = 0;
my $overwrite           = 0;
my $skip_existing       = 0;
my $header              = 0;
my $error_limit         = 5;
my $dry_run             = 0;
my $use_as_default      = 0;
my $defaults_dir        = (sglob('~/.rpst'))[0];
my $filetype            = 'auto';
my $abundance_pattern   = 'size=(\d+);';
my $seq_id_pattern      = '^\s*[>\@]\s*([^;]+)';
my $value_subst_str     = '__VALUE_HERE__';
my $id_delim            = "lib_$value_subst_str;";
my $abund_delimiter     = "size=$value_subst_str;";
my $append_defline      = 0;
my $trim_size           = 0;  #-1 = detect shortest sequence and trim to that
                              #length, 0 = no trim & error if not all the same
                              #size, >0 = trim to that size and throw anything
                              #out that is shorter

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args     = [@ARGV];  #Preserve the agruments for getCommand
my $num_explicit_args = scalar(@ARGV);
my $verbose           = 0;
my $quiet             = 0;
my($DEBUG);
my $force             = 0;
my @user_defaults     = getUserDefaults(1);

my $GetOptHash =
  {'i|seq-file=s'          => sub {push(@$input_files,   #REQUIRED unless <>
					[sglob($_[1])])},#         is supplied
   '<>'                    => sub {push(@$input_files,   #REQUIRED unless -i
					[sglob($_[0])])},#         is supplied
   'b|trim-to-size=s'      => \$trim_size,               #OPTIONAL [0] (-1=auto
				                         #           0=no trim)
   'f|merged-seq-file=s'   => sub {push(@$outfiles,      #OPTIONAL [stdout]
					sglob($_[1]))},
   'u|merged-tab-file=s'   => sub {push(@$mapfiles,      #OPTIONAL [no output]
					sglob($_[1]))},
   'o|seq-suffix=s'        => \$outfile_suffix,          #OPTIONAL [undef]
   'x|tab-suffix=s'        => \$tab_suffix,              #OPTIONAL [undef]
   'outdir=s'              => sub {push(@$outdirs,       #OPTIONAL [none]
					[sglob($_[1])])},
   't|filetype=s'          => \$filetype,                #OPTIONAL [auto]
				                         #  (fasta,fastq,auto)
   'p|abundance-pattern=s' => \$abundance_pattern,       #OPTIONAL
                                                         #       [size=(\d+);]
   'q|seq-id-pattern=s'    => \$seq_id_pattern,          #OPTIONAL
                                                         #[^\s*[>\@]\s*([^;]+)]
   'd|abund-delimiter=s'   => \$abund_delimiter,         #OPTIONAL
				                         # [size=_VL_HERE_;]
   'g|id-delimiter=s'      => \$id_delim,                #OPTIONAL
				                         # [lib__VL_HERE_;]
   'c|append-to-defline!'  => \$append_defline,          #OPTIONAL [Off]
   'overwrite'             => \$overwrite,               #OPTIONAL [Off]
   'skip-existing!'        => \$skip_existing,           #OPTIONAL [Off]
   'force'                 => \$force,                   #OPTIONAL [Off]
   'verbose:+'             => \$verbose,                 #OPTIONAL [Off]
   'quiet'                 => \$quiet,                   #OPTIONAL [Off]
   'debug:+'               => \$DEBUG,                   #OPTIONAL [Off]
   'help'                  => \$help,                    #OPTIONAL [Off]
   'extended'              => \$extended,                #OPTIONAL [Off]
   'version'               => \$version,                 #OPTIONAL [Off]
   'header!'               => \$header,                  #OPTIONAL [On]
   'error-type-limit=s'    => \$error_limit,             #OPTIONAL [0]
   'dry-run!'              => \$dry_run,                 #OPTIONAL [Off]
   'use-as-default|save-as-default!'
                           => \$use_as_default,          #OPTIONAL [Off]
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

#If there are no arguments (or it's just the extended flag) and no files
#directed or piped in
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

$DEBUG = 0 if(!defined($DEBUG));

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
#that verbose messages may be uncleanly overwritten
if($verbose && scalar(@$outfiles) == 0 && isStandardOutputToTerminal())
  {warning('You have enabled --verbose, but appear to possibly be ',
	   'outputting to the terminal.  Note that verbose messages can ',
	   'interfere with formatting of terminal output making it ',
	   'difficult to read.  You may want to either turn verbose off, ',
	   'redirect output to a file, or supply a merged seq file (-f).')}

#Make sure there is input
if(scalar(@$input_files) == 0 && isStandardInputFromTerminal())
  {
    error('No input files detected.');
    usage(1);
    quit(-7);
  }
#elsif(!isStandardInputFromTerminal())
#  {push(@$input_files,['-']) unless(scalar(grep {$_ eq '-'} map {@$_}
#					   @$input_files))}

#Make sure that an outfile suffix has been supplied if an outdir has been
#supplied
if(scalar(@$outdirs) && !defined($outfile_suffix) && !defined($tab_suffix))
  {
    error("A sequence suffix (-o) or tab suffix (-x) is required if an ",
	  "output directory (--outdir) is supplied.");
    quit(-8);
  }

my $num_outfiles = (scalar(@$outfiles) ? scalar(@$outfiles) :
		    (isStandardOutputToTerminal() ? 0 : 1));

##THIS CASE SHOULD GET CAUGHT BY GETFILESETS
##Make sure the number of outfiles equals the number of input file groups
#if($num_outfiles && $num_outfiles != 1 &&
#   $num_outfiles != scalar(@$input_files))
#  {
#    error("The number of output files (including when going to standard out ",
#	  "only) must either be 1 or equal to the number of times -i (an ",
#	  "input file group to be merged) is supplied.");
#    quit(3);
#  }

#SPECIAL CASE ENSURING NUMBER OF OUTFILES AND MAPFILES IS EQUAL - FIRST PART
#SHOULD ALSO GET CAUGHT BY GETFILESETS
#If (there are multiple output map files and their number is not equal to the
#number of input file groups) or (there are sequence output files and output
#map files and there are not the same number of them)
if(#(scalar(@$mapfiles) > 1 && scalar(@$mapfiles) != scalar(@$input_files)) ||
   ($num_outfiles && scalar(@$mapfiles) &&
    $num_outfiles != scalar(@$mapfiles)))
  {
    error("The number of merged tab files (-u), if supplied, must either be ",
	  "1 or equal to the number of times -i (an input file group to be ",
	  "merged) is supplied and if merged seq files are supplied (even ",
	  "via redirect of standard out) it must be equal to the number of ",
	  "merged seq files.");
    quit(4);
  }

#SPECIAL CASE ENSURING NUMBER OF OUTFILES AND MAPFILES IS EQUAL
#If there is 1 output sequence file and more than 1 output map files
if($num_outfiles == 1 && scalar(@$mapfiles) > 1# &&
#   scalar(@$mapfiles) == scalar(@$input_files))
  )
  {
    error("Merged sequence output (-f) is set to be aggregated in 1 file, ",
	  "but the number of merged tab files (-u) is set to be generated ",
	  "per input file (-i).  If you are directing standard out ",
	  "to /dev/null to just generate the merged tab files, keep -u as it ",
	  "is and simply do not provide -f and do not redirect output to ",
	  "/dev/null and the sequence output will not be generated.");
    quit(5);
  }
#When no outfiles are specified (either explicit or via redirect) and either no
#mapfiles or 1 map file is specified, aggregate sequence output on STDOUT
elsif($num_outfiles == 0 && scalar(@$mapfiles) <= 1)
  {$num_outfiles = 1}

#Get all the corresponding groups of files and output directories to process
my($input_file_sets,
   $outfile_stub_sets) = getFileSets($input_files,

				     #Submit the outfiles and mapfiles too so
				     #that the input files get grouped
				     #accordingly

				     #Still want to group if there will be
				     #aggregated output on STDOUT, so supply a
				     #dummy array for the STDOUT output
				     (scalar(@$outfiles) == 0 &&
				      $num_outfiles ?
				      [['STDOUT']] : [$outfiles]),

				     [$mapfiles],
				     $outdirs);

my(@existing_outfiles);

#Look for existing output files generated by the input_files array
#Note, the index for the outfile_stub_sets array in the call to
#getExistingOutfiles below corresponds to the position ofthe input_files array
#in the above call to getFileSets
my @tmp_existing_outfiles = getExistingOutfiles($outfile_stub_sets->[0],
						$outfile_suffix);
push(@existing_outfiles,@tmp_existing_outfiles)
  if(scalar(@tmp_existing_outfiles));

@tmp_existing_outfiles = getExistingOutfiles($outfile_stub_sets->[0],
					     $tab_suffix);
push(@existing_outfiles,@tmp_existing_outfiles)
  if(scalar(@tmp_existing_outfiles));

#Check for existing outfiles and mapfiles
my @existing_group_outfiles = getExistingOutfiles([@$outfiles,@$mapfiles]);
if(scalar(@existing_group_outfiles))
  {push(@existing_outfiles,@existing_group_outfiles)}

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

if($abundance_pattern ne '' &&
   $abundance_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$abundance_pattern = '(' . $abundance_pattern . ')'}

if($seq_id_pattern ne '' &&
   $seq_id_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$seq_id_pattern = '(' . $seq_id_pattern . ')'}

if($abund_delimiter !~ /./)
  {warning("The abundance delimiter (-d) is empty.  The abundance value on ",
	   "the output deflines might be difficult to distinguish from the ",
	   "rest of the line.")}

if($id_delim !~ /./)
  {warning("The global ID delimiter (-g) is empty.  The global ID on the ",
	   "output deflines might be difficult to distinguish from the rest ",
	   "of the line.")}

#Determine whether we are merging files by checking if the number of map files
#or outfiles corresponds to the number of input file groups (or is 1)
my $isglobal = (#There are the same number of map files as input file groups
		(scalar(@$mapfiles) == scalar(@$input_files)) ||
		#There is 1 map file
		scalar(@$mapfiles) == 1 ||

		#There are the same number of out files as input file groups
	        ($num_outfiles == scalar(@$input_files)) ||
		#There is 1 out file
		$num_outfiles == 1);

if(!$isglobal && (defined($outfile_suffix) || defined($tab_suffix)))
  {
    error("Output is not set to be merged (because either the number of ",
	  "files supplied to -u/-f matches the number of input files) AND ",
	  "either -o or -x has been supplied.  -o and -x only work if there ",
	  "is one merged output file (-u or -f) per set of input files.  ",
	  "Please either supply one -u/-f per input file set or do not ",
	  "supply -o or -x.");
    quit(6);
  }

if(!defined($trim_size))
  {
    error("Trim size (-b) is required.");
    usage(1);
    quit(7);
  }

if($trim_size !~ /^-?\d+$/)
  {
    error("Invalid trim size (-b): [$trim_size].");
    quit(7);
  }
elsif($trim_size > 0)
  {
    if($trim_size < 32)
      {warning("Trim size size is really small: [$trim_size].  A larger size ",
	       "is recommended but not required.  Proceeding.")}
    verbose("Trimming to size: [$trim_size].");
  }

#If output is going to STDOUT instead of output files with different extensions
#or if STDOUT was redirected, output run info once
verbose('[STDOUT] Opened for all output.') if($num_outfiles == 0 ||
					      scalar(@$outfiles) == 0 &&
					      $num_outfiles == 1);

#Store info. about the run as a comment at the top of the output file if
#STDOUT has been redirected to a file
if(!isStandardOutputToTerminal() && $header)
  {print(getHeader($extended))}

my $global_check     = {};
my $cnt              = 0;
my $seq_hash         = {};
my $abundance_hash   = {};
my $abund_check      = {};
my $opened_this_loop = 0;
my $input_file       = '';
my $skipped          = {};
my $seen_outfiles    = {};
my $partner_file     = {};
my $sample_seqfile   = '';
my $sample_tabfile   = '';
my $sample_hash      = {};
my($last_len);

if($isglobal)
  {debug("Aggregate output mode with [",scalar(@$input_files),
	 "] input file groups, [$num_outfiles] output files, and [",
	 scalar(@$mapfiles),"] map files.")}
else
  {debug("Sample output mode with [",scalar(@$input_files),
	 "] input file groups, [$num_outfiles] output files, and [",
	 scalar(@$mapfiles),"] map files.")}

my $abund_parse_errs = {};
my $diff_sizes       = 0;
my $size_error_mode  = ($trim_size == 0 ? 1 : 0);
my $got_something    = 0;
my $got_anything     = 0;

#foreach my $input_file_set (@$input_files)
foreach my $set_num (0..$#$input_file_sets)
  {
    $input_file         = $input_file_sets->[$set_num]->[0];
    my $tmp_cur_outfile = $input_file_sets->[$set_num]->[1];
    my $current_mapfile = $input_file_sets->[$set_num]->[2];
    my $outfile_stub    = $outfile_stub_sets->[$set_num]->[0];
    my $current_outfile = $outfile_stub_sets->[$set_num]->[1];

    #If an explicit path was provided with the output library file, use it
    #instead of the one with the outdir replacement. This is, after all, an
    #outfile option.
    if($tmp_cur_outfile =~ m%/% || $tmp_cur_outfile eq 'STDOUT')
      {$current_outfile = $tmp_cur_outfile}

    if(defined($current_outfile) && $current_outfile eq 'STDOUT')
      {undef($current_outfile)}
    if(defined($input_file) && $input_file eq 'STDOUT')
      {undef($input_file)}

    next if(!defined($input_file)); #This is a kludge - getFileSets shouldn't be returning undef as an input file when input is detected on stdin and supplemented with files supplied via -i.  I should make it either max out the array dimensions or have it support input files arrays that have a variable number of columns.

    my $file_key  = (defined($current_outfile) ? $current_outfile : '');
    my $file_key2 = (defined($current_mapfile) ? $current_mapfile : '');

    $partner_file->{$file_key}->{$file_key2} = 1;

    if(!$isglobal)
      {
	#Reset variables and clear out the memory
	$cnt            = 0;
	$abundance_hash = {};
	undef($last_len);

	if(defined($current_outfile) &&
	   !exists($seen_outfiles->{$current_outfile}))
	  {
	    checkFile($current_outfile,$input_file_sets->[$set_num]) || next;

	    $seen_outfiles->{$current_outfile} = 1;

	    #Attempt to open the outfiles first so that if there's a problem,
	    #we find out before taking the time to process the input files
	    openOut(*OUTPUT,$current_outfile,1) || next;
	  }
	#Else if we've switched back to an outfile we were outputting to
	#before, re-open it for appending
	elsif(defined($current_mapfile))
	  {openOut(*OUTPUT,$current_outfile,1,1) || next}

	if(defined($current_mapfile) &&
	   !exists($seen_outfiles->{$current_mapfile}))
	  {
	    checkFile($current_mapfile,$input_file_sets->[$set_num]) || next;

	    #Attempt to open the outfiles first ]]so that if there's a problem,
	    #we find out before taking the time to process the input files
	    openOut(*ABUND,$current_mapfile,0) || closeOut(*OUTPUT) && next;
	    #Print he header
	    if($header)
	      {print ABUND ("#GlobalID\tAbundance\n")}
	  }
	#Else if we've switched back to an outfile we were outputting to
	#before, re-open it for appending
	elsif(defined($current_mapfile))
	  {openOut(*ABUND,$current_mapfile,1,1) || next}
      }

    openIn(*INPUT,$input_file) || next;

    my $verbose_freq = 100;
    my $abundance    = 0;
    my $skip         = 0;
    my $rec_num      = 0;
    my $warn_dupes   = 0;
    my($rec);

    debug("Reading in [$input_file]");

    my $recs = [];
    my($smallest,$smallest_id);
    if($trim_size < 0)
      {
	while($rec = getNextSeqRec(*INPUT,0,$input_file))
	  {
	    push(@$recs,$rec);
	    my $s = length($rec->[1]);
	    if(!defined($smallest) || $s < $smallest)
	      {
		$smallest    = $s;
		$smallest_id = $rec->[0];
	      }
	  }
	if(!defined($smallest))
	  {
	    warning("Could not find smallest sequence in file ",
		    "[$input_file].  Skipping.  Use --force to over-ride.")
	      unless(-z $input_file);
	    next unless($force);
	  }
	$trim_size = $smallest;
	if($trim_size < 32)
	  {warning("Smallest sequence size is really small: [$smallest].  A ",
		   "larger size is recommended, but not required.  Please ",
		   "use -b on the command line to select a larger trim size.")}
	verbose("Trimming to smallest sequence size: [$smallest] ",
		"($smallest_id) in first file [$input_file].");
      }
    else
      {@$recs = getNextSeqRec(*INPUT,0,$input_file)}

    if(scalar(@$recs) == 1 && !defined($recs->[0]))
      {
	error("No sequences found in input file [$input_file].");
	next;
      }

    #For each line in the current input file
    foreach $rec (@$recs)
      {
	$rec_num++;
	verboseOverMe("[$input_file] Reading record: [$rec_num].");

	if(!defined($rec))
	  {
	    error("Encountered an undefined sequence record.");
	    next;
	  }

	my($def,$seq) = @$rec;
	$seq = uc($seq);

	$got_anything = 1;

	my $id = '';
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

	    if($abundance_pattern ne '' && $def =~ /$abundance_pattern/)
	      {$abundance = $1}
	    elsif($abundance_pattern ne '')
	      {
		$abund_parse_errs->{FILES}->{$input_file}++;
		$abund_parse_errs->{TOTAL}++;
		$abundance = 1;
	      }
	    else
	      {$abundance = 1}
	  }
	else
	  {
	    warning("Could not parse defline in record [$rec_num] of ",
		    "file [$input_file]: [$def].  Skipping sequence.");
	    next;
	  }

	my $len = length($seq);

	#If we're trimming to an arbitrary length defined on the command line
	if($trim_size == 0)
	  {$trim_size = $len}
	elsif($trim_size > 0 && $len != $trim_size)
	  {
	    if(!$size_error_mode && $len < $trim_size)
	      {
		verbose("Skipping short sequence: [$id], record: [$rec_num] ",
			"in file: [$input_file].");
		next;
	      }
	    elsif($size_error_mode)
	      {
		error("Sequence: [$id], record: [$rec_num] in file: ",
		      "[$input_file] is the wrong size: [$len].  Skipping.");
		next;
	      }
	    else
	      {
		$seq = substr($seq,0,$trim_size);
		$len = $trim_size;
		if(!defined($seq) || $seq eq '')
		  {
		    error("Unable to trim sequence: [$id], record: ",
			  "[$rec_num] in file: [$input_file].");
		    next;
		  }
	      }
	  }

	#Error-check the sequence
	if(defined($last_len) && $len != $last_len)
	  {
	    my $err = "Length of sequence [$id] in file [$input_file]: " .
	      "[$len] does not match the length of the previous " .
		"sequence(s): [$last_len].";
	    if($force)
	      {warning($err)}
	    else
	      {
		error($err,"  Skipping sequence.  Use --force to ",
		      "over-ride.");
		next;
	      }
	  }
	if($seq =~ /([^ATGCatcg])/)
	  {
	    my $example = $1;
	    my $err = "Invalid character(s) found in sequence [$id] in " .
	      "file [$input_file] (e.g. [$example]).";
	    if($force)
	      {warning($err) if($seq =~ /([^ATGCNatcgn])/)}
	    else
	      {
		error($err,"  Skipping sequence.  Use --force to ",
		      "over-ride.") if($seq =~ /([^ATGCNatcgn])/);
		next;
	      }
	  }

	$got_something = 1;

	#Warn about possibly erroneous duplicate records if they have the same
	#ID, the sequence exists, and the abundance of one of the records is
	#greater than 1
	if(exists($seq_hash->{$input_file}) &&
	   exists($seq_hash->{$input_file}->{$id}) &&
	   exists($abundance_hash->{$file_key}) &&
	   exists($abundance_hash->{$file_key}->{$file_key2}) &&
	   exists($abundance_hash->{$file_key}->{$file_key2}->{SEQS}
		  ->{$seq}) &&
	   ($seq_hash->{$input_file}->{$id} > 1 || $abundance > 1))
	  {$warn_dupes++}

	$seq_hash->{$input_file}->{$id} = $abundance;
	$abundance_hash->{$file_key}->{$file_key2}->{SEQS}->{$seq} +=
	  $abundance;
	$abundance_hash->{$file_key}->{$file_key2}->{SRCS}->{$outfile_stub}
	  ->{$seq}->{DEF} .= $def;
	$abundance_hash->{$file_key}->{$file_key2}->{SRCS}->{$outfile_stub}
	  ->{$seq}->{ABUND} += $abundance;
	$abund_check->{$abundance_hash->{$file_key}->{$file_key2}->{SRCS}
		       ->{$outfile_stub}->{$seq}->{ABUND}}++;

	$last_len = $len;
      }

    closeIn(*INPUT);

    if($warn_dupes)
      {warning("[$warn_dupes] duplicate IDs were found in sequence file ",
	       "[$input_file].")}

    next if($dry_run);

    if(!$isglobal)
      {
	foreach my $seq (sort {$abundance_hash->{$file_key}->{$file_key2}
				 ->{SEQS}->{$b} <=> $abundance_hash
				   ->{$file_key}->{$file_key2}->{SEQS}->{$a}}
			 keys(%{$abundance_hash->{$file_key}->{$file_key2}
				  ->{SEQS}}))
	  {
	    $cnt++;

	    #Put the ID delimiter & global ID in temp variables
	    my $tmp_delim_with_id = $id_delim;
	    my $global_id = $cnt;

	    #Unless we can insert the global ID using the substitution
	    #string, prepend the global ID to the delimiter.
	    unless($tmp_delim_with_id =~ s/$value_subst_str/$global_id/)
	      {$tmp_delim_with_id = $global_id . $tmp_delim_with_id}

	    #save the new string as the global ID string
	    $global_id = $tmp_delim_with_id;

	    #Start constructing a string containing the global ID
	    my $str = ">$global_id";

	    #Add an abundance value to the string we're building
	    my $tmp_delim_with_val = $abund_delimiter;
	    my $val = $abundance_hash->{$file_key}->{$file_key2}->{SEQS}
	      ->{$seq};

	    unless($tmp_delim_with_val =~ s/$value_subst_str/$val/)
	      {$tmp_delim_with_val = $val . $tmp_delim_with_val}

	    $str .= $tmp_delim_with_val;

	    #Print merged sequence as long as a valid outfile was detected
	    print("$str\n$seq\n")
	      unless($num_outfiles == 0);

	    if(defined($current_mapfile))
	      {print ABUND ("$cnt\t",$abundance_hash->{$file_key}->{$file_key2}
			    ->{SEQS}->{$seq},"\n")}
	  }

	closeOut(*OUTPUT) if(defined($current_outfile));
	closeOut(*ABUND) if(defined($current_mapfile));
      }
  }

if(scalar(keys(%$abund_parse_errs)))
  {
    warning("Unable to parse abundance from [$abund_parse_errs->{TOTAL}] ",
	    "deflines in [",scalar(keys(%{$abund_parse_errs->{FILES}})),
	    "] file(s) using pattern: [$abundance_pattern].  Please either ",
	    "fix the defline or use a different pattern to extract the ",
	    "abundance value.  Assuming abundance = 1.  Use \"-p ''\" to to ",
	    "avoid this warning.");
  }
elsif((!defined($abundance_pattern) || $abundance_pattern eq '') &&
      exists($abund_check->{1}) && scalar(keys(%$abund_check)) == 1)
  {
    error("Abundance pattern (-p) is empty and there was only 1 of each ",
	  "sequence.  Please make sure you are either submitting raw ",
	  "sequence data or you have supplied a valid abundance pattern ",
	  "(-p).");
    quit(8);
  }

unless($got_something)
  {
    if($got_anything)
      {
	error("No viable sequences were found in any of the input files.  ",
	      "Please check to make sure your trim length (-b) is not too ",
	      "long and that your deflines are parseable.");
	quit(9);
      }
    else
      {
	error("No viable sequences were found in any of the input files.  ",
	      "Please check to make sure your file paths are correct and ",
	      "that your input files have sequences in them.");
	quit(10);
      }
  }

if($isglobal)
  {
    foreach my $current_outfile (keys(%$abundance_hash))
      {
	my $file_key = (defined($current_outfile) ? $current_outfile : '');

	if(defined($current_outfile) && $current_outfile ne '')
	  {
	    checkFile($current_outfile,[map {@$_}
					grep {$_->[1] eq $current_outfile}
					@$input_file_sets]) || next;

	    openOut(*OUTPUT,$current_outfile,1) || next;
	  }

	foreach my $current_mapfile (keys(%{$partner_file
					      ->{$current_outfile}}))
	  {
	    my $file_key2 = (defined($current_mapfile) ?
			     $current_mapfile : '');

	    if(defined($current_mapfile) && $current_mapfile ne '')
	      {
		checkFile($current_mapfile,[map {@$_}
					    grep {$_->[2] eq $current_mapfile}
					    @$input_file_sets]) || next;

		openOut(*ABUND,$current_mapfile,0) || next;
	      }

	    next if($dry_run);

	    my $global_ids = {};

	    foreach my $seq (sort {$abundance_hash->{$file_key}->{$file_key2}
				     ->{SEQS}->{$b} <=>
				     $abundance_hash->{$file_key}->{$file_key2}
				       ->{SEQS}->{$a}}
			     keys(%{$abundance_hash->{$file_key}
				      ->{$file_key2}->{SEQS}}))
	      {
		$cnt++;

		$global_ids->{$seq} = $cnt;

		#Put the ID delimiter & global ID in temp variables
		my $tmp_delim_with_id = $id_delim;
		my $global_id = $cnt;

		#Unless we can insert the global ID using the substitution
		#string, prepend the global ID to the delimiter.
		unless($tmp_delim_with_id =~ s/$value_subst_str/$global_id/)
		  {$tmp_delim_with_id = $global_id . $tmp_delim_with_id}

		#save the new string as the global ID string
		$global_id = $tmp_delim_with_id;

		#Start constructing a string containing the global ID
		my $str = ">$global_id";

		#Add an abundance value to the string we're building
		my $tmp_delim_with_val = $abund_delimiter;
		my $val = $abundance_hash->{$file_key}->{$file_key2}->{SEQS}
		  ->{$seq};

		unless($tmp_delim_with_val =~ s/$value_subst_str/$val/)
		  {$tmp_delim_with_val = $val . $tmp_delim_with_val}

		$str .= $tmp_delim_with_val;

		#Print merged sequence as long as a valid outfile was detected
		print("$str\n$seq\n") unless($num_outfiles == 0);

		if(defined($current_mapfile) && $current_mapfile ne '')
		  {print ABUND ("$cnt\t",$abundance_hash->{$file_key}
				->{$file_key2}->{SEQS}->{$seq},"\n")}
	      }

	    closeOut(*ABUND)  if(defined($current_mapfile) &&
				 $current_mapfile ne '');

	    if(defined($outfile_suffix))
	      {
		foreach my $outfile_stub (keys(%{$abundance_hash->{$file_key}
						   ->{$file_key2}->{SRCS}}))
		  {
		    $sample_seqfile = $outfile_stub . $outfile_suffix;

		    checkFile($sample_seqfile) || next;
		    openOut(*SAMPLESEQ,$sample_seqfile,0) || next;

		    foreach my $seq (sort {$abundance_hash->{$file_key}
					      ->{$file_key2}->{SRCS}
						->{$outfile_stub}->{$b}
						  ->{ABUND} <=>
						    $abundance_hash
						      ->{$file_key}
							->{$file_key2}->{SRCS}
							  ->{$outfile_stub}
							    ->{$a}->{ABUND} ||
							      $global_ids->{$a}
								<=>
								  $global_ids
								    ->{$b}}
				     keys(%{$abundance_hash->{$file_key}
					      ->{$file_key2}->{SRCS}
						->{$outfile_stub}}))
		      {
			#Put the ID delimiter & global ID in temp variables
			my $tmp_delim_with_id = $id_delim;
			my $global_id = $global_ids->{$seq};

			#Unless we can insert the global ID using the
			#substitution string, prepend the global ID to the
			#delimiter.
			unless($tmp_delim_with_id =~
			       s/$value_subst_str/$global_id/)
			  {$tmp_delim_with_id = $global_id .
			     $tmp_delim_with_id}

			#save the new string as the global ID string
			$global_id = $tmp_delim_with_id;

			#Start constructing a string containing the global ID
			my $str = ">$global_id";

			my $tmp_delim_with_val = $abund_delimiter;
			my $val = $abundance_hash->{$file_key}
			  ->{$file_key2}->{SRCS}->{$outfile_stub}->{$seq}
			    ->{ABUND};

			unless($tmp_delim_with_val =~
			       s/$value_subst_str/$val/)
			  {$tmp_delim_with_val = $val .
			     $tmp_delim_with_val}

			if($append_defline)
			  {
			    #Save the current defline string in a new defline
			    my $olddef = $abundance_hash->{$file_key}
			      ->{$file_key2}->{SRCS}->{$outfile_stub}->{$seq}
				->{DEF};

			    #If the current defline doesn't have an abundance
			    #value add an abundance value to the string we're
			    #building
			    if($olddef !~ /$abundance_pattern/)
			      {$str .= $tmp_delim_with_val}

			    $str .= $olddef
			  }
			else
			  {$str .= $tmp_delim_with_val}

			#Print the new record
			print SAMPLESEQ ("$str\n$seq\n");
		      }

		    closeOut(*SAMPLESEQ);
		  }
	      }

	    if(defined($tab_suffix))
	      {
		foreach my $outfile_stub (keys(%{$abundance_hash->{$file_key}
						   ->{$file_key2}->{SRCS}}))
		  {
		    $sample_tabfile = $outfile_stub . $tab_suffix;

		    checkFile($sample_tabfile) || next;
		    openOut(*SAMPLETAB,$sample_tabfile,0) || next;

		    foreach my $seq (sort {$abundance_hash->{$file_key}
					      ->{$file_key2}->{SRCS}
						->{$outfile_stub}->{$b}
						  ->{ABUND} <=>
						    $abundance_hash
						      ->{$file_key}
							->{$file_key2}->{SRCS}
							  ->{$outfile_stub}
							    ->{$a}->{ABUND} ||
							      $global_ids->{$a}
								<=>
								  $global_ids
								    ->{$b}}
				     keys(%{$abundance_hash->{$file_key}
					      ->{$file_key2}->{SRCS}
						->{$outfile_stub}}))
		      {
			print SAMPLETAB ("$global_ids->{$seq}\t",
					 $abundance_hash->{$file_key}
					 ->{$file_key2}->{SRCS}
					 ->{$outfile_stub}->{$seq}->{ABUND},
					 "\n");
		      }

		    closeOut(*SAMPLETAB);
		  }
	      }
	  }

	closeOut(*OUTPUT) if(defined($current_outfile) &&
			     $current_outfile ne '');
      }
  }

verbose("[STDOUT] Output done.") if($num_outfiles == 0 ||
				    scalar(@$outfiles) == 0 &&
				    $num_outfiles == 1);

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
    return(0) if(defined($DEBUG) && !$DEBUG);

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

    my $leader_length = length($leader_string);

    if(defined($DEBUG))
      {
	#If there were debug messages before $DEBUG got defined on the command
	#line, print out the debug buffer
	if(defined($main::debug_buffer))
	  {
	    print STDERR ($main::debug_buffer);
	    undef($main::debug_buffer);
	  }

	#Put location information at the beginning of each line of the message
	print STDERR ($leader_string,
		      shift(@debug_message),
		      ($verbose &&
		       defined($main::last_verbose_state) &&
		       $main::last_verbose_state ?
		       ' ' x ($main::last_verbose_size - $debug_length) : ''),
		      "\n");
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
      }
    #We get here before $DEBUG is defined by the command line, so let's buffer
    #the output until we know whether we're going to be in debug mode or not
    else
      {
	if(!defined($main::debug_buffer))
	  {$main::debug_buffer = ''}
	$main::debug_buffer .=
	  join('',($leader_string,
		   shift(@debug_message),
		   ($verbose &&
		    defined($main::last_verbose_state) &&
		    $main::last_verbose_state ?
		    ' ' x ($main::last_verbose_size - $debug_length) : ''),
		   "\n"));
	foreach my $line (@debug_message)
	  {$main::debug_buffer .= join('',(' ' x $leader_length,
					   $line,
					   "\n"))}
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

    #Put quotes around any parameters containing un-escaped spaces or astericks
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
## This subroutine performs a more reliable glob than perl's built-in, which
## fails for files with spaces in the name, even if they are escaped.  The
## purpose is to allow the user to enter input files using double quotes and
## un-escaped spaces as is expected to work with many programs which accept
## individual files as opposed to sets of files.  If the user wants to enter
## multiple files, it is assumed that space delimiting will prompt the user to
## realize they need to escape the spaces in the file names.  This version
## works with a mix of unescaped and escaped spaces, as well as glob
## characters.  It will also split non-files on unescaped spaces and uses a
## helper sub (globCurlyBraces) to mitigate truncations from long strings.
##
sub sglob
  {
    #Convert possible 'Getopt::Long::CallBack' to SCALAR by wrapping in quotes:
    my $command_line_string = "$_[0]";
    if(!defined($command_line_string))
      {
	warning("Undefined command line string encountered.");
	return($command_line_string);
      }
    elsif(-e $command_line_string)
      {
	debug("Returning command line args: [($command_line_string)].");
	return($command_line_string);
      }

    #Expand the string from the command line based on the '{X,Y,...}'
    #pattern...  Explanation:

    #Sometimes, the glob string is larger than GLOB_LIMIT (even though
    #the shell sent in the long string to begin with).  When that
    #happens, bsd_glob just silently chops off everything except the
    #directory, so we will split the strings up here in perl (to expand
    #any '{X,Y,...}' patterns) before passing them to bsd_glob.  This
    #will hopefully shorten each individual file string for bsd_glob to
    #be able to handle.  We'll sort them too to be on the safe side
    my @partials = map {sort {$a cmp $b} globCurlyBraces($_)}
      split(/(?<!\\)\s+/,$command_line_string);

    my $real_file_found = 0;

    #Note, when bsd_glob gets a string with a glob character it can't expand,
    #it drops the string entirely.  Those strings are returned with the glob
    #characters so the surrounding script can report an error.  The GLOB_ERR
    #posix flag is not used because of the way the patterns are manipulated
    #before getting to bsd_glob - which could cause a valid expansion to
    #nothing that bsd_glob would complain about.
    my @arguments =
      map
	{
	  #Expand the string from the command line using a glob
	  my $v = $_;
	  my @x = bsd_glob($v,GLOB_CSH);
	  #If the expansion returned more than 1 thing OR
	  if(scalar(@x) > 1 ||

	     (#There's only 1 expanded and existing result AND
	      scalar(@x) == 1 && -e $x[0] &&

	      #If the glob string was too long, everything after the last
	      #directory can be truncated, so we want to avoid returning
	      #that truncated value, thus...

	      (#The expanded value is not a directory OR
	       !-d $x[0] ||

	       #Assumed: it is a directory and...

	       (#The pre-expanded value was a valid directory string already
		#or ended with a slash (implying the dir had glob characters
		#in its name/path) or the last expanded string's character
		#is not a slash (implying the end of a pattern wasn't
		#chopped off by bsd_glob, which would leave a slash).
		-d $v || $v =~ m%/$% || $x[0] !~ m%/$%))))
	    {
	      $real_file_found = scalar(grep {-e $_} @x);
	      @x;
	    }
	  else
	    {$v}
	} @partials;

    print STDERR "";

    debug("Returning command line args: [(",join('),(',@arguments),
	  ")].  Number of real files found: [$real_file_found].");

    return($real_file_found ? @arguments : $command_line_string);
  }

sub globCurlyBraces
  {
    my $nospace_string = $_[0];

    if($nospace_string =~ /(?<!\\)\s+/)
      {
	error("Unescaped spaces found in input string: [$nospace_string].");
	return($nospace_string);
      }
    elsif(scalar(@_) > 1)
      {
	error("Too many [",scalar(@_),"] parameters sent in.  Expected 1.");
	return(@_);
      }

    #Keep updating an array to be the expansion of a file pattern to
    #separate files
    my @expanded = ($nospace_string);

    #If there exists a '{X,Y,...}' pattern in the string
    if($nospace_string =~ /\{[^\{\}]+\}/)
      {
	#While the first element still has a '{X,Y,...}' pattern
	#(assuming everything else has the same pattern structure)
	while($expanded[0] =~ /\{[^\{\}]+\}/)
	  {
	    #Accumulate replaced file patterns in @g
	    my @buffer = ();
	    foreach my $str (@expanded)
	      {
		#If there's a '{X,Y,...}' pattern, split on ','
		if($str =~ /\{([^\{\}]+)\}/)
		  {
		    my $substr     = $1;
		    my $before     = $`;
		    my $after      = $';
		    my @expansions = split(/,/,$substr);
		    push(@buffer,map {$before . $_ . $after} @expansions);
		  }
		#Otherwise, push on the whole string
		else
		  {push(@buffer,$str)}
	      }

	    #Reset @f with the newly expanded file strings so that we
	    #can handle additional '{X,Y,...}' patterns
	    @expanded = @buffer;
	  }
      }

    #Pass the newly expanded file strings through
    return(wantarray ? @expanded : [@expanded]);
  }

sub getVersion
  {
    my $full_version_flag = $_[0];
    my $template_version_number = '2.14';
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
    exit($errno) if($errno == 0 || !$force || ($force && $errno != -1));
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
		     "in-place.",
		     ($verbose ? '' :
		      "  Supply --verbose for extended run report."),"\n")}
  }

#This subroutine takes multiple "types" of "sets of input files" and returns
#them in respectively associated groups.  For example, it can take multiple
#lists of input files and returns the first of each list in an array, the
#second of each list in a second array, etc.  The lists can be 1 and 2
#dimensional and the subroutine will make associations based on the dimensions
#of the lists.
#A second series of arrays is also returned which contains stubs for naming
#output files.  This is useful in 2 cases: when an input file is detected on
#STDIN (which this subroutine detects and accounts for) or when a set of output
#directories is supplied (described next...).
#Optionally, if a set of output directories are supplied, it also associates
#them with the input file sets and updates the output file name stubs.
#Here are some examples:

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

#Note that a 1D array mixed with 2D arrays will prompt the subroutine to guess
#which way to associate that series of files in the 1D array(s) with the rest.
#The dimensions of the 2D arrays are required to be the same.  With regard to
#the 1D arrays, the script will force the association to be like this:

#Example 1:
#input files of type 1: [[1,2],[a,b]]
#input files of type 2: [[4,5],[d,e]]
#input files of type 3: [[x,y]]
#resulting associations: [[1,4,x],[2,5,y],[a,d,x],[b,e,y]]
#Example 3:
#input files of type 1: [[1,2],[a,b]]
#input files of type 2: [[4,5],[d,e]]
#input files of type 3: [[x],[y]]
#resulting associations: [[1,4,x],[2,5,x],[a,d,y],[b,e,y]]

#These associations will be made in the same way with the output directories.

#Note that this subroutine also detects input on standard input and treats it
#as an input of the same type as the first array in the file types array passed
#in.  If there is only one input file in that array, it will be considered to
#be a file name "stub" to be used to append outfile suffixes.

#Globals used: $overwrite, $skip_existing, $outfile_suffix

sub getFileSets
  {
    my($file_types_array,$outdir_array);
    my $outfile_stub = 'STDIN';

    debug("Num initial arguments: [",scalar(@_),"].") if($DEBUG < -99);

    #Allow user to submit multiple arrays.  If they do, assume 1. that they are
    #2D arrays each containing a different input file type and 2. that the last
    #one contains outdirs unless the global outfile_suffix is undefined
    if(scalar(@_) > 1 && (defined($outfile_suffix) ||
			  !defined($_[-1]) ||
			  scalar(@{$_[-1]}) == 0))
      {
	debug("Assuming the last input array is outdirs.") if($DEBUG < -99);
	debug("Copy Call 1") if($DEBUG < -99);
	$outdir_array = copyArray(pop(@_));
      }
    elsif($DEBUG < -99)
      {debug("Assuming the last input array is NOT outdirs.  Outfile suffix ",
	     "is ",(defined($outfile_suffix) ? '' : 'NOT '),"defined, last ",
	     "array submitted is ",(defined($_[-1]) ? '' : 'NOT '),
	     "defined, and the last array sumitted is size ",
	     (defined($_[-1]) ? scalar(@{$_[-1]}) : 'undef'),".")}

    debug("Num arguments after dir pop: [",scalar(@_),"].") if($DEBUG < -99);

    #Assumes that outdirs were popped off above
    if(scalar(@_) > 1)
      {
	debug("Copy Call 2") if($DEBUG < -99);
	$file_types_array = [copyArray(@_)];
      }
    else
      {
	debug("Copy Call 3") if($DEBUG < -99);
	$file_types_array = copyArray($_[0]);
      }

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

	debug("There are $num_input_files input files.") if($DEBUG < -99);
	debug("Outfile stub: $outfile_stub.") if($DEBUG < -99);

	#If there's only one input file detected and the dash for STDIN was not
	#explicitly provided, and an outfile suffix has been provided, use that
	#input file as a stub for the output file name construction
	if($num_input_files == 1 && !$dash_was_explicit &&
	   defined($outfile_suffix) && $outfile_suffix ne '')
	  {
	    $outfile_stub = (grep {$_ ne '-'} map {@$_} @$input_files)[0];

	    #Unless the dash was explicitly supplied as a separate file, treat
	    #the input file as a stub only (not as an actual input file
	    @$input_files = ();
	    $num_input_files = 0;

	    #If the stub contains a directory path AND outdirs were supplied
	    if($outfile_stub =~ m%/% &&
	       defined($outdir_array) && scalar(@$outdir_array))
	      {
		error("You cannot use --outdir and embed a directory path in ",
		      "the outfile stub (-i with a single argument when ",
		      "redirecting standard input in).  Please use one or ",
		      "the other.");
		quit(-13);
	      }
	  }
	#If standard input has been redirected in (which is true because we're
	#here) and the number of input files is not equal to 1 (i.e. the input
	#file is not going to be treated as a stub) and an outfule_suffix has
	#been defined, warn the user about the name of the outfile using STDIN
	elsif($num_input_files != 1 && defined($outfile_suffix) &&
	      $outfile_suffix ne '')
	  {warning("Input on STDIN will be referred to as [$outfile_stub].")}

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
	    #If there are other input files present, push it
	    if($num_input_files)
	      {push(@{$input_files->[-1]},'-')}
	    #Else create a new input file set with it as the only file member
	    else
	      {push(@$input_files,['-'])}

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

    my $one_type_mode = 0;
    #If there's only 1 input file type, merge all the sub-arrays
    if(scalar(@$file_types_array) == 1)
      {
	$one_type_mode = 1;
	debug("Only 1 type of file was submitted, so the array is being ",
	      "pre-emptively flattened.") if($DEBUG < -99);

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

    #If output directories were supplied, error check them
    if(defined($outdir_array) && scalar(@$outdir_array))
      {
	#Error check the outdir array to make sure it's a 2D array of strings
	if(ref($outdir_array) ne 'ARRAY')
	  {
	    #Allow them to submit scalars of everything
	    if(ref(\$outdir_array) eq 'SCALAR')
	      {$outdir_array = [[$outdir_array]]}
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
	      {$outdir_array = [$outdir_array]}
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

	debug("Adding directories into the mix.") if($DEBUG < -99);

	#Put the directories in the file_types_array so that they will be
	#error-checked and modified in the same way below.
	push(@$file_types_array,$outdir_array);
      }

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
		   scalar(@subarrays) != 1)
		  {
		    debug("Row inconsistencies in 1D arrays found")
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

    #Re-determine the maximum dimensions of rows and columns in case they
    #changed with the array manipulations above
    $max_num_rows = (#Sort on descending size so we can grab the largest one
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

    $max_num_cols = (#Sort on descending size so we can grab the largest one
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

    #Now fill in the 1D arrays to match the dimensions of the other arrays
    foreach my $file_type_array (@$file_types_array)
      {
	my @subarrays = @$file_type_array;

	#If this is a 1D array
	if(scalar(scalar(@subarrays) == 1 ||
		  scalar(grep {scalar(@$_) == 1} @subarrays)))
	  {
	    #Now I want to fill in the empty columns/rows with duplicates
	    #for the associations to be constructed easily.  I'm doing this
	    #here separately because sometimes above, I had to transpose
	    #the arrays
	    my $num_rows = scalar(@$file_type_array);
	    my $num_cols = scalar(@{$file_type_array->[0]});
	    if($num_rows < $max_num_rows)
	      {
		debug("Pushing onto a 1D array with 1 row and multiple ",
		      "columns because num_rows ($num_rows) < ",
		      "max_num_rows ($max_num_rows)") if($DEBUG < -99);
		foreach(scalar(@$file_type_array)..($max_num_rows - 1))
		  {push(@$file_type_array,[@{$file_type_array->[0]}])}
	      }
	    #If all rows don't have the same number of cols
	    if(scalar(@$file_type_array) !=
	       scalar(grep {scalar(@$_) == $max_num_cols}
		      @$file_type_array))
	      {
		debug("Pushing onto a 1D array with 1 col and multiple ",
		      "rows because not all rows have the max number of ",
		      "columns: ($max_num_cols)")
		  if($DEBUG < -99);
		my $row_index = 0;
		foreach my $row_array (@$file_type_array)
		  {
		    my $empty = scalar(@$row_array) ? 0 : 1;
		    debug("Processing columns of row at index [$row_index] ",
			  "from $num_cols..($max_num_cols - 1)")
		      if($DEBUG < -99);
		    foreach($num_cols..($max_num_cols - 1))
		      {
			debug("Pushing column [",
			      ($empty ? 'undef' : $row_array->[0]),"] on.")
			  if($DEBUG < -99);
			push(@$row_array,($empty ? undef : $row_array->[0]));
		      }

		    $row_index++;
		  }
	      }
	    elsif(scalar(@$file_type_array) && $DEBUG < -99)
	      {debug("These rows already have the max number of columns ",
		     "($max_num_cols): [",
		     join(';',map {join(',',@$_)} @$file_type_array),
		     "].  Among the [",scalar(@$file_type_array),
		     "] rows, each has this many columns: [",
		     join(',',map {scalar(@$_)} @$file_type_array),"].")}
	  }
      }

    if(($twods_exist < 2 &&
	($row_inconsistencies || $twod_col_inconsistencies > 1 ||
	 $twod_col_inconsistencies != $col_inconsistencies)) ||
       ($twods_exist > 1 &&
	($row_inconsistencies || $col_inconsistencies)))
      {
	debug("Row inconsistencies: $row_inconsistencies Col ",
	      "inconsistencies: $col_inconsistencies") if($DEBUG < -99);
	error("The number of ",
	      ($row_inconsistencies ? "sets of files" .
	       (defined($outdir_array) && scalar($outdir_array) ?
		"/directories " : ' ') .
	       ($row_inconsistencies &&
		$col_inconsistencies ? 'and ' : '') : ''),
	      ($col_inconsistencies ? "files" .
	       (defined($outdir_array) && scalar($outdir_array) ?
		"/directories " : ' ') .
	       "in each set " : ''),
	      "is inconsistent among the various types of files" .
	      (defined($outdir_array) && scalar($outdir_array) ?
	       "/directories " : ' '),
	      "input.  Please check your file",
	      (defined($outdir_array) && scalar($outdir_array) ?
	       "/directory " : ' '),
	      "inputs and make sure the number of sets and numbers of files",
	      (defined($outdir_array) && scalar($outdir_array) ?
	       " and directories " : ' '),
	      "in each set match.");
	quit(-17);
      }

    #Now I need to return and array of arrays of files that are to be processed
    #together
    my $infile_sets_array   = [];
    my $outfile_stubs_array = [];

    if($DEBUG < -99)
      {
	foreach my $file_type_array (@$file_types_array)
	  {
	    debug("New file type.  These should be array references: [",
		  join(',',map {defined($_) ? $_ : 'undef'} @$file_type_array),
		  "] and these should not [",
		  join(',',map {defined($_) ? $_ : 'undef'}
		       @{$file_type_array->[0]}),
		  "] [",
		  (scalar(@$file_type_array) > 1 ?
		   join(',',map {defined($_) ? $_ : 'undef'}
			@{$file_type_array->[1]}) : ''),"].");
	    foreach my $file_set (@$file_type_array)
	      {debug(join(',',map {defined($_) ? $_ : 'undef'} @$file_set))}
	  }
      }

    #Keep a hash to look for conflicting outfile stub names
    my $unique_out_check = {};
    my $nonunique_found  = 0;

    #Create the input file groups and output stub groups that are all
    #associated with one another
    foreach my $row_index (0..($max_num_rows - 1))
      {
	foreach my $col_index (0..$#{$file_types_array->[-1]->[$row_index]})
	  {
	    debug("Creating new set.") if($DEBUG < -99);
	    push(@$infile_sets_array,[]);
	    push(@$outfile_stubs_array,[]);
	    if(defined($outdir_array) && scalar(@$outdir_array))
	      {
		foreach my $association (0..($#{$file_types_array} - 1))
		  {
		    debug("Adding to the set.") if($DEBUG < -99);
		    push(@{$infile_sets_array->[-1]},
			 $file_types_array->[$association]->[$row_index]
			 ->[$col_index]);

		    #If the filename is undefined (as would be the case if the
		    #user didn't supply a type of input file that is optional),
		    #make the sub undefined as well
		    if(!defined($file_types_array->[$association]->[$row_index]
				->[$col_index]))
		      {
			push(@{$outfile_stubs_array->[-1]},undef);
			next;
		      }

		    my $dirname = $file_types_array->[-1]->[$row_index]
		      ->[$col_index];
		    my $filename =
		      ($file_types_array->[$association]->[$row_index]
		       ->[$col_index] eq '-' ? $outfile_stub :
		       $file_types_array->[$association]->[$row_index]
		       ->[$col_index]);

		    #Eliminate any path strings from the file name
		    $filename =~ s/.*\///;

		    #Prepend the outdir path
		    my $new_outfile_stub = $dirname .
		      ($dirname =~ /\/$/ ? '' : '/') . $filename;

		    debug("Prepending directory $new_outfile_stub using [",
			  $file_types_array->[-1]->[$row_index]->[$col_index],
			  "].")
		      if($DEBUG < -99);

		    push(@{$outfile_stubs_array->[-1]},$new_outfile_stub);

		    #Check for conflicting output file names that will
		    #overwrite eachother
		    if($dimensionalities[$association] == 2 &&
		       exists($unique_out_check->{$new_outfile_stub}))
		      {$nonunique_found = 1}
		    push(@{$unique_out_check->{$new_outfile_stub}},$filename)
		      if($dimensionalities[$association] == 2);
		  }
	      }
	    else
	      {
		foreach my $association (0..($#{$file_types_array}))
		  {
		    debug("Adding to the set.") if($DEBUG < -99);
		    push(@{$infile_sets_array->[-1]},
			 $file_types_array->[$association]->[$row_index]
			 ->[$col_index]);
		    push(@{$outfile_stubs_array->[-1]},
			 (defined($file_types_array->[$association]
				  ->[$row_index]->[$col_index]) &&
			  $file_types_array->[$association]->[$row_index]
			  ->[$col_index] eq '-' ? $outfile_stub :
			  $file_types_array->[$association]->[$row_index]
			  ->[$col_index]));
		  }
	      }
	  }
      }

    if($nonunique_found)
      {
	error('The following output file name stubs were created by ',
	      'multiple input file names and will be overwritten if used.  ',
	      'Please make sure each similarly named input file outputs to ',
	      'a different output directory or that the input file names ',
	      'bare no similarity.  Offending file name conflicts: [',
	      join(',',map {"$_ is written to by [" .
			      join(',',@{$unique_out_check->{$_}}) . "]"}
		   (grep {scalar(@{$unique_out_check->{$_}}) > 1}
		    keys(%$unique_out_check))),'].');
	quit(-1);
      }

    debug("Processing input file sets: [(",
	  join('),(',(map {my $a = $_;join(',',map {defined($_) ? $_ : 'undef'}
					   @$a)} @$infile_sets_array)),
	  ")] and output stubs: [(",
	  join('),(',(map {my $a = $_;join(',',map {defined($_) ? $_ : 'undef'}
					   @$a)} @$outfile_stubs_array)),")].")
      if($DEBUG < 0);

    return($infile_sets_array,$outfile_stubs_array);
  }

#This subroutine transposes a 2D array (i.e. it swaps rwos with columns).
#Assumes argument is a 2D array.  This sub is used by getFileSets().
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
    my $outfile_suffix                = scalar(@_) >= 2 ? $_[1] : '';
                                        #undef means there won't be outfiles
                                        #Empty string means that the files in
                                        #$_[0] are already outfile names
    my $existing_outfiles             = [];

    #Check to make sure previously generated output files won't be over-written
    #Note, this does not account for output redirected on the command line.
    #Also, outfile stubs are checked for future overwrite conflicts in
    #getFileSets (i.e. separate files slated for output with the same name)
    if(defined($outfile_suffix))
      {
	#For each output file *stub*, see if the expected outfile exists
	foreach my $outfile_stub (grep {defined($_)}
				  @$outfile_stubs_for_input_files)
	  {if(-e "$outfile_stub$outfile_suffix")
	     {push(@$existing_outfiles,"$outfile_stub$outfile_suffix")}}
      }

    return(wantarray ? @$existing_outfiles : $existing_outfiles);
  }

#This subroutine takes a 1D or 2D array of output directories and creates them
#(Only works on the last directory in a path.)  Returns non-zero if suffessful
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
				   'you wish to delete the existing output ',
				   "directory: [$dir], you must do it ",
				   'manually.')}
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
    my $current_output_file = defined($_[0]) ? $_[0] : return(1);
    my $input_file_set      = $_[1];
    my $status              = 1;

    if(-e $current_output_file)
      {
	if($skip_existing)
	  {
	    warning("[$current_output_file] Output file exists.  Skipping",
		    (defined($input_file_set) ?
		     (" input file(s): [",join(',',@$input_file_set),"].") :
		     ('.')));
	    $status = 0;
	  }
	elsif(!$overwrite)
	  {
	    error("[$current_output_file] Output file exists.  Unable to ",
		  "proceed.",
		  (defined($input_file_set) ?
		   ("  Encountered while processing input file(s): [",
		    join(',',grep {defined($_)} @$input_file_set),"].") :
		   ('')),
		  "  This may have been caused by multiple input files ",
		  "writing to one output file because there were not ",
		  "existing output files when this script started.  If any ",
		  "input files are writing to the same output file, you ",
		  "should have seen a warning about this above.  Otherwise, ",
		  "you may have multiple versions of this script running ",
		  "simultaneously.  Please check your input files and ",
		  "outfile suffixes to fix any conflicts or supply the ",
		  "--skip-existing or --overwrite flag to proceed.");
	    quit(-1);
	  }
      }

    return($status);
  }

#Uses globals: $header,$main::open_handles,$dry_run,$extended
sub openOut
  {
    my $file_handle         = $_[0];
    my $current_output_file = $_[1];
    #Select the output handle if third param is non-zero:
    my $select              = (scalar(@_) >= 3 && defined($_[2]) ? $_[2] : 0);
    my $append              = (scalar(@_) >= 4 && defined($_[3]) ? $_[3] : 0);
    my $mode                = ($append ? '>>' : '>');
    my $status              = 1;

    #If we're appending, there exists an open handle on this file, & the file
    #the handle points to is the same
    if($append && exists($main::open_handles->{$file_handle}) &&
       $main::open_handles->{$file_handle} eq $current_output_file)
      {
	#Make sure it's selected
	if($select)
	  {
	    #Select the output file handle
	    select($file_handle);
	  }
	#Otherwise, there's nothing left to do
      }
    #Open the output file
    elsif(!$dry_run && !open($file_handle,"$mode$current_output_file"))
      {
	#Report an error and iterate if there was an error
	error("Unable to open output file: [$current_output_file].\n",$!);
	$status = 0;
      }
    else
      {
	$main::open_handles->{$file_handle} = $current_output_file;

	if($dry_run)
	  {
	    my $encompassing_dir = $current_output_file;
	    $encompassing_dir =~ s/[^\/]+$//;
	    $encompassing_dir =~ s%/%%;
	    $encompassing_dir = '.' unless($encompassing_dir =~ /./);

	    if(-e $current_output_file && !(-w $current_output_file))
	      {error("Output file exists and is not writable: ",
		     "[$current_output_file].")}
	    elsif(-e $encompassing_dir && !(-w $encompassing_dir))
	      {error("Encompassing directory of output file: ",
		     "[$current_output_file] exists and is not writable.")}
	    else
	      {verbose("[$current_output_file] Opened output file.")}

	    closeOut($file_handle);

	    return($status);
	  }

	verbose("[$current_output_file] Opened output file.");

	if($select)
	  {
	    #Select the output file handle
	    select($file_handle);

	    #Store info about the run as a comment at the top of the output
	    print(getHeader($extended)) if($header);
	  }
	elsif(!$append)
	  {
	    #Store info about the run as a comment at the top of the output
	    print $file_handle (getHeader($extended)) if($header);
	  }
      }

    return($status);
  }

#Globals used: $main::open_handles
sub closeOut
  {
    my $file_handle = $_[0];
    my $status = 1;

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

    return($status);
  }

#Globals used: $main::open_handles
sub openIn
  {
    my $file_handle = $_[0];
    my $input_file  = $_[1];
    my $status      = 1;

    #Open the input file
    if(!open($file_handle,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file].\n$!");
	$status = 0;
      }
    else
      {
	verbose("[$input_file] Opened input file.");

	$main::open_handles->{$file_handle} = $input_file;

	closeIn($file_handle) if($dry_run);
      }

    return($status);
  }

#Globals used: $main::open_handles
sub closeIn
  {
    my $file_handle = $_[0];
    my $status = 1;

    #Close the input file handle
    close($file_handle);

    verbose("[$main::open_handles->{$file_handle}] Input file done.  ",
	    "Time taken: [",scalar(markTime()),' Seconds].');

    delete($main::open_handles->{$file_handle});

    return($status);
  }

#Note: Creates a surrounding reference to the submitted array if called in
#scalar context and there are more than 1 elements in the parameter array
sub copyArray
  {
    if(scalar(grep {ref(\$_) ne 'SCALAR' && ref($_) ne 'ARRAY'} @_))
      {
	my @errs = map {ref($_)}
	  grep {ref(\$_) ne 'SCALAR' && ref($_) ne 'ARRAY'} @_;
	error("Invalid argument: [@errs] - not an array of scalars.");
	quit(-19);
      }
    my(@copy);
    foreach my $elem (@_)
      {push(@copy,(defined($elem) && ref($elem) eq 'ARRAY' ?
		   [copyArray(@$elem)] : $elem))}
    debug("Returning array copy of [@copy].") if($DEBUG < -99);
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
	    '#' . scalar(getCommand(1)) . "\n";
      }
    elsif($extended && !defined($main::header_str_ext))
      {
	my $version_str = getVersion($extended);
	$version_str =~ s/\n(?!#|\z)/\n#/sg;
	$main::header_str_ext = "$version_str\n" .
	  '#' . scalar(localtime($^T)) . "\n" .
	    '#' . scalar(getCommand(1)) . "\n";
      }

    return($extended ? $main::header_str_ext : $main::header_str);
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
Lewis Sigler Institute for Integratove Genomics
Bioinformatics Group
Room 133A
Princeton, NJ 08544
rleach\@genomics.princeton.edu

* WHAT IS THIS: This script takes a series of aligned, variable-length, no-gap,
                input sequence files and merges them into 1 file (trimming the
                sequences to the length of the smallest sequence in the first
                sequence file (skipping shorter sequences) or to an
                arbitrarily selected length - see -b), outputting each unique
                sequence once with its cumulative abundance across all input
                files.  Abundances can either be computed from scratch or
                parsed from the sequence file deflines and summed upon merge.
                If abundance values are supplied on the deflines of the
                sequence file and sequences are trimmed, abundance values will
                be summed if the subsequences match.  All sequences are
                reported in upper-case characters.  To ignore abundance values
                and count sequences from scratch (instead of match abundance
                patterns) supply -p ''.  This script will also regenerate the
                same set of input files with the new globally unique IDs
                prepended on the deflines.

                This script is a part of a package called 'CFF' (cluster free
                filtering).  Please refer to the README for general information
                about the package.

* INPUT SEQ FORMAT: Fasta or fastq format file containing a set of unique,
  -i                ungapped, and aligned sequences and with a unique
                    identifier followed by an optional abundance value on the
                    defline.  Default defline format looks like this:

                    >lib_6;size=1002;

                    Any defline format can be used as long as it is consistent,
                    both pieces of information are present, and the identifier
                    is the first unbroken string after the defline character.
                    The unique ID may contain the abundance value.  Use -p to
                    extract the abundance values from deflines not in the above
                    format.  Use -q to extract sequence IDs from deflines not
                    in the above format.

                    Notes, duplicate IDs in a sequence file will cause
                    sequences to be skipped with an error.  If no abundance
                    value can be parsed from a defline, it is assumed to be 1.
                    Identical sequences in 1 file will cause their abundances
                    to be summed, though this is not the primary purpose of
                    this script.  The format of deflines resulting from such a
                    sum is not guaranteed to remain the same in subsequent
                    versions of this script.

* OUTPUT SEQ FORMAT: (Both for merged and unmerged output files.)  Fasta format
  -f,-o              with a numeric sequence ID.  Records are output in
                     decreasing order of abundance.  Example of a defline:

                     >1;size=12345;

                     See HEADER FORMAT in the --extended --help output for
                     additional header information.

* TAB FORMAT: A tab-delimited text file with 2 columns: GlobalID and Abundance.
  -x,-u       If --header is supplied, a header line will be at the top of the
              output file, like this:

              #GlobalID       Abundance

              See HEADER FORMAT in the --extended --help output for additional
              header information.

end_print

    if($advanced)
      {
	my $header          = getHeader();
	my $extended_header = getHeader(1);

	print << "end_print";
* HEADER FORMAT: If --header is supplied or STANDARD output is going to the
                 terminal (and not redirected into a file), every output file,
                 including output to standard out, will get a header that is
                 commented using the '#' character (i.e. each line of the
                 header will begin with '#').  The format of the standard
                 header looks like this:

$header

                 And here is the format of the extended header:

$extended_header

                 See TAB FORMAT for additional header information.

* OVERWRITE PROTECTION: This script prevents the over-writing of files (unless
                        --overwrite is provided).  A check is performed for
                        pre-existing files before any output is generated.

* ADVANCED I/O FEATURES:

-i, -f, and -u can all accept multiple files/file groups.  All files supplied
to a single instance of -i are considered a file group to be merged.  Every
merged seq file supplied to -f acts as the output file for the corresponding -i
file group.  Likewise, every merged tab file supplied to -u acts as the
output file for the corresponding -i file group.  If no -f is supplied, output
will go to standard out.  But, if multiple -u files are supplied and there is
no -f, it is assumed that the sequence output is unwanted and thus, nothing
goes to standard out (because multiple -u files causes each instance of -i to
be merged separately and could result in redundancies if all merged sequencs
went to a single file.

Advanced -i, -f, and -u Usage Examples:

>mergeSeqs.pl -i "*.fa"
All sequence output is merged as 1 and goes to standard out

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa"
All sequence output is merged as 1 and goes to standard out

>mergeSeqs.pl -i "*.1.fa" -f merged.fa
All sequence output is merged as 1 and goes to merged.fa

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged.fa
All sequence output is merged as 1 and goes to merged.fa

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f "merged1.fa merged2.fa"
Sequence output from *.1.fa is merged as 1 and goes to merged1.fa and sequence output from *.2.fa is merged as 1 and goes to merged2.fa

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged1.fa -f merged2.fa
Sequence output from *.1.fa is merged as 1 and goes to merged1.fa and sequence output from *.2.fa is merged as 1 and goes to merged2.fa

>mergeSeqs.pl -i "*.1.fa" -f merged1.fa -f merged2.fa
ERROR1: The number of merged seq files (-f) (including when going to standard out only) must either be 1 or equal to the number of times -i (an input file group to be merged) is supplied.

>mergeSeqs.pl -i "*.fa" -u mappings.tab
All sequence output is merged as 1 and goes to standard out
All abundances are in mappings.tab

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -u mappings.tab
All sequence output is merged as 1 and goes to standard out
All abundances are in mappings.tab

>mergeSeqs.pl -i "*.1.fa" -f merged.fa -u mappings.tab
All sequence output is merged as 1 and goes to merged.fa
All abundances are in mappings.tab

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged.fa -u mappings.tab
All sequence output is merged as 1 and goes to merged.fa
All abundances are in mappings.tab

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -u mappings1.tab -u mappings2.tab
No sequence output is generated
All abundances from *.1.fa are mapped in mappings1.tab and all abundances from *.2.fa are mapped in mappings2.tab

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged.fa -u mappings1.tab -u mappings2.tab
ERROR1: The number of merged tab files (-u), if supplied, must either be 1 or equal to the number of times -i (an input file group to be merged) is supplied and if merged seq files are supplied (even via redirect of standard out) it must be equal to the number of merged seq files.

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -u mappings1.tab -u mappings2.tab > merged.fa
ERROR1: The number of merged tab files (-u), if supplied, must either be 1 or equal to the number of times -i (an input file group to be merged) is supplied and if merged seq files are supplied (even via redirect of standard out) it must be equal to the number of merged seq files.

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f "merged1.fa merged2.fa" -u mappings.tab
ERROR1: The number of merged tab files (-u), if supplied, must either be 1 or equal to the number of times -i (an input file group to be merged) is supplied and if merged seq files are supplied (even via redirect of standard out) it must be equal to the number of merged seq files.

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged1.fa -f merged2.fa -u mappings.tab
ERROR1: The number of merged tab files (-u), if supplied, must either be 1 or equal to the number of times -i (an input file group to be merged) is supplied and if merged seq files are supplied (even via redirect of standard out) it must be equal to the number of merged seq files.

>mergeSeqs.pl -i "*.1.fa" -i "*.2.fa" -f merged1.fa -f merged2.fa -u mappings1.tab -u mappings2.tab
Sequence output from *.1.fa is merged as 1 and goes to merged1.fa and sequence output from *.2.fa is merged as 1 and goes to merged2.fa
All abundances from *.1.fa are mapped in mappings1.tab and all abundances from *.2.fa are mapped in mappings2.tab

Additionally, files may be redirected into the script on standard in, thus one
may cat a group of files and pipe them to mergeSeqs.pl without any other
arguments and the output may be piped to another script (e.g. neighbors.pl) in
turn.  When there is input on standard in, it is treated as a part of the first
input file group supplied with the first instance of -i, so keep that in mind
when associating it with -f or -u.  Note that all IDs on STDIN must be unique
or you will get errors.  If you wish to explicitly place the input from
standard in among other input files supplied (or by itself) via -i, you may do
so using the dash ('-') as a stand-in for the standard input stream.  Note that
"-t auto" is not supported for input on standard in, so you must supply -t with
the format of the file if you redirect in standard input or else you will get
an error.

Advanced Standard Input Usage Examples:

>mergeSeqs.pl -t fasta < stdin.fa
All sequence output is merged as 1 and goes to standard out

>cat *.fa | mergeSeqs.pl -t fasta | neighbors.pl -t fasta > out.nbrs
All sequence output is merged as 1 and goes to neighbors.pl which outputs to
out.nbra

>mergeSeqs.pl -i "*.1.fa" -t fasta < stdin.fa
All sequence output is merged as 1 and goes to standard out

>mergeSeqs.pl -i "*.1.fa" -f "merged1.fa merged2.fa" -t fasta < stdin.fa
Sequence output from *.1.fa is merged as 1 and goes to merged1.fa and sequence
output from stdin.fa goes to merged2.fa

>cat *.2.fa | mergeSeqs.pl -i "*.1.fa" -f merged1.fa -f merged2.fa -t fasta
ERROR1: The number of files/directories in each set is inconsistent among the various types of files/directories input.  Please check your file/directory inputs and make sure the number of sets and numbers of files and directories in each set match.

>cat *.1.fa | mergeSeqs.pl -i - -i "*.2.fa" -f merged1.fa -f merged2.fa -u mappings1.tab -u mappings2.tab -t fasta
Sequence output from *.1.fa is merged as 1 and goes to merged1.fa and sequence
output from *.2.fa is merged as 1 and goes to merged2.fa
All abundances from *.1.fa are mapped in mappings1.tab and all abundances from
*.2.fa are mapped in mappings2.tab

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

    print("\n$script -i \"input file(s)\" [-f outfile] $options\n",
	  ($advanced_mode ?
	   "$script [-f outfile] $options < input_file.fa\n" .
	   "cat input file(s) | $script [-f outfile] $options\n" : ''),"\n");

    if($no_descriptions)
      {print("Run with no options for usage.\n")}
    else
      {
	if(!$advanced_mode)
	  {
	    print << 'end_print';
     -i|--seq-file        REQUIRED Input sequence file(s).  See --help for file
                                   format.
     -b|--trim-to-size    OPTIONAL [0] Trim sequences to this length.  All
                                   sequences must be the same length.  0 = no
                                   trim.  This assumes you have already trimmed
                                   your sequences.  -1 = auto (not
                                   recommended).  This selects the shortest
                                   sequence in the first file as the trim
                                   length.  Sequences shorter than a supplied
                                   trim length are skipped.
     -f|--merged-seq-file OPTIONAL [stdout] Merged sequence output file.  See
                                   --help for file format.
     -u|--merged-tab-file OPTIONAL [no output] Merged tab-delimited output
                                   data file.  See --help for file format.
     -o|--seq-suffix      OPTIONAL [no output]  Outfile extension appended to
                                   -i to generate a duplicate of the input file
                                   with global IDs.
     -x|--tab-suffix      OPTIONAL [no output] Outfile extension appended to
                                   -i to generate a tab-delimited version of
                                   the input file with global IDs and original
                                   source file abundances.
     -t|--filetype        OPTIONAL [auto](fasta,fastq,auto) Input file type
                                   (provided to -i).
     --outdir             OPTIONAL [none] Directory to put output files.
     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --quiet              OPTIONAL Quiet mode.
     --skip-existing      OPTIONAL Skip existing output files.
     --overwrite          OPTIONAL Overwrite existing output files.
     --force              OPTIONAL Ignore critical errors.  Also see
                                   --overwrite or --skip-existing.
     --header             OPTIONAL Print commented script version, date, and
                                   command line call to outfile(s).
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
     --error-type-limit   OPTIONAL [5] Limit for each type of error/warning.
                                   0 = no limit.  Also see --quiet.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info.
     --use-as-default     OPTIONAL Save the command line arguments.
     --help               OPTIONAL Print info and format descriptions.
     --extended           OPTIONAL Print extended usage/help/version/header
                                   when supplied with corresponding the flags.
Run with just --extended for advanced options and more details.
end_print
	  }
	else #Advanced options/extended usage output
	  {
	    print << 'end_print';
     -i|--seq-file*       REQUIRED Space-separated input sequence file(s)
                                   inside quotes (e.g. -i "*.txt *.text").
                                   Expands standard bsd glob characters (e.g.
                                   '*', '?', etc.).  See --extended --help for
                                   input file format & advanced usage examples.
                                   Do not need to supply if there is input on
                                   standard in.   *No flag required.
     -b|--trim-to-size    OPTIONAL [0] Trim sequences to this length.  All
                                   sequences must be the same length.  0 = no
                                   trim.  This assumes you have already trimmed
                                   your sequences and will issue an error if a
                                   sequence of a different length is
                                   encountered.  -1 = auto (not recommended).
                                   This selects the shortest sequence in the
                                   first file as the trim length and skips
                                   sequences in subsequent files that are
                                   shorter.  Sequences shorter than a supplied
                                   trim length are skipped.
     -f|--merged-seq-file OPTIONAL [stdout] Merged sequence output file.  Will
                                   not overwrite without --overwite.  Default
                                   behavior prints output to standard out.  If
                                   only -u output is desired, supply -u, but do
                                   not supply -f and do not dedirect standard
                                   out.  No sequence output will be generated
                                   in this case.  See --extended --help for
                                   output file format and advanced usage
                                   examples.
     -u|--merged-tab-file OPTIONAL [no output] Merged tab-delimited output data
                                   file containing sequential global sequence
				   IDs (the number at the beginning of the
                                   defline in -f) and abundance values.  See
                                   --extended --help for output file format and
                                   advanced usage examples.
     -o|--seq-suffix      OPTIONAL [no output]  Outfile extension appended to
                                   -i to generate a duplicate of the input file
                                   with global IDs prepended on the defline
                                   (and delimited with ';').  Will not
                                   overwrite without --overwite.  Explicitly
                                   supplying an empty string will use the input
                                   file name as the output file name (use with
                                   --outdir or when -i is a stub).  When
                                   standard input is detected and no stub is
                                   provided with -i, appends to the string
                                   "STDIN".  Does not replace existing input
                                   file extensions.  Default behavior is to not
                                   generate this output.  See --extended --help
                                   for output file format and more advanced
                                   usage examples.
     -x|--tab-suffix      OPTIONAL [no output]  Outfile extension appended to
                                   -i to generate a tab-delimited version of
                                   the input file with global IDs and original
                                   source file abundances.  Will not overwrite
                                   without --overwite.  Explicitly supplying
                                   an empty string will use the input file name
                                   as the output file name (use with --outdir
                                   or when -i is a stub).  When standard input
                                   is detected and no stub is provided with -i,
                                   appends to the string "STDIN".  Does not
                                   replace existing input file extensions.
                                   Default behavior is to not generate this
                                   output.  See --extended --help for output
                                   file format and more advanced usage
                                   examples.
     -t|--filetype        OPTIONAL [auto](fasta,fastq,auto) Input file (-i)
                                   type.  Using this instead of auto will make
                                   file reading faster.  "auto" cannot be used
                                   when redirecting a file in.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Default output
                                   directory is the same as that containing
                                   each input file.  Relative paths will be
                                   relative to each individual input file.
                                   Creates directories specified, but not
                                   recursively.  Also see --extended --help for
                                   more advanced usage examples.
     -p|--abundance-      OPTIONAL [size=(\\d+);] A perl regular expression
        pattern                    used to extract the abundance value from the
                                   fasta/fastq defline of the input sequence
                                   file.  If there is no abundance values in
                                   your sequence files, supply -p '' to count
                                   each sequence once.  The abundance value
                                   pattern must be surrounded by parenthases.
                                   If no parenthases are provided, the entire
                                   pattern will be assumed to be the abundance
                                   value.  Abundance values are captured this
                                   way to allow flexibility of file format.
                                   Note, the entire defline, without the
                                   trailing hard return but including the '>'
                                   or '\@' character is available to match
                                   against.
     -d|--abund-delimiter OPTIONAL [size=$value_subst_str;] The abundance
                                   value that is appended to the global ID on
                                   the defline of the merged sequence file (-f)
                                   and unmerged sequence file (-o) (only if
                                   abundance value is not already there - see
                                   -p) will be demilimited by this string.  If
                                   the delimiter contains the string
                                   '$value_subst_str', that string will be
                                   replaced with the abundance/N0 value so that
                                   you can control where the value is placed in
                                   the delimiting string.
     -q|--seq-id-pattern  OPTIONAL [^\s*[>\@]\s*([^;]+)] A perl regular
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
     -g|--id-delimiter    OPTIONAL [lib_$value_subst_str;] The global ID
                                   delimiter appended to the global ID on
                                   the defline of the sequence file.  If the
                                   delimiter contains the string
                                   '$value_subst_str', that string will be
                                   replaced with the global ID so that you can
                                   control where the value is placed in the
                                   delimiting string.
     -c|--append-old-     OPTIONAL Supplying this flag will cause the defline
        defline                    from the original input file (-i) to be
                                   appended to the new defline with the global
                                   ID and abundance in the unmerged sequence
                                   files.  Only used when -o is supplied.  Not
                                   used with merged sequence files (-f).
     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --quiet              OPTIONAL Quiet mode.
     --skip-existing      OPTIONAL Skip existing output files.
     --overwrite          OPTIONAL Overwrite existing output files.  By
                                   default, existing output files will not be
                                   over-written.  Also see --skip-existing.
     --force              OPTIONAL Ignore critical errors.  Use this option
                                   (with caution) to push past where an error
                                   has stopped execution.  This option will not
                                   over-ride over-write protection.  Also see
                                   --overwrite or --skip-existing.
     --header             OPTIONAL Print commented script version, date, and
                                   command line call to each output file.
                                   Includes column headers with -u and -x.
                                   Includes template version with --extended.
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
     --error-type-limit   OPTIONAL [5] Limits each type of error/warning to
                                   this number of reports.  Intended to
                                   declutter output.  Note, a summary of
                                   warning/error types is printed when the
                                   script finishes, if one occurred.  0 = no
                                   limit.  Also see --quiet.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info.  Includes template
                                   version with --extended.
     --use-as-default|    OPTIONAL Save the command line arguments.  Saved
       --save-as-default           defaults are printed at the bottom of this
                                   usage output.  Supplying this flag replaces
                                   those defaults with all options that are
                                   provided with this flag.  Values are stored
                                   in ~/.rpst/.
     --help               OPTIONAL Print general info and file format
                                   descriptions.  Includes advanced usage
                                   examples with --extended.
     --extended           OPTIONAL Print extended usage/help/version/header.
                                   Supply alone for extended usage.  See
                                   --header, --help, & --version.
end_print
	  }

	my @user_defaults = getUserDefaults();
	print("Current user defaults: [@user_defaults].\n");
      }

    return(0);
  }

#Uses global variables: lastfiletype & filetype
sub getNextSeqRec
  {
    my $input_file = $_[2];

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

	if(!-e $input_file)
	  {
	    error("`-t auto` cannot be used when the input file does not ",
		  "exist.  Please supply the exact file type.");
	    quit(8);
	  }

	my($num_fastq_defs);
	if(-e $input_file)
	  {
	    $num_fastq_defs =
	      `head -n 50 "$input_file" | grep -c -E '^[\@\+]'`;
	    debug("System output from: [",
		  qq(head -n 50 "$input_file" | grep -c -E '^[\@\+]'),
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
		$num_fasta_defs = `head -n 50 "$input_file" | grep -c -E '^>'`;

		debug("System output from: [",
		      qq(head -n 50 "$input_file" | grep -c -E '^>'),
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
    my $first_loop      = 0;
    my $line_num        = 0;
    my $verbose_freq    = 1000;
    my $line            = '';
    my $defline         = '';
    my $seq_lines       = 0;
    my($seq);

    #For each line in the current input file
    while(getLine($handle))
      {
	$line_num++;

	verboseOverMe("Reading line [$line_num].")
	  unless($line_num % $verbose_freq);

	$line = $_;

	next if($line !~ /\S/ || $line =~ /^\s*#/);
	if($line =~ />/)
	  {
	    if($defline)
	      {
		my $solidseq =
		  ($seq_lines == 1 || $no_format ?
		   $seq : formatSequence($seq));
		chomp($solidseq);
		chomp($defline);

		push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);
	      }
	    $defline   = $line;
	    $seq_lines = 0;

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
	      ($seq_lines == 1 || $no_format ? $seq : formatSequence($seq));
	    chomp($solidseq);
	    chomp($defline);

	    push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);

	    undef($seq);
	  }
	else
	  {
	    $seq .= $line;
	    $seq_lines++;
	  }
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
    my $first_loop       = 1;
    my $line_num         = 0;
    my $line             = '';
    my $defline          = '';
    my $seq              = '';
    my $qual             = '';
    my $getting_sequence = 0;
    my $comment_buffer   = '';
    my $seq_lines        = 0;
    my $qual_lines       = 0;
    my $verbose_freq     = 1000;

    #For each line in the current input file
    while(getLine($handle))
      {
	$line_num++;

	verboseOverMe("Reading line [$line_num].")
	  unless($line_num % $verbose_freq);

	$line = $_;

	next if($line !~ /\S/ || ($first_loop && $line =~ /^\s*#/));

	$first_loop = 0;

	#If this is the defline, or the quality length is the same as the seq
	if(length($qual) >= length($seq) && /^\s*\@[^\n\r]*/)
	  {
	    if($defline ne '' || $seq ne '' || $qual ne '')
	      {
		my $solidseq =
		  ($seq_lines == 1 || $no_format ? $seq :
		   formatSequence($seq));
		$qual =~ s/[\s\r\n\t]+//g if(!$no_format && $qual_lines > 1);
		chomp($solidseq);
		chomp($qual);
		chomp($defline);

		push(@{$main::{FASTQBUFFER}->{$handle}},
		     [$defline,$solidseq,$qual]);
	      }
	    $defline    = $line;
	    $seq_lines  = 0;
	    $qual_lines = 0;

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
	    $seq_lines++;
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
	    $qual_lines++;
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
