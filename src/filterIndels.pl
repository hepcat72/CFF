#!/usr/bin/perl
#Note: 'use warnings' is below instead of having -w above

#Generated using perl_script_template.pl 2.15
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
my $software_version_number = '3.9';
my $created_on_date         = '4/2/2014';

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
$| = 1;

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix, #These are not defined on purpose
   $group_suffix,
   $fakes_suffix);
my $input_files         = [];
my $outdirs             = [];
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
my $muscle              = 'muscle';
my $muscle_gaps         = 0;
my $seq_id_pattern      = '^\s*[>\@]\s*([^;]+)';
my $abundance_pattern   = 'size=(\d+);';
my $indel_sep           = ',';
my $parent_sep          = ':';
my $value_subst_str     = '__VALUE_HERE__';
my $append_delimiter    = "Indels=$value_subst_str;";
my $heuristic_str_size  = 11;
my($align_mode,
   $sum_abund,
   $long_homopol_length,
   $max_long_hpl_diff,
   $max_short_hpl_diff,
   $max_long_hpl_abund,
   $max_short_hpl_abund,
   $homopolymers_only);
my $homopolymers_only_def   = 0;
my $def_long_homopol_length = 4;
my $def_max_long_hpl_diff   = 2;
my $def_max_long_hpl_abund  = 0;
my $def_max_short_hpl_diff  = 1;
my $def_max_short_hpl_abund = 1;
my $sum_abund_pref_def      = 1;
my $sum_abund_def           = 0;
my $align_mode_def          = 'local';
my $align_mode_pref_def     = 'global';
my $prefilter_mode          = 0;  #Sets sum_abund=1,homopolymers_only=1,
                                  #align_mode=global if undefined
my $min_shifted_hits    = 1;
my $min_direct_hits     = 1;
my $mitigate_recips     = 0;
my $min_abund           = 1;
my $processes           = 0;
my $gigs_ram            = 0;
my $max_bases_per_gig   = 200000; #For muscle - Increase at own risk
my $max_bases_per_run   = 0;      #$max_bases_per_gig * $gigs_ram / $processes

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args     = [@ARGV];  #Preserve the agruments for getCommand
my $num_explicit_args = scalar(@ARGV);
my $verbose           = 0;
my $quiet             = 0;
my $DEBUG             = 0;
my $force             = 0;
my @user_defaults     = getUserDefaults(1);

my $GetOptHash =
  {#File options
   'i|input-file=s'          => sub {push(@$input_files, #REQUIRED unless <> is
			             [sglob($_[1])])},   #         supplied
   '<>'                      => sub {push(@$input_files, #REQUIRED unless -i is
				     [sglob($_[0])])},   #         supplied
   'j|group-suffix=s'        => \$group_suffix,          #OPTIONAL [undef]
   'o|outfile-suffix|reals-suffix=s'
			     => \$outfile_suffix,        #OPTIONAL [undef]
   'f|fakes-suffix=s'        => \$fakes_suffix,          #OPTIONAL [undef]
   't|filetype=s'            => \$filetype,              #OPTIONAL [auto]
				                         #   (fasta,fastq,auto)
   'outdir=s'                => sub {push(@$outdirs,     #OPTIONAL
				     [sglob($_[1])])},

   #Basic options
   'pre-filter-mode!'        => \$prefilter_mode,        #OPTIONAL [Off]
   'sum-abundances!'         => \$sum_abund,             #OPTIONAL [prefilter:
				                         #on, no-prefilter:off]

   #Homopolymer options
   'h|homopolymer-mode|454-' .
   'mode|ion-torrent-mode!'    => \$homopolymers_only,   #OPTIONAL [prefilter:
				                         #on, no-prefilter:off]
   'long-homopol-length=i'     => \$long_homopol_length, #OPTIONAL [4]
   'max-long-homopol-diff=i'   => \$max_long_hpl_diff,   #OPTIONAL [2]
   'max-long-homopol-abund=i'  => \$max_long_hpl_abund,  #OPTIONAL [0]
   'max-short-homopol-diff=i'  => \$max_short_hpl_diff,  #OPTIONAL [1]
   'max-short-homopol-abund=i' => \$max_short_hpl_abund, #OPTIONAL [1]

   #Advanced speed options (-h above provides the fastest possible speed)
   'a|minimum-abundance=s'   => \$min_abund,             #OPTIONAL [10]
   'v|heuristic-str-size=s'  => \$heuristic_str_size,    #OPTIONAL [11]
   'heuristic-min-shifts=s'  => \$min_shifted_hits,      #OPTIONAL [1]
   'heuristic-min-directs=s' => \$min_direct_hits,       #OPTIONAL [1]
   'heuristic-thorough!'     => \$mitigate_recips,       #OPTIONAL [Off]
   'parallel-processes=i'    => \$processes,             #OPTIONAL [1/core]
   'gigs-ram=i'              => \$gigs_ram,              #OPTIONAL [auto]
   'bases-per-gig=i'         => \$max_bases_per_gig,     #OPTIONAL [200000]

   #Parsing & joining options
   'q|seq-id-pattern=s'      => \$seq_id_pattern,        #OPTIONAL
                                                         #[^\s*[>\@]\s*([^;]+)]
   'p|abundance-pattern=s'   => \$abundance_pattern,     #OPTIONAL
                                                         #        [size=(\d+);]
   'd|append-delimiter=s'    => \$append_delimiter,      #OPTIONAL
				                         # [Indels=_VAL_HERE_;]
   'fakes-indel-separator=s' => \$indel_sep,             #OPTIONAL [,]
   'fakes-parent-separator=s'=> \$parent_sep,            #OPTIONAL [:]

   #Muscle options
   'y|muscle-exe=s'          => \$muscle,                #OPTIONAL [muscle]
   'use-muscle-gaps!'        => \$muscle_gaps,           #OPTIONAL [Off]
   'align-mode=s'            => \$align_mode,            #OPTIONAL [prefilter:
				                         #global, no-prefilter:
				                         #local](global,local,
				                         #pairwise)
   #Generic options
   'overwrite'               => \$overwrite,             #OPTIONAL [Off]
   'skip-existing!'          => \$skip_existing,         #OPTIONAL [Off]
   'force'                   => \$force,                 #OPTIONAL [Off]
   'verbose:+'               => \$verbose,               #OPTIONAL [Off]
   'quiet'                   => \$quiet,                 #OPTIONAL [Off]
   'debug:+'                 => \$DEBUG,                 #OPTIONAL [Off]
   'help'                    => \$help,                  #OPTIONAL [Off]
   'extended'                => \$extended,              #OPTIONAL [Off]
   'version'                 => \$version,               #OPTIONAL [Off]
   'header!'                 => \$header,                #OPTIONAL [On]
   'error-type-limit=s'      => \$error_limit,           #OPTIONAL [0]
   'dry-run!'                => \$dry_run,               #OPTIONAL [Off]
   'use-as-default|save-as-default!'
                             => \$use_as_default,        #OPTIONAL [Off]
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

#Make sure muscle is properly installed
$muscle = getMuscleExe($muscle);
quit(1) if(incompatible($muscle));

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
if($verbose && !defined($outfile_suffix) && isStandardOutputToTerminal())
  {warning('You have enabled --verbose, but appear to possibly be ',
	   'outputting to the terminal.  Note that verbose messages can ',
	   'interfere with formatting of terminal output making it ',
	   'difficult to read.  You may want to either turn verbose off, ',
	   'redirect output to a file, or supply an outfile suffix (-o).')}

#Make sure there is input
if(scalar(@$input_files) == 0 && isStandardInputFromTerminal())
  {
    error('No input files detected.');
    usage(1);
    quit(-7);
  }

#Make sure that an outfile suffix has been supplied if an outdir has been
#supplied
if(scalar(@$outdirs) && !defined($outfile_suffix))
  {
    error("An outfile suffix (-o) is required if an output directory ",
	  "(--outdir) is supplied.  Note, you may supply an empty string ",
	  "to name the output files the same as the input files.");
    quit(-8);
  }

#Get all the corresponding groups of files and output directories to process
my($input_file_sets,
   $outfile_stub_sets) = getFileSets($input_files,
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

if($seq_id_pattern ne '' &&
   $seq_id_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$seq_id_pattern = '(' . $seq_id_pattern . ')'}

if($append_delimiter !~ /./)
  {warning("The append delimiter (-d) is empty.  The N0 value on the output ",
	   "deflines might be difficult to distinguish from the rest of the ",
	   "line.")}

if($abundance_pattern ne '' &&
   $abundance_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$abundance_pattern = '(' . $abundance_pattern . ')'}

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
    quit(2);
  }

if($processes <= 0)
  {
    if(checkParallelAbility())
      {$processes = getNumCores()}
    else
      {$processes = 1}
  }
if($processes > 1)
  {
    unless(checkParallelAbility())
      {
	error('One or both of the required modules: [IO::Pipe::Producer, ',
	      'IO::Select] is missing.  Unable to run in parallel mode.  ',
	      'Please use `--parallel-processes 1` to proceed.');
	quit(3);
      }
  }

if($gigs_ram <= 0)
  {$gigs_ram = getRamSize()}
elsif($gigs_ram > getRamSize())
  {
    error("--gigs-ram [$gigs_ram] exceeds system limits: [",getRamSize(),
	  "].  Use --force to over-ride.");
    quit(4);
  }

if($gigs_ram < 1)
  {
    error("Invalid RAM size: [$gigs_ram] gigs.  Must have at least 1 gig of ",
	  "ram.  Use --force to over-ride.");
    quit(5);
  }

$max_bases_per_run = int($max_bases_per_gig * $gigs_ram / $processes);

if($min_shifted_hits < 1)
  {
    error("Invalid value for --heuristic-min-directs ($min_shifted_hits).  ",
	  "Must be greater than 0.");
    quit(6);
  }

if($prefilter_mode)
  {
    if(!defined($homopolymers_only))
      {$homopolymers_only = 1}
    if(!defined($align_mode))
      {$align_mode = $align_mode_pref_def}
    if(!defined($sum_abund))
      {$sum_abund = $sum_abund_pref_def}
  }
else
  {
    if(!defined($homopolymers_only))
      {$homopolymers_only = 0}
    if(!defined($align_mode))
      {$align_mode = $align_mode_def}
    if(!defined($sum_abund))
      {$sum_abund = $sum_abund_def}
  }

if(!defined($long_homopol_length))
  {$long_homopol_length = $def_long_homopol_length}

if($long_homopol_length < 0)
  {
    error("Invalid --long-homopol-length [$long_homopol_length].  Cannot be ",
	  "negative.");
    quit(9);
  }

if(!defined($max_long_hpl_diff))
  {$max_long_hpl_diff = $def_max_long_hpl_diff}

if($max_long_hpl_diff < 0)
  {
    error("Invalid --max-long-homopol-diff [$max_long_hpl_diff].  Cannot be ",
	  "negative.");
    quit(10);
  }

if(!defined($max_short_hpl_diff))
  {$max_short_hpl_diff = $def_max_short_hpl_diff}

if($max_short_hpl_diff < 0)
  {
    error("Invalid --max-short-homopol-diff [$max_short_hpl_diff].  Cannot ",
	  "be negative.");
    quit(11);
  }

if(!defined($max_long_hpl_abund))
  {$max_long_hpl_abund = $def_max_long_hpl_abund}

if($max_long_hpl_abund < 0)
  {
    error("Invalid --max-long-homopol-abund [$max_long_hpl_abund].  Cannot ",
	  "be negative.");
    quit(12);
  }

if(!defined($max_short_hpl_abund))
  {$max_short_hpl_abund = $def_max_short_hpl_abund}

if($max_short_hpl_abund < 0)
  {
    error("Invalid --max-short-homopol-abund [$max_short_hpl_abund].  Cannot ",
	  "be negative.");
    quit(13);
  }

if($align_mode =~ /^g/i)
  {$align_mode = 'global'}
elsif($align_mode =~ /^l/i)
  {$align_mode = 'local'}
elsif($align_mode =~ /^p/i)
  {$align_mode = 'pairwise'}
else
  {
    error("Invalid alignment mode: [$align_mode].  Must be 'global', ",
	  "'local', or 'pairwise'.");
    quit(7);
  }

if($max_bases_per_gig < 10000)
  {
    error("--bases-per-gig [$max_bases_per_gig] must be an integer greater ",
	  "than of equal to 10000.");
    quit(8);
  }

debug("PID: $$");

#If output is going to STDOUT instead of output files with different extensions
#or if STDOUT was redirected, output run info once
verbose('[STDOUT] Opened for all output.') if(!defined($outfile_suffix));

#Store info. about the run as a comment at the top of the output file if
#STDOUT has been redirected to a file
if(!isStandardOutputToTerminal() && $header)
  {print(getHeader($extended));}

my $input_file = '';

#For each set of input files associated by getFileSets
foreach my $set_num (0..$#$input_file_sets)
  {
    $input_file      = $input_file_sets->[$set_num]->[0];
    my $outfile_stub = $outfile_stub_sets->[$set_num]->[0];
    my($group_outfile,$reals_outfile,$fakes_outfile);
    if(defined($outfile_suffix))
      {$reals_outfile = $outfile_stub . $outfile_suffix}
    if(defined($group_suffix))
      {$group_outfile = $outfile_stub . $group_suffix}
    if(defined($fakes_suffix))
      {$fakes_outfile = $outfile_stub . $fakes_suffix}

    my $min_nomono_len = 0;
    my $seq_recs = getCheckAllSeqRecs($input_file,$min_abund,\$min_nomono_len);

    if(scalar(@$seq_recs) == 0)
      {
	error("Could not find any sequences in [$input_file].  Skipping.");
	next;
      }

    #Create a record hash to get records via their ID
    my $rec_hash = {};
    $rec_hash->{getID($_)} = $_ foreach(@$seq_recs);

    #Get the sequence combinations likely to contain indels in a 2D hash
    my $heur_hash = getComparisons($seq_recs,
				   $heuristic_str_size,
				   $min_shifted_hits,
				   $mitigate_recips,
				   $min_direct_hits,
				   $rec_hash,
				   $min_nomono_len);

    #Determine sequence groups differing from most abundant sequence by indels
    my $families = getIndelFamilies($rec_hash,$heur_hash);

    #Print family associations
    printGroupFile($group_outfile,$families);

    #Print the filtered sequences
    filterIndels($families,$reals_outfile,$fakes_outfile);
  }

verbose("[STDOUT] Output done.") if(!defined($outfile_suffix));

quit(0);

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
      {$verbose_message =~ s/\r$//}
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
	$main::last_verbose_size  = 0;
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

    debug("Times: [",join(',',@$main::time_marks),"].  Time since time mark ",
	  "[$mark_index] is [$time - $main::time_marks->[$mark_index] = ",
	  "$time_since_mark seconds].") if($DEBUG < -100);

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

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
    my $template_version_number = '2.15';
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

    #Report the number of errors, warnings, and debugs on STDERR
    printRunReport($verbose) if(!$quiet && ($verbose || $DEBUG ||
					    defined($main::error_number) ||
					    defined($main::warning_number)));

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
		     "in-place.\n")}
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
	#Allow them to have submitted an array of arrays of scalars
	if(scalar(@errors) == scalar(map {@$_} @$file_types_array) &&
	   scalar(@errors) == scalar(grep {$_ eq 'SCALAR'} @errors))
	  {$file_types_array = [$file_types_array]}
	else
	  {
	    #Reset the errors because I'm not looking for SCALARs anymore
	    @errors = map {my @x=@$_;map {ref($_)} @x}
	      grep {my @x=@$_;scalar(grep {ref($_) ne 'ARRAY'} @x)}
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
	    if(scalar(@$file_type_array) ==
	       scalar(grep {scalar(@$_) < $max_num_cols}
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
		    push(@{$infile_sets_array->[-1]},
			 $file_types_array->[$association]->[$row_index]
			 ->[$col_index]);

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
	foreach my $outfile_stub (@$outfile_stubs_for_input_files)
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
    my $status              = 1;

    #Open the output file
    if(!$dry_run && !open($file_handle,">$current_output_file"))
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
	else
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

* WHAT IS THIS: This script takes a sequence file with abundance values on the
                deflines and filters out lesser abundant sequences which only
                differ from more abundant sequences by indels.  If
                --sum-abundances is supplied, the abundance of the kept
                sequence is increased by the abundance of its indel-only
                matches.  In --homopolymer-mode, only homopolymer indels that
                are likely to be the result of a sequencing error from
                sequencing platforms such as Roche 454 FLX or Life Technologies
                Ion Torrent will be filtered (all other homopolymer and non-
                homopolymer indels are retained).

                This script is intended to be used as the step after
                getReals.pl in the 'CFF' (cluster free filtering) pipeline.
                However, it can also be run before mergeSeqs.pl, especially if
                homopolymer indels are a concern.  If you supply the
                --pre-filter-mode flag, the script will set --sum-abundances to
                true, --homopolymer-mode to true, and if --homopolymer-mode is
                explicitly set to false, it will set --align-mode to global.
                --homopolymer-mode is extremely fast compared to all other
                modes.

                This script, when --homopolymer-mode is not supplied, uses a
                sequence alignment tool called muscle to determine the
                existence of indels and substitutions.

* INPUT FORMAT: A Fasta or Fastq sequence file whose deflines contain abundance
                values.  The default format for abundance values is "size=#;"
                where "#" is an integer representing the abundance value.
                Other formats are acceptable as long as you can use -p to
                extract it.  Example default fasta format:

>6_1_1;ee=0.049;size=2192;N0=0;
TACGTATGTCACAAGCGTTATCCGGATTTATTGGGCGTAAAGCGCGTCTAGGTGGTTATGTAAGTCTGATGTGAAAATGCAGGGCTCAACTCTGTATTGCGTTGGAAACTGCATGACTAGAGTACTGGAG
>6_1_16;ee=0.038;size=2190;N0=0;
TACGGAGGGTGCGAGCGTTAATCGGAATAACTGGGCGTAAAGGGCACGCAGGCGGTGACTTAAGTGAGGTGTGAAAGCCCCGGGCTTAACCTGGGAATTGCATTTCATACTGGGTCGCTAGAGTACTTTA
>6_1_6;ee=0.053;size=2048;N0=0;
TACGGAAGGTCCAGGCGTTATCCGGATTTATTGGGTTTAAAGGGAGCGTAGGCTGGAGATTAAGTGTGTTGTGAAATGTAGACGCTCAACGTCTGAATTGCAGCGCATACTGGTTTCCTTGAGTACGCAC
>6_1_5;ee=0.053;size=1701;N0=0;
TACGTAGGGTGCGAGCGTTAATCGGAATTACTGGGCGTAAAGCGAGCGCAGACGGTTACTTAAGCAGGATGTGAAATCCCCGGGCTCAACCTGGGAACTGCGTTCTGAACTGGGTGACTAGAGTGTGTCA
>6_1_10;ee=0.025;size=1212;N0=0;
TACGTAGGTCCCGAGCGTTGTCCGGATTTATTGGGCGTAAAGCGAGCGCAGGCGGTTAGATAAGTCTGAAGTTAAAGGCTGTGGCTTAACCATAGTACGCTTTGGAAACTGTTTAACTTGAGTGCAAGAG

                Note, all other values on the deflines are ignored.

* REALS FORMAT: Fasta format, the same as the INPUT FORMAT, only containing
                sequences determined to be "real".  Abundances may be increased
                if --454-mode (or --sum-abundances) is supplied.  Also see
                HEADER FORMAT in the --extended --help output.

* FAKES FORMAT: Fasta format, the same as the INPUT FORMAT, only containing
                sequences determined to be "fake".  Additionally, a description
                of the indels found with a more abundant sequence will be
                appended to the defline.  See -d and --fakes-indel-separator
                for more information.  Refer to -j and GROUPS FORMAT in order
                to get the associated sequences.  Also see HEADER FORMAT in the
                --extended --help output.

* GROUPS FORMAT: Tab delimited text file containing these columns: Sequence ID
                 (see -q), group number, and indel descriptions (see -d and
                 --fakes-indel-separator).  A column header row will appear at
                 the top (if --header is supplied).  Also see HEADER FORMAT in
                 the --extended --help output.  Here's an example of the file
                 format with the header:

#filterIndels.pl Version 1.0
# Created: 4/2/2014
# Last modified: Fri Apr  4 13:33:24 2014
#Fri Apr  4 13:33:29 2014
#/usr/bin/perl ../src/filterIndels.pl -i 6_1.drp.fna.n0s-qtest.reals -o .reals --verbose --overwrite -j .indels -f .fakes
#ID     GroupNum        indelDescription(*=GroupSeed)
6_1_1   1       *
6_1_16  2       *
6_1_6   3       *
6_1_5   4       *
6_1_10  5       *
6_1_3   6       *

end_print

    if($advanced)
      {
	my $header          = getHeader();
	my $extended_header = getHeader(1);

	print << "end_print";
* HEADER FORMAT: If --header is supplied, every output file, including output
                 to standard out, will get a header that is commented using the
                 '#' character (i.e. each line of the header will begin with
                 '#').  The format of the standard header looks like this:

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

    print("\n$script -i \"input file(s)\" [-o .out] $options\n",
	  (!$advanced_mode ? '' :
	   "$script -i \"outfile_stub\" [-o .out] $options < input_file\n"),
	  "\n");

    if($no_descriptions)
      {print("Run with no options for usage.\n")}
    else
      {
	if(!$advanced_mode)
	  {
	    print << "end_print";
     -i|--input-file      REQUIRED Fasta/fastq sequence file(s).  See --help.
     -o|--reals-suffix    OPTIONAL [stdout] Outfile extension appended to -i to
                                   output "real" sequences.  See --help.
     -f|--fakes-suffix    OPTIONAL [no output] Outfile extension appended to
                                   -i to output "fake" sequences.  See --help.
     -j|--group-suffix    OPTIONAL [no output] Outfile extension appended to
                                   -i to output indel grouping information.
                                   See --help.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Also see --help.
     --pre-filter-mode    OPTIONAL Adjust options for preprocessing sequence
                                   data with "frequent" indel errors.
     --sum-abundances     OPTIONAL Add abundance of filtered sequence to parent
                                   sequence differing by indels.
     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --version            OPTIONAL Print version info.
     --help               OPTIONAL Print info and format descriptions.
     --extended           OPTIONAL Print extended usage/help/version/header
                                   when supplied with corresponding the flags.

Run with just --extended for advanced options & more details.
end_print
	  }
	else #Advanced options/extended usage output
	  {
	    print << "end_print";
     -i|--input-file*     REQUIRED Space-separated fasta or fastq sequence
                                   file(s) inside quotes
                                   (e.g. -i "*.txt *.text").  Expands standard
                                   bsd glob characters (e.g. '*', '?', etc.).
                                   When standard input detected, -o has been
                                   supplied, and -i is given only 1 value, it
                                   will be used as a file name stub.  See
                                   --extended --help for input file format and
                                   more advanced usage examples.  *No flag
                                   required.
     -t|--filetype        OPTIONAL [auto](fasta,fastq,auto) Input file (-i)
                                   type.  Using this instead of auto will make
                                   file reading faster.  "auto" cannot be used
                                   when redirecting a file in.
     -o|--outfile-suffix| OPTIONAL [stdout] Outfile extension appended to -i.
        --reals-suffix             Sequences which do not differ with other
                                   more abundant sequences by indels only will
                                   be written to this file in fasta format.
                                   Will not overwrite without --overwite.
                                   Explicitly supplying an empty string will
                                   use the input file name as the output file
                                   name (use with --outdir or when -i is a
                                   stub).  When standard input is detected and
                                   no stub is provided with -i, appends to the
                                   string "STDIN".  Does not replace existing
                                   input file extensions.  Default behavior
                                   prints output to standard out.  See
                                   --extended --help for output file format and
                                   more advanced usage examples.
     -f|--fakes-suffix    OPTIONAL [no output] Outfile extension appended to
                                   -i.  Lesser abundant sequences which differ
                                   with more abundant sequences by indels only
                                   will be written to this file in fasta
                                   format.  Will not overwrite without
                                   --overwite.  Explicitly supplying an empty
                                   string will use the input file name as the
                                   output file name (use with --outdir or when
                                   -i is a stub).  When standard input is
                                   detected and no stub is provided with -i,
                                   appends to the string "STDIN".  Does not
                                   replace existing input file extensions. 
                                   Default behavior prints no out.  See
                                   --extended --help for output file format and
                                   more advanced usage examples.
     -j|--group-suffix    OPTIONAL [no output] Outfile extension appended to
                                   -i to output indel grouping information to.
                                   Will not overwrite without --overwite.
                                   Explicitly supplying an empty string will
                                   use the input file name as the output file
                                   name (use with --outdir or when -i is a
                                   stub).  When standard input is detected and
                                   no stub is provided with -i, appends to the
                                   string "STDIN".  Does not replace existing
                                   input file extensions.  Default behavior
                                   prints no output.  See --extended --help for
                                   output file format and more advanced usage
                                   examples.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Default output
                                   directory is the same as that containing
                                   each input file.  Relative paths will be
                                   relative to each individual input file.
                                   Creates directories specified, but not
                                   recursively.  Also see --extended --help for
                                   more advanced usage examples.
     --sum-abundances|    OPTIONAL [Off*] When filtering out lesser abundant
     --no-sum-abundances           sequences differing from more abundant
                                   sequences only by indels, add the abundance
                                   of the lesser abundant sequence to the more
                                   abundant sequence.  *The default mode is
                                   "off" unless --pre-filter-mode is provided.
     --pre-filter-mode    OPTIONAL Adjust options for preprocessing sequence
                                   data containing "frequent" indel errors.
                                   Unless explicitly set, it turns on
                                   --sum-abundances, turns on
                                   --homopolymer-mode, and sets --align-mode to
                                   'global'.  Note, --align-mode is ignored
                                   when --homopolymer-mode is turned on.
     -h|                  OPTIONAL [Off*] Only filter lesser abundant sequences
     --homopolymer-mode|           that differ only by homopolymer indels which
     --454-mode|                   are likely to be the result of a sequencing
     --ion-torrent-mode|           error characteristic of systems such as the
     --no-~                        Roche 454 FLX pyrosequencer or the Life
                                   Technologies Ion Torrent sequencer.
                                   Sequences containing at least 1 indel deemed
                                   to be "real" or a pair of indels which could
                                   be interpretted as a pair of substitutions
                                   will not be filtered.  Supplying this option
                                   makes filtering extremely fast compared to
                                   all other methods.  This option activates
                                   the following options that are otherwise
                                   ignored: --long-homopol-length,
                                   --max-long-homopol-diff,
                                   --max-long-homopol-abund,
                                   --max-short-homopol-diff, and
                                   --max-short-homopol-abund.
                                   *Default is affected by --pre-filter-mode.
                                   ~This option is negatable when preceded by
                                   --no (e.g. --no-454-mode).
     --long-homopol-      OPTIONAL [4] This is the size at and above which a
       length                      homopolymer is considered to be "long".
                                   When determining whether a difference in
                                   length of homopolymers between 2 sequences
                                   is a sequencing error or not, the shorter of
                                   the homopolymers must be at least this
                                   length to be considered "long".  Otherwise,
                                   it is treated as "short".  Sequencing error
                                   rates of long homopolymers are higher than
                                   those of short homopolymers.  See
                                   --max-long-homopol-diff and
                                   --max-long-homopol-abund to see how long
                                   homopolymer indels are filtered.  See
                                   --max-short-homopol-diff and
                                   --max-short-homopol-abund to see how short
                                   homopolymer indels are filtered.
     --max-long-homopol-  OPTIONAL [2](0,2-inf) Maximum allowed difference in
       diff                        length of "long" homopolymers (as determined
                                   by --long-homopol-length) in order to be
                                   considered a homopolymer indel and filtered.
                                   A value of 0 indicates that there is no
                                   maximum.  Ignored unless -h is provided.
     --max-long-homopol-  OPTIONAL [0] Maximum allowed abundance of "long"
       abund                       homopolymers (as determined by
                                   --long-homopol-length) in order to be
                                   considered a homopolymer indel.  Sequencing
                                   errors should have very low abundance
                                   compared to other types of errors.  A value
                                   of 0 indicates that there is no maximum.
     --max-short-homopol- OPTIONAL [1] Maximum allowed difference in length of
       diff                        "short" homopolymers (as determined by
                                   --long-homopol-length) in order to be
                                   considered a homopolymer indel and filtered.
                                   Ignored unless -h is provided.  A value of 0
                                   indicates that there is no maximum.
     --max-short-homopol- OPTIONAL [1] Maximum allowed abundance of "short"
       abund                       homopolymers (as determined by
                                   --long-homopol-length) in order to be
                                   considered a homopolymer indel.  Sequencing
                                   errors should have very low abundance
                                   compared to other types of errors.  A value
                                   of 0 indicates that there is no maximum.
     --max-homopol-diff   OPTIONAL [2] Maximum allowed difference in length of
                                   homopolymer indels to be considered a 454-
                                   introduced indel error.  Otherwise, in -h
                                   mode, it will be skipped.  This limit causes
                                   pairs of sequential long indels to be
                                   substitutions (e.g. TATATAAAAAATATA versus
                                   TATAAAAAATATATA will be considered 2
                                   substitutions instead of 2 indels).  See -h.
     -q|--seq-id-pattern  OPTIONAL [^\\s*[>\\\@]\\s*([^;]+)] A perl regular
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
     -p|--abundance-      OPTIONAL [size=(\\d+);] A perl regular expression
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
     -d|--append-         OPTIONAL [Indels=$value_subst_str;] The indel
        delimiter                  descriptions that are appended to the
                                   defline of the sequence file will be
                                   demilimited by this string.  If the
                                   delimiter contains the string
                                   '$value_subst_str', that string will be
                                   replaced with the indel descriptions so that
                                   you can control where they are placed in the
                                   delimiting string.
     --fakes-indel-       OPTIONAL [,] The delimiter joining multiple indel
       separator                   descriptions.  This creates the full string
                                   value that will be inserted into the
                                   substitution string of -d.
     --fakes-parent-      OPTIONAL [:] The indel descriptions (see -d) are
       separator                   prepended by the ID of the more abundant
                                   parent sequence with which the sequence of
                                   each record differs by inly indels.  The
                                   prepended string will be followed by this
                                   character and the indel descriptions
                                   delimited by the --fakes-indel-separator.
     -a|--minimum-        OPTIONAL [1] The abundance threshold at or above
        abundance                  which a sequence will be used.  The larger
                                   this number, the faster the computation and
                                   the less accurate subsequent error estimates
                                   will be.  When a low abundance sequence is
                                   excluded, it will not be in any of the
                                   output files.  0 or 1 means use all
                                   sequences.
     --align-mode         OPTIONAL [local*](global,local,pairwise) Alignments
                                   are performed on select sequence
                                   combinations to determine which sequences
                                   differ only by indels.  This option
                                   determines how many sequences will be
                                   aligned together.  In any of the 3 modes,
                                   sequences are only aligned if they have a
                                   "shifted hit" (determined by -v).  The more
                                   sequences aligned together, the faster this
                                   script runs, but the more errors introduced
                                   by misalignments.  Pairwise is the slowest
                                   and most accurate.  Local mode groups like-
                                   sequences which are likely to have indels
                                   (determined by -v) and an alignment is
                                   performed for each group.  Local alignments
                                   provide a considerable speed-increase with
                                   only a rare possibility for inaccuracy.  A
                                   global alignment is the fastest, but is
                                   likely to produce a handful of errors:
                                   missing the optimal indel-only sequence
                                   pairs or not filtering out sequences
                                   differing only by indels.  Alignments tasks
                                   will be segmented for a performance boost
                                   when setting --parallel-processes to a value
                                   greater than 1.  Segmentation is done using
                                   the data structure created from -v.
                                   Sequences bearing similarity to multiple
                                   segmented groups are duplicated and aligned
                                   to each group, so it is recommended to use
                                   the default value for --parallel-processes,
                                   as higher values can adversely affect
                                   running time.  Note, global mode can produce
                                   differing results for different values
                                   supplied to --parallel-processes due to the
                                   ambiguities of multiple sequence alignments.
                                   A lesser abundant sequence will always list
                                   indels as they related to the greatest
                                   abundant sequence with which they differ by
                                   only indels.  *The default mode is "local"
                                   unless --454-mode or --homopolymer-mode are
                                   provided.
     -v|--heuristic-str-  OPTIONAL [11] Only compare sequences that have this
        size                       size subsequence in different places (this
                                   will be referred to as a "shifted hit").
                                   The slowest portion of this analysis is
                                   alignments - the more sequences to align,
                                   the slower the script.  This heuristic cuts
                                   down the number of necessary alignments
                                   dramatically with little to no cost in
                                   accuracy.  A sliding window of subsequences
                                   are hashed with their positions as they are
                                   read in.  If subsequences are "shifted"
                                   relative to one-another, they will be
                                   aligned together to determine if they differ
                                   only by indels.  If a sequence has no
                                   shifted hits with any other sequence, it
                                   will be assumed to not differ by only
                                   indels.  To compare all sequences, set this
                                   option to 0.  Ignored when -h is supplied.
                                   NOTE: The fastest speed-up option for this
                                   script is --homopolymer-mode, but this
                                   option is useful if that is undesireable.
                                   Refer to other --heuristic-... options to
                                   fine-tune the heuristic.
     --heuristic-min-     OPTIONAL [1] Minimum number of non-overlapping
       shifts                      shifted heuristic string hits between two
                                   sequences required to exist in order to
                                   qualify for alignment.  See -v.  NOTE: Hits
                                   are allowed to overlap.  Requires -v to be
                                   greater than 0.
     --heuristic-min-     OPTIONAL [1] Require a pair of sequences which have a
       directs                     shifted hit (see -v) at a position greater
                                   than the value of -v to have this many
                                   "direct hits" (subsequences of size -v in
                                   the same position).  Note, direct hits may
                                   overlap.  Requires -v to be greater than 0.
     --heuristic-thorough OPTIONAL In addition to the -v heuristic, this option
                                   mitigates the possibility of missing an
                                   equally sized homopolymer insertion/
                                   deletion combination within -v bases from
                                   one another (which the -v heuristic would
                                   miss).  Not recommended when indels are not
                                   likely because it greatly increases running
                                   time.  Consider using with --454-mode.
                                   Requires -v to be greater than 0.
     --bases-per-gig      OPTIONAL [200000] The maximum number of bases to be
                                   aligned by muscle per gig of ram (see
                                   --gigs-ram).  This script splits up
                                   alignments based on the heuristic parameters
                                   (aligning only sequences that share a
                                   "shifted hit").  If the amount of ram per
                                   process is too little for the size of the
                                   alignment and there is enough total ram on
                                   your machine, the script temporarily reduces
                                   the number of concurrent alignments until
                                   the largest ones are done.  Otherwise, it
                                   breaks up the alignment into chunks and
                                   merges them when done.
     --gigs-ram           OPTIONAL [auto] An integer indicating the total
                                   number of gigs of memory to use as a maximum
                                   when deciding how many concurrent processes
                                   to run (see --parallel-processes) and when
                                   determining the maximum number of sequences
                                   to align together (see --bases-per-gig).
     -y|--muscle-exe      OPTIONAL [muscle] The command to use to call muscle.
     --use-muscle-gaps    OPTIONAL Use muscle's default context-dependent gap
                                   penalties instead of our default static gap
                                   penalties of `-gapopen -400 -gapextend -399`
                                   designed to target homopolymer indels (and
                                   treat the terminal gaps properly).
     --parallel-processes OPTIONAL [1/core*] Specify the number of forked
                                   processes to use to increase speed.  Each
                                   process handles a set of muscle alignments.
                                   *By default, this script will set this value
                                   to the number of cores on your machine.
                                   Even when set to 1, this script will always
                                   perform & process alignments in a child
                                   process.  This number is a maximum.  You can
                                   run more processes than the available cores,
                                   but note that you may hit some system
                                   limitations.  The number of processes used
                                   at any one time depends on whether there is
                                   enough available memory.  See --gigs-ram and
                                   --bases-per-gig for more details.  Must be
                                   greater than 0.
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
                                   command line call at the top of each
                                   outfile.
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
                                   Supply alone for extended usage.  Includes
                                   extended version in output file headers.
                                   Incompatible with --noheader.  See --help &
                                   --version.
end_print
	  }

	my @user_defaults = getUserDefaults();
	print("Current user defaults: [@user_defaults].\n");
      }

    return(0);
  }

#Returns alignment in clustalw format of 2 very similar sequences using muscle
#Uses global: $muscle, $verbose
sub getMuscleMultipleAlignment
  {
    my $rec_hash   = $_[0];
    my $musclegaps = defined($_[1]) ? $_[1] : $muscle_gaps;
    my $afa_format = defined($_[2]) ? $_[2] : 0;
    my @ids        = @_[3..$#_];
    my $muscle_exe = defined($muscle) && $muscle ne '' ? $muscle : 'muscle';

    if(scalar(@ids) < 2)
      {
	error("Not enough sequences sent in.");
	return('');
      }

    my @seqs = map {exists($rec_hash->{$_}) &&
		      scalar(@{$rec_hash->{$_}}) >= 2 &&
			defined($rec_hash->{$_}->[1]) &&
			  $rec_hash->{$_}->[1] ne '' ?
			    $rec_hash->{$_}->[1] : undef} @ids;

    if(scalar(grep {!defined($_)} @seqs))
      {
	error("The IDs sent in did not correspond to valid sequence records.");
	return('');
      }

    use IPC::Open3;
    use IO::Select;

    my $output = '';
    local *ALNOUT;
    local *ALNERR;
    my $stdout = \*ALNOUT;
    my $stderr = \*ALNERR;

    my $muscle_command = "$muscle_exe -in /dev/stdin -out /dev/stdout " .
      '-maxiters 1 -diags' .
	($verbose ? '' : ' -quiet') .
	  ($afa_format ? '' : ' -clw') .
	    ($musclegaps ? '' : ' -gapopen -400 -gapextend -399');

    #Run the muscle command using open3, which will take input printed to ALNIN
    #as standard inout and it will output the alignment to $stdout and status/
    #errors to $stderr
    my $pid = open3(\*ALNIN, $stdout, $stderr, $muscle_command);

    #open3 succeeded if we got a PID
    if($pid)
      {
	my $fasta = ">" . join("\n>",map {"$_\n$seqs[$_]"} (0..$#ids)) . "\n";

	print ALNIN ($fasta);
	close(ALNIN);

	#Use a Select opject to determine which handles are ready to be read
	my $sel = new IO::Select;
	$sel->add($stderr,$stdout);

	my(@buffer);
	my $cnt = 0;
	my $max = scalar(@ids);
	my $line_num = 0;
	my $verbose_freq = 1000;

	#While we have a file handle with output
	while(my @fhs = $sel->can_read())
	  {
	    #For each file handle with output
	    foreach my $fh (@fhs)
	      {
		my $line;
		if($fh eq $stderr)
		  {
		    #Doing this to get ongoing status of muscle
		    local $/ = "\r";
		    $line = <$fh>;
		  }
		else
		  {
		    local $/ = "\n";
		    $line = <$fh>;
		  }

		#If we hit the end of the file, remove the handle & continue
		unless(defined($line))
		  {
		    $sel->remove($fh);
		    next;
		  }

		#Put output in the correct variable based on the current handle
		if($fh eq $stdout)
		  {
		    $line_num++;
		    if($afa_format)
		      {
			if($line =~ /^>(\d+)/)
			  {
			    my $tid = $1;
			    if($tid <= $#ids)
			      {
				$line =~ s/^$tid/$ids[$tid]/;
				$cnt++;
			      }
			    else
			      {error("Could not parse muscle alignment line: ",
				     "[$line].")}
			  }
			elsif($line !~ /^[A-Z]+$/)
			  {error("Could not parse muscle alignment line: ",
				 "[$line].")}
			$output .= $line;
		      }
		    else
		      {
			if($line =~ /^(\d+)\s/)
			  {
			    my $tid = $1;
			    if($tid <= $#ids)
			      {
				$line =~ s/^$tid/$ids[$tid]/;
				$buffer[$tid] = $line;
				$cnt++;
				if($cnt == $max)
				  {
				    $output .= join('',@buffer);
				    @buffer = ();
				    $cnt = 0;
				  }
			      }
			    else
			      {
				error("Could not parse muscle alignment ",
				      "line: [$line].");
				$output .= $line;
			      }
			  }
			elsif($line =~ /^[ *]+$/)
			  {
			    if(scalar(@buffer))
			      {error("Parse error: Match line encountered ",
				     "without the buffer being flushed.")}
			    $output .= $line;
			  }
			elsif($line !~ /^MUSCLE|^\s*$/)
			  {
			    error("Could not parse muscle alignment line: ",
				  "[$line].");
			    $output .= $line;
			  }
		      }
		  }
		elsif($fh eq $stderr)
		  {
		    if($line !~ /^MUSCLE\sv|drive5\.com|This\ssoftware|^\s*$|
				 ^Please\scite|^stdin\s/x || $verbose > 2)
		      {
			$line =~ s/[\r\n]//g;
			verbose('Aligning... ',$line);
		      }
		  }
		else
		  {error("Unable to determine file handle.")}
	      }
	  }

	if(!$afa_format)
	  {
	    if($cnt == $max)
	      {$output .= join('',@buffer)}
	    elsif(scalar(@buffer))
	      {
		my $bsize = scalar(grep {defined($_)} @buffer);
		error("Buffer unfinished (contains $bsize of $max sequence ",
		      "segments), but there's no alignment output left ",
		      "to parse.  Importing unfinished buffer.");
		$output .= join('',grep {defined($_)} @buffer);
	      }
	    elsif($output eq '')
	      {error("Could not parse muscle alignment.")}
	    elsif($cnt)
	      {error("Muscle alignment parse error.")}
	  }

	close($stdout);
	close($stderr);
      }
    else
      {
	error("open3() failed $!");
	return('');
      }

    debug("Waiting on muscle to finish up...");

    waitpid($pid,0) if($pid);

    debug("Muscle alignment done",($DEBUG > 2 ? ": $output" : '.'));

    return($output);
  }

#Takes a string alignment of 2 sequences in clustalw format and returns the
#number of non-terminal indels, substitutions, and non-terminal gaps.  Non-
#terminal means gap characters in either sequence at the 3' end of the
#alignment.  All coords reported are relative to the second sequence.  If
#argument 2 is non-zero, the sub will return unfinished values when it
#encounters the first instance of a substitution
sub clustalw2indelsSubs
  {
    my $alignment_str = $_[0];
    my $stop_at_sub   = $_[1];
    my $hash          = {};

    if(!defined($alignment_str) || $alignment_str eq '')
      {
	error("Empty alignment string sent in.");
	return(undef,undef,[],[]);
      }

    my $first_id  = '';
    my $second_id = '';

    while($alignment_str =~ /([^\n]*\n?)/g)
      {
	my $line = $1;
	next if($line =~ /^\s*$/ || $line =~ /^\s*(MUSCLE|CLUSTALW)/ ||
	        $line =~ /^[ *]+$/);
	chomp($line);

	#Grab the ID and the sequence string.
	if($line =~ /(.*\s)(\S+)$/)
	  {
	    $hash->{$1} .= uc($2);
	    if($first_id eq '')
	      {$first_id = $1}
	    elsif($1 ne $first_id && $second_id eq '')
	      {$second_id = $1}
	  }
      }

    if($second_id eq '')
      {
	error("clustalw output did not contain enough sequences ",
	      "[$alignment_str].  First ID was [$first_id].");
	return(undef,undef,[],[]);
      }

    my $numnontermindels = 0; #Total number of strings of contiguous gap chars
    my $indels           = []; #Array of insertion & deletion descriptions

    debug("$first_id/$second_id before shortening:\n$hash->{$first_id}\n",
	  $hash->{$second_id})
      if($DEBUG > 4);

    my $debug_message = '';
    #Trim off terminal gaps
    if($hash->{$first_id} =~ /(-+)$/)
      {
	my $gl = -length($1);
	$debug_message = join('',"Shortened $first_id/$second_id alignment ",
			      "by $gl for end-gaps in first sequence: ",
			      "[$hash->{$first_id}].");
	$hash->{$first_id}  = substr($hash->{$first_id} ,0,$gl);
	$hash->{$second_id} = substr($hash->{$second_id},0,$gl);
      }
    elsif($hash->{$second_id} =~ /(-+)$/)
      {
	my $gl = -length($1);
	$debug_message = join('',"Shortened $first_id/$second_id alignment ",
			      "by $gl for end-gaps in second sequence: ",
			      "[$hash->{$second_id}].");
	$hash->{$first_id}  = substr($hash->{$first_id} ,0,$gl);
	$hash->{$second_id} = substr($hash->{$second_id},0,$gl);
      }

    #Count the number of substitutions & record positions where it's a gap in
    #both sequences
    my $aln_len  = length($hash->{$first_id});
    my $dbl_gaps = [];
    my $gaps_in2 = [];
    my $p2       = 0;
    my $subs     = [];
    my $numsubs  = 0;

    foreach my $position (0..($aln_len - 1))
      {
	if($DEBUG && ($position + 1) > length($hash->{$first_id}))
	  {
	    error("String index [$position] is greater than the string ",
		  "length: [recorded: $aln_len, actual: ",
		  length($hash->{$first_id}),"]: [$hash->{$first_id}].");
	    next;
	  }

	my $b1 = substr($hash->{$first_id} ,$position,1);
	my $b2 = substr($hash->{$second_id},$position,1);
	$p2++ if($b2 ne '-');
	if($b1 eq '-' && $b2 eq '-')
	  {push(@$dbl_gaps,$position)}
	elsif($b2 eq '-')
	  {push(@$gaps_in2,1)}
	else
	  {push(@$gaps_in2,0)}
	next if($b1 eq $b2 || $b1 eq '-' || $b2 eq '-');
	return(wantarray ?
	       (0,1,'',"snp $p2$b1>$b2") :
	       [0,1,'',"snp $p2$b1>$b2"]) if($stop_at_sub);
	push(@$subs,"snp $p2$b1>$b2");
	$numsubs++;
      }

    debug("Found $numsubs substitutions between $first_id/$second_id.")
      if($DEBUG > 4);

    #Remove dual-gap positions, then re-adjust the alignment length
    foreach my $gp (reverse(@$dbl_gaps))
      {
	if($DEBUG && ($gp + 1) > length($hash->{$first_id}))
	  {
	    error("Gap string index [$gp] is greater than the string ",
		  "length: [",length($hash->{$first_id}),
		  "]: [$hash->{$first_id}].");
	    next;
	  }
	substr($hash->{$first_id} ,$gp,1,'');
	substr($hash->{$second_id},$gp,1,'');
      }
    $aln_len = length($hash->{$first_id});

    debug("Shortened $first_id/$second_id alignment by ",scalar(@$dbl_gaps),
	  ". New alignments:\n$hash->{$first_id}\n$hash->{$second_id}")
      if($DEBUG > 4);

    #Now take a look at the sequences and count the gap characters and number
    #of contiguous sets of gap characters.

    #Gaps here are insertions (relative to sequence 1 as the original sequence)
    while($hash->{$first_id} =~ /(-+)/g)
      {
	my $gaps = $1;
	$numnontermindels++;

	#Go through each position to remove from the subs_hash (so that it is
	#only left with substitutions)
	my $start = pos($hash->{$first_id}) - length($gaps);
	my $stop  = pos($hash->{$first_id}) - 1;

	#Record insertion value & position (relative to the real 2nd sequence)
	my $coord = $start + 1 -
	  scalar(grep {$_} @{$gaps_in2}[0..($start - 1)]);
	my $nts   = substr($hash->{$second_id},
			   $start,
			   length($gaps));

	my $hp = '';
	if(isHomopolymer(($start == 0 ? '' : substr($hash->{$second_id},
						    $start - 1,
						    1)),
			 $nts,
			 ($stop + 1 == $aln_len ? '' :
			  substr($hash->{$second_id},
				 $stop + 1,
				 1))))
	  {$hp = 'homopolymer '}

	push(@$indels,$hp . "ins $coord$nts");
      }

    my $num_gap_chars = 0;
    #Gaps here are deletions (relative to sequence 1 as the original sequence)
    while($hash->{$second_id} =~ /(-+)/g)
      {
	my $gaps = $1;
	$numnontermindels++;

	#Go through each position to remove from the subs_hash (so that it is
	#only left with substitutions)
	my $start = pos($hash->{$second_id}) - length($gaps);
	my $stop  = pos($hash->{$second_id}) - 1;

	#Record the insertion position & value
	my $coord = $start + 1 - $num_gap_chars;
	my $nts = substr($hash->{$first_id},
			 $start,
			 length($gaps));

	$num_gap_chars += length($gaps);

	my $hp = '';
	if(isHomopolymer(($start == 0 ? '' : substr($hash->{$first_id},
						    $start - 1,
						    1)),
			 $nts,
			 ($stop + 1 == $aln_len ? '' :
			  substr($hash->{$first_id},
				 $stop + 1,
				 1))))
	  {$hp = 'homopolymer '}

	push(@$indels,$hp . "del $coord$nts");
      }

    debug("Returning diffs between $first_id/$second_id: $numnontermindels ",
	  "indels & $numsubs subs.")
      if($DEBUG > 4);

    return(wantarray ?
	   ($numnontermindels,$numsubs,$indels,$subs) :
	   [$numnontermindels,$numsubs,$indels,$subs]);
  }

#Globals used: $processes, $align_mode
sub getIndelFamilies
  {
    my $rec_hash      = $_[0];
    my $heur_hash     = $_[1];
    my $aln_strs      = {};
    my $families      = [];
    my $already_added = {};
    my $firstd_size   = scalar(keys(%$heur_hash));
    my $cnt1          = 0;

    if($align_mode ne 'global' && $align_mode ne 'local' &&
       $align_mode ne 'pairwise')
      {
	error("Invalid alignment mode: [$align_mode].");
	return($families);
      }

    #If we're in homopolymer mode, the heuristic hash should already be loaded
    #with indel descriptions, so we do not need to load it.  But if we're not
    #in homopolymers_only mode, then we're going to need to align all the pairs
    #of sequences and define the indel strings.
    if(!$homopolymers_only)
      {
	my $split_sets = divideGroupings($heur_hash,$rec_hash);
	alignLoadHeurHash($processes,
			  $rec_hash,
			  $heur_hash,
			  $split_sets,
			  $align_mode);
      }

    verbose("Grouping into indel families.");

    #For each first ID in order of descending abundance and ascending ID
    foreach my $id1 (sort {$rec_hash->{$b}->[2]->{ABUND} <=>
			     $rec_hash->{$a}->[2]->{ABUND} || $a cmp $b}
		     keys(%$heur_hash))
      {
	$cnt1++;

	next if(exists($already_added->{$id1}));

	my $rec1         = $rec_hash->{$id1};
	my $secondd_size = scalar(keys(%{$heur_hash->{$id1}}));

	push(@$families,[$rec1]);
	$already_added->{$id1} = 1;
	debug("Putting ",getID($rec1)," in new group.")
	  if($DEBUG > 4);

	my $cnt2 = 0;
	foreach my $id2 (sort {$rec_hash->{$b}->[2]->{ABUND} <=>
				 $rec_hash->{$a}->[2]->{ABUND} || $a cmp $b}
			 keys(%{$heur_hash->{$id1}}))
	  {
	    $cnt2++;

	    next if(exists($already_added->{$id2}));

	    my $rec2 = $rec_hash->{$id2};

	    verboseOverMe("Comparing record $cnt1 of $firstd_size and $cnt2 ",
			  "of $secondd_size [$id1 vs $id2].");

	    if(exists($heur_hash->{$id1}) &&
	       exists($heur_hash->{$id1}->{$id2}) &&
	       defined($heur_hash->{$id1}->{$id2}) &&
	       ref($heur_hash->{$id1}->{$id2}) eq 'ARRAY' &&
	       scalar(@{$heur_hash->{$id1}->{$id2}}))
	      {
		my $tsize1 = getAbund($rec_hash->{$id1});
		my $tsize2 = getAbund($rec_hash->{$id2});

		next if($tsize1 == $tsize2);

		$already_added->{$id2} = 1;

		if(scalar(@$rec2) >= 3 && ref($rec2->[2]) eq 'HASH' &&
		   exists($rec2->[2]->{PARENT}))
		  {
		    #This must be a lesser abundant parent, so ignore
		    verbose("Note, [$id1] & [$id2] share only indels, ",
			    "but the lesser abundant [$id2] was already ",
			    "linked with the more abundant parent [",
			    $rec2->[2]->{PARENT},'].');
		  }
		else
		  {
		    verbose("$id1 (abund: $tsize1) & $id2 (abund: $tsize2) ",
			    "only differ by indels: [",
			    join(',',@{$heur_hash->{$id1}->{$id2}}),"].");

		    $rec2->[2]->{PARENT} = $id1;
		    $rec2->[2]->{INDELS} = $heur_hash->{$id1}->{$id2};
		  }
		
		push(@{$families->[-1]},$rec2);
	      }
	    elsif(!exists($heur_hash->{$id1}) ||
		  !exists($heur_hash->{$id1}->{$id2}))
	      {
		error("Did not find outer ID $id1 (abundance: ",
		      getAbund($rec_hash->{$id1}),
		      ") together with inner ID $id2 (abundance: ",
		      getAbund($rec_hash->{$id2}),").  Skipping combo.");
	      }
	    elsif(defined($heur_hash->{$id1}->{$id2}))
	      {
		if(ref($heur_hash->{$id1}->{$id2}) ne 'ARRAY')
		  {warning("Unrecognized indel type: [",
			   ref($heur_hash->{$id1}->{$id2}),"].")}
		else
		  {error("Empty indels array for [$id1] & [$id2]: ",
			 "[@{$heur_hash->{$id1}->{$id2}}].")}
	      }
	    else
	      {debug("Heuristic is skipping [$id1] & [$id2].")
		 if($DEBUG < -2)}
	  }
      }

    debug("Indel families:\n\t",
	  join("\n\t",map {my $a=$_;join(',',map {getID($_)} @$a)}
	       grep {scalar(@$_) > 1} @$families));

    my @unadded = grep {!exists($already_added->{$_})} keys(%$rec_hash);
    if(scalar(@unadded))
      {push(@$families,map {[$rec_hash->{$_}]} @unadded)}

    return($families);
  }

#This sub returns true if the stretch of submitted nucleotides is the same and
#match either the nucleotide before or after the stretch.  It is assumed that
#this sub is called with the values of bases that were either inserted or
#deleted and the values of the base immediately before and after the stretch.
#Based on 454 chemistry, there will always be at least 1 nt in a homopolymer
#stretch that is correct.  2 is usually also correct.  Error increases after
#that.
sub isHomopolymer
  {
    my $nt_before = $_[0];
    my $nts       = $_[1];
    my $nt_after  = $_[2];

    #If all the nucleotides are the same
    if($nts =~ /^(.)\1*$/)
      {
	my $nt = $1;

	#If the nucleotide before or after is also the same
	if($nt_before =~ /$nt$/ || $nt_after =~ /^$nt/)
	  {return(1)}
      }

    return(0);
  }

#Globals used: $indel_sep, $dry_run, $header
sub printGroupFile
  {
    my $outfile     = $_[0];
    my $groups      = $_[1];
    my $indel_delim = defined($indel_sep) ? $indel_sep : ',';

    return(0) unless(defined($outfile));

    if(!checkFile($outfile) || !openOut(*GROUPFILE,$outfile,0))
      {return(1)}
    else
      {print GROUPFILE ("#ID\tGroupNum\tindelDescription(*=GroupParent)\n")
	 if($header && !$dry_run)}

    foreach my $group_num (0..(scalar(@$groups) - 1))
      {
	my $parent_done = 0;
	#Output to the group file
	foreach my $rec (@{$groups->[$group_num]})
	  {
	    my $id = getID($rec);

	    my $indel_str = '*';
	    if(scalar(@$rec) >= 3 && ref($rec->[2]) eq 'HASH' &&
	       exists($rec->[2]->{INDELS}) &&
	       ref($rec->[2]->{INDELS}) eq 'ARRAY')
	      {$indel_str = join($indel_delim,@{$rec->[2]->{INDELS}})}
	    elsif(!$parent_done)
	      {$indel_str = 'error'}

	    print GROUPFILE ("$id\t",($group_num + 1),"\t$indel_str\n")
	      if(!$dry_run);
	  }
      }

    closeOut(*GROUPFILE);

    return(0);
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

#This sub will take a groups array (a 3D array: an array of group arrays
#containging modified fasta record arrays [containing a defline, sequence, and
#optional array of indel descriptions]).  The first record in each group should
#not contain the indels array.  All first records in each group should be
#sorted by descending abundance and each subsequent neighboring indel record
#should also be sorted in order of descending abundance and be less abundant
#than the first record.  All this is assumed by this subroutine, although it
#does not assume that there is an indel array in any case, though it does
#assume that when choosing which file to print to.
sub filterIndels
  {
    my $groups    = $_[0];
    my $realsfile = $_[1];
    my $fakesfile = $_[2];
    my $status    = 0;

    if(defined($realsfile))
      {
	checkFile($realsfile) || next;

	openOut(*REALS,$realsfile,1) || next;
      }

    if(defined($fakesfile))
      {
	checkFile($fakesfile) || next;

	openOut(*FAKES,$fakesfile,0) || next;
      }

    foreach my $group (@$groups)
      {
	if(ref($group) ne 'ARRAY')
	  {
	    error("Invalid group encountered.  It's not an array.");
	    next;
	  }

	if(scalar(@$group) == 0)
	  {
	    error("Empty indel group encountered.");
	    next;
	  }

	unless($dry_run)
	  {
	    my $def = $group->[0]->[0];
	    if($sum_abund && scalar(@$group) > 1)
	      {
		my $sum = 0;
		$sum   += getAbund($_) foreach(@$group);
		$def    = replaceAbund($def,$sum);
	      }

	    print("$def\n$group->[0]->[1]\n");
	  }

	if(defined($fakesfile))
	  {
	    next if(scalar(@$group) <= 1);

	    foreach my $rec (@{$group}[1..$#{$group}])
	      {
		if(ref($rec) ne 'ARRAY' || scalar(@$rec) < 2)
		  {
		    error("Invalid record encountered.");
		    next;
		  }

		my $indel_error = 0;
		if(scalar(@$rec) < 3 || ref($rec->[2]) ne 'HASH' ||
		   !exists($rec->[2]->{INDELS}) ||
		   ref($rec->[2]->{INDELS}) ne 'ARRAY' ||
		   scalar(@{$rec->[2]->{INDELS}}) == 0)
		  {
		    error("Invalid or no indel descriptions found in record ",
			  "[$rec->[0]] as expected.  Printing anyway.");
		    $indel_error = 1;
		  }

		next if($dry_run);

		print FAKES ($rec->[0],
			     deflineAddendum(($indel_error ? 'none' :
					      $rec->[2]->{PARENT}),
					     ($indel_error ? ['none'] :
					      $rec->[2]->{INDELS})),
			     "\n$rec->[1]\n") unless($dry_run);
	      }
	  }
      }

    closeOut(*FAKES) if(defined($fakesfile));
    closeOut(*REALS) if(defined($realsfile));

    return($status);
  }

#Uses globals: $value_subst_str, $append_delimiter, $indel_sep, $parent_sep
sub deflineAddendum
  {
    my $parent_id           = $_[0];
    my $indels              = $_[1]; #Value to append to defline
    my $delimiter           = defined($append_delimiter) ?
      $append_delimiter : ';';
    my $substitution_string = defined($value_subst_str) ?
      $value_subst_str : '';
    my $indel_delim         = defined($indel_sep)  ? $indel_sep  : ',';
    my $parent_delim        = defined($parent_sep) ? $parent_sep : ',';

    if(ref($indels) ne 'ARRAY' || scalar(grep {ref(\$_) ne 'SCALAR'} @$indels))
      {
	error("Invalid indels array.");
	return('');
      }

    my $val = ($parent_id eq 'none' ? '' : "$parent_id$parent_delim") .
      join($indel_delim,@$indels);

    if($substitution_string eq '' || $delimiter !~ /$substitution_string/)
      {$delimiter .= $_}
    else
      {$delimiter =~ s/$substitution_string/$val/}

    return($delimiter);
  }

#Uses globals: $seq_id_pattern, $abundance_pattern, $homopolymers_only,
#$heuristic_str_size
#Returns sequence records as an array of arrays and adds the original order
#number to the end of each record in a hash keyed on "ORDER".
sub getCheckAllSeqRecs
  {
    my $input_file     = $_[0];
    my $min_abund      = $_[1];
    my $min_nomono_ref = $_[2];
    my $seq_recs       = [];
    my $seq_hash       = {};

    if(!defined($$min_nomono_ref))
      {$$min_nomono_ref = 0}

    openIn(*INPUT,$input_file) || return($seq_recs);

    my $verbose_freq      = 100;
    my $cnt               = 0;
    my $skip              = 0;
    my($rec,$len);

    #For each line in the current input file
    while($rec = getNextSeqRec(*INPUT,0,$input_file))
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

	next if($abund < $min_abund);

	#We will compute the nomonos string whether we're in homopolymer mode
	#or if we are using the substring heuristic (because it can quickly
	#catch some indels that the heuristic cannot catch)
	my($nomono_seq,$nomono_len,$nomono_lens);
	if($homopolymers_only || $heuristic_str_size)
	  {
	    $nomono_seq = uc($seq);
	    $nomono_seq =~ s/(.)\1*(?=\1)//g;
	    $nomono_len = length($nomono_seq);
	    $nomono_lens = '';
	    while($seq =~ /((.)\2*)/ig)
	      {
		my $nts = $1;
		my $ntslen = length($nts);
		debug("Processing nucleotides [$nts] of length [$ntslen].")
		  if($DEBUG > 5);
		#Append a character representing the number of bases
		$nomono_lens .= getLenChar($ntslen);
	      }

	    debug("NoMonos lengths string: [$nomono_lens].") if($DEBUG > 1);

	    if($$min_nomono_ref == 0 || $$min_nomono_ref > $nomono_len)
	      {$$min_nomono_ref = $nomono_len}
	  }

	#ORDER is an integer to use to sort to get back the original file order
	#ID is the ID parsed from the defline
	#ABUND is the sequence abundance parsed from the defline
	#NOMONOS is the sequence after extracting all homopolymer repeats
	#NOMONOC is a string of ascii characters indicating homopolymer lengths
	#NOMONOL is the sequence length after homopolymer repeat extraction
	push(@$seq_recs,[$def,$seq,{ORDER   => $cnt,
				    ID      => $id,
				    ABUND   => $abund,
				    NOMONOS => $nomono_seq,
				    NOMONOC => $nomono_lens,
				    NOMONOL => $nomono_len}]);
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

sub incompatible
  {
    my $muscle = $_[0];

    if(!defined($muscle) || $muscle eq '' || !-e $muscle || !-x $muscle)
      {
	error("The muscle executable [$muscle] appears to either not be in ",
	      "your path, not exist, not have execute permissions, or you ",
	      "have not created a symbolic link named 'muscle' to the full ",
	      "name of the executable with version number.  If you have not ",
	      "installed muscle, you can find it here: http://www.drive5.com/",
	      "muscle/downloads.htm");
	return(1);
      }

    my $version = `$muscle -version`;
    chomp($version);
    if($version =~ /MUSCLE v(\S+) by Robert C. Edgar/)
      {
	my $vnum = $1;
	my $confirmed = [3,8,31];
	my $vnums = [split(/\./,$vnum,-1)];
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
	warning("This script was tested with Muscle version 3.8.31.  ",
		"Your version appears to be [$vnum], thus it may not work ",
		"properly.") unless($ok);
      }
    else
      {warning("This script was tested with Muscle version 3.8.31.  It may ",
	       "not work properly with the version you are using.")}

    return(0);
  }

sub getMuscleExe
  {
    my $muscle = $_[0];
    my $exe    = '';

    if(eval("use File::Which;1;") ||
       eval("use local::lib;use File::Which;1;"))
      {$exe = which($muscle)}
    else
      {
	verbose("File::Which not found, switching to backup method.");
	$exe = `which $muscle`;
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

#This returns a 2D hash where the key pairs are sequence IDs of sequences to
#compare because they might be a pair that differs only by indels.  The outer
#key is an ID of a sequence that is more abundant than the sequence of the ID
#in the inner key.  The value at the end of the hash is an internal value which
#is changed by other subs - do not count on it.
#Globals used: $homopolymers_only
sub getComparisons
  {
    my $recs             = $_[0];
    my $str_size         = defined($_[1]) ? $_[1] : 11;
    my $min_shifted_hits = defined($_[2]) ? $_[2] : 1;
    my $mitigate_recips  = defined($_[3]) ? $_[3] : 1;
    my $min_direct_hits  = defined($_[4]) ? $_[4] : 0;
    my $rec_hash         = $_[5];
    my $min_nomono_len   = $_[6];

    my $hash = {}; #$hash->{seqseg}->{position}->{R,F}->{ID} = 1
    my $hits = {}; #$hits->{ID1}->{ID2}->{S,R} = $cnt
                   #(S=shifted,R=deletion w/ possible reciprocal insertion)

    #If we're only doing homopolymers OR the heuristic is turned on (i.e. "AND
    #we're not doing every possible comparison")
    if($homopolymers_only || $str_size)
      {
	my $nomono_hash = getNoMonosHash($rec_hash,$min_nomono_len);
	my $kcnt = 0;
	my $ktot = scalar(keys(%$nomono_hash));
	#For every nomono string with more than one sequence ID
	foreach my $nomono_key (grep {scalar(keys(%{$nomono_hash->{$_}})) > 1}
				keys(%$nomono_hash))
	  {
	    verboseOverMe("Determining comparisons... ",
			  int(100*$kcnt/$ktot),'% done.')
	      if($homopolymers_only);
	    $kcnt++;

	    my @allids = keys(%{$nomono_hash->{$nomono_key}});
	    #Load all combos into the hits hash
	    for(my $i = 0;$i < (scalar(@allids) - 1);$i++)
	      {
		my $id1 = $allids[$i];
		for(my $j = $i + 1;$j < scalar(@allids);$j++)
		  {
		    my $id2 = $allids[$j];

		    my($first,$second) =
		      sort {getAbund($rec_hash->{$b}) <=>
			      getAbund($rec_hash->{$a})} ($id1,$id2);

		    next if(getAbund($rec_hash->{$first}) ==
			    getAbund($rec_hash->{$second}));

		    #We're only going to let homopolymer indels through here
		    #whether we're in homopolymer mode or not.  This is a
		    #shortcut to allow us to find some things quickly
		    next if(only454Indels($rec_hash->{$first},
					  $rec_hash->{$second}));

		    debug("Comparison found using nomono length ",
			  "[$min_nomono_len]: $first vs $second:\n",
			  "$rec_hash->{$first}->[2]->{NOMONOS}\n",
			  "$rec_hash->{$second}->[2]->{NOMONOS}")
		      if($homopolymers_only);

		    #Just because the NoMonos strings are the same, doesn't
		    #necessarily mean that the two sequences only differ by
		    #indels.  Substitutions can be interpreted as a combination
		    #insertion/deletion if the nucleotide being substituted is
		    #the same as one of its neighbors and is changed to its
		    #other neighbor.  Now that we have all the combinations of
		    #sequences which have the same nomonos string, we need to
		    #determine whether it's a 454 indel
		    my $indels_array = [only454Indels($rec_hash->{$first},
						      $rec_hash->{$second})];
		    if(scalar(@$indels_array) == 0)
		      {next}

		    #Every sequence pair that is not considered to differ by
		    #only 454 indel errors has at least 1 real indel or differs
		    #by likely substitutions.  Ideally, we would save the
		    #substitution occurrences so that we wouldn't compare these
		    #later (when not in homopolymers_only mode.

		    #If we're in homopolymer mode, we're going to pre-load the
		    #heuristic hash (i.e. hits hash) with the indels array
		    if($homopolymers_only)
		      {$hits->{$first}->{$second} = $indels_array}
		    else
		      {
			#We know the sequences only differ by
			#homopolymer indels, so let's set it to the minimum
			#required number of shifted hits and force them to be
			#compared.
			$hits->{$first}->{$second}->{S} = $min_shifted_hits;
		      }
		  }
	      }
	  }

	#We don't need the nomonohash anymore, so let's free up memory
	undef($nomono_hash);
      }

    #If we are only doing homopolymers, we're done.  Otherwise, we're going to
    #keep what we've found so far and continue on to try and find other types
    #of indels.
    if($homopolymers_only)
      {
	verbose("Comparison reduction done.");
	return($hits);
      }

    #If we're not using a heuristic, compare every sequence combo
    if($str_size == 0)
      {
	for(my $i = 0;$i < (scalar(@$recs) - 1);$i++)
	  {
	    my $id1 = getID($recs->[$i]);
	    for(my $j = $i + 1;$j < scalar(@$recs);$j++)
	      {
		my $id2 = getID($recs->[$j]);
		my($first,$second) =
		  sort {getAbund($rec_hash->{$b}) <=> getAbund($rec_hash->{$a})
			  || $a cmp $b} ($id1,$id2);
		$hits->{$first}->{$second} = 1;
	      }
	  }

	return($hits);
      }

    if($min_shifted_hits < 1)
      {
	error("Invalid minimum shifted hits: [$min_shifted_hits].");
	quit(9);
      }

    verbose("Reducing comparisons by looking for at least ",
	    "[$min_shifted_hits] shifted hits (see --heuristic-min-directs)",
	    ($mitigate_recips ?
	     " & reciprocal single base insertions & deletions within " .
	     "$str_size bases (see --heuristic-thorough)" : ''),".");
    debug("Starting heuristic at ",markTime(0)," seconds");

    #Create the subsequence hash
    my $cnt = 0;
    foreach my $rec (@$recs)
      {
	verboseOverMe("Hashing sequence ",++$cnt);
	my $def      = $rec->[0];
	my $seq      = $rec->[1];
	my $id       = getID($rec);
	my $seq_size = getSize($rec);
	my $end_size = $seq_size - $str_size;

	#Create a hash of every $str_size substring / position / ID
	for(my $position=0;$position < $end_size;$position++)
	  {
	    my $ukey = substr($seq,$position,$str_size);

	    #Add the real subsequence & position
	    $hash->{$ukey}->{$position}->{R}->{$id} = 1;

	    if($mitigate_recips && ($position + 1) < $end_size)
	      {
		my $nextbase = substr($seq,$position + $str_size,1);

		#Add possible/detectable mononucleotide deletion possibilities
		addFakes($hash,$position,$id,$ukey,$str_size,$nextbase);
	      }
	  }
      }

    debug("Sub-sequence Hash constructed at ",markTime(0)," seconds");

    #Now let's flip & collapse that hash so that a series of ID keys are
    #concatenated into a single key (position-groups delimited by colons) and
    #its value is the subsequence - overwriting when groupings are the same.
    #We will filter out any subsequence that only every occurs in 1 position.
    #This means that all the IDs and positions where a subsequence occurs,
    #becomes a key string like this: id1,id2,id3:(1)id4,id5,id6:... =
    #TCGTAGCTTAG
    my $collapse = {};
    if($min_shifted_hits == 1 && !$mitigate_recips)
      {
	$cnt = 0;
	foreach my $key (keys(%$hash))
	  {
	    verboseOverMe("Collapsing hash sequence ",++$cnt);
	    if(scalar(keys(%{$hash->{$key}}))  == 1)
	      {
		delete($hash->{$key});
		next;
	      }
	    $collapse->{join(':',
			     sort {$a cmp $b}
			     map {my $p=$_;join(",",
						sort {$a cmp $b}
						keys(%{$hash->{$key}->{$p}
							 ->{R}}))}
			     keys(%{$hash->{$key}}))} = $key;
	  }

	debug("Hash collapsed at ",markTime(0)," seconds");
      }

    my $total = scalar($min_shifted_hits == 1 && !$mitigate_recips ?
		       keys(%$collapse) : keys(%$hash));
    my $num = 0;

    foreach my $key ($min_shifted_hits == 1 && !$mitigate_recips ?
		     keys(%$collapse) : keys(%$hash))
      {
	$num++;
	verboseOverMe("Determining comparisons... ",int(100*$num/$total),
		      '% done.');

	my $ukey = $min_shifted_hits == 1 && !$mitigate_recips ?
	  $collapse->{$key} : $key;
	debug("UKEY:($ukey/$key):\n\t",
	      join("\n\t",
		   (map {join(",",keys(%{$hash->{$ukey}->{$_}->{R}}))}
		    keys(%{$hash->{$ukey}})))) if($DEBUG < 0);

	debug("Number of positions with real subsequences in [$ukey]: ",
	      scalar(grep {exists($hash->{$ukey}->{$_}->{R})}
		     keys(%{$hash->{$ukey}})))
	  if($DEBUG < -2 && scalar(grep {exists($hash->{$ukey}->{$_}->{R})}
				  keys(%{$hash->{$ukey}})));

	#Skip this iteration if...
	if(#There's fewer than 2 positions with real ukeys
	   scalar(grep {exists($hash->{$ukey}->{$_}->{R})}
		  keys(%{$hash->{$ukey}})) < 2 &&
	   #And there are no positions with both real and fake ukeys
	   scalar(grep {exists($_->{R}) && exists($_->{F})}
		  values(%{$hash->{$ukey}})) == 0)
	  {
	    debug("Skipping") if($DEBUG < -2);
	    next;
	  }

	#Grab the position keys sorted by ascending number of contained IDs (so
	#that there's more potential for filtering IDs out when thershold has
	#already been reached
	my @group_keys = sort {scalar(keys(%{$hash->{$ukey}->{$a}->{R}})) <=>
				 scalar(keys(%{$hash->{$ukey}->{$b}->{R}}))}
	  keys(%{$hash->{$ukey}});
	my $group_size = scalar(@group_keys);

	for(my $i=0;$i<($group_size-1);$i++)
	  {
	    my $key1   = $group_keys[$i];
	    my @group1 = sort {$a cmp $b}
	      map {keys(%{$hash->{$ukey}->{$key1}->{$_}})} grep {$_ eq "R"}
		keys(%{$hash->{$ukey}->{$key1}});

	    if($group_size > 1 && $min_shifted_hits)
	      {
		for(my $j = $i+1;$j<$group_size;$j++)
		  {
		    my $key2   = $group_keys[$j];
		    my @group2 = sort {$a cmp $b}
		      map {keys(%{$hash->{$ukey}->{$key2}->{$_}})}
			grep {$_ eq "R"} keys(%{$hash->{$ukey}->{$key2}});
		    foreach my $member1 (@group1)
		      {
			my $size1 = getAbund($rec_hash->{$member1});
			foreach my $member2 (#This removes members of group2
					     #from consideration when we've
					     #already idendified a pair
					     grep {my $size2 =
						     getAbund($rec_hash->{$_});
						   !($size2 > $size1 ||
						     ($size2 == $size1 &&
						      $_ lt $member1) ?
						     exists($hits->{$_}) &&
						     exists($hits->{$_}
							    ->{$member1}) &&
						     exists($hits->{$_}
							    ->{$member1}
							    ->{S}) &&
						     $hits->{$_}->{$member1}
						     ->{S} >=
						     $min_shifted_hits :
						     exists($hits->{$member1})
						     && exists($hits
							       ->{$member1}
							       ->{$_}) &&
						     exists($hits
							    ->{$member1}
							    ->{$_}->{S}) &&
						     $hits->{$member1}->{$_}
						     ->{S} >=
						     $min_shifted_hits)}
					     @group2)
			  {
			    my $size2 = getAbund($rec_hash->{$member2});
			    my($greater,$lesser);
			    if($size1 > $size2)
			      {
				$greater = $member1;
				$lesser  = $member2;
			      }
			    elsif($size1 < $size2)
			      {
				$greater = $member2;
				$lesser  = $member1;
			      }
			    else
			    #Ignore when equal - happens when there's a repeat
			      {next}

			    #Must also meet the minimum direct hits rule(s)
			    if((exists($hits->{$greater}) &&
				exists($hits->{$greater}->{$lesser})) ||
			       meetMinDirectsRule($rec_hash->{$member1}->[1],
						  $rec_hash->{$member2}->[1],
						  $str_size,
						  ($key1 < $key2 ? #Lesser pos.
						   $key1 : $key2),
						  $min_direct_hits))
			      {$hits->{$greater}->{$lesser}->{S}++}
			  }
		      }
		  }
	      }

	    #Now we'll add mononucleotide deletions within $str_size nts
	    if($mitigate_recips)
	      {
		my @real_ids = map {keys(%{$hash->{$ukey}->{$key1}->{$_}})}
		  grep {$_ eq 'R'} keys(%{$hash->{$ukey}->{$key1}});
		my @fake_ids = map {keys(%{$hash->{$ukey}->{$key1}->{$_}})}
		  grep {$_ eq 'F'} keys(%{$hash->{$ukey}->{$key1}});
		foreach my $rid (@real_ids)
		  {
		    foreach my $fid (@fake_ids)
		      {
			next if($rid eq $fid);

			#We must confirm this is a reciprocal indel situation
			unless(confirmRecipIndel($key1,
						 $rid,
						 $fid,
						 $rec_hash,
						 $str_size))
			  {next}

			debug("FOUND DELETION BETWEEN REAL $rid AND FAKE ",
			      "$fid AT POSITION $key1 WITH SEQUENCE $ukey")
			  if($DEBUG > 3);

			my($first,$second) =
			  sort {getAbund($rec_hash->{$b}) <=>
				  getAbund($rec_hash->{$a}) || $a cmp $b}
			    ($rid,$fid);

			#Must also meet the minimum direct hits rule(s)
			if((exists($hits->{$first}) &&
			    exists($hits->{$first}->{$second})) ||
			   meetMinDirectsRule($rec_hash->{$first}->[1],
					      $rec_hash->{$second}->[1],
					      $str_size,
					      $key1,
					      $min_direct_hits))
			  {$hits->{$first}->{$second}->{R}++}
		      }
		  }
	      }
	  }

	#Now we'll add mononucleotide deletions within $str_size nts
	if($mitigate_recips && scalar(@group_keys))
	  {
	    my $key1 = $group_keys[-1];

	    my @real_ids = map {keys(%{$hash->{$ukey}->{$key1}->{$_}})}
	      grep {$_ eq 'R'} keys(%{$hash->{$ukey}->{$key1}});
	    my @fake_ids = map {keys(%{$hash->{$ukey}->{$key1}->{$_}})}
	      grep {$_ eq 'F'} keys(%{$hash->{$ukey}->{$key1}});
	    foreach my $rid (@real_ids)
	      {
		foreach my $fid (@fake_ids)
		  {
		    next if($rid eq $fid);

		    #We must confirm this is a reciprocal indel situation
		    unless(confirmRecipIndel($key1,
					     $rid,
					     $fid,
					     $rec_hash,
					     $str_size))
		      {next}

		    my($first,$second) = sort {getAbund($rec_hash->{$b}) <=>
						 getAbund($rec_hash->{$a}) ||
						   $a cmp $b} ($rid,$fid);

		    debug("FOUND DELETION between REAL $rid AND FAKE $fid at ",
			  "POSITION $key1 with SEQUENCE $ukey")
		      if($DEBUG > 3);

		    #Must also meet the minimum direct hits rule(s)
		    if((exists($hits->{$first}) &&
			exists($hits->{$first}->{$second})) ||
		       meetMinDirectsRule($rec_hash->{$first}->[1],
					  $rec_hash->{$second}->[1],
					  $str_size,
					  $key1,
					  $min_direct_hits))
		      {$hits->{$first}->{$second}->{R}++}
		  }
	      }
	  }
      }

    debug("Compute comparisons completed at: ",scalar(markTime(0)));

    if($DEBUG)
      {
	my $num_compares = 0;
	foreach my $first (keys(%$hits))
	  {
	    foreach my $second (keys(%{$hits->{$first}}))
	      {
		my $compare =
		  (#($min_direct_hits &&
		   # exists($hits->{$first}->{$second}->{D}) &&
		   # $hits->{$first}->{$second}->{D} >= $min_direct_hits) ||
		   ($min_shifted_hits &&
		    exists($hits->{$first}->{$second}->{S}) &&
		    $hits->{$first}->{$second}->{S} >= $min_shifted_hits) ||
		   ($mitigate_recips &&
		    exists($hits->{$first}->{$second}->{R})) ?
		   1 : 0);
		$num_compares += $compare;
		if($DEBUG > 3)
		  {
		    debug("SHIFTS(",
			  (exists($hits->{$first}->{$second}->{S}) ?
			   $hits->{$first}->{$second}->{S} : '0'),
			  ")\t1DELS(",
			  (exists($hits->{$first}->{$second}->{R}) ?
			   $hits->{$first}->{$second}->{R} : '0'),
			  ")\tCOMPARE: $first\t$second\t$compare\n");
		  }
	      }
	  }

	debug("Printed comparison debug output at: ",scalar(markTime(0)),
	      " Num calls to muscle: $num_compares");
      }

    debug("Heuristic hash has [",scalar(keys(%$hits)),"] outer keys.");
    debug("Comparisons:\n\t",
	  join("\n\t",map {$_ . ':' . join(',',keys(%{$hits->{$_}}))}
	       keys(%$hits))) if($DEBUG > 2);
    verbose("Comparison reduction done.");

    return($hits);
  }

#This subroutine creates all possible single-base deletion sequences except for
#the first and last positions where it would abut a potential insertion.  We
#are using this to catch only a reciprocal single base insertion/deletion
#within the hash string size to mitigate this problem
sub addFakes
  {
    my $hash = $_[0];
    my $i    = $_[1];
    my $id   = $_[2];
    my $seq  = $_[3];
    my $size = $_[4];
    my $nb   = $_[5];

    foreach my $dp (1..($size-1))
      {
	my $tseq = $seq;
	next if(substr($tseq,$dp-1,1) ne substr($tseq,$dp,1) &&
		substr($tseq,$dp+1,1) ne substr($tseq,$dp,1));
	substr($tseq,$dp,1,'');
	debug("DELETING POSITION $dp FROM $seq TO MAKE $tseq") if($DEBUG < -2);
	$hash->{"$tseq$nb"}->{$i}->{F}->{$id} = 1
	  unless($seq eq "$tseq$nb");
      }
  }

sub sumSetBases
  {
    my $set       = $_[0];
    my $rec_hash  = $_[1];
    my $heur_hash = $_[2];
    my $sum       = 0;
    my $summed    = {};

    foreach my $outer_key (@$set)
      {
	if(!exists($summed->{$outer_key}))
	  {
	    $sum += getSize($rec_hash->{$outer_key});
	    $summed->{$outer_key} = 1;
	  }
	foreach my $inner_key (keys(%{$heur_hash->{$outer_key}}))
	  {
	    if(!exists($summed->{$inner_key}))
	      {
		$sum += getSize($rec_hash->{$inner_key});
		$summed->{$inner_key} = 1;
	      }
	  }
      }

    return($sum);
  }

#This subroutine forks off multiple child processes which run muscle as a
#system call, parse the output, and print the results which this subroutine
#gathers and compiles to alter the heuristic hash.
sub alignLoadHeurHash
  {
    my $num_kids   = $_[0];
    my $rec_hash   = $_[1];
    my $heur_hash  = $_[2];
    my $split_sets = $_[3];
    my $align_mode = $_[4];

    verbose("Determining indel matches using muscle.");

    my $max_concurrent = $num_kids < scalar(@$split_sets) ?
      $num_kids : scalar(@$split_sets);
    my $cur_concurrent = 1;

    use IO::Pipe::Producer;
    my $obj = new IO::Pipe::Producer();

    use IO::Select;
    my $sel = new IO::Select;

    my $set_base_sums = {};
    foreach my $set (@$split_sets)
      {$set_base_sums->{$set} = sumSetBases($set,$rec_hash,$heur_hash)}

    @$split_sets = sort {$set_base_sums->{$b} <=> $set_base_sums->{$a}}
      @$split_sets;

    my $report_nums = [scalar(@$split_sets) > 10 ?
		       ((map {scalar(@$_)} @{$split_sets}[0..8]),'...') :
		       map {scalar(@$_)} @$split_sets];
    verbose("Starting a max of $max_concurrent child processes, each to ",
	    "perform [",join(',',@$report_nums),
	    "] comparisons respectively.  If an alignment is particularly ",
	    "large and there is not enough memory, the number of concurrent ",
	    "processes may not reach the max.") if($verbose > 1);

    #Keep track of the outer keys that are processed to issue status messages
    my $outers_count = 0;
    my $all_outers = 0;
    $all_outers += scalar(@$_) foreach(@$split_sets);
    my $status_str = '';
    my $progress_str = 'Overall prog: [0%]';
    my $child_status_str = '';

    my $nprocs = {};
    my $running_procs = 0;
    my $running_jobs  = 0;
    my @kids;
    my $n = 0;
    foreach my $key_group_i (0..$#{$split_sets})
      {
	#Get the next set of outer IDs
	my $key_group = $split_sets->[$key_group_i];

	#Get the number of bases to be aligned
	my $bases = $set_base_sums->{$key_group};

	#Determine how many cores will be used to process this size alignment
	#(This is not based on cores actually, but rather on memory.  It's
	#convenient however to count this by the number of cores instead of by
	#memory units.)
	my $cur_procs = int($bases / $max_bases_per_run);
	#Round up
	if($bases % $max_bases_per_run)
	  {$cur_procs++}

	#If there currently are not enough cores to take on this run
	if(($running_procs + $cur_procs) > $num_kids &&
	   !($running_procs == 0 && $cur_procs > $num_kids))
	  {last}

	#If the number of cores needed is more than we've got
	if($cur_procs > $num_kids)
	  {
	    my @report_keys = scalar(@$key_group) > 10 ?
	      (@{$key_group}[0..8],'...') : @$key_group;
	    debug("Outer key sample in key group [$key_group_i]: ",
		  "[@report_keys], which has alignment size [$bases] is ",
		  "larger than what can be done in 1 go on this computer.");
	  }

	#Increment the runs by the number of cores this job needs
	$running_procs += $cur_procs;
	$running_jobs++;

	$n++;

	$status_str =
	  join('',
	       "On alignment [$n] of [",scalar(@$split_sets),'].',
	       ($verbose > 2 ?
		"  Running [$running_jobs] concurrently." : ''));
	$child_status_str = 'Aligning...';
	verboseOverMe("$status_str  $progress_str  $child_status_str");

	debug("Starting child [$n].") if($DEBUG > 1);

	my($stdout,$stderr) =
	  $obj->getSubroutineProducer(\&alignPrintIndelPairs,
				      $key_group,
				      $heur_hash,
				      $rec_hash,
				      $align_mode,
				      $cur_procs > $num_kids);

	debug("Child [$n] started.") if($DEBUG > 1);

	$nprocs->{$stdout} = $cur_procs;
	$nprocs->{$stderr} = $cur_procs;
	push(@kids,$stdout,$stderr);
	$sel->add($stdout,$stderr);
      }

    my $seen = {};
    my $onerr = 0;
    while(my @fhs = $sel->can_read())
      {
	foreach my $fh (@fhs)
	  {
	    my $fhi  = (grep {$fh eq $kids[$_]} (0..$#kids))[0];
	    if($fhi % 2)
	      {$onerr = 1}
	    else
	      {$onerr = 0}
	    if(eof($fh))
	      {
		debug('Standard ',($onerr ? 'Error' : 'Out'),
		      ' of an alignment is done.  Closing it up.');

		#Remove the expired file handle
		$sel->remove($fh);
		close($fh);

		#Unless this is the ending of stderr, decrement the jobs
		if(!$onerr)
		  {
		    $running_procs -= $nprocs->{$fh};
		    $running_jobs--;
		  }
		else
		  {next}

		debug('Seeing if there are more jobs to run.');

		#For each of the remaining key group indexes, start more jobs
		foreach my $key_group_i ($n..$#{$split_sets})
		  {
		    #Get the next set of outer IDs
		    my $key_group = $split_sets->[$key_group_i];

		    #Get the number of bases to be aligned
		    my $bases = $set_base_sums->{$key_group};

		    #Determine how many cores will be used to process this size
		    #alignment.  (This is not based on cores actually, but
		    #rather on memory.  It's convenient however to count this
		    #by the number of cores instead of by memory units.)
		    my $cur_procs = int($bases / $max_bases_per_run);
		    #Round up
		    if($bases % $max_bases_per_run)
		      {$cur_procs++}

		    #If there currently are not enough cores 2 take on this run
		    if(($running_procs + $cur_procs) > $num_kids &&
		       !($running_procs == 0 && $cur_procs > $num_kids))
		      {last}

		    #If the number of cores needed is more than we've got
		    if($cur_procs > $num_kids)
		      {
			my @report_keys = scalar(@$key_group) > 10 ?
			  (@{$key_group}[0..8],'...') : @$key_group;
			debug("Outer key sample in key group [$key_group_i]: ",
			      "[@report_keys], which has alignment size ",
			      "[$bases] is larger than what can be done in 1 ",
			      "go on this computer.");
		      }

		    if($running_jobs >= $num_kids)
		      {
			error("Too many jobs running.");
			debug("last if((running_procs + cur_procs) > ",
			      "num_kids && !(running_procs == 0 && cur_procs ",
			      "> num_kids)): if(($running_procs + ",
			      "$cur_procs) > $num_kids && !($running_procs ",
			      "== 0 && $cur_procs > $num_kids))");
			last;
		      }

		    #Increment the runs by the number of cores this job needs
		    $running_procs += $cur_procs;
		    $running_jobs++;

		    $n++;

		    $status_str =
		      join('',
			   "On alignment [$n] of [",scalar(@$split_sets),'].',
			   ($verbose > 2 ?
			    "  Running [$running_jobs] concurrently." : ''),
			   ($DEBUG > 2 ? "  Parsed: [$outers_count] out of " .
			   "[$all_outers] alignments." : ''));
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);

		    my($stdout,$stderr) =
		      $obj->getSubroutineProducer(\&alignPrintIndelPairs,
						  $key_group,
						  $heur_hash,
						  $rec_hash,
						  $align_mode,
						  $cur_procs > $num_kids);

		    $nprocs->{$stdout} = $cur_procs;
		    $nprocs->{$stderr} = $cur_procs;
		    push(@kids,$stdout,$stderr);
		    $sel->add($stdout,$stderr);
		  }

		debug("I count [$running_jobs] running jobs.  There are [",
		      $sel->count(),"] file handles being tracked by IO::",
		      "Select, which should be twice the number of running ",
		      "jobs (possibly plus 1).  There appear to be [",
		      scalar(grep {defined(fileno($_))} @kids),
		      "] open file handles according to fileno().")
		  if($DEBUG > 5);

		next;
	      }
	    my $line = <$fh>;
	    chomp($line);
	    if($onerr)
	      {
		my $childstr = 'CHILD' . int($fhi/2) . ':';
		if($line =~ /^ERROR/)
		  {
		    error("$childstr$line");
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);
		  }
		elsif($line =~ /^WARNING/)
		  {
		    warning("$childstr$line");
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);
		  }
		elsif($line =~ /^DEBUG/)
		  {
		    debug("$childstr$line");
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);
		  }
		elsif($line =~ /^\s+\S/) #Assume multi-line err/warn/debug
		  {
		    verbose("$childstr$line");
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);
		  }
		else
		  {
		    $line =~ s/[\r\n]//g;
		    $child_status_str = $line . '  JOB(' . int($fhi/2) .
		      ')';
		    verboseOverMe("$status_str  $progress_str  ",
				  $child_status_str);
		  }
	      }
	    elsif($line =~ /^([^\t]*)\t([^\t]*)\t([^\t]*)$/)
	      {
		my $first  = $1;
		my $second = $2;
		my $indels = $3;
		debug("READING: [$1 $2 $3]") if($DEBUG > 2);
		unless(exists($seen->{$first}))
		  {
		    $outers_count++;
		    $seen->{$first} = 1;
		  }
		if($indels eq '')
		  {$heur_hash->{$first}->{$second} = undef}
		else
		  {$heur_hash->{$first}->{$second} = [split(/,/,$indels)]}
	      }
	    else
	      {error("Internal parse error.  Could not parse line: [$line].")}
	  }

	$progress_str = "Overall prog: [" .
	  (int(10000 * $outers_count / $all_outers)/100) . '%]';
	verboseOverMe("$status_str  $progress_str  $child_status_str");
      }

    debug("End of child processes") if($DEBUG > 1);
  }

#This sub divides the outer keys of the heuristic hash into sets composed of a
#max that can be run on one processor with an equal amount of memory.  Any
#single outer key that is too big will be subsequently split elsewhere.  Right
#now, it just puts the outer keys in arbitrary sets as it encounters them.  In
#the future, it would be nice to direct them into intelligent groupings to
#maximize overlap of inner keys
#Globals used: $max_bases_per_run, $processes
sub divideGroupings
  {
    my $heur_hash   = $_[0];
    my $rec_hash    = $_[1];
    my $base_thresh = $max_bases_per_run;

    my $base_count = 0;
    my @key_groups = ([]);
    my $ids_seen   = {};
    my $sizes      = [];
    foreach my $key (keys(%$heur_hash))
      {
	#Count the number of new bases we are proposing to add
	my $new_bases = 0;
	if(!exists($ids_seen->{$key}))
	  {
	    $new_bases += getSize($rec_hash->{$key});
	    $ids_seen->{$key} = 1;
	  }
	foreach my $inner_key (keys(%{$heur_hash->{$key}}))
	  {
	    if(!exists($ids_seen->{$inner_key}))
	      {
		$new_bases += getSize($rec_hash->{$inner_key});
		$ids_seen->{$inner_key} = 1;
	      }
	  }

	#If there is something in this set already and these new bases will put
	#us over the threshold, create a new group
	if($base_count && ($base_count + $new_bases) >= $base_thresh)
	  {
	    push(@key_groups,[]);
	    if($base_count)
	      {push(@$sizes,$base_count)}
	    $base_count = 0;
	    $ids_seen = {};
	  }

	$base_count += $new_bases;
	push(@{$key_groups[-1]},$key);
      }

    push(@$sizes,$base_count);

    my $report_sizes = [(scalar(@key_groups) > 10 ?
			 ((map {scalar(@$_)} @key_groups[0..8]),'...') :
			 map {scalar(@$_)} @key_groups)];
    debug("Split outer-key heuristic sequences into [",scalar(@key_groups),
	  "] sets of [",join(',',@$report_sizes),
	  "] outer keys per alignment, running [$processes] at a time.");

    return([@key_groups]);
  }

#For alignments that are too big for the amount of available memory, break up
#the input data, align them separately, then stitch them back together using a
#common sequence.  Assumes that all IDs are unique (1 outer ID joined with a
#single series of inner IDs from the heur_hash).
#Globals used: $gigs_ram, $max_bases_per_gig, $muscle_gaps
sub splitStitchAlignAndHash
  {
    my $rec_hash = $_[0];
    my $all_ids  = [@{$_[1]}];
    my $bases = 0;
    $bases += $_ foreach(map {getSize($rec_hash->{$_})} @$all_ids);
    my $num_divisions = $bases / $max_bases_per_gig / $gigs_ram;
    my $common_id = shift(@$all_ids);
    my $div_size = int(scalar(@$all_ids) / $num_divisions);
    my @alignment_strs = ();
    for(my $i = 0;$i < $num_divisions;$i++)
      {
	my($subset);
	if(($i + 1) == $num_divisions)
	  {$subset = [@$all_ids]}
	else
	  {$subset = [splice(@$all_ids,0,$div_size)]}

	push(@alignment_strs,getMuscleMultipleAlignment($rec_hash,
							$muscle_gaps,
							0,
							$common_id,@$subset));
      }

    return(mergeMuscleAlignments($common_id,@alignment_strs));
  }

#This subroutine takes a series of muscle alignments, each with a common
#sequence, and merges them into one alignment based on differences in the
#common sequence alignment string.
sub mergeMuscleAlignments
  {
    my $common_id = shift(@_);
    my @alignment_strs = @_;

    debug("Merging [",scalar(@alignment_strs),"] alignments.");

    my $merged_hash = {};
    my $cnt = 0;
    foreach my $alignment_str (@alignment_strs)
      {
	my $cur_hash = hashAlignment($alignment_str);

	#Temporarily edit the alignment strings to contain only sequence - no
	#spaces or IDs.
	foreach my $idkey (keys(%$cur_hash))
	  {
	    my $pat = quotemeta($idkey) . '[ \t]+';
	    $cur_hash->{$idkey} =~ s/$pat//g;
	    $cur_hash->{$idkey} =~ s/[^A-Za-z\-]+//g;
	    $cur_hash->{$idkey} .= "\n";
	  }

	if(!exists($merged_hash->{$common_id}))
	  {
	    if($DEBUG > 1)
	      {debug("ALIGNED$cnt: $_\t$cur_hash->{$_}")
		 foreach(keys(%$cur_hash))}
	    %$merged_hash = %$cur_hash;
	  }
	elsif($merged_hash->{$common_id} eq $cur_hash->{$common_id})
	  {
	    foreach my $key (keys(%$cur_hash))
	      {
		debug("ALIGNED$cnt: $key\t$cur_hash->{$key}") if($DEBUG > 1);
		if(exists($merged_hash->{$key}))
		  {
		    if($merged_hash->{$key} ne $cur_hash->{$key})
		      {warning("Separate alignments for [$key] differ.")}
		  }
		else
		  {$merged_hash->{$key} = $cur_hash->{$key}}
	      }
	  }
	else
	  {
	    #Find positions where the merged hash & cur_hash must have dashes
	    #added
	    my $gts = $merged_hash->{$common_id};
	    my $cts = $cur_hash->{$common_id};
	    $gts .= '-' while(length($gts) < length($cts));
	    $cts .= '-' while(length($gts) > length($cts));
	    my $end = length($gts);
	    my $goffset = 0;
	    my $coffset = 0;
	    my $gadds = [];
	    my $cadds = [];
	    my $position = 0;
	    do
	      {
		if(($position + $coffset) > ($end - 1))
		  {
		    error("String index out of bounds.");
		    last;
		  }

		my $gb = substr($merged_hash->{$common_id},
				$position+$goffset,1);
		my $cb = substr($cur_hash->{$common_id},$position+$coffset,1);

		if($gb ne $cb)
		  {
		    if($gb eq '-')
		      {
			push(@$cadds,$position+$coffset);
			$coffset--;
		      }
		    elsif($cb eq '-')
		      {
			push(@$gadds,$position+$goffset);
			$goffset--;
		      }
		    else
		      {error("Alignment merge error between sequences: ",
			     "[$gts] and [$cts] at position [$position] with ",
			     "relative offsets: [$goffset] and [$coffset].")}
		  }
		$position++;
	      } while(($position + $goffset) < $end);

	    #Add gaps to the merged hash
	    if(scalar(@$gadds))
	      {foreach my $gid (keys(%$merged_hash))
		 {foreach my $position (reverse(@$gadds))
		    {substr($merged_hash->{$gid},$position,0,'-')}}}
	    #Add gaps to the current hash
	    if(scalar(@$cadds))
	      {
		foreach my $cid (keys(%$cur_hash))
		  {
		    foreach my $position (reverse(@$cadds))
		      {
			if(length($cur_hash->{$cid}) < $position)
			  {
			    error("Position [$position] is larger than ",
				  "alignment string of ID: [$cid]: ",
				  "[$cur_hash->{$cid}].");
			    next;
			  }
			substr($cur_hash->{$cid},$position,0,'-');
		      }
		  }
	      }

	    #Merge the current and merged hashes
	    foreach my $key (keys(%$cur_hash))
	      {
		debug("ALIGNED$cnt: $key\t$cur_hash->{$key}") if($DEBUG > 1);
		if(exists($merged_hash->{$key}))
		  {
		    if($merged_hash->{$key} ne $cur_hash->{$key})
		      {warning("Separate alignments for [$key] differ.")}
		  }
		else
		  {$merged_hash->{$key} = $cur_hash->{$key}}
	      }
	  }
	$cnt++;
      }

    #Now put the keys and spaces back in the hash
    foreach my $idkey (keys(%$merged_hash))
      {
	debug("MERGED: $idkey\t$merged_hash->{$idkey}") if($DEBUG > 1);
	$merged_hash->{$idkey} = "$idkey\t$merged_hash->{$idkey}";
      }

    return($merged_hash);
  }

#This subroutine runs as a separate child process.  It calls getMuscleMultiple-
#Alignment (or splitStitchAlignAndHash - based on the split_mode flag [which is
#true if the alignment is too big to run in one process]) which runs muscle in
#a system call.  This subroutine catches that output and any status output on
#stderr and prints all pairs (with indel info) on standard out and status
#messages on standard error, which the parent process catches and processes.
#Globals used: $muscle_gaps, $homopolymers_only
sub alignPrintIndelPairs
  {
    my $split_set  = $_[0];
    my $heur_hash  = $_[1]; #This hash is altered in parent scope
    my $rec_hash   = $_[2];
    my $align_mode = $_[3];
    my $split_mode = $_[4];
    my $aln_strs   = {};

    if($homopolymers_only)
      {
	error("Internal error: alignPrintIndelPairs is not intended for use ",
	      "in homopolymer mode.");
	return();
      }

    if($align_mode eq 'global')
      {
	my $line_num     = 0;
	my $verbose_freq = 1000;

	my $added = {};
	my $all_ids = [grep {my $e = exists($added->{$_});$added->{$_}=1;!$e}
		       map {$_,keys(%{$heur_hash->{$_}})} @$split_set];

	debug("Running muscle alignment on [",scalar(@$split_set),
	      "] outer-key sequences and their [",
	      (scalar(@$all_ids) - scalar(@$split_set)),
	      "] inner-key partners.") if($DEBUG > 4);

	if($split_mode)
	  {$aln_strs = splitStitchAlignAndHash($rec_hash,$all_ids)}
	else
	  {
	    my $alignment_str = getMuscleMultipleAlignment($rec_hash,
							   $muscle_gaps,
							   0,
							   @$all_ids);

	    $aln_strs = hashAlignment($alignment_str);
	  }
      }

    my $saved_handle = select();
    select(STDOUT) unless($saved_handle eq *STDOUT);

    my $done_hash     = {};
    my $tmp_done_hash = {};

    #For each first ID in order of descending abundance and ascending ID
    foreach my $id1 (sort {getAbund($rec_hash->{$b}) <=>
			     getAbund($rec_hash->{$a})} @$split_set)
      {
	#If this ID was added as a child of an earlier ID, it cannot have any
	#children, so skip it.
	if(exists($done_hash->{$id1}))
	  {
	    print(join("\n",map {"$id1\t$_\t"} keys(%{$heur_hash->{$id1}})),
		  "\n");
	    next;
	  }

	#Update the done hash with what was added on the last iteration
	if(scalar(keys(%$tmp_done_hash)))
	  {
	    foreach(keys(%$tmp_done_hash))
	      {
		$done_hash->{$_} = 1;
		delete($tmp_done_hash->{$_});
	      }
	  }

	my $rec1      = $rec_hash->{$id1};
	my $diff_hash = {};
	my $alnstr    = '';
	if($align_mode eq 'global' && !exists($aln_strs->{$id1}))
	  {
	    error("Outer ID [$id1] was not found in the alignment string ",
		  "hash though it should have been there.  It is ",
		  (scalar(grep {$_ eq $id1} @$split_set) ? '' : 'also not '),
		  "in the split set.");
	    next;
	  }
	elsif($align_mode eq 'local')
	  {
	    my @remaining_ids = sort {$rec_hash->{$b}->[2]->{ABUND} <=>
					$rec_hash->{$a}->[2]->{ABUND} ||
					  $a cmp $b}
	      grep {!exists($done_hash->{$_})}
		keys(%{$heur_hash->{$id1}});

	    if(scalar(@remaining_ids) != scalar(keys(%{$heur_hash->{$id1}})))
	      {print(join("\n",map {"$id1\t$_\t"}
			  grep {exists($done_hash->{$_})}
			  keys(%{$heur_hash->{$id1}})),"\n")}

	    next if(scalar(@remaining_ids) == 0);

	    if($split_mode)
	      {$aln_strs =
		 splitStitchAlignAndHash($rec_hash,
					 [$id1,
					  @remaining_ids])}
	    else
	      {
		my $alignment_str =
		  getMuscleMultipleAlignment($rec_hash,
					     $muscle_gaps,
					     0,
					     ($id1,
					      @remaining_ids));

		$aln_strs = hashAlignment($alignment_str);
	      }
	  }

	foreach my $id2 (grep {!exists($done_hash->{$_})}
			 keys(%{$heur_hash->{$id1}}))
	  {
	    my $rec2 = $rec_hash->{$id2};

	    #$indels is an array of strings describing each indel
	    my($numnontermindels,$numsubs,$indels);

	    ($numnontermindels,$numsubs,$indels) =
	      ($align_mode eq 'local' || $align_mode eq 'global' ?
	       clustalw2indelsSubs($aln_strs->{$id1} . $aln_strs->{$id2},1) :
	       clustalw2indelsSubs(getMuscleAlignment($rec1->[1],
						      $rec2->[1],
						      $muscle_gaps)));

	    if($numsubs == 0 && $numnontermindels)
	      {
		$tmp_done_hash->{$id2} = 1;
		verbose("$id1 & $id2 have $numnontermindels indels and ",
			"$numsubs substitutions.");
		print("$id1\t$id2\t",join(',',@$indels),"\n");
	      }
	    else
	      {
		print("$id1\t$id2\t\n");
	      }
	  }
      }

    select($saved_handle) unless($saved_handle eq *STDOUT);
  }

#This subroutine takes an alignment string and divides it by ID.  It uses the
#ID as a key in a hash and the values are the raw alignment strings that were
#extracted from the entire alignment, formatting and all.
sub hashAlignment
  {
    my $alignment_str = $_[0];
    my $local_verbose = $_[1];
    my $aln_strs      = {};
    my $line_num      = 0;
    my $verbose_freq  = 1000;

    verbose("Parsing muscle alignment.") if($local_verbose);

    while($alignment_str =~ /([^\n]*\n?)/g)
      {
	my $line = $1;

	$line_num++;
	verboseOverMe("Reading line $line_num.")
	  if($local_verbose && $line_num % $verbose_freq == 0);

	next if($line =~ /^\s*$/ || $line =~ /^\s*(MUSCLE|CLUSTALW)/ ||
		$line =~ /^[ *]+$/);

	#Grab the ID and the sequence string
	if($line =~ /^(.*\S)\s+\S+$/)
	  {$aln_strs->{$1} .= $line}
	else
	  {error("Could not parse alignment line: [$line].")}
      }

    return($aln_strs);
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

#Sees if the required modules are present in order to do alignments in parallel
sub checkParallelAbility
  {
    unless(eval('use IO::Pipe::Producer;1;') ||
	   eval('use local::lib;use IO::Pipe::Producer;1;'))
      {
	error('Perl Module [IO::Pipe::Producer] not found.');
	return(0);
      }
    unless(eval('use IO::Select;1;') ||
	   eval('use local::lib;use IO::Select;1;'))
      {
	error('Perl Module [IO::Select] not found.');
	return(0);
      }
    return(1);
  }

#Determine the amount of ram on this machine in gigabytes (integer) - rounds up
sub getRamSize
  {
    my $gigs_ram = 1; #Default to 1
    my($bytes);
    if(exists($ENV{OSTYPE}) && $ENV{OSTYPE} =~ /darwin/i)
      {
	$bytes = `sysctl -n hw.memsize`;
	chomp($bytes);
	if($bytes =~ /^\d+$/ && $bytes)
	  {
	    $gigs_ram = int($bytes / 1073741824); #$bytes / 1024 / 1024 / 1024
	    verbose("RAM: [$gigs_ram] Gigs.") if($verbose > 1);
	    return($gigs_ram);
	  }
	else
	  {warning("Unable to parse [sysctl -n hw.memsize] output.  ",
		   "Resorting to perl module [Sys::MemInfo], however as of ",
		   "7/8/2014, it has a bug on macs that returns the wrong ",
		   "amount of physical memory.  Consider using --gigs-ram to ",
		   "run more efficiently.")}
      }

    unless(eval('use Sys::MemInfo qw(totalmem);1;') ||
	   eval('use local::lib;use Sys::MemInfo qw(totalmem);1;'))
      {
	error("Perl Module [Sys::MemInfo] not found.  Defaulting to ",
	      "[$gigs_ram] gigs.");
	return($gigs_ram);
      }

    $bytes = totalmem();
    $gigs_ram = int($bytes / 1073741824);
    if($bytes % 1073741824 > 0.5)
      {$gigs_ram++}

    $gigs_ram = 1 unless($gigs_ram);

    verbose("RAM: [$gigs_ram] Gigs.") if($verbose > 1);

    return($gigs_ram);
  }

#This subroutine is called when a deletion is found in the "fake sequence" -
#one that was created when the sequences were parsed by deleting a base at
#every position (except the ends) - and it then looks at the next substring
#immediately following this substring in both the real and fake sequences.  It
#divides that sequence in half and returns true in three cases:  1. the
#sequences are too short to confirm.  2. The first halves match and the second
#halves do not.  3. The first halves do not match and the second halves do.
sub confirmRecipIndel
  {
    my $position = $_[0];
    my $real_id  = $_[1];
    my $fake_id  = $_[2];
    my $rec_hash = $_[3];
    my $str_size = $_[4];

    my $real_rec = $rec_hash->{$real_id};
    my $fake_rec = $rec_hash->{$fake_id};

    return(1) if($position + $str_size*2 + 1 >= getSize($real_rec));

    my $real_following = uc(substr($real_rec->[1],
				   ($position + $str_size),
				   $str_size));
    my $fake_following = uc(substr($fake_rec->[1],
				   ($position + $str_size + 1),
				   $str_size));

    my $real_first_half =
      uc(substr($real_rec->[1],
		($position + $str_size),
		int($str_size/2)));                       #Assumes str_size > 1
    my $fake_first_half =
      uc(substr($fake_rec->[1],
		($position + $str_size + 1),
		int($str_size/2)));                       #Assumes str_size > 1

    return(1) if($real_first_half eq $fake_first_half &&
		 $real_following ne $fake_following);

    my $real_second_half =
      uc(substr($real_rec->[1],
		($position + $str_size + int($str_size/2) + 1),
		($str_size - int($str_size/2))));         #Assumes str_size > 1
    my $fake_second_half =
      uc(substr($fake_rec->[1],
		($position + $str_size + int($str_size/2) + 1),
		($str_size - int($str_size/2))));         #Assumes str_size > 1

    return($real_second_half eq $fake_second_half &&
	   $real_following ne $fake_following);
  }

#Returns alignment in clustalw format of 2 very similar sequences using muscle
#Uses global: $muscle, $verbose
sub getMuscleAlignment
  {
    my $seq1       = $_[0];
    my $seq2       = $_[1];
    my $musclegaps = defined($_[2]) ? $_[2] : $muscle_gaps;
    my $muscle_exe = defined($muscle) && $muscle ne '' ? $muscle : 'muscle';

    if(!defined($seq1) || !defined($seq2) || $seq1 eq '' || $seq2 eq '')
      {
	error("Empty sequence sent in.");
	return('');
      }

    use IPC::Open3;
    use IO::Select;

    my $output = '';
    local *ALNOUT;
    local *ALNERR;
    my $stdout = \*ALNOUT;
    my $stderr = \*ALNERR;

    my $muscle_command = "$muscle_exe -in /dev/stdin -out /dev/stdout " .
      "-maxiters 1 -diags" . ($verbose ? '' : ' -quiet') . ' -clw' .
	($musclegaps ? '' : ' -gapopen -400 -gapextend -399');

    my $pid = open3(\*ALNIN, $stdout, $stderr, $muscle_command);

    if($pid)
      {
	my $fasta = ">1\n$seq1\n>2\n$seq2\n";

	print ALNIN ($fasta);
	close(ALNIN);

	#Use a Select opject to determine which handles are ready to be read
	my $sel = new IO::Select;
	$sel->add($stderr,$stdout);

	#While we have a file handle with output
	while(my @fhs = $sel->can_read())
	  {
	    #For each file handle with output
	    foreach my $fh (@fhs)
	      {
		my $line = <$fh>;

		#If we hit the end of the file, remove the handle & continue
		unless(defined($line))
		  {
		    $sel->remove($fh);
		    next;
		  }

		#Put output in the correct variable based on the current handle
		if($fh == $stdout)
		  {$output .= $line}
		elsif($fh == $stderr)
		  {verbose($line)}
		else
		  {error("Unable to determine file handle")}
	      }
	  }

	close($stdout);
	close($stderr);
      }
    else
      {
	error("open3() failed $!");
	return('');
      }

    waitpid($pid,0);

    debug("Muscle alignment done",($DEBUG > 2 ? ": $output" : '.'));

    return($output);
  }

#This subroutine edits the abundance in the defline of the sequence records.
sub replaceAbund
  {
    my $def = $_[0];
    my $sum = $_[1];

    if($def =~ /($abundance_pattern)/)
      {
	my $str   = $1;
	my $abund = $2;
	my $str_pat = quotemeta($str);
	if(scalar(@{[$str =~ /$abund/g]}) > 1)
	  {warning("Abundance pattern ambiguity.  The abundance value ",
		   "[$abund] occurs multiple times in the matched abundance ",
		   "string: [$str].  Replacing the last match with the sum ",
		   "value.")}
	my $new_str = $str;
	$new_str =~ s/(.*)$abund/$1$sum/;
	$def =~ s/$str_pat/$new_str/;
      }
    else
      {
	error("Unable to properly parse the abundance string from defline.  ",
	      "Appending arbitrary string [size=$sum;] with sum.");
	$def .= "size=$sum;";
      }

    return($def);
  }

sub meetMinDirectsRule
  {
    my $seq1        = $_[0];
    my $seq2        = $_[1];
    my $str_size    = $_[2];
    my $end_coord   = $_[3];
    my $min_directs = $_[4];
    my $direct_hits = 0;

    #If there are no minimum direct hits required or the shifted hit (i.e.
    #end_coord) is too close to the beginning of the sequence, return true
    return(1) if($min_directs == 0 ||
		 ($str_size + $min_directs - 1) >= $end_coord);

    for(my $p = 0;$p < ($end_coord - $str_size);$p++)
      {
	if(substr($seq1,$p,$str_size) eq substr($seq2,$p,$str_size))
	  {
	    $direct_hits++;
	    return(1) if($direct_hits >= $min_directs);
	  }
      }

    debug("Failed meetMinDirectsRule: shift coord: [$end_coord]:\n$seq1\n",
	  "$seq2");

    return(0);
  }

#Globals Used: $homopolymers_only
sub getNoMonosHash
  {
    my $rec_hash       = $_[0];
    my $min_nomono_len = $_[1];
    my $nomonos_hash   = {};

    foreach my $key (keys(%$rec_hash))
      {$nomonos_hash->{substr($rec_hash->{$key}->[2]->{NOMONOS},
			      0,$min_nomono_len)}->{$key} = 0}

    return($nomonos_hash);
  }

#Returns a single character indicating the size of a submitted integer (max 94)
#Lengths larger than 94 will be modulus'ed.  These numbers will be used to
#compare strings expected to only differ by a little bit, so it's very unlikely
#this will be a problem.  It is also assumed that the input value will not be
#0.
sub getLenChar
  {
    if($_[0] > 94 && $DEBUG)
      {
	warning("Long homopolymer stretch exceding print limit detected.  ",
		"Long homopolymer lengths are stored in a string where each ",
		"character represents a length.  The longest length that can ",
		"be represented in ASCII format is 94.  The script will ",
		"still work, but debug prints for long homopolymers will not ",
		"be correctly represented.");
      }
    debug("Returning [",(chr(31 + $_[0])),"] for length [$_[0]].")
      if($DEBUG > 5);
    return(chr(31 + $_[0]));
  }

#Returns an integer length value from an ascii-encoded single character
sub getCharLen
  {return(ord($_[0]) - 31)}

#Returns an array of hashes describing the indel difference between the first record (assumed to be the more abundant parent) and the second record.
sub getHomopolDiffDescriptions
  {
    #You can supply 2 sequence objects or 4 strings (sequences and nomono strs)
    #Can accept a sequence object (reference to an array) or a sequence string
    #ONLY SUPPLY 2 sequence records - all other
    my $seq1   = ref($_[0]) eq 'ARRAY' ? $_[0]->[2]->{NOMONOC} : $_[0];
    my $seq2   = ref($_[1]) eq 'ARRAY' ? $_[1]->[2]->{NOMONOC} : $_[1];
    my $nmseq1 = ref($_[0]) eq 'ARRAY' ? $_[0]->[2]->{NOMONOS} : $_[2];
    my $nmseq2 = ref($_[1]) eq 'ARRAY' ? $_[1]->[2]->{NOMONOS} : $_[3];

    my $len    = $_[4] ?                       #INTERNAL - DO NOT SUPPLY
      $_[4] : (length($seq1) < length($seq2) ?
	       length($seq1) : length($seq2));
    my $pos    = $_[5] ? $_[5] : 1;            #INTERNAL - DO NOT SUPPLY
    my $rec1   = ref($_[0]) eq 'ARRAY' ? $_[0] : $_[6];
    my $rec2   = ref($_[1]) eq 'ARRAY' ? $_[1] : $_[7];

    #Error-check length
    if(length($seq2) != $len || length($seq1) != $len)
      {
	$seq1   = substr($seq1,0,$len);
	$seq2   = substr($seq2,0,$len);
	$nmseq1 = substr($nmseq1,0,$len);
	$nmseq2 = substr($nmseq2,0,$len);
      }

    debug("getHomopolDiffDescriptions called with\n\t$seq1\n\t$seq2\n\t",
	  "$nmseq1\n\t$nmseq2\n\tLENGTH $len\n\tPOSITION $pos");

    if($len == 1)
      {
	debug("Length is 1 - returning.");
	if($seq1 ne $seq2)
	  {return([{POS              => $pos,
		    PARENTMONOMER    => ($nmseq1 x getCharLen($seq1)),
		    CHILDMONOMER     => ($nmseq2 x getCharLen($seq2)),
		    PARENTMONOMERLEN => getCharLen($seq1),
		    CHILDMONOMERLEN  => getCharLen($seq2),
		    SHORTER          => (getCharLen($seq1) <
					 getCharLen($seq2) ?
					 getCharLen($seq1) :
					 getCharLen($seq2)),
		    PARENTREDUCLEN   => $rec1->[2]->{NOMONOL},
		    CHILDREDUCLEN    => $rec2->[2]->{NOMONOL},
		    CHILDABUND       => getAbund($rec2)}])}
	else
	  {return([])}
      }

    my $halflen = int($len/2);

    #Split the sequences into 2 halves
    my($lseq1,$rseq1)     = unpack("a$halflen".'a*',$seq1);
    my($lseq2,$rseq2)     = unpack("a$halflen".'a*',$seq2);
    my($nmlseq1,$nmrseq1) = unpack("A$halflen".'A*',$nmseq1);
    my($nmlseq2,$nmrseq2) = unpack("A$halflen".'A*',$nmseq2);

    my $eql = ($lseq1 eq $lseq2);
    my $eqr = ($rseq1 eq $rseq2);

    #If only 1 side is different
    if($eql != $eqr)
      {
	debug("One side is the same.");
	#If the left sides are the same
	if($eql)
	  {return(getHomopolDiffDescriptions($rseq1,
					     $rseq2,
					     $nmrseq1,
					     $nmrseq2,
					     $halflen + ($len % 2),
					     $halflen + $pos,
					     $rec1,
					     $rec2))}
	else
	  {return(getHomopolDiffDescriptions($lseq1,
					     $lseq2,
					     $nmlseq1,
					     $nmlseq2,
					     $halflen,
					     $pos,
					     $rec1,
					     $rec2))}
      }
    elsif(!$eql && !$eqr)
      {
	debug("Both sides are different.");
	my $right_diffs = getHomopolDiffDescriptions($rseq1,
						     $rseq2,
						     $nmrseq1,
						     $nmrseq2,
						     $halflen + ($len % 2),
						     $halflen + $pos,
						     $rec1,
						     $rec2);
	my $left_diffs = getHomopolDiffDescriptions($lseq1,
						    $lseq2,
						    $nmlseq1,
						    $nmlseq2,
						    $halflen,
						    $pos,
						    $rec1,
						    $rec2);
        if(scalar(@$left_diffs) && scalar(@$right_diffs))
          {
	    debug("Got descriptions back from both sides.");
	    return([@$left_diffs,@$right_diffs]);
	  }
        elsif(scalar(@$left_diffs))
          {
	    debug("Just got descriptions back from the left side.");
	    return($left_diffs);
	  }
        elsif(scalar(@$right_diffs))
          {
	    debug("Just got descriptions back from the right side.");
	    return($right_diffs);
	  }
	debug("Neither side returned descriptions.");
	return([]);
      }

    debug("Both sides are the same.");

    return([]);
  }

#This determines if the homopolymer differences are likely to be 454 errors
#Takes a reference to an array of hash references describing indels between two
#Returns an array of indel descriptions - populated if all are 454 indel errors
#and empty if at least 1 indel is not a 454 indel error
#sequences (created by getHomopolDiffDescriptions)
sub only454Indels
  {
    my $rec1 = $_[0];
    my $rec2 = $_[1];
    my $monomer_diffs_array = getHomopolDiffDescriptions($rec1,$rec2);
                                     #Returned array looks like this:
                                     #[{POS=>#,PARENTMONOMER=>N,
                                     #  CHILDMONOMER=>N,PARENTMONOMERLEN=>#,
                                     #  CHILDMONOMERLEN=>#,SHORTER=>#,
                                     #  CHILDREDUCLEN=>#,PARENTREDUCLEN=>#},
                                     #  CHILDABUND=>#,...]

    debug("Comparing records: [$rec1->[0]] and [$rec2->[0]]: seqs: ",
	  "[$rec1->[1]] and [$rec2->[1]].\n",
	  "CHECKING HPOL DESCRIPTIONS: ",
	  join(';',map {join(',',%{$_})} @$monomer_diffs_array))
      if($DEBUG > 2);

    my @four54_errors = ();

    for(my $i = 0;$i < scalar(@$monomer_diffs_array);$i++)
      {
        #The last reduced string position should not be considered for possibly
        #being an indel because it's been artificially chopped.
        last if(abs($monomer_diffs_array->[$i]->{CHILDREDUCLEN} -
                    $monomer_diffs_array->[$i]->{PARENTREDUCLEN}) <= 1 &&
                ($monomer_diffs_array->[$i]->{POS} >=
		 $monomer_diffs_array->[$i]->{CHILDREDUCLEN} ||
		 $monomer_diffs_array->[$i]->{POS} >=
		 $monomer_diffs_array->[$i]->{PARENTREDUCLEN}));

        my $nxt = (($i + 1) < scalar(@$monomer_diffs_array) ?
          $monomer_diffs_array->[$i + 1] : undef);

        if(!is454Indel($monomer_diffs_array->[$i],$nxt))
          {
	    debug("Hash index [$i] indicates real indel or substitution ",
		  "(i.e. is NOT a 454 indel)") if($DEBUG > 2);
	    return(());
	  }
        #else Indel looks like 454 error, so continue checking

	#Save the indel as a string in the output format (eg homopolymer del A)
	my $char = substr($monomer_diffs_array->[$i]->{PARENTMONOMER},0,1);
	my $diff = abs($monomer_diffs_array->[$i]->{PARENTMONOMERLEN} -
		       $monomer_diffs_array->[$i]->{CHILDMONOMERLEN});
	my $str  = $char x $diff;
	my $pos = getRealIndelPos($rec1,
				  $rec2,
				  $monomer_diffs_array->[$i]->{POS});
	push(@four54_errors,
	     'homopolymer ' .
	     ($monomer_diffs_array->[$i]->{PARENTMONOMERLEN} >
	      $monomer_diffs_array->[$i]->{CHILDMONOMERLEN} ? 'del' : 'ins') .
	     " $pos$str");

	debug("Indel [$i] is a 454 indel apparently.  Next neighbor ",
	      (defined($nxt) ? "was defined" : "was NOT defined"))
	  if($DEBUG > 2);
      }

    debug("Only differs by 454 indel errors") if($DEBUG > 2);

    return(@four54_errors);
  }

#Determines whether an indel is likely a 454 error.  Uses the next indel to
#help make that determination
#Takes the hash describing indels between two sequences for the indel being
#decided on and the next indel following it
#Returns true or false (1 or 0 respectively)
#Globals used: $long_homopol_length, $max_long_hpl_diff, $max_short_hpl_diff,
#$max_long_hpl_abund, $max_short_hpl_abund
sub is454Indel
  {
    my $cur_info = $_[0];
    my $nxt_info = $_[1];

    my $shortest_min         = $long_homopol_length;
    my $max_len_diff         = $max_long_hpl_diff;
    my $singlet_max_len_diff = $max_short_hpl_diff;
    my $long_max_abund       = $max_long_hpl_abund;
    my $short_max_abund      = $max_short_hpl_abund;

    my $cur_diff =
      abs($cur_info->{PARENTMONOMERLEN} - $cur_info->{CHILDMONOMERLEN});
    my $nxt_diff = defined($nxt_info) ? abs($nxt_info->{PARENTMONOMERLEN} -
                                            $nxt_info->{CHILDMONOMERLEN}) : 0;
    my $cur_type =
      ($cur_info->{PARENTMONOMERLEN} < $cur_info->{CHILDMONOMERLEN} ?
       'ins' : 'del');
    my $nxt_type =
      (defined($nxt_info) ? ($nxt_info->{PARENTMONOMERLEN} <
			     $nxt_info->{CHILDMONOMERLEN} ? 'ins' : 'del') :
       '');

    my $dbg_msg = join('',"if((cur_info->{POS} < (cur_info->{PARENTREDUCLEN} ",
		       "- 2) || cur_info->{POS} < (cur_info->{CHILDREDUCLEN} ",
		       "- 2)) && ((cur_info->{SHORTER} >= shortest_min && ",
		       "(long_max_abund == 0 || (exists(cur_info->{CHILDABUND",
		       "}) && cur_info->{CHILDABUND} <= long_max_abund)) && (",
		       "((max_len_diff == 0 || cur_diff <= max_len_diff) && ",
		       "cur_diff > 1 &&!(defined(nxt_info) && cur_type ne ",
		       "nxt_type && cur_diff == nxt_diff && cur_diff == 2 && ",
		       "(cur_info->{POS} + 1) == nxt_info->{POS})) || (",
		       "cur_diff == 1 &&!(defined(nxt_info) && cur_type ne ",
		       "nxt_type && nxt_diff == 1 && ((cur_info->{POS} + 2) ",
		       "== nxt_info->{POS} || (cur_info->{POS} + 1) == ",
		       "nxt_info->{POS}))))) ||(cur_info->{SHORTER} < ",
		       "shortest_min && (cur_diff <= singlet_max_len_diff || ",
		       "singlet_max_len_diff == 0) && (short_max_abund == 0 ",
		       "|| (exists(cur_info->{CHILDABUND}) && cur_info->{",
		       "CHILDABUND} <= short_max_abund)) && (!(defined(",
		       "nxt_info) && cur_type ne nxt_type && cur_diff == 1 ",
		       "&& nxt_diff == 1 && (cur_info->{POS} + 1) == ",
		       "nxt_info->{POS}) && !(defined(nxt_info) && cur_type ",
		       "ne nxt_type && cur_diff == 1 && nxt_diff == 1 && (",
		       "cur_info->{POS} + 2) == nxt_info->{POS})))))\n",

		       "if(($cur_info->{POS} < ($cur_info->{PARENTREDUCLEN} ",
		       "- 2) || $cur_info->{POS} < (",
		       "$cur_info->{CHILDREDUCLEN}  - 2)) && ((",
		       "$cur_info->{SHORTER} >= $shortest_min && (",
		       "$long_max_abund == 0 || (",
		       exists($cur_info->{CHILDABUND}),
		       " && $cur_info->{CHILDABUND} <= $long_max_abund)) && ",
		       "((($max_len_diff == 0 || $cur_diff <= $max_len_diff) ",
		       "&& $cur_diff > 1 &&!(",
		       defined($nxt_info)," && $cur_type ne ",
		       (defined($nxt_type) ? $nxt_type : 'undef'),
		       " && $cur_diff == ",
		       (defined($nxt_diff) ? $nxt_diff : 'undef'),
		       " && $cur_diff == 2 && ($cur_info->{POS} + 1) == ",
		       (defined($nxt_info) ? $nxt_info->{POS} : 'undef'),
		       ")) || ($cur_diff == 1 && !(",defined($nxt_info),
		       " && $cur_type ne ",
		       (defined($nxt_type) ? $nxt_type : 'undef')," && ",
		       (defined($nxt_diff) ? $nxt_diff : 'undef')," == 1 && ",
		       "(($cur_info->{POS} + 2) == ",
		       (defined($nxt_info) ? $nxt_info->{POS} : 'undef'),
		       " || ($cur_info->{POS} + 1) == ",
		       (defined($nxt_info) ? $nxt_info->{POS} : 'undef'),
		       "))))) || ($cur_info->{SHORTER} < $shortest_min && ",
		       "($cur_diff <= $singlet_max_len_diff || ",
		       "$singlet_max_len_diff == 0) && ($short_max_abund == ",
		       "0 || (",exists($cur_info->{CHILDABUND})," && ",
		       "$cur_info->{CHILDABUND} <= $short_max_abund)) && (!(",
		       defined($nxt_info)," && $cur_type ne ",
		       (defined($nxt_type) ? $nxt_type : 'undef')," && ",
		       "$cur_diff == 1 && ",
		       (defined($nxt_diff) ? $nxt_diff : 'undef')," == 1 && (",
		       "$cur_info->{POS} + 1) == ",
		       (defined($nxt_info) ? $nxt_info->{POS} : 'undef'),
		       ") && !(",defined($nxt_info)," && $cur_type ne ",
		       (defined($nxt_type) ? $nxt_type : 'undef')," && ",
		       "$cur_diff == 1 && ",
		       (defined($nxt_diff) ? $nxt_diff : 'undef')," == 1 && (",
		       "$cur_info->{POS} + 2) == ",
		       (defined($nxt_info) ? $nxt_info->{POS} : 'undef'),
		       ")))))") if($DEBUG > 2);

    if(#this position of the current homopolymer is not within the last 3
       #homopolymers of the reduced string of the child or parent
       ($cur_info->{POS} < ($cur_info->{PARENTREDUCLEN} - 2) ||
        $cur_info->{POS} < ($cur_info->{CHILDREDUCLEN}  - 2)) && (
       (
        #the shorter of the child/parent monomers is at least length =
	#shortest_min and abundance of the child is <= long_max_abund AND (
        $cur_info->{SHORTER} >= $shortest_min &&
	($long_max_abund == 0 ||
	 (exists($cur_info->{CHILDABUND}) &&
	  $cur_info->{CHILDABUND} <= $long_max_abund)) &&
        (
         #(difference in length of the child/parent monomers is >= 2 and at
	 #most max_len_diff (or max_len_diff is 0) AND
         (($max_len_diff == 0 || $cur_diff <= $max_len_diff) &&
	  $cur_diff > 1 &&
          #there is not an opposite type indel with a difference in length that
          #is "the same" (and = 2) and is immediately adjacent) OR
          !(defined($nxt_info) && $cur_type ne $nxt_type &&
	    $cur_diff == $nxt_diff && $cur_diff == 2 &&
            ($cur_info->{POS} + 1) == $nxt_info->{POS}
           )
         ) ||
         #(difference in length of the child/parent monomers is 1 AND
         ($cur_diff == 1 &&
          #there is not an opposite type indel with a difference in length 1
          #that is either immediately adjacent or after a single non-indel
          #monomer
          !(defined($nxt_info) && $cur_type ne $nxt_type && $nxt_diff == 1 &&
            (($cur_info->{POS} + 2) == $nxt_info->{POS} ||
             ($cur_info->{POS} + 1) == $nxt_info->{POS})
           )
         )
        )
       ) ||
       (#the shorter of the child/parent monomers is at most 2, the difference
        #in length <= singlet_max_len_diff and the abundance of the lesser
        #abundant sequence is 1 AND
        $cur_info->{SHORTER} < $shortest_min &&
	($cur_diff <= $singlet_max_len_diff || $singlet_max_len_diff == 0) &&
	($short_max_abund == 0 ||
	 (exists($cur_info->{CHILDABUND}) &&
	  $cur_info->{CHILDABUND} <= $short_max_abund)) &&
        (
         #there is not an opposite type indel with a difference in length 1
         #(and is the same) that is immediately adjacent) AND
         !(defined($nxt_info) && $cur_type ne $nxt_type && $cur_diff == 1 &&
	   $nxt_diff == 1 && ($cur_info->{POS} + 1) == $nxt_info->{POS}
          ) &&
         #there is not an opposite type indel with a difference in length 1
         #that is after a non-indel monomer
         !(defined($nxt_info) && $cur_type ne $nxt_type && $cur_diff == 1 &&
	   $nxt_diff == 1 && ($cur_info->{POS} + 2) == $nxt_info->{POS}
          )
        )
       ))
      )
      {
	debug("TRUE: $dbg_msg") if($DEBUG > 3);

	return(1);
      }

    debug("FALSE: $dbg_msg") if($DEBUG > 3);

    return(0);
  }

#Returns the alignment position of the indicated reduced string indel position
#Assumes first record is parent
sub getRealIndelPos
  {
    my $rec1      = $_[0];
    my $rec2      = $_[1];
    my $reduc_pos = $_[2];
    my $real_pos  = 1;

    #Assumes reduc_pos is less than the smaller length of the two records'
    #reduced strings
    for(my $i = 0;$i < $reduc_pos;$i++)
      {
	my $parent_monomer_length = getCharLen(substr($rec1->[2]->{NOMONOC},
						      $i,
						      1));
	my $child_monomer_length  = getCharLen(substr($rec2->[2]->{NOMONOC},
						      $i,
						      1));
	if(($i + 1) == $reduc_pos)
	  {$real_pos += ($parent_monomer_length < $child_monomer_length ?
			 $parent_monomer_length : $child_monomer_length)}
	else
	  {$real_pos += ($parent_monomer_length > $child_monomer_length ?
			 $parent_monomer_length : $child_monomer_length)}
      }

    return($real_pos);
  }
