#!/usr/bin/perl
#Note: 'use warnings' is below instead of having -w above

#Generated using perl_script_template.pl 2.11
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Center for Computational Research
#Copyright 2012

#These variables (in main) are used by getVersion() and usage()
my $software_version_number = '1.4';
my $created_on_date         = '2/12/2014';

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
my($outfile_suffix); #Not defined so input can be overwritten
my $input_files         = [];
my $outdirs             = [];
my $current_output_file = '';
my $help                = 0;
my $adv_help            = 0;
my $version             = 0;
my $overwrite           = 0;
my $force               = 0;
my $skip_existing       = 0;
my $header              = 1;
my $error_limit         = 50;
my $dry_run             = 0;
my $use_as_default      = 0;
my $defaults_dir        = (sglob('~/.rpst'))[0];
my $filetype            = 'auto';

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, getCommand, quit, and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;
my $ignore_errors = 0;
my @user_defaults = getUserDefaults();

my $GetOptHash =
  {'i|seq-file=s'       => sub {push(@$input_files,    #REQUIRED unless <> is
			             [sglob($_[1])])}, #         supplied
   '<>'                 => sub {push(@$input_files,    #REQUIRED unless -i is
				     [sglob($_[0])])}, #         supplied
   'o|outfile-suffix=s' => \$outfile_suffix,           #OPTIONAL [undef]
   'outdir=s'           => sub {push(@$outdirs,        #OPTIONAL
				     [sglob($_[1])])},
   't|filetype=s'       => \$filetype,                 #OPTIONAL [auto](fasta,
				                       #         fastq,auto)
   'overwrite'          => \$overwrite,                #OPTIONAL [Off]
   'force'              => \$force,                    #OPTIONAL [Off]
   'skip-existing!'     => \$skip_existing,            #OPTIONAL [Off]
   'ignore'             => \$ignore_errors,            #OPTIONAL [Off]
   'verbose:+'          => \$verbose,                  #OPTIONAL [Off]
   'quiet'              => \$quiet,                    #OPTIONAL [Off]
   'debug:+'            => \$DEBUG,                    #OPTIONAL [Off]
   'help'               => \$help,                     #OPTIONAL [Off]
   'advanced-help'      => \$adv_help,                 #OPTIONAL [Off]
   'version'            => \$version,                  #OPTIONAL [Off]
   'header!'            => \$header,                   #OPTIONAL [On]
   'error-type-limit=s' => \$error_limit,              #OPTIONAL [0]
   'dry-run!'           => \$dry_run,                  #OPTIONAL [Off]
   'use-as-default!'    => \$use_as_default,           #OPTIONAL [Off]
  };

#If the user has previously stored any defaults
if(scalar(@user_defaults))
  {
    #Save the defaults, because GetOptionsFromArray alters the array
    my @tmp_user_defaults = @user_defaults;

    #Get any default values the user stored, set the default values before
    #calling GetOptions using the real @ARGV array and before calling usage so
    #that the user-defined default values will show in the usage output
    GetOptionsFromArray(\@user_defaults,%$GetOptHash);

    #Reset the user defaults array for later use
    @user_defaults = @tmp_user_defaults;
  }

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && isStandardInputFromTerminal())
  {
    usage();
    quit(0);
  }

#Get the input options & catch any errors in option parsing
unless(GetOptions(%$GetOptHash))
  {
    #Try to guess which arguments GetOptions is complaining about
    my @possibly_bad = grep {!(-e $_)} map {@$_} @$input_files;

    error('Getopt::Long::GetOptions reported an error while parsing the ',
	  'command line arguments.  The error should be above.  Please ',
	  'correct the offending argument(s) and try again.');
    usage(1);
    quit(-1);
  }

#Error-check for mutually exclusive flags supplied together
if(scalar(grep {$_} ($use_as_default,$help,$adv_help,$version)) > 1)
  {
    error("Options [",join(',',grep {$_} ($use_as_default,$help,$adv_help,
					  $version)),
	  "] are mutually exclusive.");
    quit(-20);
  }

#If the user specified that they would like to use the current options as
#default values, store them
if($use_as_default)
  {
    if(saveUserDefaults($preserve_args))
      {
	print("Old user defaults: [",join(' ',@user_defaults),"].\n",
	      "New user defaults: [",join(' ',getUserDefaults()),"].\n");
	quit(0);
      }
    else
      {quit(-19)}
  }

print STDERR ("Starting dry run.\n") if($dry_run);

#Print the debug mode (it checks the value of the DEBUG global variable)
debug('Debug mode on.') if($DEBUG > 1);

#If the user has asked for help, call the help subroutine
if($adv_help)
  {
    help(1);
    quit(0);
  }
elsif($help)
  {
    help();
    quit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    print(getVersion($verbose),"\n");
    quit(0);
  }

#Check validity of verbosity options
if($quiet && ($verbose || $DEBUG))
  {
    $quiet = 0;
    error('You cannot supply the quiet and (verbose or debug) flags ',
	  'together.');
    quit(-2);
  }

#Check validity of existing outfile options
if($skip_existing && $overwrite)
  {
    error('You cannot supply the --overwrite and --skip-existing flags ',
	  'together.');
    quit(-3);
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
    quit(-4);
  }

#Make sure that an outfile suffix has been supplied if an outdir has been
#supplied
if(scalar(@$outdirs) && !defined($outfile_suffix))
  {
    error("An outfile suffix (-o) is required if an output directory ",
	  "(--outdir) is supplied.  Note, you may supply an empty string ",
	  "to name the output files the same as the input files.");
    quit(-5);
  }

#Get all the corresponding groups of files and output directories to process
my($input_file_sets,
   $outfile_stub_sets) = getFileSets($input_files,
				     $outdirs);

#Look for existing output files generated by the input_files array
#Note, the index for the submitted array is determined by the position of the
#input_files array in the call to getFileSets
my(@existing_outfiles);

my @tmp_existing_outfiles = getExistingOutfiles($outfile_stub_sets->[0],
						$outfile_suffix);
push(@existing_outfiles,@tmp_existing_outfiles)
  if(scalar(@tmp_existing_outfiles));

#CHECK EXISTING OUTPUT FILES ASSOCIATED WITH INPUT FILE ARRAYS AS ABOVE

#If any of the expected output files already exist, quit with an error
if(scalar(@existing_outfiles) && !$overwrite && !$skip_existing)
  {
    error("Files exist: [",join(',',@existing_outfiles),"].\nUse --overwrite ",
	  'or --skip-existing to continue.');
    quit(-6);
  }

#Create the output directories
mkdirs(@$outdirs);

verbose('Run conditions: ',getCommand(1));

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

#If output is going to STDOUT instead of output files with different extensions
#or if STDOUT was redirected, output run info once
verbose('[STDOUT] Opened for all output.') if(!defined($outfile_suffix));

#Store info. about the run as a comment at the top of the output file if
#STDOUT has been redirected to a file
if(!isStandardOutputToTerminal() && $header)
  {print(getVersion(),"\n",
	 '#',scalar(localtime($^T)),"\n",
	 '#',getCommand(1),"\n");}

#This variable for using arbitrary length seeds is legacy (from testing) and it
#didn't perform as well, but might be useful later if the sequence size gets
#very long
my $seedsize = 0;
my $input_file = '';

#For each set of input files associated by getFileSets
foreach my $set_num (0..$#$input_file_sets)
  {
    $input_file      = $input_file_sets->[$set_num]->[0];
    my $outfile_stub = $outfile_stub_sets->[$set_num]->[0];

    openIn(*INPUT,$input_file) || next;

    if(defined($outfile_suffix))
      {
	$current_output_file = $outfile_stub . $outfile_suffix;

	checkFile($current_output_file,$input_file_sets->[$set_num]) || next;

	openOut(*OUTPUT,$current_output_file) || next;
      }

    next if($dry_run);

    my $verbose_freq = 100;
    my $ary          = [];
    my $lookup       = {};
    my $cnt          = 0;
    my($rec,$last_len);

    #For each line in the current input file
    while($rec = getNextSeqRec(*INPUT))
      {
	$cnt++;
	verboseOverMe("[$input_file] Reading record: [$cnt].")
	  unless($cnt % $verbose_freq);

	my($def,$seq) = @$rec;

	my $id = '';
	if($def =~ /\s*[\%\>]\s*(\S+)/)
	  {$id = $1}
	else
	  {
	    warning("Could not parse defline in record [$cnt] of file ",
		    "[$input_file]: [$def].  Using entire defline as ID.");
	    $id = $def;
	  }

	push(@$ary,[$id,$seq]);

	##
	## Create a lookup hash to search faster later
	##

	my $len = length($seq);

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
		error($err,"  Skipping file.  Use --force to over-ride.");
		last;
	      }
	  }
	if($seq =~ /([^ATGCatcg])/)
	  {
	    my $example = $1;
	    my $err = "Invalid character(s) found in sequence [$id] in file " .
	      "[$input_file] (e.g. [$example]).";
	    if($force)
	      {warning($err)}
	    else
	      {
		error($err,"  Skipping sequence.  Use --force to over-ride.");
		next;
	      }
	  }
	$last_len = $len;

	#This code for using arbitrary length seeds is legacy (from testing)
	#and it didn't perform as well, but might be useful later if the
	#sequence size gets very long
	my(@seeds);
	if($seedsize)
	  {
	    #Split the sequences into parts
	    @seeds = unpack("(A$seedsize)2",$seq);
	    if((scalar(@seeds) * $seedsize) > $len && scalar(@seeds) > 1)
	      {
		$seeds[-2] .= $seeds[-1];
		pop(@seeds);
	      }
	  }
	else
	  {
	    my($halflen);
	    #Split the sequences into 2 halves
	    $halflen = int($len/2);
	    @seeds = unpack("A$halflen".'A*',$seq);
	  }

	foreach my $seed (@seeds)
	  {push(@{$lookup->{$seed}},$#{$ary})}
      }

    closeIn(*INPUT);

    printFirstNeighbors($ary,$lookup,$cnt);

    closeOut(*OUTPUT) if(defined($outfile_suffix));
  }

verbose("[STDOUT] Output done.") if(!defined($outfile_suffix));

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
    my($command);

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
	chomp($command);
	$command .= ' ';
      }

    #Build the original command
    $command .= join(' ',($0,@$arguments));

    #Add any default flags that were previously saved
    my @default_options = getUserDefaults();
    if(scalar(@default_options))
      {
	$command .= ' [USER DEFAULTS ADDED: ';
	$command .= join(' ',@default_options);
	$command .= ']';
      }

    return($command);
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
    #Note, when bsd_glob gets a string with a glob character it can't expand,
    #it drops the string entirely.  Those strings are returned with the glob
    #characters so the surrounding script can report an error.
    return(map {my @x = bsd_glob($_);scalar(@x) &&
		  scalar(@x) == scalar(grep {-e $_} @x) ? @x : $_}
	   split(/(?<!\\)\s+/,$command_line_string));
  }

sub getVersion
  {
    my $full_version_flag = $_[0];
    my $template_version_number = '2.11';
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
		' Contact: rwleach@ccr.buffalo.edu',
		' Company: Center for Computational Research',
		' Copyright 2012'));
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
#yourself before calling this.  Does not exit if $ignore_errors is true.  Takes
#the error number to supply to exit().
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

    #Exit if we are not ignoring errors or if there were no errors at all
    exit($errno) if(!$ignore_errors || $errno == 0);
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

    #Assumes that outdirs were popped off above
    if(scalar(@_) > 1)
      {
	debug("Copy Call 2") if($DEBUG < -99);
	$file_types_array = [copyArray(grep {scalar(@$_)} @_)];
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
	    quit(-7);
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
	    quit(-8);
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
	    quit(-9);
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
	quit(-10);
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
		error("You cannot embed a directory path in the outfile stub ",
		      "(provided via -i with a single argument, when ",
		      "redirecting standard input in) along with an output ",
		      "directory (--outdir).  Please use one or the other.");
		quit(-16);
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
		quit(-11);
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
		quit(-12);
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
	    quit(-13);
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
		   scalar(@{$subarrays[0]}) != 1)
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
		    debug("Processing from $num_cols..($max_num_cols - 1)")
		      if($DEBUG < -99);
		    foreach($num_cols..($max_num_cols - 1))
		      {
			debug("Pushing [$row_array->[0]] on")
			  if($DEBUG < -99);
			push(@$row_array,$row_array->[0]);
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
	quit(-14);
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
		  join(',',@$file_type_array),"] and these should not [",
		  join(',',@{$file_type_array->[0]}),"] [",
		  (scalar(@$file_type_array) > 1 ?
		   join(',',@{$file_type_array->[1]}) : ''),"].");
	    foreach my $file_set (@$file_type_array)
	      {debug(join(',',@$file_set))}
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
			 ($file_types_array->[$association]->[$row_index]
			  ->[$col_index] eq '-' ? $outfile_stub :
			  $file_types_array->[$association]->[$row_index]
			  ->[$col_index]));
		  }
	      }
	  }
      }

    if($nonunique_found)
      {
	warning('The following output file name stubs were created by ',
		'multiple input file names and will be overwritten if used.  ',
		'Please make sure each similarly named input file outputs to ',
		'a different output directory or that the input file names ',
		'bare no similarity.  Offending file name conflicts: [',
		join(',',map {"$_ is written to by [" .
				join(',',@{$unique_out_check->{$_}}) . "]"}
		     (grep {scalar(@{$unique_out_check->{$_}}) > 1}
		      keys(%$unique_out_check))),'].');
      }

    debug("Processing input file sets: [(",
	  join('),(',(map {join(',',@$_)} @$infile_sets_array)),
	  ")] and output stubs: [(",
	  join('),(',(map {join(',',@$_)} @$outfile_stubs_array)),")].")
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
    my $outfile_suffix = $_[1];

    my $existing_outfiles = [];

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
	    quit(-17) unless($use_as_default);
	  }
      }

    return($status);
  }

#This subroutine checks for existing output files
sub checkFile
  {
    my $current_output_file = $_[0];
    my $input_file_set      = $_[1];
    my $status              = 1;

    if(-e $current_output_file)
      {
	if($skip_existing)
	  {
	    warning("[$current_output_file] Output file exists.  Skipping ",
		    "input file(s): [",join(',',@$input_file_set),"].");
	    $status = 0;
	  }
	elsif(!$overwrite)
	  {
	    error("[$current_output_file] Output file exists.  Unable to ",
		  "proceed.  Encountered while processing input file(s): [",
		  join(',',@$input_file_set),
		  "].  This may have been caused by multiple input files ",
		  "writing to one output file because there were not ",
		  "existing output files when this script started.  If any ",
		  "input files are writing to the same output file, you ",
		  "should have seen a warning about this above.  Otherwise, ",
		  "you may have multiple versions of this script running ",
		  "simultaneously.  Please check your input files and ",
		  "outfile suffixes to fix any conflicts or supply the ",
		  "--skip-existing or --overwrite flag to proceed.");
	    quit(-15);
	  }
      }

    return($status);
  }

#Uses globals: $header,$main::open_handles,$dry_run
sub openOut
  {
    my $file_handle         = $_[0];
    my $current_output_file = $_[1];
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

	#Select the output file handle
	select($file_handle);

	#Store info about the run as a comment at the top of the output
	print(getVersion(),"\n",
	      '#',scalar(localtime($^T)),"\n",
	      '#',getCommand(1),"\n") if($header);
      }

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

    #Close the input file handle
    close($file_handle);

    verbose("[$main::open_handles->{$file_handle}] Input file done.  ",
	    "Time taken: [",scalar(markTime()),' Seconds].');

    delete($main::open_handles->{$file_handle});
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

#Note: Creates a surrounding reference to the submitted array if called in
#scalar context and there are more than 1 elements in the parameter array
sub copyArray
  {
    if(scalar(grep {ref(\$_) ne 'SCALAR' && ref($_) ne 'ARRAY'} @_))
      {
	error("Invalid argument - not an array of scalars.");
	quit(-18);
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
    my $script        = $0;
    $script           =~ s/^.*\/([^\/]+)$/$1/;
    my $defaults_file = $defaults_dir . "/$script";
    my $return_array  = [];

    if(open(DFLTS,$defaults_file))
      {
	@$return_array = map {chomp;$_} <DFLTS>;
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
    my $argv          = $_[0];
    my $script        = $0;
    $script           =~ s/^.*\/([^\/]+)$/$1/;
    my $defaults_file = $defaults_dir . "/$script";
    my $status        = 1;

    my $save_argv = [grep {$_ ne '--use-as-default'} @$argv];

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

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script = $0;
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
Lewis Seigler Institute for Integrative Genomics, Princeton University
Princeton, NJ
rleach\@genomics.princeton.edu

end_print

    if(!$advanced)
      {
	print << "end_print";
* WHAT IS THIS: This script represents the first step of a 4 step process in
                the package called 'hamming1':

                *1. neighbors.pl  generates a hamming distance 1 neighbors file
                 2. errorRates.pl generates a Z-Score histogram (see -h)
                 3. errorRates.pl generates error rate estimates (see -z)
                 4. nZeros.pl     generates expected error abundances

                Use these scripts when you have a metagenomic sample of
                ungapped, aligned, & same-sized sequences, to help give you an
                idea which sequences might be real and which are the result of
                PCR substitution errors.  The N0 expected error abundances
                output can be compared to actual abundance to infer whether the
                sequence is real.  I.e. If the actual abundance is much larger
                than the abundance you would expect to see if a sequence is the
                result of PCR substitution errors, then you would expect the
                sequence to be real (i.e. exist in the original sample).

                This script finds all hamming distance 1 neighbors of a set of
                aligned, same-length, no-gap, input sequences.  I.e., for each
                sequence, it finds all other sequences which differ from it by
                1 substitution.  Only works for mismatches, not indels.

* INPUT FORMAT: Fasta or fastq format DNA sequence files.

* OUTPUT FORMAT: Tab-delimited ascii text file where the first column contains
                 every sequence's ID (the string on the defline up to the first
                 white space, after the '>' or '\@'.  Subsequent columns
                 contain the IDs of the sequences of the first neighbors,
                 alternating with columns of "position:N>N" where "N>N" is the
                 base change from the "mother sequence" in the first column to
                 its hamming distance 1 sequence neighbor.  E.g.:

1	3	4:A>G	9	99:T>A
2	5	7:T>C	44	20:G>C
3	1	4:G>A	21	16:T>A
4	8	61:A>G	50	37:A>C
5	2	55:C>T	33	90:C>G

end_print
      }
    else
      {
	print << "end_print";
* ADVANCED FILE I/O FEATURES:

Sets of input files, each with different output directories can be supplied.
Supply each file set with an additional -i (or --seq-file) flag.  Wrap each
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

    print("$script -i \"outfile_stub\" [-o .ext] $options < input_file\n",
	  "$script -i \"input file(s)\" [-o .ext] $options\n");

    if($no_descriptions)
      {print("Run with no options for expanded usage.\n")}
    else
      {
        print << 'end_print';
     -i|--seq-file*       REQUIRED Space-separated input file(s).  Expands glob
                                   characters ('*', '?', etc.).  When standard
                                   input detected, -o has been supplied, and
                                   there's only 1 value, this is used as a file
                                   name stub.  See --help for file format and
                                   advanced usage.  *No flag required.
     -t|--filetype        OPTIONAL [auto](fasta,fastq,auto) Input file (-i)
                                   type.  Using this instead of auto will make
                                   file reading faster.  "auto" cannot be used
                                   when redirecting a file in.
     -o|--outfile-suffix  OPTIONAL [nothing] Outfile extension appended to
                                   input files.  Empty string overwrites input
                                   files (unless --outdir  supplied).  Appends
                                   to "STDIN" when standard input is detected
                                   (unless a stub is provided via -i).  See
                                   --help for file format and advanced usage.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Also see --help.
     --verbose            OPTIONAL Verbose mode/level.  (e.g. --verbose 2)
     --quiet              OPTIONAL Quiet mode.
     --skip-existing      OPTIONAL Skip existing output files.
     --overwrite          OPTIONAL Overwrite existing output files.
     --ignore             OPTIONAL Ignore critical errors.  Also see
                                   --overwrite or --skip-existing.
     --noheader           OPTIONAL Do not print commented script version, date,
                                   and command line call to each outfile.
     --debug              OPTIONAL Debug mode/level.  (e.g. --debug --debug)
     --error-type-limit   OPTIONAL [50] Limit for each type of error/warning.
                                   0 = no limit.  Also see --quiet.
     --dry-run            OPTIONAL Run without generating output files.
     --version            OPTIONAL Print version info and quit.
     --use-as-default     OPTIONAL Save the command line arguments and quit.
     --help               OPTIONAL Print info and format descriptions and quit.
     --advanced-help      OPTIONAL Print advanced help and quit.
end_print

	my @user_defaults = getUserDefaults();
	print("Current user defaults: [@user_defaults].\n");
      }

    return(0);
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
	    error("`-f auto` cannot be used when the input file is supplied ",
		  "on standard input.  Please supply the exact file type.");
	    quit(2);
	  }

	my $num_fastq_defs =
	  `head -n 50 $input_file | grep -c -E '^[\@\+]'`;
	debug("System output from: [",
	      qq(head -n 50 $input_file | grep -c -E '^[\@\+]'),
	      "]:\n$num_fastq_defs");
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

#Only returns 1 for mismatches - no indels. Must be equal length sequences
sub areHam1
  {
    my $seq1 = $_[0];
    my $seq2 = $_[1];
    my $len  = $_[2] ? $_[2] : length($seq1); #INTERNAL - DO NOT SUPPLY

    #Error-check length
    return(0) if(length($seq2) != $len);

    if($len <= 1)
      {return($seq1 ne $seq2)}

    my $halflen = int($len/2);

    #Split the sequences into 2 halves
    my($lseq1,$rseq1) = unpack("A$halflen".'A*',$seq1);
    my($lseq2,$rseq2) = unpack("A$halflen".'A*',$seq2);

    my $eql = ($lseq1 eq $lseq2);
    my $eqr = ($rseq1 eq $rseq2);

    #If only 1 side is different
    if($eql != $eqr)
      {
	if($eql)
	  {return(areHam1($rseq1,$rseq2,$halflen + ($len % 2)))}
	else
	  {return(areHam1($lseq1,$lseq2,$halflen))}
      }
    else
      {return(0)}
  }

#Only returns 1 for mismatches - no indels. Must be equal length sequences
#Assumes sequences are equal length
sub getHam1bases
  {
    my $seq1 = $_[0];
    my $seq2 = $_[1];
    my $len  = $_[2] ? $_[2] : length($seq1); #INTERNAL - DO NOT SUPPLY
    my $pos  = $_[3] ? $_[3] : 1;             #INTERNAL - DO NOT SUPPLY

    #Error-check length
    return(0) if(length($seq2) != $len);

    if($len <= 1)
      {return($seq1 ne $seq2 ? "$pos:$seq1>$seq2" : '')}

    my $halflen = int($len/2);

    #Split the sequences into 2 halves
    my($lseq1,$rseq1) = unpack("A$halflen".'A*',$seq1);
    my($lseq2,$rseq2) = unpack("A$halflen".'A*',$seq2);

    my $eql = ($lseq1 eq $lseq2);
    my $eqr = ($rseq1 eq $rseq2);

    #If only 1 side is different
    if($eql != $eqr)
      {
	if($eql)
	  {return(getHam1bases($rseq1,
			       $rseq2,
			       $halflen + ($len % 2),
			       $halflen + $pos))}
	else
	  {return(getHam1bases($lseq1,
			       $lseq2,
			       $halflen,
			       $pos))}
      }
    else
      {return(0)}
  }

sub printFirstNeighbors
  {
    my $ary      = $_[0]; #Array of 2 member arrays [[defline,sequence],...]
    my $lookup   = $_[1]; #hash of arrays {$sequenceseed=>[neighbor1id,...]}
    my $cnt      = $_[2] ? $_[2] : scalar(@$ary); #Assume cnt is correct
    my $seedsize = $_[3] ? $_[3] : (defined($main::seedsize) ?
				    $main::seedsize : 0);

    for(my $i1 = 0;$i1 < ($cnt - 1);$i1++)
      {
	verboseOverMe("Doing mother sequence [$ary->[$i1]->[0]].");

	#Break up the sequence into seeds for lookup
	my(@seeds);
	my $len = length($ary->[$i1]->[1]);

	if($seedsize)
	  {
	    #Split the sequences into parts
	    @seeds = unpack("(A$seedsize)2",$ary->[$i1]->[1]);
	    if((scalar(@seeds) * $seedsize) > $len && scalar(@seeds) > 1)
	      {
		$seeds[-2] .= $seeds[-1];
		pop(@seeds);
	      }
	  }
	else
	  {
	    #Split the sequences into 2 halves
	    my $halflen = int($len/2);
	    my $upstr = "A$halflen".'A*';
	    (@seeds) = unpack($upstr,$ary->[$i1]->[1]);
	  }

	#Grab all the array indexes where I expect to find similar sequences
	#(guaranteed to get all hamming distance 1 sequences
	my $seen = {};
	my @indexes =
	  grep {my $e = !exists($seen->{$_});$seen->{$_}=1;$e && $_ >= $i1}
	  map {@{$lookup->{$_}}} @seeds;

	#For each possible first neighbor
	foreach my $i2 (@indexes)
	  {
	    my $result = getHam1bases($ary->[$i1]->[1],$ary->[$i2]->[1]);

	    #If the 2 sequences differ by 1 substitution
	    if($result =~ /^(\d+):([A-Za-z])>([A-Za-z])$/)
	      {
		push(@{$ary->[$i1]},$ary->[$i2]->[0],$result);
		push(@{$ary->[$i2]},$ary->[$i1]->[0],"$1:$3>$2");
	      }
	    elsif($result)
	      {error("Unable to parse hamming distance 1 result: [$result].")}
	  }

	print(join("\t",(scalar(@{$ary->[$i1]}) > 2 ?
			 @{$ary->[$i1]}[0,2..$#{$ary->[$i1]}] :
			 $ary->[$i1]->[0])),"\n");
      }

    print(join("\t",(scalar(@{$ary->[-1]}) > 2 ?
		     @{$ary->[-1]}[0,2..$#{$ary->[-1]}] : $ary->[-1]->[0])),
	  "\n");

    verbose("First Neighbors done.");
  }
