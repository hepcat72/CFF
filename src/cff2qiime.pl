#!/usr/bin/perl
#Generated using perl_script_template.pl

#USAGE: Run with no options to get usage or with --extended for more details

my $software_version_number = '1.8';                   #Global
my $created_on_date         = '8/4/2014';              #Global

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

#Global variables used in main
my $outfile_suffix    = '_otus.txt';
my $rep_set_suffix    = '_rep_set.fna';
my $qiime_scpt_suffix = '_qiime_tax_commands.sh';
my $runall_scpt       = 'run_all_qiime_tax_commands.sh';
my $input_files       = [];
my $smry_files        = [];
my $outdirs           = [];
my $filetype          = 'auto';
my $seq_id_pattern    = '^\s*[>\@]\s*([^;]+)';
my $abundance_pattern = 'size=(\d+);';

#Command line parameters
my $GetOptHash =
  {'i|input-file|stdin-stub|stub|reals-file=s'
			  => sub {push(@$input_files,    #REQUIRED unless <> is
				       [sglob($_[1])])}, #         supplied
   '<>'                   => sub {checkFileOpt($_[0],1);
				  push(@$input_files,    #REQUIRED unless -i is
				       [sglob($_[0])])}, #         supplied
   's|summary-file=s'	  => sub {push(@$smry_files,     #OPTIONAL [none]
				       [sglob($_[1])])},
   't|filetype=s'         => \$filetype,                 #OPTIONAL [auto]
				                         #   (fasta,fastq,auto)
   'o|outfile-suffix|otus-suffix=s'
                          => \$outfile_suffix,           #OPTIONAL [_otus.txt]
   'r|rep-set-suffix=s'   => \$rep_set_suffix,           #OPTIONAL
                                                         #       [_rep_set.fna]
   'tax-script-suffix=s'  => \$qiime_scpt_suffix,        #OPTIONAL [_qiime_tax_
                                                         #         commands.sh]
   'run-all-script=s'     => \$runall_scpt,              #OPTIONAL [run_all_qii
                                                         #  me_tax_commands.sh]
   'outdir=s'             => sub {push(@$outdirs,        #OPTIONAL [none]
				       [sglob($_[1])])},
   'q|seq-id-pattern=s'   => \$seq_id_pattern,           #OPTIONAL
                                                         #[^\s*[>\@]\s*([^;]+)]
   'p|abundance-pattern=s'=> \$abundance_pattern,        #OPTIONAL
                                                         #        [size=(\d+);]
   'overwrite!'           => \$overwrite,                #OPTIONAL [Off]
   'skip-existing!'       => \$skip_existing,            #OPTIONAL [Off]
   'force:+'              => \$force,                    #OPTIONAL [Off]
   'verbose:+'            => \$verbose,                  #OPTIONAL [Off]
   'quiet'                => \$quiet,                    #OPTIONAL [Off]
   'debug:+'              => \$DEBUG,                    #OPTIONAL [Off]
   'help'                 => \$help,                     #OPTIONAL [Off]
   'extended'             => \$extended,                 #OPTIONAL [Off]
   'version'              => \$version,                  #OPTIONAL [Off]
   'header!'              => \$header,                   #OPTIONAL [Off]
   'error-type-limit=i'   => \$error_limit,              #OPTIONAL [5]
   'dry-run'              => \$dry_run,                  #OPTIONAL [Off]
   'save-as-default'      => \$use_as_default,           #OPTIONAL [Off]
   'aggregate-mode!'      => \$aggregate_mode,           #OPTIONAL [On]
   'join-conflicts!'      => \$compound_mode,            #OPTIONAL [Off]
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
processDefaultOptions(defined($outfile_suffix));

if($seq_id_pattern ne '' &&
   $seq_id_pattern !~ /(?<!\\)\((?!\?[adluimsx\-\^]*:)/)
  {$seq_id_pattern = '(' . $seq_id_pattern . ')'}

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
    quit(1);
  }

#Require input file(s)
if(scalar(@$input_files) == 0 && isStandardInputFromTerminal())
  {
    error('No input detected.');
    usage(1);
    quit(-7);
  }

#Require the same number of filtered(/input/reals) files as summary files (if
#supplied)
if(#Summary files were provided
   (scalar(@$smry_files) > 1 ||
    (scalar(@$smry_files) == 1 && scalar(@{$smry_files->[0]}))) &&
   #its array dimensions are not the same as the input_files array
   (scalar(@$smry_files) != scalar(@$input_files) ||
    scalar(grep {scalar(@{$input_files->[$_]}) != scalar(@{$smry_files->[$_]})}
	   (0..$#{$input_files}))))
  {
    error("The files supplied to -s must be equal in number to the files ",
	  "supplied to -i.");
    quit(3);
  }

#Require an outfile suffix if an outdir has been supplied
if(scalar(@$outdirs) && !defined($outfile_suffix))
  {
    error("An outfile suffix (-o) is required if an output directory ",
	  "(--outdir) is supplied.  Note, you may supply an empty string ",
	  "to name the output files the same as the input files.");
    quit(-8);
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
   $output_file_sets) = getFileSets([$input_files,
				     $smry_files],

				    [[$outfile_suffix,
				      $rep_set_suffix,
				      $qiime_scpt_suffix]],

				    $outdirs);

#Create the output directories
mkdirs(@$outdirs);

my($input_file);

#If a runall script is being generated
if(defined($runall_scpt) && $runall_scpt ne '')
  {openOut(*ALL,$runall_scpt) || quit(5)}

#For each set of corresponding input files
foreach my $set_num (0..$#$input_file_sets)
  {
    $input_file   = $input_file_sets->[$set_num]->[0];
    my $smry_file = $input_file_sets->[$set_num]->[1];
    my($output_file,$rep_set_file,$qiime_scpt) =
      @{$output_file_sets->[$set_num]->[0]};

    if(defined($smry_file))
      {openIn(*SMRY,  $smry_file)      || next}
    openIn(   *INPUT, $input_file)     || next;
    openOut(  *OTUS,  $output_file)    || next;
    openOut(  *REPSET,$rep_set_file,0) || next;
    openOut(  *SCPT,  $qiime_scpt,0)   || next;

    next if($dry_run);

    my $cnt          = 0;
    my $verbose_freq = 100;
    my $filt_hash    = {};
    my $length       = 0;
    my $length_errs  = 0;

    #For each line in the current input file
    while(my $rec = getNextSeqRec(*INPUT,0,$input_file))
      {
	$cnt++;
	verboseOverMe("[$input_file] Reading record: [$cnt].")
	  unless($cnt % $verbose_freq);

	my $id    = getID($rec);
	my $abund = getAbund($rec);
	my $def   = getDef($rec);
	my $seq   = getSeq($rec);
	my $len   = getSize($rec);
	if($length == 0)
	  {$length = $len}
	elsif($len && $length != $len)
	  {
	    $length = $len if($len < $length);
	    $length_errs++;
	  }

	if(!defined($id) || $id !~ /\S/ || !$abund)
	  {
	    error("Invalid record.  Skipping.");
	    next;
	  }

	#Save the filtered/real IDs for generating a sample-level otus file
	$filt_hash->{$id} = $abund;

	#Create the new defline for the rep_set file.
	$def =~ s/^[\@>]//;
	my $id_pattern = quotemeta($id) . '[; ]*';
	$def =~ s/$id_pattern//;
	$def =~ s/^\s+//;
	$def = ">$id" . ($def =~ /\S/ ? " $def" : '');
	chomp($def);
	chomp($seq);

	print OTUS   ($id,("\t1" x $abund),"\n")
	  if(!defined($smry_file));
	print REPSET ("$def\n$seq\n");
      }

    if($cnt == 0)
      {
	warning("No records could be parsed from sequence file: ",
		"[$input_file].  Skipping it and summary file: [$smry_file].");
	next;
      }

    if(defined($smry_file))
      {
	my @sample_ids   = ();
	my $may_shift    = 1;
	my $missing_hash = {};
	my $col_anoms    = {};
	my $a_success    = 0;
	$cnt             = 0;

	while(getLine(*SMRY))
	  {
	    $cnt++;
	    verboseOverMe("[$smry_file] Reading line: [$cnt].")
	      unless($cnt % $verbose_freq);

	    chomp;

	    if(/^#/ && /\t/)
	      {
		if(scalar(@sample_ids))
		  {warning("Multiple column header lines detected.  Using ",
			   "most recent.")}

		@sample_ids = split(/ *\t */,$_,-1);
		$may_shift  = 1;
	      }
	    elsif(/\S/)
	      {
		s/^ +//;
		s/ +$//;
		my @abunds = split(/ *\t */,$_,-1);
		my $id = shift(@abunds);
		my @bad_abunds = grep {/\D/} @abunds;
		#Error-check the abundance values
		if(scalar(grep {/\D/} @abunds))
		  {
		    my @report_sum = (scalar(@bad_abunds) > 10 ?
				      (@bad_abunds[0..8],'...') : @bad_abunds);
		    warning("Bad abundance values found in the summary file ",
			    "[$smry_file]: [",join(',',@report_sum),
			    "].  Skipping line [$cnt].  Use --force to ",
			    "include this line.");
		    next unless($force);
		  }
		#Error-check the sequence ID
		if(!exists($filt_hash->{$id}))
		  {
		    $missing_hash->{$id} = 1;
		    next unless($force);
		  }
		#Error-check the number of abundance values
		if(scalar(@abunds) != scalar(@sample_ids))
		  {
		    if(scalar(@sample_ids) == 0)
		      {
			@sample_ids = (1..scalar(@abunds));
			$may_shift = 0;
		      }
		    elsif($may_shift &&
			  (scalar(@sample_ids) - 1) == scalar(@abunds))
		      {
			shift(@sample_ids);
			$may_shift = 0;
		      }
		    else
		      {
			$col_anoms->{$id} = scalar(@abunds);
			next unless($force);
		      }
		  }

		#Everything checks out, so let's print
		print($id,(map {my $sid = (scalar(@sample_ids) > $_ ?
					   $sample_ids[$_] : ($_ + 1));
				"\t$sid" x $abunds[$_]}
			   (0..$#abunds)),"\n");

		#Track successes to make error reporting more succinct
		$a_success++;
	      }
	  }

	if($a_success == 0)
	  {
	    error("There were problems with your summary file: [$smry_file].",
		  (scalar(keys(%$missing_hash)) ?
		   ("  The IDs did not appear to match your sequence file: ",
		    "[$input_file].") : ''),
		  (scalar(keys(%$col_anoms)) ?
		   ("  The number of columns on each line (or the optional ",
		    "column headers) were inconsistent.") : ''));
	  }
	else
	  {
	    if(scalar(keys(%$col_anoms)))
	      {
		my @report_anoms = (scalar(keys(%$col_anoms)) > 10 ?
				    ((keys(%$col_anoms))[0..8],'...') :
				    keys(%$col_anoms));
		warning("These OTU IDs found in the summary file ",
			"[$smry_file] didn't have a consistent number of ",
			"columns with the rest of the file and have been ",
			"skipped: [",join(',',@report_anoms),
			"].  Use --force to include these IDs.");
	      }
	  }

	closeIn(*SMRY);
      }

    #Now print a script to generate the biom file using qiime commands
    if(defined($qiime_scpt))
      {
	if($length_errs)
	  {
	    $length = 50 unless($length);
	    error("Length inconsistencies found in sequence file ",
		  "[$input_file].  Using [$length] for qiime shell script ",
		  "[$qiime_scpt].");
	  }

	my $script =
	  getQiimeScript($output_file,$rep_set_file,$length);
	print SCPT ($script);
	print ALL  ("bash $qiime_scpt\n")
	  if(defined($runall_scpt) && $runall_scpt ne '');
      }

    closeOut(*REPSET,$rep_set_file);
    closeOut(*OTUS,  $output_file);
    closeOut(*SCPT,  $qiime_scpt);
    chmod(0755,$qiime_scpt) if(defined($qiime_scpt) && $qiime_scpt ne '');
    closeIn(*INPUT);
  }

if(defined($runall_scpt) && $runall_scpt ne '')
  {
    closeOut(*ALL,$runall_scpt);
    chmod(0755,$runall_scpt);
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
    my $command_line_string = $_[0];
    unless(defined($command_line_string))
      {
	warning("Undefined command line string encountered.");
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
	      $real_file_found = 1;
	      @x;
	    }
	  else
	    {$v}
	} @partials;

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
	    @errors = map {ref($_) eq '' ? ref(\$_) : ref($_)}
	      grep {ref($_) ne 'ARRAY'}
	      @$file_types_array;
	    error("Expected an array of arrays for the first argument, but ",
		  "found a [",join(',',@errors),"] inside the outer array.");
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
          "defined and is of type [",ref($output_file),"].");

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

	$main::open_handles->{*STDOUT} = 'STDOUT';
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
	$main::open_handles->{$file_handle} = $output_file;

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

	verbose("[$main::open_handles->{$file_handle}] Output file done.  ",
		"Time taken: [",scalar(markTime()),' Seconds].');

	delete($main::open_handles->{$file_handle});
      }

    return($status);
  }

#Globals used: $main::open_handles, $force
sub openIn
  {
    my $file_handle = $_[0];
    my $input_file  = $_[1];
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
		    '] Opened input file.');

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

    verbose('[',($main::open_handles->{$file_handle} eq '-' ?
		 $default_stub : $main::open_handles->{$file_handle}),
	    '] Input file done.  Time taken: [',scalar(markTime()),
	    ' Seconds].');

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
		{checkFile($outfile,undef,1,0) || $exist{$outfile}++}}}

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
	       'the  terminal.  Verbose messages may interfere with ',
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

* WHAT IS THIS: This script takes 2 input files: (-i) a file of sequences
                containing abundance values on the deflines and the summary
                file (-s) which is a tab-delimited file that contains a column
                of abundances for each sample.  The files are converted it into
                files that can be used in the qiime metagenomics pipeline,
                version 1.3.0 (see qiime.org) and also a short shell script of
                qiime commands to generate a phylogeny and a biom file with
                taxonomic information.  An accompanying wrapper script to run
                all output scripts with a single command is also generated
                (--run-all-script).

                This script is a part of a package called 'CFF' (cluster free
                filtering).  Please refer to the README for general information
                about the package.

* INPUT FORMAT: Fasta or fastq files containing abundance values in the
                deflines.  It is recommended to use this with either the
                sequence output of the getReals.pl script or the
                filterIndels.pl script.  The file is used mainly to generate
                the qiime rep_set file, but is also used to filter the content
                of the OTUs file and to obtain the sequence length in order to
                supply it to qiime's align_seqs.py command.  By default, after
                the defline character, the first string on the defline is
                assumed to be the sequence ID followed by a semicolon.  -q can
                be used to change the ID pattern match.  The default format for
                abundance values is "size=#;" where "#" is an integer
                representing the abundance value.  Other formats are acceptable
                as long as you can use -p to extract it.  Example default fasta
                format:

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

                Deflines may contain other information and it will be retained
                in the output.

* OTU OUTPUT FORMAT: This output file, which matches the format output by
                     pick_otus.py in qiime, can be used as input for qiime's
                     make_otu_table.py script with the -i option.  It is a tab-
                     delimited file containing the sequence ID as the first
                     column (interpretted by qiime as the OTU ID) and a series
                     of columns with either repeating sample IDs indicating
                     per-sample abundance (if a summary file is provided) or a
                     series of 1s (placeholders simply to convey global
                     abundance across all samples).  The number of occurrences
                     of each sample-ID/"1" implies the abundance of the
                     sequence in that sample.  Qiime's make_otu_table.py script
                     will simply count the occurrences to get the abundance.

                     Example with a summary file (-s):

lib_1   sample1 sample1 sample1 sample2 sample2 sample3...
lib_2   sample2 sample2 sample4 sample4 sample5 sample6...
lib_2   sample6 sample6 sample6 sample6 sample6 sample9...
...

                     Example without a summary file:

lib_1   1       1       1       1       1       1...
lib_2   1       1       1       1       1       1...
lib_3   1       1       1       1       1       1...
...

* REP SET OUTPUT FORMAT: This is a fasta format file similar to the input file
                         except the sequence ID is forced to be at the
                         beginning of the defline followed by a space (if there
                         is anything other than the ID on the defile.  This
                         script does not change the number of sequences in your
                         file.  The file format is the same as is produced by
                         pick_rep_set.py in qiime.  It can be used as input for
                         qiime's align_seqs.py and assign_taxonomy.py scripts
                         with the -i option.

>lib_3 size=23629;
TACGTATGTCACAAGCGTTATCCGGATTTATTGGGCGTAAAGCGCGTCTAGGTGGTTATGTAAGTCTGATGTGAAAATGCAGGGCTCAACTCTGTATTGCGTTGGAAACTGCATGACTAGAGTACTGGAG
>lib_5 size=8934;
TACGTAGGTCCCGAGCGTTGTCCGGATTTATTGGGCGTAAAGCGAGCGCAGGCGGTTAGATAAGTCTGAAGTTAAAGGCTGTGGCTTAACCATAGTACGCTTTGGAAACTGTTTAACTTGAGTGCAAGAG
>lib_6 size=5991;
TACGTAGGTGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGATCAGTCAGTCTGTCTTAAAAGTTCGGGGCTTAACCCCGTGATGGGATGGAAACTGCTGATCTAGAGTATCGGAG
>lib_7 size=4621;
TACGTAGGTGGCAAGCGTTGTCCGGAATTATTGGGCGTAAAGCGCGCGCAGGCGGATCAGTTAGTCTGTCTTAAAAGTTCGGGGCTTAACCCCGTGATGGGATGGAAACTGCTGATCTAGAGTATCGGAG
>lib_8 size=3460;
TACGGAAGGTCCAGGCGTTATCCGGATTTATTGGGTTTAAAGGGAGCGTAGGCGGATTGTTAAGTCAGCGGTTAAAGGGTGTGGCTCAACCATACATTGCCGTTGAAACTGGCGATCTTGAGTGCAGACA

* QIIME SHELL SCRIPT: Each output shell script (which you may edit to fine-tune
                      the parameters) is a bash script which basically contains
                      these qiime commands:

                      assign_taxonomy.py -i REP_SET -o taxa
                      align_seqs.py -i REP_SET -o aln
                      filter_alignment.py -i aln/REP_SET_aligned.ext -o filt
                      make_phylogeny.py -i filt/REP_SET_aligned_pfiltered.ext -o phy
                      make_otu_table.py -i OTUS -o otu_file -e aln/*_failures.fna -t taxa/*_tax_assignments.txt

                      Alternatively, you could just do these two steps:

                      assign_taxonomy.py -i REP_SET -o taxa
                      make_otu_table.py -i OTUS -o otu_file -t taxa/*_tax_assignments.txt

                      Or this one step:

                      make_otu_table.py -i OTUS -o otu_file

                      A shell script is output for each input sequence
                      (/summary) file (pair).  Also a single --run-all-script
                      is generated in order to run all output scripts with a
                      single command.  This script will be generated by default
                      as "run_all_qiime_tax_commands.sh".  The file name you
                      supply to --run-all-script will be the command to execute
                      the qiime scripts.  It must be run in the directory you
                      ran cff2qiime.pl in.  You can run it, for example, like
                      this:

                      bash run_all_qiime_tax_commands.sh

end_print

    if($advanced)
      {
	my $header          = getHeader(0);
	my $extended_header = getHeader(1);

	print << "end_print";
* HEADER FORMAT: Unless --noheader is supplied or STANDARD output is going to
                 the terminal (and not redirected into a file), every output
                 file, including output to standard out, will get a header that
                 is commented using the '#' character (i.e. each line of the
                 header will begin with '#').  The format of the standard
                 header looks like this:

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

    print("\n$script -i \"input file(s)\" [-o .out] $options\n",
	  (!$local_extended ? '' :
	   "$script -i \"outfile_stub\" [-o .out] $options < input_file\n"),
	  "\n");

    if($error_mode)
      {print("Run with no options for usage.\n")}
    else
      {
	if(!$local_extended)
	  {
	    print << 'end_print';
     -i                   REQUIRED Input sequence file(s).
     -s                   OPTIONAL [none] Input summary file (see getReals.pl).
     -o                   OPTIONAL [_otus.txt] Outfile extension for otu file.
     -r                   OPTIONAL [_rep_set.fna] Outfile extension for
                                   representative sequences file.
     --run-all-script     OPTIONAL [run_all_qiime_tax_commands.sh] Output shell
                                   qiime script to generate phylogeny and biom
                                   file with taxonomic information (saved in
                                   current directory).
     --outdir             OPTIONAL [none] Output directory.  Requires -o.
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
     -i,--input-file,     REQUIRED Input sequence file(s) (recommended: as
     --stdin-stub,--stub*          output by either filterIndels.pl or
                                   getReals.pl).  Used to generate qiime
                                   rep_set file and to filter the qiime otus
                                   file, and to determine sequence length for
                                   running qiime's align_seqs.py (see
                                   --tax-script-suffix).  Space separated,
                                   globs OK (e.g. -i
                                   "*.text [A-Z].{?,??}.txt").  When standard
                                   input detected, -o has been supplied, and -i
                                   is given only 1 argument, it will be used as
                                   a file name stub for combining with -o to
                                   create the outfile name.  See --extended
                                   --help for file format and advanced usage
                                   examples.  *No flag required.
     -t|--filetype        OPTIONAL [auto](fasta,fastq,auto) Input file (-i)
                                   type.  Using this instead of auto will make
                                   file reading faster.  "auto" cannot be used
                                   when redirecting a file in.
     -s|--summary-file    OPTIONAL [none] Input tab-delimited summary file as
                                   output by getReals.pl (-s).  This file is
                                   used to add sample-level columns to the
                                   output otus file (see -o).  Without this
                                   file, there will be only global abundance
                                   information in the resulting otus file.
     --tax-script-suffix  OPTIONAL [_qiime_tax_commands.sh] Each input
                                   (/summary) file (pair) generates a shell
                                   script containing qiime commands to generate
                                   a phylogeny and biom file with taxonomic
                                   information.  An accomanying master script
                                   is also output (see --run-all-script).  You
                                   must have qiime installed in order for this
                                   generated script to run.  See --help for
                                   more information.
     --run-all-script     OPTIONAL [run_all_qiime_tax_commands.sh] Supply the
                                   path/name of the master qiime-command shell
                                   script with this option.  This output shell
                                   script can be used to run all the
                                   --tax-script-suffix generated shell scripts,
                                   so that running multiple files in batch can
                                   be made easier.  You must have qiime
                                   installed in order for this generated script
                                   to run.  Note that this script will be saved
                                   in the current directory, not any supplied
                                   --outdir.  See --help for more information.
     -o,--outfile-suffix, OPTIONAL [_otus.txt] Outfile extension appended to
     --otus-suffix                 -i to create the otus file (as is produced
                                   by pick_otus.py in qiime).  Output can be
                                   used as input for qiime's make_otu_table.py
                                   script with the -i option.  Will not
                                   overwrite without --overwrite.   Supplying
                                   an empty string will effectively treat the
                                   input file name (-i) as a stub (may be used
                                   with --outdir as well).  When standard input
                                   is detected and no stub is provided via -i,
                                   appends to the string "STDIN".  Does not
                                   replace existing input file extensions.  See
                                   --extended --help for output file format and
                                   advanced usage examples.
     -r,--rep-set-suffix  OPTIONAL [_rep_set.fna] Outfile extension appended to
                                   -i to create the representative set file (as
                                   is produced by pick_rep_set.py in qiime).
                                   Output can be used as input for qiime's
                                   align_seqs.py and assign_taxonomy.py scripts
                                   with the -i option.  Will not overwrite
                                   without --overwrite.   Supplying an empty
                                   string will effectively treat the input file
                                   name (-i) as a stub (may be used with
                                   --outdir as well).  When standard input is
                                   detected and no stub is provided via -i,
                                   appends to the string "STDIN".  Does not
                                   replace existing input file extensions.  See
                                   --extended --help for output file format and
                                   advanced usage examples.
     --outdir             OPTIONAL [none] Directory to put output files.  This
                                   option requires -o.  Default output
                                   directory is the same as that containing
                                   each input file.  Relative paths will be
                                   relative to each individual input file.
                                   Creates directories specified, but not
                                   recursively.  Also see --extended --help for
                                   advanced usage examples.
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
     --header,--noheader  OPTIONAL [Off] Print commented script version, date,
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

#Copied from errorRates.pl on 8/4/2014 so as to be independent -Rob
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

sub getDef
  {return($_[0]->[0])}

sub getSeq
  {return($_[0]->[1])}

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

sub getQiimeScript
  {
    my $output_file  = $_[0];
    my $rep_set_file = $_[1];
    my $length       = $_[2];
    my $biom_file    = "$output_file.with_taxa.biom";

    if(!defined($biom_file) || $biom_file eq '')
      {$biom_file = $rep_set_file}

    my $scpt_str = <<"end_var";
#!/bin/bash

#USAGE: Run this script from the directory in which the cff2qiime.pl script was run

if [ ! -a $rep_set_file ]; then
    if [ "\$PWD" != "$ENV{PWD}" ]; then
        echo "ERROR: You must run this script from [$ENV{PWD}] (the directory in which cff2qiime.pl was run) or edit this script to correct the locations of the files."
        exit 1;
    fi
fi

#File locations/names (Edit here [and delete the if..fi code above] if running from a different location)
REPSFL=$rep_set_file
OTUSFL=$output_file
TAXDIR=$rep_set_file.taxa-dir
ALNDIR=$rep_set_file.aligned-dir
FLTDIR=$rep_set_file.filtered-dir
TREEFL=$rep_set_file.tre
BIOMFL=$biom_file
LENGTH=$length

echo "Starting \$REPSFL, \$OTUSFL, \$BIOMFL"

mkdir "\$TAXDIR"
mkdir "\$ALNDIR"
mkdir "\$FLTDIR"

echo "Assigning taxonomy..."
assign_taxonomy.py -i "\$REPSFL" -o "\$TAXDIR"

echo "Aligning..."
align_seqs.py -i "\$REPSFL" -o "\$ALNDIR" -e "\$LENGTH"

echo "Filtering..."
filter_alignment.py -i "\$ALNDIR"/*_aligned* -o "\$FLTDIR"

echo "Making phylogeny..."
make_phylogeny.py -i "\$FLTDIR"/*_aligned_pfiltered* -o "\$TREEFL"

echo "Making biom file..."
make_otu_table.py -i "\$OTUSFL" -o "\$BIOMFL" -e "\$ALNDIR"/*_failures.fasta -t "\$TAXDIR"/*_tax_assignments.txt

echo
echo "Output files:"
echo "============="
echo "\$TAXDIR/*"
echo "\$ALNDIR/*"
echo "\$FLTDIR/*"
echo "\$TREEFL"
echo "\$BIOMFL"
echo
echo DONE
end_var

    return($scpt_str);
  }
