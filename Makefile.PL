require 5.000;
use ExtUtils::MakeMaker;
my @scripts = qw(neighbors.pl errorRates.pl nZeros.pl);
WriteMakefile
  (
   NAME => 'hamming1',
   ($[ >= 5.005) ?
   (AUTHOR   => 'Robert W. Leach (rleach@genomics.princeton.edu)',
    ABSTRACT => 'Determine which sequences are real in a metagenomic sample.') : (),
   VERSION   => "1.0",
   PREREQ_PM => { 'Getopt::Long' => 2.38 },
   EXE_FILES => [ map { "src/$_" } @scripts ]
  );
