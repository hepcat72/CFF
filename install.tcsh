#!/bin/tcsh

#CFF easy-install script
#install.tcsh
#Version: 2.0
#See README for advanced install instructions

#USAGE 1: install.tcsh <USEARCH>
#USAGE 2: install.tcsh <USEARCH> <MUSCLE>
#
#   Where "<USEARCH>" and "<MUSCLE>" can be:
#     1. A download URL of a gzipped tarball or executable
#     2. A tarball or executable
#     3. "skip" or "ignore"
#
#Examples:
#   install.tcsh http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000
#   install.tcsh usearch7.0.1090_i86osx32
#   install.tcsh usearch muscle3.8.31_i86darwin64.tar
#   install.tcsh skip http://www.drive5.com/muscle/downloads3.8.31/muscle3.8.31_i86linux64.tar.gz
#
#usearch and muscle are third-party applications not maintained by CFF.
#The muscle executable is directly downloadable for anyone.
#To get usearch, you must visit this site and agree to the terms:
#
#   http://www.drive5.com/usearch/download.html

setenv MYARGV       "$argv dummy"
setenv USEARCHPARAM `echo "$MYARGV" | cut -f 1 -d " "`
setenv MUSCLEPARAM  `echo "$MYARGV" | cut -f 2 -d " "`
setenv LIBOPT       "-l$HOME/perl5"
setenv ROOTINSTALL  0
setenv ROOTOPT      ''
setenv ROOTNOSKIP   ''
setenv INSTALLLIBS  'App::cpanminus local::lib'
setenv BASHENV      "$HOME/.bashrc"
setenv TCSHENV      "$HOME/.cshrc"

#Make sure that MUSCLEPARAM isn't a duplicate of USEARCHPARAM
if ( "$MUSCLEPARAM" == "dummy" ) then
  setenv MUSCLEPARAM ''
endif

##
## Error-check the number of supplied parameters
##

if ( "$USEARCHPARAM" == '' ) then
  echo 'ERROR 1: The first parameter (a usearch download URL, usearch tarball, usearch executable or the value "skip") is a required parameter.  Please go to http://www.drive5.com and obtain a usearch download URL.  (E.g. ./install http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000)'
  exit 1
endif

##
## Make sure the user is executing this script from the CFF directory
##

setenv CWD      `pwd`
setenv INCFFDIR `perl -e 'if($ARGV[0] =~ m%/CFF[^/]*/?$%){print("1\n")}else{print("0\n")}' $CWD`
if ( ! $INCFFDIR ) then
  echo "ERROR 2: Must execute this script from inside the CFF directory.  (Do not remove 'CFF' from the name of the directory either or you won't be able to run the install script.)"
  exit 2
endif

##
## Prepare for either a system- or local-level install
##

#setenv CPANMEXE ''
#If we are running as root
if ( `id -u` == "0" ) then
  setenv ROOTINSTALL 1
  setenv ROOTOPT '--sudo'
  setenv LIBOPT ''
  setenv ROOTNOSKIP '--no-skip-installed'
  #setenv INSTALL_BASE /usr/local
  which cpanm >& /dev/null
  if ( $status ) then
    #We're installing cpanm temporarily here:
    setenv PATH "$HOME/perl5/bin:$PATH"
    rehash
  endif
  #setenv CPANMEXE `which cpanm | grep -v -i "not found"`
  #echo "PATH3: $PATH"
  echo
  echo '## Installing at system level'
  echo
else
  setenv INSTALLLIBS "$INSTALLLIBS local::lib"
endif

##
## See if we're on Mac OS X so that we can ensure the developer tools are installed
##

which xcode-select >>& logfile

if ( ! $status ) then
  #We're on Mac OS X
  #Now let's see if they have the developer tools installed
  xcode-select -p >>& logfile
  if ( $status ) then
    echo
    echo Developer tools not installed.
    xcode-select --install >>& logfile
    echo A window will appear in a moment to walk you through the XCode installation.
    echo -n 'Hit return when XCode installation is complete: '
    echo $<
    #Now make sure they succeeded with the xcode install
    xcode-select -p >>& logfile
    if ( $status ) then
      echo "ERROR 3: Apple Developer Tools install failed"
      echo "Installation failed"
      exit 3
    endif
  endif
endif

##
## Determine if cpanminus and local::lib (if we're using it) are already installed
##

perl -e 'use App::cpanminus' >& /dev/null
setenv CPANMBAD $status
perl -e 'use local::lib' >& /dev/null
setenv LLIBBAD $status
setenv CPME `which cpanm | grep -v -i "not found"`
if ( $status || "$CPME" == '' ) setenv CPANMBAD 1

if ( $CPANMBAD || $LLIBBAD ) then
  echo
  echo '## Installing cpanminus'
  echo

  echo "curl -L https://cpanmin.us | perl - $ROOTOPT $LIBOPT $INSTALLLIBS"
  curl -L https://cpanmin.us | perl - $ROOTOPT $LIBOPT $INSTALLLIBS

  if ( $status ) then
    curl -L https://git.io/cpanm | perl - $ROOTOPT $LIBOPT $INSTALLLIBS

    if ( $status ) then
      echo "ERROR 4: Install of $INSTALLLIBS encountered an error"
      echo "Installation failed"
      exit 4
    endif
  endif
endif

##
## Set up the environment
##

if ( "$ROOTINSTALL" == "0" ) then
  echo
  echo '## Setting up environment'
  echo
 
  #bash
  echo 'export MANPATH=$MANPATH'":$HOME/perl5/man" >> $BASHENV
  perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=--shelltype,bourne >> $BASHENV
 
  #csh
  echo 'if ! $?MANPATH setenv MANPATH "";' >> $TCSHENV
  echo 'if "${MANPATH}" != "" setenv MANPATH "${MANPATH}:${HOME}/perl5/man";' >> $TCSHENV
  echo 'if "${MANPATH}" == "" setenv MANPATH "${HOME}/perl5/man";' >> $TCSHENV
  perl -I$HOME/perl5/lib/perl5 -Mlocal::lib=--shelltype,csh >> $TCSHENV
 
  #This sets up the environment for this script
  eval `perl -I ~/perl5/lib/perl5 -Mlocal::lib=--shelltype,csh`

  if ( $status ) then
    echo "ERROR 5: local::lib environment setup failed"
    echo "Installation failed"
    exit 5
  endif

  #make sure cpanm is in the PATH
  if ( `which cpanm | grep -v -i "not found"` == "" ) then
    setenv CPANMPATH `perl -I ~/perl5/lib/perl5 -Mlocal::lib=--shelltype,csh | grep INSTALL_BASE | cut -d = -f 2 | cut -d ':' -f 1 | cut -d '"' -f 1 | perl -pne 'chomp;$_ .= "/bin";'`
    if ( "$CPANMPATH" != '' ) then
      setenv PATH "${PATH}:$CPANMPATH"
      rehash
    endif
  endif
else
  #We'll assume that /usr/local is set up properly in their environment

  if ( ! $LLIBBAD ) then
    #This sets up the environment for this script
    eval `perl -I ~/perl5/lib/perl5 -Mlocal::lib=--shelltype,csh,--deactivate-all`

    if ( $status ) then
      echo "ERROR 6: Setting up environment failed"
      echo "Installation failed"
      exit 6
    endif
  endif

  #If local::lib is installed, deactivate it
  setenv CPANMPATH `which cpanm | grep -v -i "not found"`
  if ( "$CPANMPATH" == "" ) then
    #cpanminus, when installed by the user can be detected by root, but root doesn't
    #always have the path to the cpanm executable, so we will attempt to grab it here:
    setenv CPANMPATH `perl -I ~/perl5/lib/perl5 -Mlocal::lib=--shelltype,csh | grep INSTALL_BASE | cut -d = -f 2 | cut -d ':' -f 1 | cut -d '"' -f 1 | perl -pne 'chomp;$_ .= "/bin";'`
    if ( "$CPANMPATH" != '' ) then
      echo "Adding cpanm's PATH [$CPANMPATH] to the PATH"
      setenv PATH "${PATH}:$CPANMPATH"
      rehash
    else
      echo "WARNING: Unable to determine cpanm PATH"
    endif
  else
    echo "DEBUG: cpanm's PATH [$CPANMPATH] is in your PATH [$PATH]. Yay!"
  endif
endif

#Sometimes (maybe just on virtual machines) you have
#to rehash to find new executables in your path
rehash

which cpanm >>& logfile

if ( $status ) then
  #Last ditch effort - maybe it's in /usr/local/bin and the root account doesn't have it in its path for whatever reason
  which /usr/local/bin/cpanm

  if ( ! $status ) then
    setenv PATH /usr/local/bin:$PATH
    rehash
    which cpanm
    if ( $status ) then
      echo "ERROR 7: cpanm not found in PATH: $PATH"
      echo "Installation failed"
      exit 7
    endif
  else
    echo "ERROR 8: cpanm not found in PATH: $PATH"
    echo "Installation failed"
    exit 8
  endif
endif

@ plmoderrs = ( 0 );
setenv MODLIST ""

foreach MODULENAME ( "Getopt::Long" "File::Glob" "IPC::Open3" "IO::Select" "IO::Pipe::Producer" "Sys::Info" "Sys::MemInfo" "File::Which" "Math::Random" )

  setenv MODULEFILE `perl -e '$ARGV[0] =~ s%::%/%g;print("$ARGV[0].pm")' $MODULENAME`

  echo
  echo "## Installing $MODULENAME"
  echo

  if ( "$ROOTINSTALL" == "1" ) then
    setenv ISLOCAL `perl -e 'if(exists($ENV{PERL_LOCAL_LIB_ROOT}) == 0){print("0\n")}my $p = quotemeta($ENV{PERL_LOCAL_LIB_ROOT});print(($INC{$ARGV[0]} =~ /^$p/ ? 0 : 1),"\n")' $MODULEFILE`
    if ( "$ISLOCAL" == "1" ) then
      echo "cpanm $ROOTOPT $ROOTNOSKIP $MODULENAME"
      cpanm $ROOTOPT $ROOTNOSKIP $MODULENAME
    else
      echo "cpanm $ROOTOPT $MODULENAME"
      cpanm $ROOTOPT $MODULENAME
    endif
  else
    echo "cpanm $MODULENAME"
    cpanm $MODULENAME
  endif

  perl -M$MODULENAME -e 'if(exists($INC{$ARGV[0]}) == 0 || $INC{$ARGV[0]} eq ""){exit(1)}' $MODULEFILE

  if ( $status ) then
    echo
    echo "## Error installing $MODULENAME. Trying to force."
    echo
    echo "cpanm $ROOTOPT $ROOTNOSKIP --force $MODULENAME"
    cpanm $ROOTOPT $ROOTNOSKIP --force $MODULENAME
    if ( $status ) then
      echo "Installation incomplete"
      @ plmoderrs = ( ${plmoderrs} + 1 );
      setenv MODLIST "$MODLIST $MODULENAME"
    endif
  else
    echo -n "$MODULENAME is installed in "
    perl -M$MODULENAME -e 'print $INC{$ARGV[0]}' $MODULEFILE
    echo
  endif
end

echo
echo '## Checking installed perl modules'
echo

perl -e 'use Getopt::Long;use File::Glob;use IPC::Open3;use IO::Select;use IO::Pipe::Producer;use Sys::Info;use Sys::MemInfo;use File::Which;use Math::Random'

if ( $status ) then
  #Last ditch effort - source the user's environment file.
  if ( -e $HOME/.cshrc ) then
    source $HOME/.cshrc
  endif
  
  perl -e 'use Getopt::Long;use File::Glob;use IPC::Open3;use IO::Select;use IO::Pipe::Producer;use Sys::Info;use Sys::MemInfo;use File::Which;use Math::Random'
  
  if ( $status ) then
    echo "ERROR 9: Test of dependent perl modules failed."
    echo "         The following "${plmoderrs}" perl modules were not installed."
    echo "         $MODLIST"
    echo "Installation incomplete"
    exit 9
  else
    echo "WARNING: Had to source your $HOME/.cshrc file to use the installed perl modules."
  endif
else
  echo "Perl modules look good."
  echo
endif

##
## Install muscle
##

if ( "$MUSCLEPARAM" != "skip" && "$MUSCLEPARAM" != "ignore" ) then
  echo
  echo '## Installing muscle'
  echo

  ./install_muscle.tcsh "$MUSCLEPARAM"

  if ( $status ) then
    echo 'ERROR 10: muscle installation failed.  Not found in PATH ['"$PATH"'].  Please run the following commands, replacing "<muscle3.8.31_i86darwin64.tar.gz>" and "<muscle3.8.31_i86darwin64>" with the path to your muscle download and executable (without the ".tar/gz" extension)'
    echo
    echo '    tar -xvf <muscle3.8.31_i86darwin64.tar.gz>'
    echo '    chmod 755 <muscle3.8.31_i86darwin64>'
    echo '    mkdir '"$HOME/bin"
    echo '    PATH=$PATH:'"$HOME/bin"
    echo '    mv <muscle3.8.31_i86darwin64> $HOME/bin/'
    echo '    cd $HOME/bin/'
    echo '    ln -s <muscle3.8.31_i86darwin64> muscle'
    echo
    echo "Installation incomplete"
    exit 10
  endif

  echo
  echo '## Checking muscle installation'
  echo

  #Sometimes (maybe just on virtual machines) you have to rehash to find new executables in your path
  rehash

  which muscle >>& logfile

  if ( $status && "$ROOTINSTALL" == "0" ) then
    #Assume that the executable is in the current directory
    setenv PATH "${PATH}:"`pwd`
    rehash
  endif

  which muscle

  if ( $status ) then
    echo 'ERROR 11: muscle check failed.  Not found in PATH ['"$PATH"'].  Please run the following commands, replacing "<muscle3.8.31_i86darwin64.tar.gz>" and "<muscle3.8.31_i86darwin64>" with the path to your muscle download and executable (without the ".tar/gz" extension)'
    which muscle >>& logfile
    echo "PATH: $PATH" >>& logfile
    ls /usr/local/bin >>& logfile
    echo
    echo '    tar -xvf <muscle3.8.31_i86darwin64.tar.gz>'
    echo '    chmod 755 <muscle3.8.31_i86darwin64>'
    echo '    mkdir '"$HOME/bin"
    echo '    PATH=$PATH:'"$HOME/bin"
    echo '    mv <muscle3.8.31_i86darwin64> $HOME/bin/'
    echo '    cd $HOME/bin/'
    echo '    ln -s <muscle3.8.31_i86darwin64> muscle'
    echo
    echo "Installation incomplete"
    exit 11
  endif
else
  echo 'Skipping muscle config/install.'
endif

##
## Install usearch
##

if ( "$USEARCHPARAM" != "skip" && "$USEARCHPARAM" != "ignore" ) then
  echo
  echo '## Installing usearch'
  echo

  ./install_usearch.tcsh "$USEARCHPARAM"

  if ( $status ) then
    echo 'ERROR 12: usearch installation failed.  Not found in PATH ['"$PATH"'].  Please run the following commands, replacing "<usearch7.0.1090_i86osx32>" with the path to your usearch executable'
    echo
    echo '    chmod 755 <usearch7.0.1090_i86osx32>'
    echo '    mkdir '"$HOME/bin"
    echo '    PATH=$PATH:'"$HOME/bin"
    echo '    mv <usearch7.0.1090_i86osx32> $HOME/bin/'
    echo '    cd $HOME/bin/'
    echo '    ln -s <usearch7.0.1090_i86osx32> muscle'
    echo
    echo "Installation incomplete"
    exit 12
  endif

  echo
  echo '## Checking usearch installation'
  echo

  #Sometimes (maybe just on virtual machines) you have to rehash to find new executables in your path
  rehash

  which usearch >>& logfile

  if ( $status && "$ROOTINSTALL" == "0" ) then
    #Assume that the executable is in the current directory
    setenv PATH "${PATH}:"`pwd`
    rehash
  endif

  which usearch

  if ( $status ) then
    echo 'ERROR 13: usearch check failed.  Not found in PATH ['"$PATH"'].  Please run the following commands, replacing "<usearch7.0.1090_i86osx32>" with the path to your usearch executable'
    echo
    echo '    chmod 755 <usearch7.0.1090_i86osx32>'
    echo '    mkdir '"$HOME/bin"
    echo '    PATH=$PATH:'"$HOME/bin"
    echo '    mv <usearch7.0.1090_i86osx32> $HOME/bin/'
    echo '    cd $HOME/bin/'
    echo '    ln -s <usearch7.0.1090_i86osx32> usearch'
    echo
    echo "Installation incomplete"
    exit 13
  endif
else
  echo 'Skipping usearch config/install.'
endif

##
## Install CFF
##

echo
echo '## Configuring CFF'
echo

perl Makefile.PL

if ( $status ) then
  echo "ERROR 14: `perl Makefile.PL` failed."
  echo "Installation incomplete"
  exit 14
endif

echo
echo '## Compiling CFF'
echo

make

if ( $status ) then
  echo "ERROR 15: `make` failed."
  echo "Installation incomplete"
  exit 15
endif

echo
echo '## Installing CFF'
echo

make install

if ( $status ) then
  echo "ERROR 16: 'make install' failed."
  echo "Installation incomplete"
  exit 16
endif

##
## Test the installation
##

echo
echo '## Testing CFF installation'
echo

#Run the tests in the test directory
cd test
./run_test.tcsh
setenv FAILURE $status
cd ..

if ( $FAILURE ) then
  echo "ERROR 17: CFF test failed."
  echo "Installation incomplete"

  exit 17
else
  ##
  ## Clean up
  ##

  #Clean up files owned by root from the user's directory
  if ( "$ROOTINSTALL" == "1" ) then
    make clean >& /dev/null
    \rm -f Makefile.old muscle* usearch* logfile

    setenv AFILE "$HOME/perl5"
    setenv OWNER `ls -ld $AFILE | awk '{print $3}'`
    if ( "$OWNER" == "root" ) then
      \rmdir $AFILE >& /dev/null
    endif

    setenv AFILE "$HOME/.cpanm"
    setenv OWNER `ls -ld $AFILE | awk '{print $3}'`
    if ( "$OWNER" == "root" ) then
      \rm -rf $AFILE
    endif

    #This should actually be fixed in the run_test script
    setenv AFILE "samples/run_all_qiime_tax_commands.sh"
    setenv OWNER `ls -ld $AFILE | awk '{print $3}'`
    if ( "$OWNER" == "root" ) then
      \rm -f $AFILE
    endif
  endif

  echo
  echo "## CFF installation successful"
  echo
  echo "To start using CFF, please open a new terminal window or run:"
  echo "  for bash: '. $BASHENV'"
  echo "  for tcsh: 'source $TCSHENV'"

  exit 0
endif
