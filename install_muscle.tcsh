#!/bin/tcsh

#This script attempts to install muscle based on the user's system.
#USAGE 1: install_muscle.tcsh
#USAGE 2: install_muscle.tcsh ~/bin/muscle3.8.31_i86linux32         (example)
#USAGE 3: install_muscle.tcsh http://www.drive5.com/muscle/downloads3.8.31/muscle3.8.31_i86linux32.tar.gz

setenv CWD         `pwd`
setenv BASEURL     'http://www.drive5.com/muscle/downloads3.8.31/'
setenv VAR         `echo "$argv" | cut -f 1 -d " "`
setenv ROOTINSTALL 0
setenv TYPE        `perl -e 'if(shift =~ /http|\.com|www|drive5/i){print "URL\n"}else{print "EXE\n"}' "$VAR"`
setenv URL         ''
setenv NAME        ''
setenv DOWNLD      ''
setenv EXE         ''

##
## If we are root, prepare for a system-level installation
##

if ( `id -u` == "0" ) then
  setenv ROOTINSTALL 1

  echo
  echo '## Performing a system level install of muscle'
  echo

  if ( ! -e /usr/local ) then
    mkdir /usr/local
    if ( $status ) then
      echo "ERROR 1: Unable to create /usr/local directory."
      echo "Installation incomplete"
      exit 1
    endif
  endif
  if ( ! -e /usr/local/bin ) then
    mkdir /usr/local/bin
    if ( $status ) then
      echo "ERROR 2: Unable to create /usr/local/bin directory."
      echo "Installation incomplete"
      exit 2
    endif
  endif
endif

##
## See if install is already complete
##

cd ..
which muscle >& /dev/null
if ( ! $status ) then
  if ( "$ROOTINSTALL" == "1" ) then
    setenv INUSRLOCAL `which muscle | perl -pne 'm%^/usr/local/bin/% ? 1 : 0'`
    if ( "$INUSRLOCAL" == "1" ) then
      echo "muscle is already installed in /usr/local/bin/"
      muscle -version
      cd $CWD
      exit 0
    endif
  else
    echo "muscle is already installed and in your path [PATH = $PATH]"
    echo
    which muscle
    muscle -version
    cd $CWD
    exit 0
  endif
endif
cd $CWD

if ( "$VAR" == '' && -e muscle ) then
  setenv NAME 'muscle'
  setenv EXE 'muscle'
endif

##
## Set the URL/EXE/NAME/DOWNLD if applicable
##

if ( "$VAR" == '' || $TYPE == 'URL' ) then
  if ( "$VAR" != '' ) then
    setenv URL $VAR
  else if ( "$VAR" == '' ) then
    setenv BITS `uname -m | perl -ne 'print(/64/ ? "64\n" : "32\n")'`
    setenv OS `uname`
    if ( $OS == "Darwin" ) then
      if ( $BITS == "64" ) then
        setenv URL ${BASEURL}muscle3.8.31_i86darwin64.tar.gz
      else
        setenv URL ${BASEURL}muscle3.8.31_i86darwin32.tar.gz
      endif
    else if ( $OS == "Linux" ) then
      if ( $BITS == "64" ) then
        setenv URL ${BASEURL}muscle3.8.31_i86linux64.tar.gz
      else
        setenv URL ${BASEURL}muscle3.8.31_i86linux32.tar.gz
      endif
    else
      echo
      echo "ERROR 4: Unable to determine compatible OS type.  Note, only Mac & Linux are supported."
      echo "Please run with the muscle download URL for your system."
      echo "(e.g. install_muscle.tcsh ${BASEURL}muscle3.8.31_i86linux32.tar.gz)"
      echo
      exit 4
    endif
  endif

  setenv NAME `perl -e 'print(map {s%.*/%%;$_} shift)' $URL`

  if ( ! -e $NAME ) then

    ##
    ## Download muscle
    ##

    echo "Getting muscle from $URL"
    curl -L "$URL" > $NAME

    if ( $status ) then
      echo "ERROR 5: Muscle download failed."
      echo "Installation incomplete"
      exit 5
    endif
  endif

  ##
  ## Determine file type
  ##

  setenv FILETYPE `perl -e 'if(shift =~ /\.tar\.gz/i){print "GZTARBALL\n"}else{print "EXE\n"}' "$NAME"`
  if ( $FILETYPE == "GZTARBALL" ) then
    setenv DOWNLD "$NAME"
    setenv EXE `perl -e 'print(map {s/\.tar\.gz//;$_} shift)' $DOWNLD`
  else
    setenv FILETYPE `perl -e 'if(shift =~ /\.tar/i){print "TARBALL\n"}else{print "EXE\n"}' "$NAME"`
    if ( $FILETYPE == "TARBALL" ) then
      setenv DOWNLD "$NAME"
      setenv EXE `perl -e 'print(map {s/\.tar//;$_} shift)' $DOWNLD`
    else
      setenv EXE "$NAME"
    endif
  endif
else
  setenv FILETYPE `perl -e 'if(shift =~ /\.tar\.gz/i){print "GZTARBALL\n"}else{print "EXE\n"}' "$VAR"`
  if ( $FILETYPE == "GZTARBALL" ) then
    setenv DOWNLD "$VAR"
    setenv EXE `perl -e 'print(map {s/\.tar\.gz//;$_} shift)' $DOWNLD`
  else
    setenv FILETYPE `perl -e 'if(shift =~ /\.tar/i){print "TARBALL\n"}else{print "EXE\n"}' "$VAR"`
    if ( $FILETYPE == "TARBALL" ) then
      setenv DOWNLD "$VAR"
      setenv EXE `perl -e 'print(map {s/\.tar//;$_} shift)' $DOWNLD`
    else
      setenv EXE "$VAR"
    endif
  endif
endif

##
## Extract the downloaded file
##

if ( $DOWNLD != '' ) then
  setenv EXE `perl -e 'print(map {s/\.tar(\.gz)?//;$_} shift)' $DOWNLD`
  if ( ! -e $EXE ) then
    echo "Extracting muscle's download file"
    if ( $FILETYPE == "GZTARBALL" ) then
      tar -zxvf $DOWNLD
      if ( $status ) then
        echo "ERROR 6: Extraction of $DOWNLD failed."
        echo "Installation incomplete"
        exit 6
      else
        echo "DEBUG: $DOWNLD has been extracted"
      endif
    else
      tar -xvf $DOWNLD
      if ( $status ) then
        echo "ERROR 6: Extraction of $DOWNLD failed."
        echo "Installation incomplete"
        exit 6
      else
        echo "DEBUG: $DOWNLD has been extracted"
      endif
    endif
    setenv EXTFILE "`\ls -t | grep -E "^muscle" | tail -n 1`"
    if ( $EXTFILE != '' && $EXE != $EXTFILE ) then
      setenv EXE $EXTFILE
      echo "DEBUG: Extracted executable: $EXE"
    endif
  endif
endif

##
## Give execute permission
##

if ( "$EXE" != '' ) then
  if ( -e $EXE ) then
    if ( ! -x $EXE ) then
      echo "Adjusting muscle's execute permissions"
      chmod 755 $EXE
      if ( $status || ! -x $EXE ) then
        echo "ERROR 7: Unable to give execute permission to $EXE."
        echo "Installation incomplete"
        exit 7
      endif
    endif
  else
    echo "ERROR 8: $EXE Unable to find executable file."
    echo "Installation incomplete"
    exit 8
  endif
else
  echo "ERROR 9: Unable to find extracted file."
  echo "Installation incomplete"
  exit 9
endif

##
## Copy the executable to /usr/local/bin if we are root
##

if ( $ROOTINSTALL == "1" ) then
  cp $EXE /usr/local/bin/
  if ( $status ) then
    echo "ERROR 12: Unable to copy $EXE to /usr/local/bin."
    echo "Installation incomplete"
    exit 12
  endif
endif

##
## If local install and the file is not in the current directory, copy it there
##

if ( "$ROOTINSTALL" == "0" && "$DOWNLD" != '' && -e "$DOWNLD" && "$EXE" != "$DOWNLD" && ! -e `perl -e 'print(map {s%.*/%%;$_} shift)' $DOWNLD` ) then
  echo "cp '$DOWNLD' ./"
  cp "$DOWNLD" ./

  if ( $status ) then
    echo "ERROR 5: Unable to copy $DOWNLD into the CFF directory."
    echo "Installation incomplete"
    exit 5
  endif

  #Strip any path off the $DOWNLD variable
  setenv DOWNLD `perl -e 'print(map {s%.*/%%;$_} shift)' $DOWNLD`
else if ( "$ROOTINSTALL" == "0" && "$EXE" != '' && -e "$EXE" && ! -e `perl -e 'print(map {s%.*/%%;$_} shift)' $EXE` ) then
  echo "cp '$EXE' ./"
  cp "$EXE" ./

  if ( $status ) then
    echo "ERROR 7: Unable to copy $EXE into the CFF directory."
    echo "Installation incomplete"
    exit 7
  endif

  #Strip any path off the $EXE variable
  setenv EXE `perl -e 'print(map {s%.*/%%;$_} shift)' $EXE`
else if ( "$ROOTINSTALL" == "0" ) then
  echo "$EXE is already in the CFF installation directory"

  #Strip any path off the $EXE variable
  setenv EXE `perl -e 'print(map {s%.*/%%;$_} shift)' $EXE`
endif

##
## Link the executable if it's not already called "muscle"
##

setenv FILENAME `perl -e 'print(map {s%.*/%%;$_} shift)' "$EXE"`
setenv FILEPATH `perl -e 'print(map {s%[^/]+\Z%%;($_ eq "" ? "." : $_)} shift)' "$EXE"`
if ( $ROOTINSTALL == "1" ) then
  setenv EXE /usr/local/bin/$FILENAME
  setenv FILEPATH /usr/local/bin
endif
which $FILEPATH/muscle
setenv LINKABSENT $status
if ( "$FILENAME" != "muscle" && ! -e $FILEPATH/muscle && $LINKABSENT ) then
  echo "Creating a muscle soft link"
  ln -s $EXE $FILEPATH/muscle
  if ( $status ) then
    echo "ERROR 13: Unable to link executable $EXE to 'muscle'."
    echo "Installation incomplete"
    exit 13
  endif
else
  echo "$FILEPATH/$FILENAME muscle link already exists"
endif

##
## Add the linked executable to the user's path if it's not there
##

setenv TESTEXE $EXE
setenv INPATH 1
cd ..
which muscle >& /dev/null
if ( $status ) then
  setenv INPATH 0
endif
cd $CWD
if ( $INPATH == 0 ) then
  if ( "$ROOTINSTALL" == "0" ) then
    setenv BASHENV "$HOME/.bashrc"
    setenv TCSHENV "$HOME/.cshrc"
  else
    setenv BASHENV "/etc/profile"
    setenv TCSHENV "/etc/csh.cshrc"
  endif

  setenv ABSOLUTE `perl -e 'if(shift =~ m%^/%){print("1\n")}else{print("0\n")}' "$FILEPATH"`
  setenv ALREADYBASH 0
  setenv ALREADYTCSH 0
  if ( $ABSOLUTE == 1 ) then
    setenv TESTEXE "$FILEPATH/$FILENAME"
    #bash
    if ( ! -e $BASHENV ) then
      setenv ALREADYBASH 0
      #echo "DEBUG: $BASHENV does not exist yet to contain the absolute path"
    else if ( `grep -c "$FILEPATH" "$BASHENV"` == 0 ) then
      setenv ALREADYBASH 0
      #echo "DEBUG: $BASHENV exists but does not have the absolute path"
    else
      setenv ALREADYBASH 1
      #echo "DEBUG: $BASHENV exists and has the absolute path"
    endif
    if ( $ALREADYBASH == 0 ) then
      #echo "DEBUG: Appending the absolute path to $BASHENV"
      echo '' >> "$BASHENV"
      echo "#Amending PATH for muscle executable" >> "$BASHENV"
      echo 'export PATH=$PATH:'$FILEPATH >> "$BASHENV"

      if ( $status ) then
        echo "ERROR 15: Unable to append to $BASHENV"
        exit 15
      endif
    endif

    #tcsh
    if ( ! -e $TCSHENV ) then
      setenv ALREADYTCSH 0
    else if ( `grep -c "$FILEPATH" "$TCSHENV"` == 0 ) then
      setenv ALREADYTCSH 0
    else
      setenv ALREADYTCSH 1
    endif
    if ( $ALREADYTCSH == 0 ) then
      echo '' >> "$TCSHENV"
      echo "#Amending PATH for muscle executable" >> "$TCSHENV"
      echo 'setenv PATH ${PATH}:'${FILEPATH} >> "$TCSHENV"

      if ( $status ) then
        echo "ERROR 15: Unable to append to $TCSHENV"
        exit 15
      endif
    endif
  else
    setenv TESTEXE "$CWD/$FILEPATH/$FILENAME"
    #bash
    if ( ! -e $BASHENV ) then
      setenv ALREADYBASH 0
      #echo "DEBUG: $BASHENV does not exist yet to contain the relative path $CWD/$FILEPATH"
    else if ( `grep -c "${CWD}/$FILEPATH" $BASHENV` == "0" ) then
      setenv ALREADYBASH 0
      #echo "DEBUG: $BASHENV exists but does not have the relative path $CWD/$FILEPATH"
    else
      setenv ALREADYBASH 1
      #echo "DEBUG: $BASHENV exists and has the relative path $CWD/$FILEPATH"
    endif
    if ( $ALREADYBASH == 0 ) then
      #echo "DEBUG: Appending the relative path to $BASHENV"
      echo '' >> $BASHENV
      echo "#Amending PATH for muscle executable" >> $BASHENV
      echo 'export PATH=$PATH:'"$CWD/$FILEPATH" >> $BASHENV

      if ( $status ) then
        echo "ERROR 15: Unable to append to $BASHENV"
        exit 15
      endif
    endif

    #tcsh
    if ( ! -e $TCSHENV ) then
      setenv ALREADYTCSH 0
    else if ( `grep -c "${CWD}/${FILEPATH}" $TCSHENV` == "0" ) then
      setenv ALREADYTCSH 0
    else
      setenv ALREADYTCSH 1
    endif
    if ( $ALREADYTCSH == 0 ) then
      echo '' >> $TCSHENV
      echo "#Amending PATH for muscle executable" >> $TCSHENV
      echo 'setenv PATH ${PATH}:'"${CWD}/${FILEPATH}" >> $TCSHENV

      if ( $status ) then
        echo "ERROR 15: Unable to append to $TCSHENV"
        exit 15
      endif
    endif
  endif
  if ( $ALREADYBASH == 1 ) then
    echo "Great, muscle's path is already in your bash profile.  You should be good to go."
  else
    echo "Adding muscle's path to your bash profile.  Please open a new terminal window or run '. $BASHENV' to enable the muscle command."
  endif
  if ( $ALREADYTCSH == 1 ) then
    echo "Great, muscle's path is already in your shell environment rc script.  You should be good to go."
  else
    echo "Adding muscle's path to your shell environment rc script.  Please open a new terminal window or run 'source $TCSHENV' to enable the muscle command."
  endif
  $TESTEXE -version
  if ( $status ) then
    echo "ERROR 14: $TESTEXE Test failed."
    echo "Installation incomplete"
    exit 14
  else
    echo Muscle installation complete
  endif
endif

if ( "$ROOTINSTALL" == "1" ) then
  cd "$CWD"
endif

exit 0
