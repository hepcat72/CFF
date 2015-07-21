#!/bin/tcsh

#This script attempts to install usearch given a usearch download URL or the name-of/path-to the usearch executable.
#USAGE 1: install_usearch.tcsh ~/bin/usearch7.0.1090_i86osx32         (example)
#USAGE 2: install_usearch.tcsh http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000

setenv CWD         `pwd`
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
  echo '## Performing a system level install of usearch'
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
else
  echo
  echo '## Performing a local install of usearch'
  echo
endif

##
## See if install is already complete
##

cd ..
which usearch >& /dev/null
if ( ! $status ) then
  if ( "$ROOTINSTALL" == "1" ) then
    setenv INUSRLOCAL `which usearch | perl -pne 'm%^/usr/local/bin/% ? 1 : 0'`
    if ( "$INUSRLOCAL" == "1" ) then
      echo "usearch is already installed in /usr/local/bin/"
      usearch -version
      cd $CWD
      exit 0
    endif
  else
    echo "usearch is already installed and in your path [PATH = $PATH]"
    echo
    which usearch
    usearch -version
    cd $CWD
    exit 0
  endif
endif
cd $CWD

#echo DEBUG4

if ( "$VAR" == '' && ! -e 'usearch7' ) then
  echo "ERROR 3: A download URL, gzipped tarball, or usearch executable is a required parameter.  E.g. ./install_usearch.tcsh http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000"
  exit 3
else if ( "$VAR" == '' && -e 'usearch7' ) then
  setenv NAME 'usearch7'
  setenv EXE 'usearch7'
endif

##
## Set the URL/EXE/NAME/DOWNLD if applicable
##
#echo DEBUG5

if ( "$TYPE" == 'URL' ) then
  setenv URL "$VAR"
  setenv NAME usearch7

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
else if ( "$VAR" != '' ) then
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

#echo DEBUG6
##
## If root install, copy existing files from local install and cd to /usr/local/bin
##

if ( "$ROOTINSTALL" == "1" && ( "$EXE" == '' || ! -e "$EXE" ) && ( "$DOWNLD" == '' || ! -e "$DOWNLD" ) ) then
  echo "cd /usr/local/bin"
  cd /usr/local/bin

  if ( $status ) then
    echo "ERROR 4: Unable to change directories into /usr/local/bin."
    echo "Installation incomplete"
    exit 4
  endif
else if ( "$ROOTINSTALL" == "1" && "$DOWNLD" != '' && -e "$DOWNLD" && "$EXE" != "$DOWNLD" ) then
  echo "cp '$DOWNLD' /usr/local/bin"
  cp "$DOWNLD" /usr/local/bin

  if ( $status ) then
    echo "ERROR 5: Unable to copy $DOWNLD into /usr/local/bin."
    echo "Installation incomplete"
    exit 5
  endif

  echo "cd /usr/local/bin"
  cd /usr/local/bin

  if ( $status ) then
    echo "ERROR 6: Unable to change directories into /usr/local/bin."
    echo "Installation incomplete"
    exit 6
  endif

  #Strip any path off the $DOWNLD variable
  setenv DOWNLD `perl -e 'print(map {s%.*/%%;$_} shift)' $DOWNLD`
else if ( "$ROOTINSTALL" == "1" && "$EXE" != '' && -e "$EXE" ) then
  echo "cp '$EXE' /usr/local/bin"
  cp "$EXE" /usr/local/bin

  if ( $status ) then
    echo "ERROR 7: Unable to copy $EXE into /usr/local/bin."
    echo "Installation incomplete"
    exit 7
  endif

  echo "cd /usr/local/bin"
  cd /usr/local/bin

  if ( $status ) then
    echo "ERROR 8: Unable to change directories into /usr/local/bin."
    echo "Installation incomplete"
    exit 8
  endif

  #Strip any path off the $EXE variable
  setenv EXE `perl -e 'print(map {s%.*/%%;$_} shift)' $EXE`
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
  #Strip any path off the $EXE variable
  setenv EXE `perl -e 'print(map {s%.*/%%;$_} shift)' $EXE`

  echo "$EXE is already in the CFF installation directory"
endif

#echo DEBUG7
##
## Download usearch
##

if ( "$TYPE" == 'URL' && ( "$EXE" == '' || ! -e "$EXE" ) && ( "$DOWNLD" == '' || ! -e "$DOWNLD" ) ) then
  echo "Getting usearch from $URL"
  curl -L "$URL" > $NAME

  if ( $status ) then
    echo "ERROR 9: usearch download failed."
    echo "Installation incomplete"
    exit 9
  endif
else
  echo "usearch already downloaded"
endif

#echo DEBUG8
##
## Extract the downloaded file
##

if ( "$DOWNLD" != '' && -e "$DOWNLD" && ( "$EXE" == '' || ! -e "$EXE" ) ) then
  setenv EXE `perl -e 'print(map {s/\.tar(\.gz)?//;$_} shift)' $DOWNLD`
  setenv NEEDEXTRACT `perl -e 'print(map {/\.tar(\.gz)?/ ? "1\n" : "0\n"} shift)' $DOWNLD`
  if ( $NEEDEXTRACT && ! -e $EXE ) then
    echo "Extracting usearch's download file"
    if ( $FILETYPE == "GZTARBALL" ) then
      tar -zxvf $DOWNLD
      if ( $status ) then
        echo "ERROR 6: Extraction of $DOWNLD failed."
        echo "Installation incomplete"
        exit 6
      endif
    else
      tar -xvf $DOWNLD
      if ( $status ) then
        echo "ERROR 6: Extraction of $DOWNLD failed."
        echo "Installation incomplete"
        exit 6
      endif
    endif
    setenv EXTFILE "`\ls -t | grep -E "^usearch" | tail -n 1`"
    if ( $EXTFILE != '' && $EXE != $EXTFILE ) then
      setenv EXE $EXTFILE
    endif
  endif
else
  echo "usearch already extracted"
endif

#echo DEBUG9
##
## Give execute permission
##

if ( "$EXE" != '' ) then
  if ( -e $EXE ) then
    if ( ! -x $EXE ) then
      echo "Adjusting usearch's execute permissions"
      chmod 755 $EXE
      if ( $status || ! -x $EXE ) then
        echo "ERROR 11: Unable to give execute permission to $EXE."
        echo "Installation incomplete"
        exit 11
      endif
    endif
  else
    echo "ERROR 12: $EXE Unable to find executable file."
    echo "Installation incomplete"
    exit 12
  endif
else
  echo "ERROR 13: Unable to find extracted file."
  echo "Installation incomplete"
  exit 13
endif

#echo DEBUG10
##
## Link the executable if it's not already called "usearch"
##

setenv FILENAME `perl -e 'print(map {s%.*/%%;$_} shift)' "$EXE"`
setenv FILEPATH `perl -e 'print(map {s%[^/]+\Z%%;($_ eq "" ? "." : $_)} shift)' "$EXE"`
if ( "$ROOTINSTALL" == "1" ) then
  setenv EXE /usr/local/bin/$FILENAME
  setenv FILEPATH /usr/local/bin
endif
which $FILEPATH/usearch
setenv LINKABSENT $status
if ( $FILENAME != "usearch" && ! -e $FILEPATH/usearch && $LINKABSENT ) then
  echo "Creating a usearch soft link"
  ln -s $EXE $FILEPATH/usearch

  if ( $status ) then
    echo "ERROR 14: Unable to link executable $EXE to 'usearch'."
    echo "Installation incomplete"
    exit 14
  endif
else
  echo "$FILEPATH/$FILENAME usearch link already exists"
endif

#echo DEBUG11
##
## Add the linked executable to the user's path if it's not there
##

setenv TESTEXE $EXE
setenv INPATH 1
cd ..
which usearch >& /dev/null
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
    else if ( `grep -c "$FILEPATH" $BASHENV` == 0 ) then
      setenv ALREADYBASH 0
    else
      setenv ALREADYBASH 1
    endif
    if ( $ALREADYBASH == 0 ) then
      echo '' >> $BASHENV
      echo "#Amending PATH for usearch executable" >> $BASHENV
      echo 'export PATH=$PATH:'$FILEPATH >> $BASHENV

      if ( $status ) then
        echo "ERROR 15: Unable to append to $BASHENV"
        exit 15
      endif
    endif

    #tcsh
    if ( ! -e $TCSHENV ) then
      setenv ALREADYTCSH 0
    else if ( `grep -c "$FILEPATH" $TCSHENV` == 0 ) then
      setenv ALREADYTCSH 0
    else
      setenv ALREADYTCSH 1
    endif
    if ( $ALREADYTCSH == 0 ) then
      echo '' >> $TCSHENV
      echo "#Amending PATH for usearch executable" >> $TCSHENV
      echo 'setenv PATH ${PATH}:'${FILEPATH} >> $TCSHENV

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
    else if ( `grep -c "${CWD}/$FILEPATH" $BASHENV` == "0" ) then
      setenv ALREADYBASH 0
    else
      setenv ALREADYBASH 1
    endif
    if ( $ALREADYBASH == 0 ) then
      #bash
      echo '' >> $BASHENV
      echo "#Amending PATH for usearch executable" >> $BASHENV
      echo 'export PATH=$PATH:'"$CWD/$FILEPATH" >> $BASHENV

      if ( $status ) then
        echo "ERROR 15: Unable to append to $BASHENV"
        exit 15
      endif
    endif

    #tcsh
    if ( ! -e $TCSHENV ) then
      setenv ALREADYTCSH 0
    else if ( `grep -c "${CWD}/${FILEPATH}" "$TCSHENV"` == "0" ) then
      setenv ALREADYTCSH 0
    else
      setenv ALREADYTCSH 1
    endif
    if ( $ALREADYTCSH == 0 ) then
      echo '' >> "$TCSHENV"
      echo "#Amending PATH for usearch executable" >> "$TCSHENV"
      echo 'setenv PATH ${PATH}:'"${CWD}/${FILEPATH}" >> "$TCSHENV"

      if ( $status ) then
        echo "ERROR 15: Unable to append to $TCSHENV"
        exit 15
      endif
    endif
  endif
  if ( $ALREADYBASH == 1 ) then
    echo "Great, usearch's path is already in your bash profile.  You should be good to go."
  else
    echo "Adding usearch's path to your bash profile.  Please open a new terminal window or run '. $BASHENV' to enable the usearch command."
  endif
  if ( $ALREADYTCSH == 1 ) then
    echo "Great, usearch's path is already in your shell environment rc script.  You should be good to go."
  else
    echo "Adding usearch's path to your shell environment rc script.  Please open a new terminal window or run 'source $TCSHENV' to enable the usearch command."
  endif
  $TESTEXE -version
  if ( $status ) then
    echo "ERROR 15: $TESTEXE Test failed."
    echo "Installation incomplete"
    exit 15
  else
    echo usearch installation complete
  endif
endif

if ( "$ROOTINSTALL" == "1" ) then
  cd "$CWD"
endif

exit 0