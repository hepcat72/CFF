#!/bin/bash

#This script is for linux installation only.

#MUST BE EXECUTED FROM THE MAIN CFF DIRECTORY
#USAGE 1: sudo install.tcsh <USEARCH>
#USAGE 2: sudo install.tcsh <USEARCH> <MUSCLE>
#Where the first and second parameters indicate how to install/configure the usearch
#and muscle executables.  In other words, "<USEARCH>" and "<MUSCLE>" can be:
#  1. A download URL directly to a tarball or executable file
#  2. A tarball or executable file
#  3. "skip" or "ignore"
#Example:
#      install.tcsh http://drive5.com/cgi-bin/upload3.py?license=0000000000000000000
#
#A usearch URL/tarball/executable is required for a successful install.  usearch and
#muscle are third-party applications not maintained by us.  The muscle executable is
#directly downloadable for anyone, but to get usearch, you must visit
#http://www.drive5.com/usearch/download.html and agree to the terms.

preinstall=0


##
## Make sure curl is installed
##

which curl

if [ $? -ne 0 ]
  then
    if [ "$EUID" -ne 0 ]
      then
        echo "Please run as root with no parameters so that curl can be installed first. Then run again with your CFF install parameters."
        exit 1
    fi
    apt-get install curl
    if [ $? -ne 0 ]
      then
        #We will try to update the OS
        apt-get update

        echo
        echo '## ERROR 2: Unable to install curl. Going to try an update of apt-get.  A window should appear in a moment to walk you through the update.'
        echo -n 'Hit return when the update is complete: '

        apt-get install curl
        if [ $? -ne 0 ]
          then
            echo "ERROR 3: Still unable to install curl.  Please manually install curl to proceed."
            exit 3
        fi
    fi
    echo "curl has been installed successfully."
    preinstall=1
fi


##
## Make sure tcsh is installed
##

which tcsh

if [ $? -ne 0 ]
  then
    if [ "$EUID" -ne 0 ]
      then
        echo "Please run as root with no parameters so that the tcsh shell can be installed first. Then run again with your CFF install parameters."
        exit 1
    fi
    apt-get install tcsh
    if [ $? -ne 0 ]
      then
        #We will try to update the OS
        apt-get update

        echo
        echo '## ERROR 4: Unable to install tcsh. Going to try an update of apt-get.  A window should appear in a moment to walk you through the update.'
        echo -n 'Hit return when the update is complete: '

        apt-get install tcsh
        if [ $? -ne 0 ]
          then
            echo "ERROR 5: Still unable to install tcsh.  Please manually install tcsh to proceed."
            exit 5
        fi
    fi
    echo "tcsh has been installed successfully."
    preinstall=1
fi


##
## make sure ExtUtils::MakeMaker is installed
##

perl -e 'use ExtUtils::MakeMaker' >& /dev/null

if [ $? -ne 0 ]
  then
    if [ "$EUID" -ne 0 ]
      then
        echo "Please run as root with no parameters so that perl's ExtUtils::MakeMaker module can be installed first. Then run again with your CFF install parameters."
        exit 1
    fi
    sudo yum install -y perl-CPAN
    if [ $? -ne 0 ]
      then
        echo "ERROR 5: Unable to install the tcsh shell"
        exit 5
    fi
    echo "perl module: ExtUtils::MakeMaker has been installed successfully. You may now install CFF by running this script again with a usearch download link."
    preinstall=1
fi

args=$#
if [ $args -eq 0 ]
  then
    if [ "$EUID" -eq 0 ]
      then
        if [ "$preinstall" -eq 1 ]
          then
            echo
            echo "Dependencies installed.  Install CFF by running this script again with a usearch download link."
            echo
            exit 0
        fi
        echo
        echo "Dependencies appear to already have been installed.  Install CFF by running this script again with a usearch download link."
        echo
        exit 0
    fi
fi

if [ ! -e install.tcsh ]
  then
    echo "ERROR 6: Please run this script from inside the CFF directory."
    exit 6
fi

echo "./install.tcsh $@"
./install.tcsh "$@"
