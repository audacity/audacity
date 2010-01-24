#!/usr/bin/env perl

# this script patches a config.status file, to use our own perl script
# in the main loop
# we do it this way to circumvent hacking (and thereby including)
# autoconf function (which are GPL) into our LGPL acinclude.m4.in
# written by Michael Matz <matz@kde.org>
# adapted by Dirk Mueller <mueller@kde.org>
#
#   This file is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Library General Public
#   License as published by the Free Software Foundation; either
#   version 2 of the License, or (at your option) any later version.

#   This library is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   Library General Public License for more details.

#   You should have received a copy of the GNU Library General Public License
#   along with this library; see the file COPYING.LIB.  If not, write to
#   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#   Boston, MA 02110-1301, USA.

# we have to change two places
# 1. the splitting of the substitutions into chunks of 90 (or even 48 in
#    later autoconf's
# 2. the big main loop which patches all Makefile.in's

use strict;
use File::Basename;

my $ac_aux_dir = dirname($0);
my ($flag);
my $ac_version = 0;
my $vpath_seen = 0;
$flag = 0;

while (<>) {
# usage of $flag: 0 -- we have seen nothing yet
#   1 -- we are in (1)
#   2 -- we have ended (1)
#   3 -- we are in (2)
#   4 -- we ended (2)

    if ($flag == 4) {
        print;
    } elsif ($flag == 0) {
# 1. begins with (including): "ac_max_sed_\S+\s*=\s*[0-9]+..."
#    ends with (excluding) "CONFIG_FILE=..."
#    in later autoconf (2.14.1) there is no CONFIG_FILES= line,
#    but instead the (2) directly follow (1)
        if (/^\s*ac_max_sed_([a-z]+).*=\s*([0-9]+)/ ) {
	    $flag = 1;
	    if ($1 eq 'lines') {
                # lets hope its different with 2141, 
                # wasn't able to verify that
              if ($2 eq '48') {
                $ac_version = 250;
              }
              else {
	        $ac_version = 2141;
              }
	    } elsif ($1 eq 'cmds') {
	        $ac_version = 213;
	    }
	    # hmm, we don't know the autoconf version, but we try anyway
	} else {
	    print;
	}
    } elsif ($flag == 1) {
        if (/^\s*CONFIG_FILES=/ && ($ac_version != 250)) {
	     print;
	     $flag = 2;
	} elsif (/^\s*for\s+ac_file\s+in\s+.*CONFIG_FILES/ ) {
	     $flag = 3;
	}
    } elsif ($flag == 2) {
# 2. begins with: "for ac_file in.*CONFIG_FILES"  (the next 'for' after (1))
#    end with: "rm -f conftest.s\*"
# on autoconf 250, it ends with '# CONFIG_HEADER section'
#
# gg: if a post-processing commands section is found first, 
#    stop there and insert a new loop to honor the case/esac.
# (pattern: /^\s+#\sRun the commands associated with the file./)

	if (/^\s*for\s+ac_file\s+in\s+.*CONFIG_FILES/ ) {
	    $flag = 3;
	} else {
	    print;
	}
    } elsif ($flag == 3) {
        if (/^\s*rm\s+-f\s+conftest/ ) {
	    $flag = 4;
	    &insert_main_loop();
	} elsif (/^\s*rm\s+-f\s+.*ac_cs_root/ ) {
	    $flag = 4;
	    &insert_main_loop();
	    #die "hhhhhhh";
	    if ($ac_version != 2141) {
	        print STDERR "hmm, don't know autoconf version\n";
	    }
        } elsif (/^\#\s*CONFIG_(HEADER|COMMANDS) section.*|^\s+#\s(Run) the commands associated/) {
          $flag = 4;
          my $commands = defined $2;
          &insert_main_loop();
          $commands && insert_command_loop();
          if($ac_version != 250) {
            print STDERR "hmm, something went wrong :-(\n";
          }
	} elsif (/VPATH/ ) {
	    $vpath_seen = 1;
	}
    }
}

die "wrong input (flag != 4)" unless $flag == 4;
print STDERR "hmm, don't know autoconf version\n" unless $ac_version;

sub insert_main_loop {

  if ($ac_version == 250) {
    &insert_main_loop_250();
  }
  else {
    &insert_main_loop_213();
  }
}

sub insert_main_loop_250 {

  print <<EOF;
  #echo Doing the fast build of Makefiles -- autoconf $ac_version
EOF
    if ($vpath_seen) {
        print <<EOF;
        # VPATH subst was seen in original config.status main loop
  echo '/^[ 	]*VPATH[ 	]*=[^:]*\$/d' >>\$tmp/subs.sed
EOF
      }
  print <<EOF;
  rm -f \$tmp/subs.files
  for ac_file in .. \$CONFIG_FILES ; do
      if test "x\$ac_file" != x..; then
          echo \$ac_file >> \$tmp/subs.files
      fi
  done
  if test -f \$tmp/subs.files ; then
      perl $ac_aux_dir/config.pl "\$tmp/subs.sed" "\$tmp/subs.files" "\$srcdir" "\$INSTALL"
  fi
  rm -f \$tmp/subs.files

fi
EOF
  return;
}

sub insert_main_loop_213 {
    print <<EOF;
#echo Doing the fast build of Makefiles -- autoconf $ac_version
if test "x\$ac_cs_root" = "x" ; then
    ac_cs_root=conftest
fi
EOF
    if ($vpath_seen) {
      print <<EOF;
# VPATH subst was seen in original config.status main loop
echo '/^[ 	]*VPATH[ 	]*=[^:]*\$/d' >> \$ac_cs_root.subs
EOF
    }
    print <<EOF;
rm -f \$ac_cs_root.sacfiles
for ac_file in .. \$CONFIG_FILES ; do
    if test "x\$ac_file" != x..; then
        echo \$ac_file >> \$ac_cs_root.sacfiles
    fi
done
if test -f \$ac_cs_root.sacfiles ; then
    perl $ac_aux_dir/config.pl "\$ac_cs_root.subs" "\$ac_cs_root.sacfiles" "\$ac_given_srcdir" "\$ac_given_INSTALL"
fi
rm -f \$ac_cs_root.s*

EOF
    return;
}

sub insert_command_loop {
    print <<EOF;
  for ac_file in .. \$CONFIG_FILES ; do
EOF
}
