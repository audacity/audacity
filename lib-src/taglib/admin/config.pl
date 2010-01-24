#!/usr/bin/env perl
# a script for use by autoconf to make the Makefiles
# from the Makefile.in's
#
# the original autoconf mechanism first splits all substitutions into groups
# of ca. 90, and than invokes sed for _every_ Makefile.in and every group
# (so around 2-3 times per Makefile.in). So this takes forever, as sed
# has to recompile the regexps every time.
#
# this script does better. It changes all Makefile.ins in one process.
# in kdelibs the time for building Makefile went down from 2:59 min to 13 sec!
#
# written by Michael Matz <matz@kde.org>
# adapted by Dirk Mueller <mueller@kde.org>

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

use strict;

use File::Path;

my $ac_subs=$ARGV[0];
my $ac_sacfiles = $ARGV[1];
my $ac_given_srcdir=$ARGV[2];
my $ac_given_INSTALL=$ARGV[3];

my @comp_match;
my @comp_subs;

#print "ac_subs=$ac_subs\n";
#print "ac_sacfiles=$ac_sacfiles\n";
#print "ac_given_srcdir=$ac_given_srcdir\n";
#print "ac_given_INSTALL=$ac_given_INSTALL\n";

my $configure_input;
my ($srcdir, $top_srcdir);
my $INSTALL;
my $bad_perl = ($] < 5.005);
my $created_file_count = 0;

open(CF, "< $ac_subs") || die "can't open $ac_subs: $!";
my @subs = <CF>;
my $pat;
close(CF);
chomp @subs;
@comp_match=();
@comp_subs=();

if ($bad_perl) {
    print "Using perl older than version 5.005\n";
    foreach $pat (@subs) {
	if (  ($pat =~ m/s%([^%]*)%([^%]*)%g/ )
	   || ($pat =~ m/s%([^%]*)%([^%]*)%;t/ )
           || ($pat =~ m/s,([^,]*),(.*),;t/)
	   || ($pat =~ m%s/([^/]*)/([^/]*)/g% )
	   || ($pat =~ m%s/([^/]*)/([^/]*)/;t% )
	   ) {
            # form : s%bla%blubb%g
            # or     s%bla%blubb%;t t   (autoconf > 2.13 and < 2.52 ?)
            # or     s,bla,blubb,;t t   (autoconf 2.52)
            my $srch = $1;
            my $repl = $2;
            $repl =~ s/\\(.)/$1/g;
	    push @comp_subs, make_closure($srch, $repl);

	} elsif ( ($pat =~ /%([^%]*)%d/ )
	   || ($pat =~ m%/([^/]*)/d% )
	   ) {
	    push @comp_subs, make_closure($1, "");
	} else {
	    die "Uhh. Malformed pattern in $ac_subs ($pat)"
		unless ( $pat =~ /^\s*$/ );   # ignore white lines
	}
    }
} else {
    foreach $pat (@subs) {
       if ( ($pat =~ /s%([^%]*)%([^%]*)%g/ ) ||
            ($pat =~ /s%([^%]*)%([^%]*)%;t/ ) ||
            ($pat =~ /s,([^,]*),(.*),;t/) ) {
         # form : s%bla%blubb%g
         # or     s%bla%blubb%;t t   (autoconf > 2.13 and < 2.52 ?)
         # or     s,bla,blubb,;t t   (autoconf 2.52)
         my $srch = $1;
         my $repl = $2;
         push @comp_match, eval "qr/\Q$srch\E/";  # compile match pattern
         $repl =~ s/\\(.)/$1/g;
         push @comp_subs, $repl;
      } elsif ( ($pat =~ /%([^%]*)%d/ )
                || ($pat =~ m%/([^/]*)/d% )
              ) {
        push @comp_match, eval "qr/\Q$1\E/";
        push @comp_subs, "";
      } else {
          die "Uhh. Malformed pattern in $ac_subs ($pat)"
          unless ( $pat =~ /^\s*$/ );   # ignore white lines
      }
    }
}
undef @subs;

# read the list of files to be patched, form:
# ./Makefile arts/Makefile arts/examples/Makefile arts/flow/Makefile

open(CF, "< $ac_sacfiles") || die "can't open $ac_sacfiles: $!";
my @ac_files = <CF>;
close(CF);
chomp @ac_files;


my $ac_file;
foreach $ac_file (@ac_files) {
    next if $ac_file =~ /\.\./;
    next if $ac_file =~ /^\s*$/;
    my $ac_file_in;
    my ($ac_dir, $ac_dots, $ac_dir_suffix);

    if ($ac_file =~ /.*:.*/ ) {
	($ac_file_in = $ac_file) =~ s%[^:]*:%%;
	$ac_file =~ s%:.*%%;
    } else {
	$ac_file_in = $ac_file.".in";
    }

# Adjust a relative srcdir, top_srcdir, and INSTALL for subdirectories.

# Remove last slash and all that follows it.  Not all systems have dirname.
    ($ac_dir = $ac_file) =~ s%/[^/][^/]*$%%;
    if ( ($ac_dir ne $ac_file) && ($ac_dir ne ".")) {
# The file is in a subdirectory.
	if (! -d "$ac_dir") { mkpath "$ac_dir", 0, 0777; }
	($ac_dir_suffix = $ac_dir) =~ s%^./%%;
	$ac_dir_suffix="/".$ac_dir_suffix;
# A "../" for each directory in $ac_dir_suffix.
	($ac_dots = $ac_dir_suffix) =~ s%/[^/]*%../%g;
    } else {
	$ac_dir_suffix="";
	$ac_dots="";
    }

    if ($ac_given_srcdir eq ".") {
	$srcdir=".";
	if ($ac_dots) {
	    ( $top_srcdir = $ac_dots) =~ s%/$%%;
	} else { $top_srcdir="."; }
    } elsif ($ac_given_srcdir =~ m%^/%) {
	$srcdir=$ac_given_srcdir.$ac_dir_suffix;
	$top_srcdir = $ac_given_srcdir;
    } else {
	$srcdir = $ac_dots.$ac_given_srcdir.$ac_dir_suffix;
	$top_srcdir = $ac_dots.$ac_given_srcdir;
    }

    if ($ac_given_INSTALL) {
	if ($ac_given_INSTALL =~ m%^/% ) {
	    $INSTALL = $ac_given_INSTALL;
	} else {
	    $INSTALL = $ac_dots.$ac_given_INSTALL;
	}
    }

    print "fast creating $ac_file\n";
    unlink $ac_file;
    my $ac_comsub="";
    my $fname=$ac_file_in;
    $fname =~ s%.*/%%;
    $configure_input="$ac_file.  Generated from $fname by config.pl.";

    my $ac_file_inputs;
    ($ac_file_inputs = $ac_file_in) =~ s%^%$ac_given_srcdir/%;
    $ac_file_inputs =~ s%:% $ac_given_srcdir/%g;

    patch_file($ac_file, $ac_file_inputs);
    ++$created_file_count;
}

print "config.pl: fast created $created_file_count file(s).\n";

sub patch_file {
    my ($outf, $infiles) = @_;
    my $filedata;
    my @infiles=split(' ', $infiles);
    my $i=0;
    my $name;

    foreach $name (@infiles) {
	if (open(CF, "< $name")) {
	    while (<CF>) {
		$filedata .= $_;
	    }
	    close(CF);
	} else {
	    print STDERR "can't open $name: $!"."\n";
	}
    }

    $filedata =~ s%\@configure_input\@%$configure_input%g;
    $filedata =~ s%\@srcdir\@%$srcdir%g;
    $filedata =~ s%\@top_srcdir\@%$top_srcdir%g;
    $filedata =~ s%\@INSTALL\@%$INSTALL%g;

    if ($bad_perl) {
	while ($i <= $#comp_subs) {
	    my $ref = $comp_subs[$i];
	    &$ref(\$filedata);
	    $i++;
	}
    } else {
	while ($i <= $#comp_match) {
	    $filedata =~ s/$comp_match[$i]/$comp_subs[$i]/g;
	    $i++;
	}
    }
    open(CF, "> $outf") || die "can't create $outf: $!";
    print CF $filedata;
    close(CF);
}

sub make_closure {
    my ($pat, $sub) = @_;
    my $ret = eval "return sub { my \$ref=shift; \$\$ref =~ s%\Q$pat\E%\Q$sub\E%g; }";
    if ($@) {
        print "can't create CODE: $@\n";
    }
    return $ret;
}
