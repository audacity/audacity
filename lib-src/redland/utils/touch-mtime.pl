#!/usr/bin/perl
#
#* touch-mtime.pl - Copy file modification times
#
# Copyright (C) 2007, David Beckett http://purl.org/net/dajobe/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 

use File::Basename;
use File::stat;

my $usage="REFERENCE-FILE FILE\n";


my $program=basename $0;

die "USAGE: $usage" if @ARGV != 2;


my($ref,$file)=@ARGV;

my $st;

$st=stat($ref) or die "$program: No such reference file $ref - $!\n";

my $mtime=$st->mtime;

$st=stat($file) or die "$program: No such file $file - $!\n";

utime $mtime, $mtime, $file;
