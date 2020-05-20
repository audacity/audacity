#!/usr/bin/perl

use strict 'vars';

open(my $fh, "<", "audacity.pot")
	or die "Can't open < audacity.pot: $!";

my @sourcelines = ();

$\ = "\n";

foreach my $line (<$fh>) {
    chop $line;
    my @tokens = split(/ /, $line, 2);
    if ($tokens[0] eq "#:") {
	push @sourcelines, split(/ /, $tokens[1]);
    }
    elsif ($tokens[0] eq "msgid") {
	if( @sourcelines > 1) {
	    print;
	    print $tokens[1];
	    foreach my $sourceline (@sourcelines) {
		print $sourceline;
	    }
	}
	@sourcelines = ();
    }
}
