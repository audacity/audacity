#!/usr/bin/perl

use strict 'vars';

open(my $fh, "<", "audacity.pot")
    or die "Can't open < audacity.pot: $!";

my @sourcelines = ();

$\ = "\n";

while (my $line =<$fh>) {
    chop $line;
    my @tokens = split(/ /, $line, 2);
    if ($tokens[0] eq "#:") {
	push @sourcelines, split(/ /, $tokens[1]);
    }
    elsif ($tokens[0] eq "msgid") {
	if( @sourcelines > 1) {
	    print;
	    my $string = $tokens[1];
	    do {
		print $string;
		$string = <$fh>;
		chop $string;
	    } while $string =~ /^"/;
	    foreach my $sourceline (@sourcelines) {
		print $sourceline;
	    }
	}
	@sourcelines = ();
    }
}
