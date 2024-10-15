#!/usr/bin/perl
# Take LangugeNames.txt and write C++ string literals to standard out,
# which describe the UTF-8 byte sequence using only the basic coding
# character set.
# Copy and paste that over the table of strings in src/Languages.cpp.

open LANGUAGES, "LanguageNames.txt" or die "Can't open LanguageNames.txt: $!\n";
while( <LANGUAGES> ) {
    chop;
    print "\"";
    @codes = unpack 'C*';
    @newCodes = ();
    for $code (@codes) {
	if ($code <= ord 'z') {
	    push @newCodes, $code;
	}
	else {
	    push @newCodes, unpack('C*', sprintf("\\%03o", $code));
	}
    }
    print pack 'C*', @newCodes;
    print "\",\n";
}
