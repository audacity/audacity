#!/usr/bin/env perl

# Mailman-discard
# Written by Dominic Mazzoni, 2003
# Released into the Public Domain
# NO WARRANTY

print "This script uses 'curl' to connect to Sourceforge's mailman server\n";
print "and discard all of the pending messages in a mailbox.  This is\n";
print "useful when an account gets hit with a virus or lots of spam.\n";
print "\n";
print "Note that ALL pending messages are discarded, with no notice sent\n";
print "to anyone!  You CANNOT recover messages after running this script!\n";
print "";

print "Enter Sourceforge list name (such as 'audacity-devel'): ";
$list = <STDIN>;

print "Enter list password (warning, not hidden): ";
$pass = <STDIN>;
chop $pass;

print "Logging in and getting info...\n";

$out = `curl --data 'adminpw=$pass' --dump-header cookiefile https://lists.sourceforge.net/lists/admindb/$list`;

$data = "";

@lines = split("\n", $out);

$count = 0;
foreach $line (@lines) {
    if ($line =~ 'INPUT name="([0-9]+)" type="RADIO" value="3"') {
	$count++;
	if ($data eq "") {
	    $data = "$1=3";
	}
	else {
	    $data = "$data&$1=3";
	}
    }
}

if ($count == 0) {
    print "Couldn't find any messages to discard!\n";
    exit;
}

print "Discarding $count messages from $list\n";

$out = `curl --cookie cookiefile --data '$data' https://lists.sourceforge.net/lists/admindb/$list`;

@lines = split("\n", $out);

foreach $line (@lines) {
    if ($line =~ 'no pending requests') {
	print "Success!\n";
	exit;
    }
}

print "Done, but couldn't verify success.";

# arch-tag: dc81de17-0bcb-4131-988e-3a3dad0e8e8d

