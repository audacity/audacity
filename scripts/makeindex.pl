#!/usr/bin/env perl
print "Rebuilding the index file for wxhelp...\n";
chdir "../help/wxhelp";
@files = glob("*.htm");

open OF, ">audacity.hhk";
print OF <<ENDF;

<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">
<html>
<head>
Index
</head>
<body>

<ul>

ENDF

foreach $f (@files) {
    if ($f ne "credits.htm") {
	open IF, "$f";
	@lines = <IF>;
	close IF;
	foreach $l (@lines) {
	    if ($l =~ "<b>([^<]*)</b>") {
		$phrase = $1;
		@words = split(" ", $phrase);
		if (@words <= 5 && $phrase ne "Audacity") {
		    $dict{$phrase} = $f;
		}
	    }
	}
    }
}

foreach $d (keys %dict) {
    $f = $dict{$d};
    print OF "\t<li><object type=\"text/sitemap\">\n";
    print OF "\t\t<param name=\"Name\" value=\"$d\">\n";
    print OF "\t\t<param name=\"Local\" value=\"$f\">\n";
    print OF "\t</object>\n";
};

print OF <<ENDF;

</ul>

</body>
</html>

ENDF

close OF;

# arch-tag: 89455395-07c3-4a9a-a359-063d7b5883e6

