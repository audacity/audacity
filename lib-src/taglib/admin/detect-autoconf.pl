#!/usr/bin/env perl

# Try to locate best version of auto*
# By Michael Pyne <michael.pyne@kdemail.net>
#
# Copyright (c) 2005.
# This code is public domain.  You may use it however you like (including
# relicensing).

# Emulate the 'which' program.
sub which
{
    my $prog = shift;
    my @paths = split(/:/, $ENV{'PATH'});

    for $path (@paths)
    {
	return "$path/$prog" if -x "$path/$prog";
    }

    return "";
}

# Subroutine to lexicographically compare two version strings, a and b.
# If a > b, 1 is returned.
# If a == b, 0 is returned.
# If a < b, -1 is returned.
#
# If the strings are of uneven number length then the shorter string is
# prepended by enough zeroes to make the two string lengths equal in order to
# allow an accurate comparison.  Note that the zero-padding only occurs in
# between version separators (i.e. 1.6 and 1.10, results in 1.06 vs. 1.10).
# Parts of the version ending in -foo (or any other text) are not considered
# when doing the compare. (i.e. 2.53a vs 2.53 doesn't end up in 2.53a vs.
# 2.053)
sub compareVersions
{
    my ($a, $b) = @_;

    # Split the strings up by '.' (version separator) and start comparing digit
    # length.

    my @aParts = split(/\./, $a);
    my @bParts = split(/\./, $b);

    # Make the arrays equal in length by adding missing zeroes to the end of the
    # version.
    push @aParts, '0' while scalar @aParts < scalar @bParts;
    push @bParts, '0' while scalar @bParts < scalar @aParts;

    # Now compare each individual portion.
    for (my $i = 0; $i < scalar @aParts; ++$i)
    {
	# Make sure that any portion that has numbers is contiguous.  I'm sure
	# there's a technique for saving stuff like 2.52a2 but I don't feel
	# like implementing it.
	if ($aParts[$i] !~ /^[^\d]*\d+[^\d]*$/ or
	    $bParts[$i] !~ /^[^\d]*\d+[^\d]*$/)
	{
	    die "Not able to compare $a to $b!\n";
	}

	my ($aDigits) = ($aParts[$i] =~ /(\d+)/);
	my ($bDigits) = ($bParts[$i] =~ /(\d+)/);

	# Perl is $MODERATELY_INSULTING_TERM, don't remove the parentheses in
	# the delta calculation below.
	my $delta = (length $aDigits) - (length $bDigits);
	if ($delta < 0) # b is longer
	{
	    my $replacement = ('0' x (-$delta)) . $aDigits;
	    $aParts[$i] =~ s/$aDigits/$replacement/;
	}
	elsif ($delta > 0) # a is longer
	{
	    my $replacement = ('0' x $delta) . $bDigits;
	    $bParts[$i] =~ s/$bDigits/$replacement/;
	}
    }

    # Arrays now have standardized version components, let's re-merge them
    # to strings to do the compare.
    my $newA = join('.', @aParts);
    my $newB = join('.', @bParts);

    return 1 if ($newA gt $newB);
    return -1 if ($newA lt $newB);
    return 0;
}

# Subroutine to determine the highest installed version of the given program,
# searching from the given paths.
sub findBest
{
    my ($program, @paths) = @_;
    my $best_version_found = '0'; # Deliberately a string.
    my %versions;
    my %minimumVersions = (
	'autoconf' => '2.5',
	'automake' => '1.6',
    );
    my $sgn; # Used for compareVersions results.

    # Allow user to use environment variable to override search.
    return $ENV{uc $program} if $ENV{uc $program};

    for $prefix (@paths)
    {
	@files = glob "$prefix/$program*";
	for $file (@files)
	{
	    # Don't check non-executable scripts.
	    next unless -x $file;

	    ($version) = $file =~ /$prefix\/$program-?(.*)$/;

	    # Don't check the -wrapper ones (or any other non program one).
	    # The real deal should start with a version number, or have no
	    # suffix at all.
	    next if $version =~ /^[^\d]/;

	    # Special case some programs to make sure it has a minimum version.
	    if (not $version and exists $minimumVersions{$program})
	    {
		my $min_version = $minimumVersions{$program};
		my $versionOutput = `$program --version 2>/dev/null | head -n 1`;

		# If we can't run the script to get the version it likely won't work later.
		next unless $versionOutput;

		# Use number.number for version (we don't need the excess in general).
		($versionOutput) = ($versionOutput =~ /(\d+\.\d+)/);

		# compareVersions returns -1 if the left argument is less than
		# the right argument.  It can also die for invalid input so
		# wrap with eval.
		eval {
		    $sgn = compareVersions($versionOutput, $min_version);
		};

		# $@ would be set if an error was encountered.
		if ($@ or not $versionOutput or $sgn == -1) {
		    next;
		}
	    }

	    # If no version suffix then use it in favor of a versioned autotool
	    # since the ever-popular WANT_AUTOFOO should then work (in theory).
	    return $file unless $version;

	    # Emulate 'which', and abort if we've already seen this version.
	    next if exists $versions{$version};

	    # Save filename of program.
	    $versions{$version} = $file;

	    # Use string comparison so that e.g. 253a will be > 253 but < 254.
	    # See above about the need for eval.
	    eval {
		$sgn = compareVersions($version, $best_version_found);
	    };

	    if (not $@ and $sgn == 1)
	    {
		$best_version_found = $version;
	    }
	}
    }

    return $versions{$best_version_found};
}

# Find an appropriate "which" program for later use by the shell script calling
# us.
sub findWhich
{
    for $candidate ('type -p', 'which', 'type')
    {
	$test = `$candidate sh 2>/dev/null`;
	chomp $test;

	return $candidate if -x $test;
    }
}

# Uses which() to find a program unless the user provided its path in the
# environment (the upper case program name is searched).
sub findProgram
{
    $suffix = ""; # For use if @_ has only one param.
    my ($program, $suffix) = @_;

    return $ENV{uc $program} if $ENV{uc $program};
    return which("$program$suffix");
}

# SCRIPT STARTS.

# Search in path.
@paths = split(/:/, $ENV{'PATH'});

# Make sure at least /usr/bin and /usr/local/bin are in this search.
unshift @paths, '/usr/local/bin' unless grep $_ eq '/usr/local/bin', @paths;
unshift @paths, '/usr/bin' unless grep $_ eq '/usr/bin', @paths;

$autoconf = findBest('autoconf', @paths);
($autoconf_suffix) = $autoconf =~ /.*autoconf(.*)$/;

# Find matching autoconf companions.
$autoheader = findProgram('autoheader', $autoconf_suffix);
$autom4te = findProgram('autom4te', $autoconf_suffix);

# Get best automake, and look for unsermake to possibly override it.
$automake = findBest('automake', @paths);
$unsermake = "";
# backward compatible: if $UNSERMAKE points to a path, use it
$unsermake = findProgram('unsermake') if (defined($ENV{'UNSERMAKE'}) and $ENV{'UNSERMAKE'} =~ /\//);
# new compatible: if it says 'yes', use the one from path
$unsermake = which('unsermake') if ($ENV{'UNSERMAKE'} ne 'no');

($automake_suffix) = $automake =~ /.*automake(.*)$/;

# Find matching automake companions.
$aclocal = findProgram('aclocal', $automake_suffix);

# Use unsermake if we found it.
$automake = "$unsermake -c" if ($unsermake and $aclocal);

$which = findWhich();

# Make sure we have all of the needed programs.
for $i (qw'autoconf autoheader autom4te automake aclocal')
{
    unless(${$i})
    {
	print STDERR "# Unable to find $i!!\n";
    }
}

# Print results in eval-able form.
print <<EOF;
AUTOCONF="$autoconf"
AUTOHEADER="$autoheader"
AUTOM4TE="$autom4te"

AUTOMAKE="$automake"
ACLOCAL="$aclocal"

WHICH="$which"

export AUTOCONF AUTOHEADER AUTOM4TE AUTOMAKE ACLOCAL WHICH
EOF

exit 0;

# vim: set noet ts=8 sw=4:
