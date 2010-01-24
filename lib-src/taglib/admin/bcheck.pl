#!/usr/bin/perl -w

use DB_File;
use Fcntl ':flock';

if (!defined($ARGV[0])) {
    print "usage: requires .class dump as parameter!\n";
    exit;
}

sub bailout
{
    untie %bcheckdb if(defined(%bcheckdb));

    if(defined(MYLOCK)) {
        flock MYLOCK, LOCK_UN;
        close(MYLOCK);
    }

    print @_;
    exit 5;
}

sub ask_user
{
    my ($dbkey, $dbchunk) = @_;

    if (defined($ENV{"BCHECK_UPDATE"})) {
        $bcheckdb{$dbkey} = $dbchunk;
        return;
    }

    &bailout("BC problem detected") if (! -t STDIN);

    print "(I)gnore / (Q)uit / (U)pdate: ";

    my $key;
    while(defined(read STDIN, $key, 1)) {
        $key = lc($key);

        print "got: >$key<\n";

        return if ($key eq 'i');

        &bailout("BC problem. aborted") if ($key eq 'q');

        if ($key eq 'u') {
            $bcheckdb{$dbkey} = $dbchunk;
            return;
        }
        print "\n(I)gnore / (Q)uit / (U)pdate: ";
    }
}

sub diff_chunk($$)
{
    my ($oldl, $newl) = @_;
    my @old = split /^/m, $oldl;
    my @new = split /^/m, $newl;
    my $haschanges = 0;
    my $max = $#old > $#new ? $#old : $#new;

    die "whoops. key different" if ($old[0] ne $new[0]);

    if ($#old != $#new) {
        warn ("Structural difference.\n");
        print @old;
        print "-----------------------------------------------\n";
        print @new;
        $haschanges = 1;
        return $haschanges;
    }

    print $old[0];

    my ($class) = ($old[0] =~ /^(?:Class |Vtable for )(\S+)/);

    my $c = 1;
    while ($c < $max) {
        my ($o, $n) = ($old[$c], $new[$c]);
        chomp $o;
        chomp $n;
        $c++;
        next if ($o eq $n);

        if(defined($class) and $n =~ /^(\d+\s+)\w+(::\S+\s*.*)$/) {
            next if ($n eq "$1$class$2");
        }

        $haschanges = 1;

        print "-$o\n+$n\n\n";
    }

    return $haschanges;
}

local $dblock = $ENV{"HOME"} . "/bcheck.lock";
my $dbfile = $ENV{"HOME"} . "/bcheck.db";
my $cdump  = $ARGV[0];

die "file $cdump is not readable: $!" if (! -f $cdump);

# make sure the advisory lock exists
open(MYLOCK, ">$dblock");
print MYLOCK "";

flock MYLOCK, LOCK_EX;

tie %bcheckdb, 'DB_File', $dbfile;

my $chunk = "";

open (IN, "<$cdump") or die "cannot open $cdump: $!";
while (<IN>) {

    chop;

    s/0x[0-9a-fA-F]+/0x......../g;
    s/base size=/size=/g;
    s/\(\)\s*$//g;
    s/base align=/align=/g;

    $chunk .= $_ . "\n";

    if(/^\s*$/) {
        my @lines = split /^/m, $chunk;
        my $key = $lines[0];
        chomp $key;

        if($key !~ /<anonymous struct>/ &&
           $key !~ /<anonymous union>/) {
            if(defined($bcheckdb{$key})) {
                my $dbversion = $bcheckdb{$key};

                if($dbversion ne $chunk) {
                     &ask_user($key, $chunk) if(&diff_chunk($dbversion, $chunk));
                }
            }
            else {
                $bcheckdb{$key} = $chunk;
                print "NEW: $key\n";
            }
        }

        $chunk = "";
        next;
    }

}
close(IN);

untie %bcheckdb;
flock MYLOCK, LOCK_UN;
close(MYLOCK);

exit 0;
