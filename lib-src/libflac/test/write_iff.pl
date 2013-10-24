#!/usr/bin/perl -w

use strict;

require Math::BigInt;

my $usage = "
$0 <format> <bps> <channels> <sample-rate> <#samples> <sample-type>

     <format> is one of aiff,wave,wave64,rf64
        <bps> is 8,16,24,32
   <channels> is 1-8
<sample-rate> is any 32-bit value
   <#samples> is 0-2^64-1
<sample-type> is one of zero,rand

";

die $usage unless @ARGV == 6;

my %formats = ( 'aiff'=>1, 'wave'=>1, 'wave64'=>1, 'rf64'=>1 );
my %sampletypes = ( 'zero'=>1, 'rand'=>1 );
my @channelmask = ( 0, 1, 3, 7, 0x33, 0x607, 0x60f, 0, 0 ); #@@@@@@ need proper masks for 7,8

my ($format, $bps, $channels, $samplerate, $samples, $sampletype) = @ARGV;
my $bigsamples = new Math::BigInt $samples;

die $usage unless defined $formats{$format};
die $usage unless $bps == 8 || $bps == 16 || $bps == 24 || $bps == 32;
die $usage unless $channels >= 1 && $channels <= 8;
die $usage unless $samplerate >= 0 && $samplerate <= 4294967295;
die $usage unless defined $sampletypes{$sampletype};

# convert bits-per-sample to bytes-per-sample
$bps /= 8;

my $datasize = $samples * $bps * $channels;
my $bigdatasize = $bigsamples * $bps * $channels;

my $padding = int($bigdatasize & 1); # for aiff/wave/rf64 chunk alignment
my $padding8 = 8 - int($bigdatasize & 7); $padding8 = 0 if $padding8 == 8; # for wave64 alignment
# wave-ish file needs to be WAVEFORMATEXTENSIBLE?
my $wavx = ($format eq 'wave' || $format eq 'wave64' || $format eq 'rf64') && ($channels > 2);

# write header

if ($format eq 'aiff') {
	die "sample data too big for format\n" if 46 + $datasize + $padding > 4294967295;
	# header
	print "FORM";
	print pack('N', 46 + $datasize + $padding);
	print "AIFF";
	# COMM chunk
	print "COMM";
	print pack('N', 18); # chunk size = 18
	print pack('n', $channels);
	print pack('N', $samples);
	print pack('n', $bps * 8);
	print pack_sane_extended($samplerate);
	# SSND header
	print "SSND";
	print pack('N', $datasize + 8); # chunk size
	print pack('N', 0); # ssnd_offset_size
	print pack('N', 0); # blocksize
}
elsif ($format eq 'wave' || $format eq 'wave64' || $format eq 'rf64') {
	die "sample data too big for format\n" if $format eq 'wave' && ($wavx?60:36) + $datasize + $padding > 4294967295;
	# header
	if ($format eq 'wave') {
		print "RIFF";
		# +4 for WAVE
		# +8+{40,16} for fmt chunk
		# +8 for data chunk header
		print pack('V', 4 + 8+($wavx?40:16) + 8 + $datasize + $padding);
		print "WAVE";
	}
	elsif ($format eq 'wave64') {
		# RIFF GUID 66666972-912E-11CF-A5D6-28DB04C10000
		print "\x72\x69\x66\x66\x2E\x91\xCF\x11\xD6\xA5\x28\xDB\x04\xC1\x00\x00";
		# +(16+8) for RIFF GUID + size
		# +16 for WAVE GUID
		# +16+8+{40,16} for fmt chunk
		# +16+8 for data chunk header
		my $bigriffsize = $bigdatasize + (16+8) + 16 + 16+8+($wavx?40:16) + (16+8) + $padding8;
		print pack_64('V', $bigriffsize);
		# WAVE GUID 65766177-ACF3-11D3-8CD1-00C04F8EDB8A
		print "\x77\x61\x76\x65\xF3\xAC\xD3\x11\xD1\x8C\x00\xC0\x4F\x8E\xDB\x8A";
	}
	else {
		print "RF64";
		print pack('V', 0xffffffff);
		print "WAVE";
		# ds64 chunk
		print "ds64";
		print pack('V', 28); # chunk size
		# +4 for WAVE
		# +(8+28) for ds64 chunk
		# +8+{40,16} for fmt chunk
		# +8 for data chunk header
		my $bigriffsize = $bigdatasize + 4 + (8+28) + 8+($wavx?40:16) + 8 + $padding;
		print pack_64('V', $bigriffsize);
		print pack_64('V', $bigdatasize);
		print pack_64('V', $bigsamples);
		print pack('V', 0); # table size
	}
	# fmt chunk
	if ($format ne 'wave64') {
		print "fmt ";
		print pack('V', $wavx?40:16); # chunk size
	}
	else { # wave64
		# fmt GUID 20746D66-ACF3-11D3-8CD1-00C04F8EDB8A
		print "\x66\x6D\x74\x20\xF3\xAC\xD3\x11\xD1\x8C\x00\xC0\x4F\x8E\xDB\x8A";
		print pack('V', 16+8+($wavx?40:16)); # chunk size (+16+8 for GUID and size fields)
		print pack('V', 0);                  # ...is 8 bytes for wave64
	}
	print pack('v', $wavx?65534:1); # compression code
	print pack('v', $channels);
	print pack('V', $samplerate);
	print pack('V', $samplerate * $channels * $bps);
	print pack('v', $channels * $bps); # block align = channels*((bps+7)/8)
	print pack('v', $bps * 8); # bits per sample = ((bps+7)/8)*8
	if ($wavx) {
		print pack('v', 22); # cbSize
		print pack('v', $bps * 8); # validBitsPerSample
		print pack('V', $channelmask[$channels]);
		# GUID = {0x00000001, 0x0000, 0x0010, {0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71}}
		print "\x01\x00\x00\x00\x00\x00\x10\x00\x80\x00\x00\xaa\x00\x38\x9b\x71";
	}
	# data header
	if ($format ne 'wave64') {
		print "data";
		print pack('V', $format eq 'wave'? $datasize : 0xffffffff);
	}
	else { # wave64
		# data GUID 61746164-ACF3-11D3-8CD1-00C04F8EDB8A
		print "\x64\x61\x74\x61\xF3\xAC\xD3\x11\xD1\x8C\x00\xC0\x4F\x8E\xDB\x8A";
		print pack_64('V', $bigdatasize+16+8); # +16+8 for GUID and size fields
	}
}
else {
	die;
}

# write sample data

if ($sampletype eq 'zero') {
	my $chunk = 4096;
	my $buf = pack("x[".($channels*$bps*$chunk)."]");
	for (my $s = $samples; $s > 0; $s -= $chunk) {
		if ($s < $chunk) {
			print substr($buf, 0, $channels*$bps*$s);
		}
		else {
			print $buf;
		}
	}
}
elsif ($sampletype eq 'rand') {
	for (my $s = 0; $s < $samples; $s++) {
		for (my $c = 0; $c < $channels; $c++) {
			for (my $b = 0; $b < $bps; $b++) {
				print pack('C', int(rand(256)));
			}
		}
	}
}
else {
	die;
}

# write padding
if ($format eq 'wave64') {
	print pack("x[$padding8]") if $padding8;
}
else {
	print "\x00" if $padding;
}

exit 0;

sub pack_sane_extended
{
	my $val = shift;
	die unless $val > 0;
	my $shift;
	for ($shift = 0; ($val>>(31-$shift)) == 0; ++$shift) {
	}
	$val <<= $shift;
	my $exponent = 63 - ($shift + 32);
	return pack('nNN', $exponent + 16383, $val, 0);
}

sub pack_64
{
	my $c = shift; # 'N' for big-endian, 'V' for little-endian, ala pack()
	my $v1 = shift; # value, must be Math::BigInt
	my $v2 = $v1->copy();
	if ($c eq 'V') {
		$v1->band(0xffffffff);
		$v2->brsft(32);
	}
	elsif ($c eq 'N') {
		$v2->band(0xffffffff);
		$v1->brsft(32);
	}
	else {
		die;
	}
	return pack("$c$c", 0+$v1->bstr(), 0+$v2->bstr());
}
