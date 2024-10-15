#!/usr/bin/perl

use warnings;
use strict;

use Digest::MD5 qw(md5_hex);
use Test::More tests => 77;

my $TWOLAME_CMD = $ENV{TWOLAME_CMD} || "../frontend/twolame";
my $STWOLAME_CMD = $ENV{STWOLAME_CMD} || "../simplefrontend/stwolame";
die "Error: twolame command not found: $TWOLAME_CMD" unless (-e $TWOLAME_CMD);
die "Error: stwolame command not found: $STWOLAME_CMD" unless (-e $STWOLAME_CMD);


my $encoding_parameters = [
  {
    # Test Case 1 (default settings)
    'input_filename' => 'testcase-44100.wav',
    'input_md5sum' => 'f50499fded70a74c810dbcadb3f28062',
    'bitrate' => 192,
    'samplerate' => 44100,
    'version' => '1',
    'mode' => 'stereo',
    'psycmode' => 3,
    'original' => 1,
    'copyright' => 0,
    'padding' => 0,
    'protect' => 0,
    'deemphasis' => 'n',
    'total_frames' => 22,
    'total_bytes' => 13772,
    'total_samples' => 25344,
    'output_md5sum' => '956f85e3647314750a1d3ed3fbf81ae3'
  },
  {
    # Test Case 2 (toolame 0.2l default settings)
    'input_filename' => 'testcase-44100.wav',
    'input_md5sum' => 'f50499fded70a74c810dbcadb3f28062',
    'bitrate' => 192,
    'samplerate' => 44100,
    'version' => '1',
    'mode' => 'joint',
    'psycmode' => 1,
    'original' => 0,
    'copyright' => 0,
    'padding' => 1,
    'protect' => 0,
    'deemphasis' => 'n',
    'total_frames' => 22,
    'total_bytes' => 13792,
    'total_samples' => 25344,
    'output_md5sum' => 'fef3bb4926978e56822d33eaa89208d2'
  },
  {
    # Test Case 3 (MPEG-2 test)
    'input_filename' => 'testcase-22050.wav',
    'input_md5sum' => 'a5ec3077c2138a1023bcd980aec8e4b4',
    'bitrate' => 32,
    'samplerate' => 22050,
    'version' => '2',
    'mode' => 'mono',
    'psycmode' => 4,
    'original' => 0,
    'copyright' => 1,
    'padding' => 1,
    'protect' => 0,
    'deemphasis' => '5',
    'total_frames' => 11,
    'total_bytes' => 2298,
    'total_samples' => 12672,
    'output_md5sum' => '3175f5332040aaad42b00823cd1ec913'
  },
  {
    # Test Case 4 (error protection test)
    'input_filename' => 'testcase-44100.wav',
    'input_md5sum' => 'f50499fded70a74c810dbcadb3f28062',
    'bitrate' => 192,
    'samplerate' => 44100,
    'version' => '1',
    'mode' => 'stereo',
    'psycmode' => 3,
    'original' => 1,
    'copyright' => 0,
    'padding' => 0,
    'protect' => 1,
    'deemphasis' => 'n',
    'total_frames' => 22,
    'total_bytes' => 13772,
    'total_samples' => 25344,
    'output_md5sum' => '7e7aa8e3cfafdd1cd2eda53a9ab8bef3'
  },
];


my $count = 1;
foreach my $params (@$encoding_parameters) {
  my $INPUT_FILENAME = input_filepath($params->{input_filename});
  my $OUTPUT_FILENAME = "testcase-$count.mp2";

  die "Input file does not exist: $INPUT_FILENAME" unless (-e $INPUT_FILENAME);
  is(md5_file($INPUT_FILENAME), $params->{input_md5sum}, "[$count] MD5sum of $INPUT_FILENAME");

  my $result = system($TWOLAME_CMD,
    '--quiet',
    '--bitrate', $params->{bitrate},
    '--mode', $params->{mode},
    '--psyc-mode', $params->{psycmode},
    $params->{copyright} ? '--copyright' : '--non-copyright',
    $params->{original} ? '--original' : '--non-original',
    $params->{protect} ? '--protect' : '',
    $params->{padding} ? '--padding' : '',
    '--deemphasis', $params->{deemphasis},
    $INPUT_FILENAME, $OUTPUT_FILENAME
  );

  is($result, 0, "[$count] twolame response code");

  my $info = mpeg_audio_info($OUTPUT_FILENAME);
  is($info->{syncword}, 0xff, "[$count] MPEG Audio Header - Sync Word");
  is($info->{version}, $params->{version}, "[$count] MPEG Audio Header - Version");
  is($info->{layer}, 2, "[$count] MPEG Audio Header - Layer");
  is($info->{mode}, $params->{mode}, "[$count] MPEG Audio Header - Mode");
  is($info->{samplerate}, $params->{samplerate}, "[$count] MPEG Audio Header - Sample Rate");
  is($info->{bitrate}, $params->{bitrate}, "[$count] MPEG Audio Header - Bitrate");
  is($info->{copyright}, $params->{copyright}, "[$count] MPEG Audio Header - Copyright Flag");
  is($info->{original}, $params->{original}, "[$count] MPEG Audio Header - Original Flag");
  is($info->{protect}, $params->{protect}, "[$count] MPEG Audio Header - Error Protection Flag");
  is($info->{deemphasis}, $params->{deemphasis}, "[$count] MPEG Audio Header - De-emphasis");

  # FIXME: test that CRC is correct

  is($info->{total_frames}, $params->{total_frames}, "[$count] total number of frames");
  is($info->{total_bytes}, $params->{total_bytes}, "[$count] total number of bytes");
  is($info->{total_samples}, $params->{total_samples}, "[$count] total number of samples");

  is(filesize($OUTPUT_FILENAME), $params->{total_bytes}, , "[$count] file size of output file");
  is(md5_file($OUTPUT_FILENAME), $params->{output_md5sum}, "[$count] md5sum of output file");

  $count++;
}


# Test encoding from STDIN
SKIP: {
  my $result = system("which sndfile-convert > /dev/null");
  skip("sndfile-convert is not available", 5) unless ($result == 0);

  my $INPUT_FILENAME = input_filepath('testcase-44100.wav');
  $result = system("sndfile-convert -pcm16 $INPUT_FILENAME testcase.raw");
  is($result, 0, "sndfile-convert to raw response code");

  my $OUTPUT_FILENAME = 'testcase-stdin.mp2';
  $result = system("$TWOLAME_CMD --quiet --raw-input - $OUTPUT_FILENAME < testcase.raw");
  is($result, 0, "converting from STDIN - response code");

  my $info = mpeg_audio_info($OUTPUT_FILENAME);
  is($info->{total_frames}, 22, "converting from STDIN - total number of frames");
  is($info->{total_bytes}, 13772, "converting from STDIN - total number of bytes");
  is(md5_file($OUTPUT_FILENAME), '956f85e3647314750a1d3ed3fbf81ae3', "converting from STDIN - md5sum of output file");
}


# Test encoding using the simplefrontend
{
  my $INPUT_FILENAME = input_filepath('testcase-44100.wav');
  my $OUTPUT_FILENAME = 'testcase-simple.mp2';
  my $result = system("../simplefrontend/stwolame $INPUT_FILENAME $OUTPUT_FILENAME");
  is($result, 0, "converting using simplefrontend - response code");

  my $info = mpeg_audio_info($OUTPUT_FILENAME);
  is($info->{total_frames}, 22, "converting using simplefrontend - total number of frames");
  is($info->{total_bytes}, 13772, "converting using simplefrontend - total number of bytes");
  is(md5_file($OUTPUT_FILENAME), '956f85e3647314750a1d3ed3fbf81ae3', "converting using simplefrontend - md5sum of output file");
}


## END OF TESTS ##

sub input_filepath {
  # Input test data files are in the same directory as the test script
  my ($filename) = @_;
  my $filepath = __FILE__;
  $filepath =~ s/test.pl/$filename/;
  return $filepath;
}

sub filesize {
  return (stat(@_))[7];
}

sub md5_file {
  my ($filename) = @_;

  my $ctx = Digest::MD5->new;

  open(FILE, $filename) or die "Failed to open file: $filename ($!)";
  $ctx->addfile(*FILE);
  close(FILE);

  return $ctx->hexdigest;
}

sub mpeg_audio_info {
  my ($filename) = @_;
  my $info = undef;

  open(MPAFILE, $filename) or die "Failed to open file: $filename ($!)";

  until (eof(MPAFILE)) {
    my $header = '';
    my $bytes = read(MPAFILE, $header, 4);
    if ($bytes != 4) {
      warn "Failed to read MPEG Audio header";
      last;
    }

    my $frame_info = parse_mpeg_header($header);
    if ($frame_info->{syncword} != 0xff) {
      warn "Lost MPEG Audio header sync";
      last;
    }

    # Now read in the rest of the frame
    my $buffer = '';
    my $remaining = ($frame_info->{framesize}-4);
    $bytes = read(MPAFILE, $buffer, $remaining);
    if ($bytes != $remaining) {
      warn "Failed to read remaining buts of MPEG Audio frame";
      last;
    }

    if ($frame_info->{protect}) {
      $frame_info->{crc} = unpack('n', $buffer);
    }

    $info = $frame_info unless ($info);
    $info->{total_frames} += 1;
    $info->{total_samples} += $frame_info->{samples};
    $info->{total_bytes} += $frame_info->{framesize};
  };

  close(MPAFILE);

  return $info;
}

sub parse_mpeg_header {
  my ($buffer) = @_;
  my $header = unpack('N', $buffer);
  my $info = {};

  my $bitrate_table = [
    [ # MPEG 1
      [0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448], # Layer 1
      [0, 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384], # Layer 2
      [0, 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320]  # Layer 3
    ],
    [ # MPEG 2
      [0, 32, 48, 56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256], # Layer 1
      [0,  8, 16, 24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160], # Layer 2
      [0,  8, 16, 24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160]  # Layer 3
    ],
    [ # MPEG 2.5
      [0, 32, 48, 56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256], # Layer 1
      [0,  8, 16, 24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160], # Layer 2
      [0,  8, 16, 24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160]  # Layer 3
    ]
  ];

  my $samplerate_table = [
    [ 44100, 48000, 32000 ], # MPEG 1
    [ 22050, 24000, 16000 ], # MPEG 2
    [ 11025, 12000,  8000 ]  # MPEG 2.5
  ];

  $info->{syncword} = ($header >> 23) & 0xff;

  my $version = (($header >> 19) & 0x03);
  if ($version == 0x00) {
    $info->{version} = '2.5';   # MPEG 2.5
  } elsif ($version == 0x02) {
    $info->{version} = '2';   # MPEG 2
  } elsif ($version == 0x03) {
    $info->{version} = '1';   # MPEG 1
  } else {
    $info->{version} = undef;
  }

  $info->{layer} = 4-(($header >> 17) & 0x03);
  if ($info->{layer}==4) {
    $info->{layer} = 0;
  }

  $info->{protect} = (($header >> 16) & 0x01) ? 0 : 1;
  $info->{padding} = ($header >> 9) & 0x01;
  $info->{extension} = ($header >> 8) & 0x01;
  $info->{mode_ext} = ($header >> 4) & 0x03;
  $info->{copyright} = ($header >> 3) & 0x01;
  $info->{original} = ($header >> 2) & 0x01;

  my $bitrate_index = ($header >> 12) & 0x0F;
  my $samplerate_index = ($header >> 10) & 0x03;
  if ($info->{layer} && $info->{version}) {
    $info->{bitrate} = $bitrate_table->[$info->{version}-1]->[$info->{layer}-1][$bitrate_index];
    $info->{samplerate} = $samplerate_table->[$info->{version}-1]->[$samplerate_index];
  } else {
    $info->{bitrate} = undef;
    $info->{samplerate} = undef;
  }

  my $deemphasis = $header & 0x03;
  if ($deemphasis == 0) {
    $info->{deemphasis} = 'n';    # None
  } elsif ($deemphasis == 1) {
    $info->{deemphasis} = '5';    # 50/15 ms
  } elsif ($deemphasis == 3) {
    $info->{deemphasis} = 'c';    # CCITT J.17
  } else {
    $info->{deemphasis} = undef;
  }

  my $mode = ($header >> 6) & 0x03;
  if ($mode == 0) {
    $info->{mode} = 'stereo';
  } elsif ($mode == 1) {
    $info->{mode} = 'joint';
  } elsif ($mode == 2) {
    $info->{mode} = 'dual';
  } elsif ($mode == 3) {
    $info->{mode} = 'mono';
  } else {
    $info->{mode} = undef;
  }

  if ($info->{layer} == '1') {
    $info->{samples} = 384;
  } elsif ($info->{layer} == '2') {
    $info->{samples} = 1152;
  } elsif ($info->{layer} == '3') {
    $info->{samples} = ($info->{version} eq '1') ? 1152 : 576;
  }

  if ($info->{samplerate}) {
    $info->{framesize} = int(($info->{samples} * $info->{bitrate} * 1000 / $info->{samplerate}) / 8 + $info->{padding});
  } else {
    $info->{framesize} = undef;
  }

  return $info;
}

