## Audacity Loudness effect unit test
#
# Max Maisel
#
# This tests the Loudness effect with 30 seconds long pseudo-random stereo
# noise sequences. The test sequences have different amplitudes per
# channel and sometimes a DC component. For best test coverage, irrelevant
# parameters for the current operation are randomly varied.
#

printf("Running Loudness effect tests.\n");
printf("This requires the octave-forge-signal package to be installed.\n");

pkg load signal;

EXPORT_TEST_SIGNALS = true;
TEST_LUFS_HELPER = true;
# LUFS need a higher epsilon because they are a logarithmic unit.
LUFS_epsilon = 0.02;

# A straightforward and simple LUFS implementation which can
# be easily compared with the specification ITU-R BS.1770-4.
function [gated_lufs] = calc_LUFS(x, fs)
  # HSF
  f0 = 38.13547087602444;
  Q  =  0.5003270373238773;
  K  = tan(pi * f0 / fs);

  rb0 = 1.0;
  rb1 = -2.0;
  rb2 = 1.0;
  ra0 = 1.0;
  ra1 = 2.0 * (K * K - 1.0) / (1.0 + K / Q + K * K);
  ra2 = (1.0 - K / Q + K * K) / (1.0 + K / Q + K * K);

  rb = [rb0 rb1 rb2];
  ra = [ra0 ra1 ra2];

  # HPF
  db = 3.999843853973347;
  f0 = 1681.974450955533;
  Q  = 0.7071752369554196;
  K  = tan(pi * f0 / fs);
  Vh = power(10.0, db / 20.0);
  Vb = power(Vh, 0.4996667741545416);

  pa0 = 1.0;
  a0 =      1.0 + K / Q + K * K;
  pb0 =     (Vh + Vb * K / Q + K * K) / a0;
  pb1 =           2.0 * (K * K -  Vh) / a0;
  pb2 =     (Vh - Vb * K / Q + K * K) / a0;
  pa1 =           2.0 * (K * K - 1.0) / a0;
  pa2 =         (1.0 - K / Q + K * K) / a0;

  pb = [pb0 pb1 pb2];
  pa = [pa0 pa1 pa2];

  # Apply k-weighting
  x = filter(rb, ra, x, [], 1);
  x = filter(pb, pa, x, [], 1);

  # - gating blocks (every 100 ms over 400 ms)
  block_size    = 0.4*fs;
  block_overlap = 0.3*fs;
  block_count   = floor((size(x)(1)-block_size)/(block_size-block_overlap))+1+1;

  x_blocked        = zeros(block_size, block_count, size(x)(2));
  for i=1:1:size(x)(2)
    x_blocked(:,:,i) = buffer(x(:,i), block_size, 0.3*fs, 'nodelay');
  end

  lufs_blocked = 1/(block_size)*sum(x_blocked.^2, 1);
  lufs_blocked = sum(lufs_blocked, 3);

  # Apply absolute threshold
  GAMMA_A = -70;
  lufs_blocked = -0.691 + 10*log10(lufs_blocked);
  valid_blocks = length(lufs_blocked);
  valid_blocks = valid_blocks - length(lufs_blocked(lufs_blocked < GAMMA_A));
  lufs_blocked(lufs_blocked < GAMMA_A) = -100;
  lufs_blocked = 10.^((lufs_blocked+0.691)/10);

  # Apply relative threshold
  GAMMA_R = -0.691 + 10*log10(sum(lufs_blocked)/valid_blocks) - 10;
  lufs_blocked = -0.691 + 10*log10(lufs_blocked);
  valid_blocks = length(lufs_blocked);
  valid_blocks = valid_blocks - length(lufs_blocked(lufs_blocked < GAMMA_R));
  lufs_blocked(lufs_blocked < GAMMA_R) = -100;
  lufs_blocked = 10.^((lufs_blocked+0.691)/10);
  hold off

  gated_lufs = -0.691 + 10*log10(sum(lufs_blocked)/valid_blocks);
end

if TEST_LUFS_HELPER
  printf("Running calc_LUFS() selftest.\n");
  printf("Compare the following results with a trusted LUFS calculator.\n");

  fs = 44100;
  k  = 1:1:60*fs;
  x  = 0.3*sin(2*pi*1000/fs*k) + 0.2*sin(2*pi*1200/fs*k);
  x  = (x .* [1:1:30*fs, 30*fs:-1:1]./60./fs).';

  audiowrite(cstrcat(pwd(), "/LUFS-selftest1.wav"), x, fs);
  printf("LUFS-selftest1.wav should be %f LUFS\n", calc_LUFS(x, fs));

  randn("seed", 1);
  x = [0.2*randn(2, 10*fs) zeros(2, 10*fs) 0.1*randn(2, 10*fs)].';
  x(:,1) = x(:,1) * 0.4 + 0.2;

  audiowrite(cstrcat(pwd(), "/LUFS-selftest2.wav"), x, fs);
  printf("LUFS-selftest2.wav should be %f LUFS\n", calc_LUFS(x, fs));

  fs = 8000;
  randn("seed", 2);
  x = [0.2*randn(2, 10*fs) zeros(2, 10*fs) 0.1*randn(2, 10*fs)].';
  x(:,1) = x(:,1) * 0.6 - 0.1;

  # MMM: I'm not sure how trustworthy free loudness meters are
  #      in case of non-standard sample rates.
  audiowrite(cstrcat(pwd(), "/LUFS-selftest3.wav"), x, fs);
  printf("LUFS-selftest3.wav should be %f LUFS\n", calc_LUFS(x, fs));
end

## Test Loudness LUFS mode: block to short and all silent
CURRENT_TEST = "Loudness LUFS mode, short silent block";
fs= 44100;
x = zeros(ceil(fs*0.35), 2);
audiowrite(TMP_FILENAME, x, fs);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Loudness-LUFS-silence-test.wav"), x, fs);
end

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-23 DualMono=1 NormalizeTo=0 StereoIndependent=0\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test_equ(y, x, "identity");

## Test Loudness LUFS mode: stereo dependent
CURRENT_TEST = "Loudness LUFS mode, keep DC and stereo balance";
randn("seed", 1);
# Include some silence in the test signal to test loudness gating
# and vary the overall loudness over time.
x = [0.1*randn(15*fs, 2).', zeros(5*fs, 2).', 0.1*randn(15*fs, 2).'].';
x(:,1) = x(:,1) .* sin(2*pi/fs/35*(1:1:35*fs)).' .* 1.2;
x(:,2) = x(:,2) .* sin(2*pi/fs/35*(1:1:35*fs)).';
audiowrite(TMP_FILENAME, x, fs);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Loudness-LUFS-stereo-test.wav"), x, fs);
end

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-23 DualMono=1 NormalizeTo=0 StereoIndependent=0\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test_equ(calc_LUFS(y, fs), -23, "loudness", LUFS_epsilon);
do_test_neq(calc_LUFS(y(:,1), fs), calc_LUFS(y(:,2), fs), "stereo balance", 1);

## Test Loudness LUFS mode, stereo independent
CURRENT_TEST = "Loudness LUFS mode, stereo independence";
audiowrite(TMP_FILENAME, x, fs);
remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-23 DualMono=0 NormalizeTo=0 StereoIndependent=1\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
# Independently processed stereo channels have half the target loudness.
do_test_equ(calc_LUFS(y(:,1), fs), -26, "channel 1 loudness", LUFS_epsilon);
do_test_equ(calc_LUFS(y(:,2), fs), -26, "channel 2 loudness", LUFS_epsilon);

## Test Loudness LUFS mode: mono as mono
CURRENT_TEST = "Test Loudness LUFS mode: mono as mono";
x = x(:,1);
audiowrite(TMP_FILENAME, x, fs);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Loudness-LUFS-mono-test.wav"), x, fs);
end

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-26 DualMono=0 NormalizeTo=0 StereoIndependent=1\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=1\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test_equ(calc_LUFS(y, fs), -26, "loudness", LUFS_epsilon);

## Test Loudness LUFS mode: mono as dual-mono
CURRENT_TEST = "Test Loudness LUFS mode: mono as dual-mono";
audiowrite(TMP_FILENAME, x, fs);

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-26 DualMono=1 NormalizeTo=0 StereoIndependent=0\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=1\n"));
system("sync");

y = audioread(TMP_FILENAME);
# This shall be 3 LU quieter as it is compared to strict spec.
do_test_equ(calc_LUFS(y, fs), -29, "loudness", LUFS_epsilon);

## Test Loudness LUFS mode: multi-rate project
CURRENT_TEST = "Test Loudness LUFS mode: multi-rate project";
audiowrite(TMP_FILENAME, x, fs);

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));

randn("seed", 2);
fs1= 8000;
x1 = [0.2*randn(2, 10*fs1) zeros(2, 10*fs1) 0.1*randn(2, 10*fs1)].';
x1(:,1) = x1(:,1) * 0.6;
audiowrite(TMP_FILENAME, x1, fs1);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Loudness-LUFS-stereo-test-8kHz.wav"), x1, fs1);
end

aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: LUFSLevel=-30 DualMono=0 NormalizeTo=0 StereoIndependent=0\n");

select_tracks(0, 1);
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=1\n"));
system("sync");
y = audioread(TMP_FILENAME);

select_tracks(1, 1);
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");
y1 = audioread(TMP_FILENAME);

do_test_equ(calc_LUFS(y, fs),   -30, "loudness track 1", LUFS_epsilon);
# XXX: Audacity does not export at 8kHz through scripting thus this test is expected to fail!
#      To ensure that this works you have to set the project rate to 8 kHz,
#      export the track and check the results manually.
do_test_equ(calc_LUFS(y1, fs1), -30, "loudness track 2", LUFS_epsilon, true);
# No stereo balance check for track 1 - it's a mono track.
do_test_neq(calc_LUFS(y1(:,1), fs), calc_LUFS(y1(:,2), fs), "stereo balance track 2", LUFS_epsilon);

## Test Loudness RMS mode: stereo independent
CURRENT_TEST = "Loudness RMS mode, stereo independent";
randn("seed", 1);
fs= 44100;
x = 0.1*randn(30*fs, 2);
x(:,1) = x(:,1) * 0.6;
audiowrite(TMP_FILENAME, x, fs);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Loudness-RMS-test.wav"), x, fs);
end

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: RMSLevel=-20 DualMono=0 NormalizeTo=1 StereoIndependent=1\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test_equ(20*log10(sqrt(sum(y(:,1).*y(:,1)/length(y)))), -20, "channel 1 RMS");
do_test_equ(20*log10(sqrt(sum(y(:,2).*y(:,2)/length(y)))), -20, "channel 2 RMS");

## Test Loudness RMS mode: stereo dependent
CURRENT_TEST = "Loudness RMS mode, stereo dependent";
audiowrite(TMP_FILENAME, x, fs);

remove_all_tracks();
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
select_tracks(0, 100);
aud_do("LoudnessNormalization: RMSLevel=-22 DualMono=1 NormalizeTo=1 StereoIndependent=0\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
# Stereo RMS must be calculated in quadratic domain.
do_test_equ(20*log10(sqrt(sum(rms(y).^2)/size(y)(2))), -22, "RMS");
do_test_neq(20*log10(rms(y(:,1))), 20*log10(rms(y(:,2))), "stereo balance", 1);

