## Audacity Normalize effect unit test
#
# Max Maisel
#
# This tests the Normalize effect with 30 seconds long pseudo-random stereo
# noise sequences. The test sequences have different amplitudes per
# channel and sometimes a DC component.
#
# Note: The isnan tests can only detect errors if audio is exported
#       as 32bit float wav files.

printf("Running Normalize effect tests.\n");

EXPORT_TEST_SIGNALS = false;

## Test Normalize, keep DC and stereo balance
CURRENT_TEST = "Normalize, keep DC and stereo balance";
randn("seed", 1);
fs = 44100;
x = 0.1*randn(30*fs, 2);
x(:,1) = x(:,1) * 0.4 + 0.2;
audiowrite(TMP_FILENAME, x, fs);
if EXPORT_TEST_SIGNALS
  audiowrite(cstrcat(pwd(), "/Normalize-test.wav"), x, fs);
end

aud_do("SelectTracks: Track=0 TrackCount=100 Mode=Set\n");
aud_do("RemoveTracks:\n");
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
aud_do("Normalize: ApplyGain=1 RemoveDcOffset=0 PeakLevel=-1 StereoIndependent=0\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test( float_eq(max(max(abs(y))), 10^(-1/20)), "peak level");
do_test(!float_eq(max(abs(y(:,1))), max(abs(y(:,2)))), "stereo balance");
do_test( mean(y(:,1)) > 0.2 || float_eq(mean(y(:,2)), 0.0), "DC");
do_test( sum(sum(isnan(y))) == 0, "check for NaN");

## Test Normalize, remove DC only
CURRENT_TEST = "Normalize, remove DC only";
audiowrite(TMP_FILENAME, x, fs);
aud_do("SelectTracks: Track=0 TrackCount=100 Mode=Set\n");
aud_do("RemoveTracks:\n");
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
aud_do("Normalize: ApplyGain=0 RemoveDcOffset=1 PeakLevel=-10 StereoIndependent=1\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test(float_eq(max(y(:,1)), max(x(:,1)-0.2)) || float_eq(min(y(:,1)), min(x(:,1)-0.2)), "channel 1 amplitude");
do_test(float_eq(max(y(:,2)), max(x(:,2)))     || float_eq(min(y(:,2)), min(x(:,2))), "channel 2 amplitude");
do_test(float_eq(mean(y(:,1)), 0.0)            || float_eq(mean(y(:,2)), 0.0), "DC removal");
do_test(sum(sum(isnan(y))) == 0, "check for NaN");

## Test Normalize, stereo independent
CURRENT_TEST = "Normalize, stereo independence";
audiowrite(TMP_FILENAME, x, fs);
aud_do("SelectTracks: Track=0 TrackCount=100 Mode=Set\n");
aud_do("RemoveTracks:\n");
aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
aud_do("Normalize: ApplyGain=1 RemoveDcOffset=1 PeakLevel=-2 StereoIndependent=1\n");
aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=2\n"));
system("sync");

y = audioread(TMP_FILENAME);
do_test(float_eq(max(abs(y(:,1))), 10^(-2/20)), "channel 1 amplitude");
do_test(float_eq(max(abs(y(:,2))), 10^(-2/20)), "channel 2 amplitude");
do_test(float_eq(mean(y(:,1)), 0.0) || float_eq(mean(y(:,2)), 0.0), "DC removal");
do_test(sum(sum(isnan(y))) == 0, "check for NaN");
