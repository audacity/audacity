## Audacity Compressor2 effect unit test
#
# Max Maisel
#
# This tests the Compressor effect with various pseudo-random mono and stereo
# noise sequences and sinewaves. The test sequences have different amplitudes
# per channel and sometimes a DC component.
#
# Avoid large parameters for AttackTime, ReleaseTime and LookaroundTime in
# this script as settling behaviour is different and will cause test failure.
#

pkg load signal;
pkg load image;

printf("Running Compressor effect tests.\n");

EXPORT_TEST_SIGNALS = true;

## PT1 envelope helper function for symmetric attack and release times.
function y = env_PT1(x, fs, t_ar, gain = 0)
  T = 1/(t_ar*fs);
  si = mean(mean(abs(x(1:fs*t_ar/20,:))));
  c = (gain != 0) * gain + (gain == 0) * (1.0 + exp(t_ar/30.0));
  y = c*filter(T, [1 T-1], mean(abs(x), 2), si*c);
end

## PT1 envelope helper function for asymmetric attack and release times.
# This function is much slower than the symmetric counterpart.
function y = env_PT1_asym(x, fs, t_a, t_r, gain = 0)
  C_a = 1.0 / (fs*t_a);
  C_r = 1.0 / (fs*t_r);
  si = mean(mean(abs(x(1:fs*t_a/20,:))));
  c = (gain != 0) * gain + (gain == 0) * (1.0 + exp(t_a/30.0));

  x_m = mean(abs(x), 2);
  y = zeros(length(x_m), 1);
  level = si;

  for k = 1:1:length(x_m)
    if x_m(k) >= level
      level = level + C_a * (x_m(k) - level);
    else
      level = level + C_r * (x_m(k) - level);
    end
    y(k) = c * level;
  end
end

## Compressor gain helper function
function gain = comp_gain(env, thresh_DB, ratio, kneeW_DB, outG_DB)
  env_DB      = 20*log10(env);
  kneeCond_DB = 2*(env_DB-thresh_DB);

  belowKnee  = kneeCond_DB < -kneeW_DB;
  aboveKnee  = kneeCond_DB >= kneeW_DB;
  # & is element-wise &&
  withinKnee = (kneeCond_DB >= -kneeW_DB) & (kneeCond_DB < kneeW_DB);

  gain_DB = zeros(size(env));
  gain_DB(belowKnee) = outG_DB;
  gain_DB(aboveKnee) = thresh_DB + ...
    (env_DB(aboveKnee) - thresh_DB) / ratio + ...
    outG_DB - env_DB(aboveKnee);
  # Prevent division by zero
  kneeW_DB(kneeW_DB==0) = 0.000001;
  gain_DB(withinKnee) = (1/ratio-1) * ...
    (env_DB(withinKnee) - thresh_DB + kneeW_DB/2).^2 / ...
    (2*kneeW_DB) + outG_DB;

  gain = 10.^(gain_DB/20);
end

# Ignore first samples due to settling effects helper function
function y = settled(x, fs = 44100, tau = 1, both = 0)
  y = x(round(3*fs*tau):length(x)-round(3*fs*tau*both),:);
end

# XXX: This Octave function is REALLY slow.
#      Maximum value of n*fs < 10000
function y = lookaround_RMS(x, fs, n1, n2)
  kernel = cat(1, zeros(n2*fs,1), ones(n1*fs, 1), ones(n2*fs, 1), zeros(n1*fs, 1));
  y = zeros(size(x));
  for i=1:1:size(x)(2)
    y(:,i) = conv(x(:,i).^2, kernel, 'same')./(n1+n2)/fs;
  end

  y = 2*sqrt(sum(y, 2)./size(y)(2));
end

# XXX: This Octave function is REALLY slow.
#      Maximum value of n*fs < 10000
function y = lookaround_max(x, fs, n1, n2)
  kernel = cat(1, zeros(n2*fs,1), ones(n1*fs, 1), ones(n2*fs, 1), zeros(n1*fs, 1));
  x = mean(abs(x), 2);
  y = imdilate(x, kernel);
end

################################################################################

## Test Compressor, mono thresholding
CURRENT_TEST = "Compressor2, mono thresholding";
fs = 44100;

randn("seed", 1);
x1 = 0.01*randn(30*fs,1) .* sin(2*pi/fs/35*(1:1:30*fs)).';

remove_all_tracks();
x = export_to_aud(x1, fs, name = "Compressor-threshold-test.wav");
aud_do("DynamicCompressor: Threshold=-20 Algorithm=0 AttackTime=0.1 ReleaseTime=0.3 LookaheadTime=0 LookbehindTime=0 KneeWidth=0\n");
y = import_from_aud(1);

# All input samples are below threshold so output must be equal to input.
do_test_equ(x, y);

## Test Compressor, mono compression PT1 - no lookaround
CURRENT_TEST = "Compressor2, mono compression PT1";
x1 = x1.*10;
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 CompressBy=0 Ratio=2.5 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0.0 LookbehindTime=0.0 KneeWidth=12\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.5, 1), fs, 1), -20, 2.5, 12, 0).* ...
  settled(x, fs, 1));

## Test Compressor, mono compression PT1 - sinewave - no lookaround
CURRENT_TEST = "Compressor2, mono compression PT1 - sinewave";

x2 = sin(2*pi*300/fs*(1:1:20*fs)).';
remove_all_tracks();
x = export_to_aud(x2, fs, "Compressor-mono-sine-test.wav");
aud_do("DynamicCompressor: Threshold=-23 Algorithm=1 CompressBy=1 Ratio=2.5 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0 LookbehindTime=0 KneeWidth=12\n");
y = import_from_aud(1);

# Gain factor 2 because we compress by RMS but do not use lookaround_RMS as
# lookaround is zero.
do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(2*env_PT1(x, fs, 0.5), fs, 1), -23, 2.5, 12, 0).* ...
  settled(x, fs, 1));

## Test Compressor, mono compression PT1 - faded sinewave - medium signal
CURRENT_TEST = "Compressor2, mono compression PT1 - faded sinewave - medium signal";

x2 = sin(2*pi*300/fs*(1:1:50*fs)).' .* horzcat(1:1:25*fs, 25*fs:-1:1).' ./ (25*fs);
remove_all_tracks();
x = export_to_aud(x2, fs, "Compressor-mono-sine-test.wav");
aud_do("DynamicCompressor: Threshold=-10 Algorithm=1 CompressBy=0 Ratio=100 AttackTime=0.01 ReleaseTime=0.01 LookaheadTime=0 LookbehindTime=0 KneeWidth=0\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.01, 1), fs, 1), -10, 100, 0, 0).* ...
  settled(x, fs, 1));

## Test Compressor, mono compression PT1 - faded sinewave - 50 sec signal - no lookaround
CURRENT_TEST = "Compressor2, mono compression PT1 - faded sinewave - long signal";

x2 = vertcat(x2, x2);
remove_all_tracks();
x = export_to_aud(x2, fs, "Compressor-mono-sine-test.wav");
aud_do("DynamicCompressor: Threshold=-10 Algorithm=1 CompressBy=0 Ratio=100 AttackTime=0.01 ReleaseTime=0.01 LookaheadTime=0 LookbehindTime=0 KneeWidth=0\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.01, 1), fs, 1), -10, 100, 0, 0).* ...
  settled(x, fs, 1));

## Test Compressor, mono compression PT1 - sinewave - no lookaround - long attack time
CURRENT_TEST = "Compressor2, mono compression PT1 - sinewave - asymetric attack / release";

x2 = sin(2*pi*300/fs*(1:1:20*fs)).';
remove_all_tracks();
x = export_to_aud(x2, fs, "Compressor-mono-sine-test.wav");
aud_do("DynamicCompressor: Threshold=-6 Algorithm=1 CompressBy=0 Ratio=2.0 AttackTime=1.0 ReleaseTime=0.3 LookaheadTime=0 LookbehindTime=0 KneeWidth=0 OutputGain=0\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1_asym(x, fs, 1.0, 0.3, 1.0), fs, 1), -6, 2.0, 0, 0).*
  settled(x, fs, 1));

## Test Compressor, mono lookaround max
CURRENT_TEST = "Compressor2, mono asymmetric lookaround max";
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-17 Algorithm=1 CompressBy=0 Ratio=1.2 AttackTime=0.3 ReleaseTime=0.3 LookaheadTime=0.2 LookbehindTime=0.1 KneeWidth=5 OutputGain=1\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 0.6), ...
  comp_gain(settled(env_PT1(lookaround_max(x, fs, 0.2, 0.1), fs, 0.3, 1), fs, 0.6), ...
  -17, 1.2, 5, 1).*settled(x, fs, 0.6));

## Test Compressor, mono lookaround RMS
CURRENT_TEST = "Compressor2, mono asymmetric lookaround RMS";
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 CompressBy=1 Ratio=3 AttackTime=1 ReleaseTime=1 LookaheadTime=0.1 LookbehindTime=0.2 KneeWidth=3 OutputGain=2\n");
y = import_from_aud(1);

do_test_equ(settled(y, fs, 2), ...
  comp_gain(settled(env_PT1(lookaround_RMS(x, fs, 0.1, 0.2), fs, 1), fs, 2), -20, 3, 3, 2) ...
  .*settled(x, fs, 2));

## Test Compressor, mono lookaround max with selection
CURRENT_TEST = "Compressor2, mono lookaround max with selection";
remove_all_tracks();
x = export_to_aud(x1, fs);

aud_do("Select: Start=2 End=5 Mode=Set\n");
aud_do("DynamicCompressor: Threshold=-17 Algorithm=1 CompressBy=0 Ratio=1.2 AttackTime=0.3 ReleaseTime=0.3 LookaheadTime=0.2 LookbehindTime=0.2 KneeWidth=5 OutputGain=0.5\n");
y = import_from_aud(1);
x = x(2*fs+1:5*fs);

do_test_equ(settled(y, fs, 0.1), ...
  comp_gain(settled(env_PT1(lookaround_max(x, fs, 0.2, 0.2), fs, 0.3, 1), fs, 0.1), ...
  -17, 1.2, 5, 0.5).*settled(x, fs, 0.1));

## Test Compressor, mono, ultra short attack time
CURRENT_TEST = "Compressor2, mono, ultra short attack time";
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 CompressBy=0 Ratio=2 AttackTime=0.0001 ReleaseTime=0.0001 LookaheadTime=0 LookbehindTime=0 KneeWidth=10\n");
y = import_from_aud(2);

# XXX: use larger epsilon due to numerical issues
# (float in audacity vs double in octave vs wav files for exchange)
do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.00001, 1), fs), -20, 2, 10, 0) ...
  .*settled(x, fs, 1), "", 0.15);

## Test Compressor, stereo compression PT1 - no lookaround
randn("seed", 2);
x1 = 0.2*randn(35*fs, 2);
x1(:,1) = x1(:,1) .* sin(2*pi/fs/35*(1:1:35*fs)).';
x1(:,2) = x1(:,2) .* (sin(2*pi/fs/75*(1:1:35*fs)).' + 0.1);

CURRENT_TEST = "Compressor2, stereo compression PT1";
remove_all_tracks();
x = export_to_aud(x1, fs, "Compressor-stereo-test.wav");
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 CompressBy=0 Ratio=2 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0 LookbehindTime=0 KneeWidth=10\n");
y = import_from_aud(2);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.5, 1), fs), -20, 2, 10, 0) ...
  .*settled(x, fs, 1), "stereo dependent");

remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 Ratio=2 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0 LookbehindTime=0 KneeWidth=10 StereoIndependent=1\n");
y = import_from_aud(2);

do_test_equ(settled(y(:,1), fs, 1), ...
  comp_gain(settled(env_PT1(x(:,1), fs, 0.5, 1), fs, 1), -20, 2, 10, 0) ...
  .*settled(x(:,1), fs, 1), "channel 1");
do_test_equ(settled(y(:,2), fs, 1), ...
  comp_gain(settled(env_PT1(x(:,2), fs, 0.5, 1), fs, 1), -20, 2, 10, 0) ...
  .*settled(x(:,2), fs, 1), "channel 2");

## Test Compressor, stereo compression PT1 - sinewave
CURRENT_TEST = "Compressor2, stereo compression PT1 - sinewave";
x2 = sin(2*pi*300/fs*(1:1:20*fs)).';
x2 = [x2, sin(2*pi*310/fs*(1:1:20*fs)).'];

remove_all_tracks();
x = export_to_aud(x2, fs, "Compressor-stereo-sine-test.wav");
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 CompressBy=0 Ratio=2 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0 LookbehindTime=0 KneeWidth=10\n");
y = import_from_aud(2);

do_test_equ(settled(y, fs, 1), ...
  comp_gain(settled(env_PT1(x, fs, 0.5, 1), fs, 1), -20, 2, 10, 0) ...
  .*settled(x, fs, 1), "stereo dependent");

remove_all_tracks();
x = export_to_aud(x2, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 Ratio=2 AttackTime=0.5 ReleaseTime=0.5 LookaheadTime=0 LookbehindTime=0 KneeWidth=10 StereoIndependent=1\n");
y = import_from_aud(2);

do_test_equ(settled(y(:,1), fs, 1), ...
  comp_gain(settled(env_PT1(x(:,1), fs, 0.5, 1), fs, 1), -20, 2, 10, 0) ...
  .*settled(x(:,1), fs, 1), "channel 1");
do_test_equ(settled(y(:,2), fs, 1), ...
  comp_gain(settled(env_PT1(x(:,2), fs, 0.5, 1), fs, 1), -20, 2, 10, 0) ...
  .*settled(x(:,2), fs, 1), "channel 2");

## Test Compressor, stereo lookaround max
CURRENT_TEST = "Compressor2, stereo lookaround max";
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-17 Algorithm=1 Ratio=1.2 AttackTime=0.3 ReleaseTime=0.3 LookaheadTime=0.2 LookbehindTime=0.2 KneeWidth=5 OutputGain=1\n");
y = import_from_aud(2);

do_test_equ(settled(y, fs, 0.6), ...
  comp_gain(settled(env_PT1(lookaround_max(x, fs, 0.2, 0.2), fs, 0.3, 1), fs, 0.6), ...
  -17, 1.2, 5, 1).*settled(x, fs, 0.6));

## Test Compressor, stereo lookaround RMS
CURRENT_TEST = "Compressor2, stereo lookaround RMS";
remove_all_tracks();
x = export_to_aud(x1, fs);
aud_do("DynamicCompressor: Threshold=-20 Algorithm=1 Ratio=3 AttackTime=1 ReleaseTime=1 LookaheadTime=0.1 LookbehindTime=0.1 KneeWidth=3 CompressBy=1 OutputGain=1.3\n");
y = import_from_aud(2);

do_test_equ(settled(y, fs, 2.5), ...
  comp_gain(settled(env_PT1(lookaround_RMS(x, fs, 0.1, 0.1), fs, 1), fs, 2.5), -20, 3, 3, 1.3) ...
  .*settled(x, fs, 2.5));
