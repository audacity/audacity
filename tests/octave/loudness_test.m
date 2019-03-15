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

# TODO: add tests here

