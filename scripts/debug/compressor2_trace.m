## plot realtime trace data from Compressor2 effect

stereo = true;
bfile = fopen("/tmp/audio.out");

if stereo
  width = 15;
else
  width = 13;
end

raw_data = reshape(fread(bfile, 'float'), width, []).';

data = struct;
data.threshold_DB = raw_data(:,1);
data.ratio = raw_data(:,2);
data.kneewidth_DB = raw_data(:,3);
data.attack_time = raw_data(:,4);
data.release_time = raw_data(:,5);
data.lookahead_time = raw_data(:,6);
data.lookbehind_time = raw_data(:,7);
data.makeup_gain_pct = raw_data(:,8);
data.dry_wet_pct = raw_data(:,9);

if stereo
  data.in = horzcat(raw_data(:,10), raw_data(:,11));
  data.env = raw_data(:,12);
  data.gain = raw_data(:,13);
  data.out = horzcat(raw_data(:,14), raw_data(:,15));
else
  data.in = raw_data(:,10);
  data.env = raw_data(:,11);
  data.gain = raw_data(:,12);
  data.out = raw_data(:,13);
end

figure(1);
plot(data.in.*100, 'b');
hold on;
plot(data.out.*100, 'g');
plot(data.threshold_DB, 'r');
plot(data.ratio, 'r');
plot(data.kneewidth_DB, 'r');
plot(data.attack_time.*10, 'c', "linewidth", 2);
plot(data.release_time.*10, 'c', "linewidth", 2);
plot(data.lookahead_time, 'm');
plot(data.lookbehind_time, 'm');
plot(data.makeup_gain_pct, 'r');
plot(data.dry_wet_pct, 'r');
plot(data.env.*100, 'k', "linewidth", 2);
plot(data.gain.*50, 'k', "linestyle", '--');
hold off;
grid;

if stereo
  legend("in*100", "in*100", "out*100", "out*100", "threshold", "ratio", ...
    "kneewidth", "attack*10", "release*10", "lookahead", "lookbehind", ...
    "makeup", "dry/wet", "env*100", "gain*50");
else
  legend("in*100", "out*100", "threshold", "ratio", ...
    "kneewidth", "attack*10", "release*10", "lookahead", "lookbehind", ...
    "makeup", "dry/wet", "env*100", "gain*50");
end
