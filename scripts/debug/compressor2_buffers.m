%% Debug Compressor v2 pipeline buffers
buffer_ids = [1,2,3,4,5];
prefix = '/tmp';

figure(1);
for k = 1:length(buffer_ids)
  subplot(length(buffer_ids), 1, k)
  bfile = fopen(sprintf('%s/envbuf.%d.bin', prefix, buffer_ids(k)));
  env = fread(bfile, 'float').';
  bfile = fopen(sprintf('%s/blockbuf.%d.bin', prefix, buffer_ids(k)));
  block_raw = fread(bfile, 'float').';

  sizes = reshape(block_raw(1:12), 3, 4);
  capacity = (1:4).*sizes(3,:);
  track_size = horzcat(0, capacity(1:3)) + sizes(1,:);
  block = block_raw(13:end);

  plot(block, 'b', 'linewidth', 3);
  hold on;
  plot(circshift(env, length(env)/3), 'r');
  stem(capacity, ones(1, length(capacity)), 'g');
  stem(track_size, 1.5.*ones(1, length(capacity)), 'b');
  ylim([-2 2]);
  hold off;
end