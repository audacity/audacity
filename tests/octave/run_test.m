#!/usr/bin/octave -qf
## Audacity Octave unit test runner
#
# Max Maisel
#

if !(nargin == 1 || nargin == 2)
  printf("Usage: ./testbench.m <test-file.m> [-v]\n");
  exit(2);
end

arg_list = argv();

if exist(arg_list{1}, "file") != 2
  printf("Specified test file does not exist!\n");
  exit(2);
end

global VERBOSE;
VERBOSE=0;

if nargin == 2
  if strcmp(arg_list{2}, "-v") == 1
    VERBOSE=1;
  else
    printf("Unknown argument %s. Abort!", arg_list{2});
    exit(2);
  end
end

## Initialization and helper functions
global TMP_FILENAME;
global EXPORT_TEST_SIGNALS;
UID=num2str(getuid());
PIPE_TO_PATH=strcat("/tmp/audacity_script_pipe.to.", UID);
PIPE_FROM_PATH=strcat("/tmp/audacity_script_pipe.from.", UID);
TMP_FILENAME=strcat(pwd(), "/tmp.wav");
EXPORT_TEST_SIGNALS = false;

printf("Open scripting pipes, this may freeze if Audacity does not run...\n");

global PIPE_TO;
global PIPE_FROM;
global PRINT_COMMANDS;
PRINT_COMMANDS=false;
PIPE_TO=fopen(PIPE_TO_PATH, "w");
PIPE_FROM=fopen(PIPE_FROM_PATH, "r");

## aud-do helper function
function aud_do(command)
  global PIPE_TO;
  global PIPE_FROM;
  global PRINT_COMMANDS;
  if PRINT_COMMANDS
    puts(command);
  end
  fwrite(PIPE_TO, command);
  fflush(PIPE_TO);
  do
    string = fgets(PIPE_FROM);
    if PRINT_COMMANDS
      puts(string);
    end
    fflush(stdout);
  until strncmp(string, "BatchCommand finished:", length("BatchCommand finished:"));
end

## Selection helper functions
function remove_all_tracks()
  aud_do("SelectTracks: Track=0 TrackCount=100 Mode=Set\n");
  aud_do("RemoveTracks:\n");
end

function select_tracks(num, count)
  aud_do("Select: Start=0 Mode=Set\n");
  aud_do("SelCursorToTrackEnd:\n");
  aud_do(sprintf("SelectTracks: Track=%d TrackCount=%d Mode=Set\n", num, count));
end

function x_in = import_from_aud(channels)
  global TMP_FILENAME;
  aud_do(cstrcat("Export2: Filename=\"", TMP_FILENAME, "\" NumChannels=", ...
    num2str(channels), "\n"));
  system("sync");
  x_in = audioread(TMP_FILENAME);
end

function x_out = export_to_aud(x, fs, name = "")
  global TMP_FILENAME;
  global EXPORT_TEST_SIGNALS;
  audiowrite(TMP_FILENAME, x, fs);
  if EXPORT_TEST_SIGNALS && length(name) != 0
    audiowrite(cstrcat(pwd(), "/", name), x, fs);
  end
  # Read it back to avoid quantization-noise in tests
  x_out = audioread(TMP_FILENAME);
  aud_do(cstrcat("Import2: Filename=\"", TMP_FILENAME, "\"\n"));
  select_tracks(0, 100);
end

## Float equal comparison helper
function [ret] = float_eq(x, y, eps=0.001)
  ret = abs(x - y) < eps;
end

## Test report helper
global TESTS_FAILED;
global TESTS_RUN;
global TESTS_SKIPPED;
global CURRENT_TEST;
TESTS_FAILED = 0;
TESTS_RUN = 0;
TESTS_SKIPPED = 0;
CURRENT_TEST="";

function plot_failure(x, y)
  global VERBOSE;
  if VERBOSE == 0
    return;
  end

  figure(1)
  plot(x, 'r')
  hold on
  plot(y, 'b')
  delta = abs(x-y);
  max(delta)
  plot(log10(delta), 'g')
  hold off
  legend("Audacity", "Octave", "log-delta", "location", "southeast")
  input("Press enter to continue", "s")
end

function do_test_equ(x, y, msg = "", eps = 0.001, skip = false)
  cmp = all(all(float_eq(x, y, eps)));
  if do_test(cmp, msg, skip) == 0
    plot_failure(x, y);
  end
end

function do_test_neq(x, y, msg = "", eps = 0.001, skip = false)
  cmp = all(all(!float_eq(x, y, eps)));
  if do_test(cmp, msg, skip) == 0
    plot_failure(x, y);
  end
end

function do_test_gte(x, y, msg = "", skip = false)
  cmp = all(all(x >= y));
  if do_test(cmp, msg, skip) == 0
    plot_failure(x, y);
  end
end

function do_test_lte(x, y, msg = "", skip = false)
  cmp = all(all(x <= y));
  if do_test(cmp, msg, skip) == 0
    plot_failure(x, y);
  end
end

function result = do_test(result, msg = "", skip = false)
  global TESTS_RUN;
  global TESTS_FAILED;
  global TESTS_SKIPPED;
  global CURRENT_TEST;
  TESTS_RUN = TESTS_RUN + 1;

  ANSI_COLOR_RED    = "\x1b[31m";
  ANSI_COLOR_GREEN  = "\x1b[32m";
  ANSI_COLOR_YELLOW = "\x1b[33m";
  ANSI_COLOR_RESET  = "\x1b[0m";

  suffix = "";
  if !strcmp(msg, "")
    suffix = cstrcat(" - ", msg);
  end

  if skip
    TESTS_SKIPPED = TESTS_SKIPPED + 1;
    printf(ANSI_COLOR_YELLOW);
    printf(cstrcat("[Skip]:    ", CURRENT_TEST, suffix, "\n"));
    printf(ANSI_COLOR_RESET);
    result = 1;
  else
    if result
      printf(ANSI_COLOR_GREEN);
      printf(cstrcat("[Success]: ", CURRENT_TEST, suffix, "\n"));
      printf(ANSI_COLOR_RESET);
      return;
    else
      TESTS_FAILED = TESTS_FAILED + 1;
      printf(ANSI_COLOR_RED);
      printf(cstrcat("[Failed]:  ", CURRENT_TEST, suffix, "\n"));
      printf(ANSI_COLOR_RESET);
      return;
    end
  end
end

## Run tests
printf("Starting tests...\n");
source(arg_list{1});

## Cleanup and result reporting
unlink(TMP_FILENAME);
fclose(PIPE_FROM);
fclose(PIPE_TO);

printf("%d tests run, %d tests failed, %d tests skipped\n", TESTS_RUN, TESTS_FAILED, TESTS_SKIPPED);
if TESTS_FAILED != 0
  printf("Some tests failed!\n");
  if VERBOSE == 0
    printf("Re-run with -v option for details.\n");
  end
  exit(1);
elseif TESTS_SKIPPED != 0
  printf("Some tests were skipped!\n");
else
  printf("All tests succeeded!\n");
end

exit(0)
