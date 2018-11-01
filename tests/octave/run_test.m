#!/usr/bin/octave -qf
## Audacity Octave unit test runner
#
# Max Maisel
#

if nargin != 1
  printf("Usage: ./testbench.m <test-file.m>\n");
  exit(2);
end

arg_list = argv();

if exist(arg_list{1}, "file") != 2
  printf("Specified test file does not exist!\n");
  exit(2);
end

## Initialization and helper functions
UID=num2str(getuid());
PIPE_TO_PATH=strcat("/tmp/audacity_script_pipe.to.", UID);
PIPE_FROM_PATH=strcat("/tmp/audacity_script_pipe.from.", UID);
TMP_FILENAME=strcat(pwd(), "/tmp.wav");

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

function do_test(result, msg, skip = false)
  global TESTS_RUN;
  global TESTS_FAILED;
  global TESTS_SKIPPED;
  global CURRENT_TEST;
  TESTS_RUN = TESTS_RUN + 1;
  if skip
    TESTS_SKIPPED = TESTS_SKIPPED + 1;
    printf(cstrcat("[Skip]:    ", CURRENT_TEST, " - ", msg, "\n"));
  else
    if result
      printf(cstrcat("[Success]: ", CURRENT_TEST, " - ", msg, "\n"));
    else
      TESTS_FAILED = TESTS_FAILED + 1;
      printf(cstrcat("[Failed]:  ", CURRENT_TEST, " - ", msg, "\n"));
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
  exit(1);
elseif TESTS_SKIPPED != 0
  printf("Some tests were skipped!\n");
else
  printf("All tests succeeded!\n");
end

exit(0)
