 ## Submitting Issues.

* If your issue is that libsndfile is not able to or is incorrectly reading one
  of your files, please include the output of the `sndfile-info` program run
  against the file.
* If you are writing a program that uses libsndfile and you think there is a bug
  in libsndfile, reduce your program to the minimal example, make sure you compile
  it with warnings on (for GCC I would recommend at least `-Wall -Wextra`) and that
  your program is warning free, and that is is error free when run under Valgrind
  or compiled with AddressSanitizer.

## Submitting Patches.

* Patches should pass all existing tests
* Patches should pass all pre-commit hook tests.
* Patches should always be submitted via a either Github "pull request" or a
  via emailed patches created using "git format-patch".
* Patches for new features should include tests and documentation.
* Patches to fix bugs should either pass all tests, or modify the tests in some
  sane way.
* When a new feature is added for a particular file format and that feature
  makes sense for other formats, then it should also be implemented for one
  or two of the other formats.
