sndfile-regtest
===============

The 'sndfile-regtest' program is a regression test-suite for libsndile.

This program is intended to allow anyone who has an interest in the
reliability and correctness of libsndfile to do their own regression
testing. From the point of view of the libsndfile developers, this
program now allows for distributed regression testing of libsndfile
which will make libsndfile better.


How Does it Work
----------------
Anyone who wishes to take part in the distributed regression testing of
libsndfile can download the regression test program and install it.

Once installed the user can start collecting files and adding them to
their own personal database. Then, as new versions of libsndfile come
out, the user should test the new library version against their database
of files (instructions below).

Any files which were successfully added to the database in the past but
now fail the check with the new library version represent a regression.
The user should then contact the libsndfile developers so that a copy
of the test file can be made available to the developers.


Requirements
------------
The regression test program uses sqlite3 as the database engine. On
Debian, the required packages are :

    sqlite3
	libsqlite3-0
	libsqlite3-dev

but similar packages should be available on any other Linux style
system.

The regression test currently only compiles under Unix-like systems.
At some time in the future the regression test will distributed along
with the libsndfile source code distribution.


Organization of Files
---------------------
The regession test program keeps its database file in the directory it
is run from. In addition, the database only contains information about
the files, not the files themselves.

This means that database file should probably be kept in the same
directory (or a directory above) the test files.


Setting it Up for the First Time
--------------------------------
The sndfile-regtest program should be on your PATH. You can then cd into
the directory where you intend to keep you test files and
run the command:

        sndfile-regtest --create-db

which creates a file named '.sndfile-regtest.db' in the current directory.

Files can then be added to the database using the command:

        sndfile-regtest --add-file file1.wav

The --add-file option allows more than one file to be added at a time
using:

        sndfile-regtest --add-file file1.wav file2.aif .....


Checking Files
--------------
One or more files that have already been added to the database can be
checked using:

        sndfile-regtest --check-file file1.wav file2.aif .....

It is also possible to check all files in the database using:

        sndfile-regtest --check-all


Running a Regression Test
-------------------------
Once you have a collection of files and a database it is possible to test
new versions of libsndfile before you install them. If for instance you
have just compiled a new version of libsndfile in the directory
/usr/src/libsndfile-X.Y.Z, then you can use an existing sndfile-regtest
binary with the new libsndfile using something like:

    LD_PRELOAD=/usr/src/libsndfile-X.Y.Z/src/.libs/libsndfile.so.X.Y.Z \
	sndfile-regtest --check-all


Reporting Regressions
---------------------
Any user who finds a file which was added to the regression database with
an earlier version of libsndfile and then fails the check with a later
version of the library should contact the author (erikd at mega dash nerd
dot com). If possible place the file on a web server and email the author
a link to it.


