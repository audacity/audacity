PortAudio uses "autoconf" tools to generate Makefiles for Linux and Mac platforms.
The source for these are configure.in and Makefile.in
If you modify either of these files then please run this command before
testing and checking in your changes. I run this command on Linux.

   autoreconf -if

If you do not have autoreconf then do:
   sudo apt-get install autoconf

If you get error like "possibly undefined macro: AC_LIBTOOL_WIN32_DLL"
then you try installing some more packages and then try again.

   sudo apt-get install build-essential
   sudo apt-get install pkg-config
   sudo apt-get install libtool
   autoreconf -if

Then test a build by doing:
   
   ./configure
   make clean
   make

then check in the related files that are modified.
These might include files like:

   configure
   config.guess
   depcomp
   install.sh
   
