Notes on Using the Pre-compiled libsndfile DLL.
===============================================

In order to use this pre-compiled DLL with Visual Studio, you will need to
generate a .LIB file from the DLL.

This can be achieved as follows:

  1) In a CMD window, change to the directory containing this file and
     run the command:

          lib /machine:i386 /def:libsndfile-1.def

You now have two files:

        libsndfile-1.dll
        libsndfile-1.lib

to be used with VisualStudio.

If the lib command fails with a command saying "'lib' is not recognized as
an internal or external command, operable program or batch file", you need
to find the location of "lib.exe" and add that directory to your PATH
environment variable. Another alternative is to use the "Visual Studio 2005
Command Prompt" Start menu item:

   Start ->
   		All Programs ->
			Visual Studio 2005 ->
				Visual Studio Tools ->
					Visual Studio 2005 Command Prompt

If for some reason these instructions don't work for you or you are still
not able to use the libsndfile DLL with you project, please do not contact
the main author of libsndfile. Instead, join the libsndfile-users mailing
list :

        http://www.mega-nerd.com/libsndfile/lists.html

and ask a question there.
