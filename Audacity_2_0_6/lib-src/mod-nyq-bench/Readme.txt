
Two things first...

1)  This is only an example of a dynamic module.  The interfaces
    it uses WILL change in the future.
2)  The NyqBench effect is just my hairbrained idea and it
    may not produce desired results.

For Mac and Linux user, you must change the AUDACITY_DIR variable
at the top of your Makefile to point to the base of the Audacity
source directory.

For Windows users, you need to define an environment variable
called AUDACITY_DIR whose value points to the base of the Audacity
source directory.

The NyqBench.dll will automatically be copied to the Audacity
Modules directory in either the "win\Unicode Debug" or
"win\Unicode Release" subdirectories.

Other questions?  Ask on the devel list.
