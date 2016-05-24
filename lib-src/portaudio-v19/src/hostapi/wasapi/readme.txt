**************
* WASAPI API *
**************

-------------------------------------------
Microsoft Visual Studio 2005 SP1 and higher
-------------------------------------------
No specific action is required to compile WASAPI API under Visual Studio.
You are only required to install min. Windows Vista SDK (v6.0A) prior
the compilation. To compile with WASAPI specific functionality for Windows 8
and higher the min. Windows 8 SDK is required.

----------------------------------------
MinGW (GCC 32/64-bit)
----------------------------------------
To compile with MinGW you are required to include 'mingw-include' directory
which contains necessary files with WASAPI API. These files are modified
for the compatibility with MinGW compiler. These files are taken from 
the Windows Vista SDK (v6.0A). MinGW compilation is tested and proved to be
fully working.
MinGW   (32-bit) tested min. version: gcc version 4.4.0 (GCC)
MinGW64 (64-bit) tested min. version: gcc version 4.4.4 20100226 (prerelease) (GCC)