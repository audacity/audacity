"readme_LADSPA_plugins-win.txt"

Leland Lucius
Vaughan Johnson (vaughan @ audacityteam . org)
Updated September 27, 2006


LADSPA_plugins-win is a project to port Steve Harris's LADSPA plugins 
(http://plugin.org.uk/) to Windows OS as DLLs. 

LADSPA_plugins-win is a part of Audacity (http://www.audacityteam.org) 
and is licensed under the Audacity license, which is the GNU GPL. See 
http://www.audacityteam.org/about/license/ or audacity\LICENSE.txt.

The primary goal is to port these plugins for use in the Windows version of 
Audacity (http://www.audacityteam.org), but the plugins should work in 
any LADSPA host on Windows (if there are any others).



INSTALL THE PLUGINS:
The latest LADSPA_plugins-win installer is 
"LADSPA_plugins-win-0.4.15.exe".

Run the installer. For the installation location, select the 
Plug-Ins subdirectory of the Audacity you want to add them to. 
The standard location is the default. 

If you have multiple installed copies of Audacity (e.g., stable and 
beta), run  the LADSPA_plugins-win installer for each one.
Note that if you do multiple installations, there will be only one 
item in the Windows Control Panel > Add/Remove Programs, and that will 
remove the last one you installed. To remove the others, find and run the 
"unins000.exe" file in the directory where you installed the plugins.



REQUIREMENTS FOR BUILDING THE PLUGINS:
* Microsoft Visual Studio 2005. 

* audacity\win\LADSPA_plugins-win directory from, e.g., 
	http://www.audacityteam.org/download/plug-ins/

* Latest plugin distro from http://plugin.org.uk.

* If you wish to use the following plugins, you will need to get the
	libfftw3f-3.dll from http://www.fftw.org/ and place it 
	in the Audacity plugin directory:

		imp_1199.dll
		mbeq_1197.dll
		pitch_scale_1193.dll
		pitch_scale_1194.dll
		
	The installer includes these plugins and the necessary 
	FFTW DLL and documentation (by permission of 
	Steven G. Johnson of FFTW, 9/26/2006).

Because the GNU C library ("GLIBC", http://www.gnu.org/software/libc/libc.html) 
#include files typically are part of Linux but not Windows, we need to track 
those too, so another goal is to make it easy to update for new releases of 
GLIBC include files. GLIBC is a big download that changes infrequently, 
and that code requires more changes to work properly on Windows. So, we don't 
keep the same directory structure as the GLIBC release, but instead keep 
them in a simple directory structure that best fits the #include statements 
in Steve's code. 

LADSPA_plugins-win has been proven with these versions:
	* glibc-2.4
	* fftw-3.1.2-dll.zip from http://www.fftw.org/install/windows.html
	* swh-plugins-0.4.15 from http://plugin.org.uk


BUILD THE PLUGINS:
1. Put the LADSPA_plugins-win directory inside the latest distro 
	from http://plugin.org.uk. 
	The glibc and fftw headers and defs are already included 
	with LADSPA_plugins-win.

2. Open "LADSPA_plugins-win.sln". Build. DLLs will appear in Debug and/or 
	Release subdirectories of LADSPA_plugins-win.
	
3. To use them, move the DLLs to the Audacity\Plug-Ins folder.



BUILD THE INSTALLER (requires Inno Setup 5):
1. Build the Release configuration of the plugins.

2. Move the Release folder from the distro's copy of LADSPA_plugins-win to 
	audacity\win\LADSPA_plugins-win. 
	
3. Download the latest FFTW for Windows from 
	http://www.fftw.org/install/windows.html. 
	
	3a. Move this file into audacity\win\LADSPA_plugins-win\Release:
	
		libfftw3f-3.dll
		
	3b. Make a new folder, 
		audacity\win\LADSPA_plugins-win\Release\FFTW_docs.
		Copy all the documentation (files with no extension) to 
		that folder. Currently, the list is:
		
		COPYING
		COPYRIGHT
		NEWS
		README
		README-bench
		README-WINDOWS

4. Open and compile the LADSPA_plugins-win.iss file in 
	audacity\win\LADSPA_plugins-win. The installer will appear in 
	audacity\win\LADSPA_plugins-win\Installer.


