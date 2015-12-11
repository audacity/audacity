
Audacity(R): A Free, Cross-Platform Digital Audio Editor
  WWW: http://audacityteam.org/

We welcome feedback on Audacity, suggestions for new or improved features, 
bug reports and patches at:
  feedback@audacityteam.org .

Personal support with Audacity is not provided by e-mail, but on our Forum:
  http://forum.audacityteam.org/ .

Audacity is copyright (c) 1999-2015 by Audacity Team. This copyright notice
applies to all documents in the Audacity source code archive, except as
otherwise noted (mostly in the lib-src subdirectories).

The documentation for Audacity is licensed under the Creative Commons
Attribution 3.0 license:
http://creativecommons.org/licenses/by/3.0/legalcode .


"Audacity" is a registered trademark of Dominic Mazzoni.

Version 2.1.2 

Contents of this README:

1.  Licensing
2.  Changes since version 2.1.1 
3.  Known Issues at Release
4.  Source Code, Libraries and Additional Copyright Information
5.  Compilation Instructions
6.  Previous Changes going back to version 1.1.0.

--------------------------------------------------------------------------------

1. Licensing

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version. The program source code is also freely
available as per Section 4 of this README.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with this program (in a file called LICENSE.txt); if not, go
to http://www.gnu.org/licenses/old-licenses/gpl-2.0.html or write to

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA


-------------------------------------------------------------------------------

2. Changes since version 2.1.1: 

Changes and Improvements:
   
 * Interface:  
   * Spectrogram settings are now available per track.
   * Clearer (sharper) display of pitch (EAC).
   * New 'Spectral Reassignment' option in spectrogram.  Good for vocal work.
   
 * Other Changes:   
   * Upgraded the wxWidgets library from wx 2.8.12 to wx 3.0.2.  This is 
     the main change in this release.
   
Bug fixes:

 * Crashes
   * Crash using 'space' in Selection toolbar context menu fixed.
   * Crash setting equalization effect parameters in chains fixed.
   * Crash when pressing both mouse buttons over toolbar buttons fixed.
   * (Rare) crash or freeze in sound activated recording fixed.
   * (Rare) crash on using plot spectrum for first time fixed.

 * Interface
   * Equalization effect settings are now saved.
   * Oversized Export Options window (FFmpeg) now OK on smaller screens
   * FLAC import/export fast again.
   * Can now set undefined frequency in Spectral Selection bar.
   * Imported presets on custom FFmpeg export fixed.
   * Text input boxes working with VAMP
   * Keyboard playback commands now work again.
   * Import Raw Data now works when in Polish language.
   
 * Mac OS X
   * Mouse preference bindings now show 'command', not 'ctrl'
   
-------------------------------------------------------------------------------

3. Known Issues in 2.1.2:

For known issues at release of 2.1.2 please see:
  http://wiki.audacityteam.org/wiki/Release_Notes_2.1.2#known

Please also check:
  http://wiki.audacityteam.org/index.php?title=Known_Issues

for details of any issues that have been identified after release of
this version.


-------------------------------------------------------------------------------

4.  Source Code, Libraries and Additional Copyright Information

Source code to this program is always available; for more information visit
our web site at:

  http://audacityteam.org/download/source

Audacity is built upon other free libraries; some of these libraries may have
come with Audacity in the lib-src directory.  Others you are expected to install
first if you want Audacity to have certain capabilities.  Most of these libraries
are not distributed under the terms of the GPL, but rather some other free,
GPL-compatible license.  Specifically:

  expat: BSD-like license.
    Provides XML parsing.  Included with Audacity.

  FFmpeg: GPL or LGPL (according to how you obtain/configure it)
    Provides decoding/encoding of additional formats. Optional separate
    download.

  libid3tag: GPL
    Reads/writes ID3 tags in MP3 files.  Optional
    separate download as part of libmad.

  libflac: Xiph.Org BSD-like licence (the parts we use)
    Decodes and Encodes Free Lossless Audio Codec files. Optional separate
    download.

  libmad: GPL
    Decodes MP3 files.  Optional separate download.

  libnyquist: BSD-like license.
    Functional language for manipulating audio; available
    within Audacity for effects processing.

  libogg: BSD-like license.
    Optional separate download, along with libvorbis.

  libsndfile: LGPL
    Reads and writes uncompressed PCM audio files.
    Included with Audacity.

  libsoxr: LGPL
    The SoX Resampler library performs one-dimensional sample-rate conversion.

  libvamp: new-style BSD
    Plug-in interface and support library for audio analysis plug-ins.
    Included with Audacity.

  libvorbis: BSD-like license.
    Decodes and encodes Ogg Vorbis files.  Optional
    separate download.

  lv2: a merging of the lilv (ISC license), lv2 (LGPL), msinttypes, serd (ISC), 
    sord, sratom, and suil libraries to support LV2 plug-ins. 

  portsmf: BSD-like license.
    library for reading and writing midi files. Included with Audacity

  sbsms: GPL v2
    Pitch and tempo changing library. Included in Audacity

  SoundTouch: LGPL
    Changes tempo without changing pitch and vice versa.
    Included in audacity

  Twolame: LGPL
    Encodes MPEG I layer 2 audio (used in DVDs and Radio). Optional separate
    download.

  wxWidgets: wxWindows license (based on LGPL)
    Cross-platform GUI library - must be downloaded and
    compiled separately.


For more information, see the documentation inside each library's
source code directory.

--------------------------------------------------------------------------------
Additional copyright information:
--------------------------------------------------------------------------------

Nyquist

Copyright (c) 2000-2002, by Roger B. Dannenberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

Redistributions of source code must retain the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP."

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Redistributions in binary form must reproduce the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP," in the documentation
and/or other materials provided with the distribution.

Neither the name of Roger B. Dannenberg, Carnegie Mellon University, nor the
names of any contributors may be used to endorse or promote products derived
from this software without specific prior written permission.

COPYRIGHT AND LICENSE INFORMATION FOR XLISP (part of Nyquist):

Copyright (c) 1984-2002, by David Michael Betz
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Neither the name of David Michael Betz nor the names of any contributors may be
used to endorse or promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


--------------------------------------------------------------------------------

5. Compilation instructions

First you must download wxWidgets. Audacity 2.1.2 requires wxWidgets 3.0.x, 
preferably 3.0.2, which can be obtained from:

   http://www.wxWidgets.org/ .

The libsndfile library is also required and is included in Audacity obtained 
from GitHub. 
 
CMake ( http://www.cmake.org/ ) is required to build the local copy of 
the libsoxr resampling library used by Audacity, unless you install 
libsoxr-dev and use that system library.

Installation of other libraries is optional, see:
   http://wiki.audacityteam.org/wiki/Developing_On_Linux#optional . 

If you install libraries using a package management system like Apt or RPM,
you need to install the "dev" (development) packages for each library.

To compile on Linux, Mac OS X and other Unix systems, execute these 
commands:

  ./configure
  make
  make install  # as root

To see compile-time options you can set, you can type
  ./configure --help

If you want to do any development, you might want to generate a configure cache
and header dependencies:

  ./configure -C
  make dep

To compile on Windows using MSVC++, please follow the instructions in
win\compile.txt in the source code. 

To compile using Xcode on Mac OS X, see the instructions in mac/compile.txt.

For more information on compilation, please visit:

  http://wiki.audacityteam.org/wiki/Developer_Guide#Platform_Specific_Guides

or ask at:
   http://forum.audacityteam.org/viewforum.php?f=19 .


--------------------------------------------------------------------------------

6.  Previous Changes going back to version 1.1.0

Changes in version 2.1.1: 

Bug fixes:

 * Crashes
   * Crash using Undo while dragging sample points
   * Crash using File > Close on project window when Screenshot Tools was 
     open.
   * FFmpeg Custom Export: Crashes importing presets.
   * (OS X) Crash closing Track Gain or Pan adjustment box.
   * (OS X) Crash closing project window between save project dialogues
   * (Linux) TAL VST (but not other VST's) crashed if previewing built-in 
     effect
   * (Linux) SPACE could not be used/could crash in context menus that have 
     a checkbox.

 * Interface
   * LV2 effects did not use parameters when using Chains.
   * Built in Generators were not usable in Chains.
   * Plot Spectrum could not change values without losing focus.
   * Track dropdown menu settings could affect other tracks.
   * Slight mismatch of vertical scale with linear spectrogram view.
   * RTP effect Play/Stop button remained deactivated after built-in preview 
     ended.
   * Contrast: "Move forward or backward through active windows" did not 
     refocus Contrast.
   * LADSPA and LV2 generate plug-ins failed when white space selected.

 * Mac OS X
   * Confusing behaviour importing / exporting AU presets. In particular, 
     Apple Audio Units silently applied an imported preset.
     
Changes and Improvements:

 * Effects:
   * Built in effects now support presets.
   * New Limiter effect replaces Hard Limiter effect.
   * New Crossfade Clips effect to apply a simple crossfade to a selected 
     pair of clips in a single audio track.   
   * Can now add/remove effects from Generate and Effects menus.
   * New version of Vocal Removal Effect.
   * Classic Filters' now included as an opt-in effect.

 * Interface:  
   * Much faster editing with larger projects, thanks to a faster method 
     for storing the autosave recovery file.
   * Performance improvements for Draw Tool and zooming of Spectrogram views.
   * Zero-padding Spectrograms Preference smooths the image for short 
     window sizes.
   * Scrubbing and Seeking, including backwards play. 
   * Quick-Play from Timeline enhancements, particularly for looping.
   * (Windows) Language of Audacity user interface is now set in installer.
   * More VI usability enhancements for track focus & navigation.
   
 * Other Changes:   
   * Upgraded to Nyquist 3.0.9 and libflac 1.3.1.
   * Upgraded LV2 libs, LV2 GUIs on Linux, LV2 factory Presets.
   * Crash report integration.
   * Modules can be enabled in Preferences. Mod-nyq-bench available as an 
     experimental module (but not in the default download).
 
Changes in version 2.1.0: 

Bug fixes:

 * Interface:
   * Typing "j" or "k" in a label track activated the "move cursor" shortcut.
   * Spectrogram log (f) view displayed incorrectly until vertically zoomed.
   * Fixed crash after zooming out on vertical scale beyond +/-1.0. 
   * Selections made with Selection Toolbar were not restored after Undo.
   * Undo could fail silently if a selection included/touched a clip boundary.

 * Imports and Exports 
   * If there were invalid FFmpeg libs in system PATH this prevented Audacity
      recognising the installed FFmpeg or the FFmpeg specified in audacity.cfg.

 * Mac OS X:
   * Fix uninitialized buffer - this should correct playback buzz or crackle
      where the upper of multiple tracks started with or contained white space.   
   * Device names were corrupted when using system language other than English. 
   * Fixed crashes using (external program). 
   * Waves v9 Audio Units should now work correctly.

 * GNU/Linux: 
   * Fix "Audacity already running" error when using the command-line or 
      context menu to open multiple or further files. 
   * Fix segfault exporting an FFmpeg format to an unwritable folder.
   * Fix silent failure exporting FFmpeg, MP2 or OGG to an unwritable folder.
   * Fix ENTER activated an effect when OK button was greyed out.  

 * Operating system and sound device support:
   * (Windows) Audacity 2.1.0 requires Windows XP SP3 (32-bit) or XP SP2
      (64-bit), Vista, Windows 7, Windows 8 or 8.1. 
   * (Windows) Audacity is now compiled using Visual Studio 2013.  
   * (Windows) Recording with WASAPI host now includes experimental support 
      for physical inputs (up to 24-bit depth) as well as loopback recording.  
   * (OS X) 10.10 (Yosemite) is now supported including Apple Audio Units.
   * Please report any issues with WASAPI recording/playback or Yosemite 
      to http://audacity.sourceforge.net/contact/#feedback .
   * (Linux Ubuntu) Under Unity, keyboard shortcuts are not visible in the
     Audacity menus. Keyboard shortcuts are visible if you install the classic
     GNOME Flashback interface or under Unity if you open Audacity with the
     UBUNTU_MENUPROXY=0 environment variable. Audacity compiled from source
     will ship with src/audacity.desktop.in set to UBUNTU_MENUPROXY=0 but it
     will remain up to distributions to use this desktop file.    
   
Changes and Improvements:

 * Effects:
   * LADSPA, VST and Audio Unit (OS X) effects now support real-time preview, 
      save/load of user presets and saving effect settings across sessions. 
      Note: Real-time preview does not yet support latency compensation.
   * VST effects now support import/export of FXB preset banks. 
   * Shell VST effects that host multiple plugins are now supported.
   * All Effect Menu items (built-in or plugin) can now be used in a Chain. 
   * Items in the Effect, Generate or Analyze Menus can be sorted or grouped
      by name, publisher or class of effect.   
   * Noise Removal is improved and renamed to "Noise Reduction".  
   * Change Speed has new time controls for current and new length. You can 
      now enter the speed change as a multiplier e.g. "2" is twice as fast.  
   * New "Crossfade Tracks" effect can be used for crossfading two tracks. 
      This replaces Cross Fade In and Cross Fade Out.  	
   * Nyquist Prompt and most shipped Nyquist effects now have Preview button. 

 * Interface:  
   * Redesigned Meter Toolbars: The default shows separate Record and Playback   
      Meters, half-height so they can be wider while docked, in gradient style. 
   * A frequency selection can now be made (and spectral edit effects applied
      to those frequencies) when in a spectrogram view. You can also create
      or adjust frequency selections in a new "Spectral Selection Toolbar"
      (available at View > Toolbars).  
   * Transcription Toolbar (Play-at-speed) can now loop play and cut-preview. 
   * Timer Record now saves recordings automatically into an existing project.  
   * New Armenian translation. 

Changes in version 2.0.6:

Bug fixes for:

 * Interface:
   * Region Restore did not restore the region after using Preferences. 
   * Dragging selections with the keyboard or Selection Toolbar digits 
      was very slow.  
   * (Windows) Help > About Audacity crashed when run in Magyar language.  
   * (OS X) Some full and reduced Menu Bar items were not translated.  
   * (OS X and Linux) Fixed various interface crashes.

 * Effects:
   * Reverb and Paulstretch were missing from Chains.
   * Analyze > Contrast could report very inaccurate rms levels.
   * Noise Removal: Attack and decay times were half as long as set.  
   * (OS X and Linux) Nyquist effects ran much more slowly than on Windows. 

 * Click or drag on the Timeline after Loop Play continued to loop.
 * Transcription Toolbar did not play slower than 0.1x speed.
 * (Linux) Audacity did not build if python 2 was not available.  
 * Projects did not reopen correctly if they contained tracks having 
   2^31 samples or greater of audio (just over 13.5 hours at 44100 Hz).


Changes and Improvements:

 * Interface:
   * Redesigned, searchable Keyboard Preferences with Tree, Name and Key views.  
   * Edit Menu: "Cut" and "Delete" are now in the top level of the menu.  
   * Transport Menu now includes "Play/Stop" and "Play/Stop and Set Cursor" 
      (use Keyboard Preferences to create shortcuts for "Play" and "Stop").
   * Tracks Menu now includes "Mix and Render to New Track". 
   * Track Drop-Down Menu now has Move Track To Top and Move Track To Bottom. 
   * New right-click menu choice "Delete Label" to remove single labels.
   * "Snap To" now offers choice of snap to the "closest" or "prior" position.
      Note: the previous "Snap To On" keyboard shortcut will no longer work.   
   * "Snap To" settings are now independent for each project.

 * Effects:
   * Truncate Silence: redesigned with simpler option "Truncate Detected 
      Silence" to shorten to the specified length without compressing silence.
   * VST effects: New "Settings" dialog lets you specify buffer size (for 
      faster processing) and enable buffer delay compensation (to prevent
      inserted silence). Compensation may cause a crash in a few plug-ins. 
   * VST effects now support standard FXP presets.  
   * LV2 effects are now supported on all platforms (textual interface only).    

 * Import or export using FFmpeg now requires FFmpeg 1.2 or later (or libav
    0.8 or later). For recommended downloads of recent FFmpeg please visit:
    http://manual.audacityteam.org/o/man/faq_installation_and_plug_ins.html#ffdown .
 * New Tamil translation (largely complete). 
 * (Windows) FLAC exports can now exceed 2 GB in size. 
 * (OS X) Easier Audacity installation using the DMG: drag the Audacity folder
    to the /Applications shortcut. 
 * (OS X) Audacity 2.0.6 will not officially support OS X 10.10 Yosemite when 
    released (in particular, Apple Audio Units may not open in Audacity).
 * (Linux) Self-compiled builds of Audacity now search for system LADSPA 
    effects in /usr/lib/ladspa.  


Changes in version 2.0.5:

Bug fixes for:

 * Shaped dither was corrupted and too loud on all stereo exports except FLAC. 

 * Keyboard Preferences: some Edit and Align commands for different sub-menus
    showed the same name.   

 * Recordings stopped with "Stop and Set Cursor" shortcut could not be undone.

 * In locales that use comma for decimal separator:
    * Text boxes with slider in Nyquist effects only produced whole numbers 
       when using comma to enter a fractional number. Text boxes without 
       slider still have this problem.   
    * Built-in generators produced silence after running a Nyquist effect.  

 * (Windows) When first changing to Windows WASAPI host, the input volume slider
    in Mixer Toolbar was enabled when it should have been permanently disabled. 

 * (Windows) On some machines, launching Audacity then recording from the current
    Device Toolbar input would not record until the input was reselected. 

 * (OS X) Frequent crashes occurred on importing audio files on some machines.

 * (OS X) Files did not open using Finder "Open with", double-clicking the 
    file or dragging the file to the Audacity icon.    

 * (Linux 64-bit) Fixed a crash when using Equalization.

 * (Linux) It was not possible to open an effect or other dialog then navigate
    through the dialog using TAB.

 * (Linux) The Play shortcut did not play a read-directly WAV, AIFF or FLAC 
    import if the warning for importing uncompressed files appeared. 


Changes and Improvements:

 * Tracks Menu:
   * The separate commands that aligned track start or end with the cursor or
      with selection start are combined into "Cursor/Selection Start" commands.
   * "Align and Move Cursor" renamed to "Move Selection when Aligning".   

 * Label Tracks:
   * Labels Editor now allows empty labels to be saved on closing the editor.
   * TAB and SHIFT+TAB when the label track has focus now always move forwards
      or backwards respectively to the nearest label.

 * (Windows) On a very few machines, the Windows WDM-KS low latency audio host
    introduced in Audacity 2.0.4 caused Audacity to hang or the computer to 
    crash. WDM-KS has been removed from 2.0.5 until it can be safely enabled.

 * (Windows and OS X) Screen reader improvements for Install VST Effects dialog.  

 * (OS X) Audio Unit plug-ins detected by Audacity on launch are now not loaded
    until chosen from the Effect menu. This should speed up launch and avoid
    crashes at launch due to misbehaving Audio Units.  

 * (Linux) Update to PortAudio r1910 fixes memory and other bugs under ALSA.

 * (Linux) Applied fix for wxGTK 2.8.12 bug which resulted in loss of Audacity's
    menu bar (or visual corruption under Unity) on Debian-based systems.


Changes in version 2.0.4:

Bug fixes for:

 * Keyboard Preferences: Shortcuts for Generators, Effects and Analyzers 
    were not exported. All imported shortcut changes were discarded.
 * Equalization curves were corrupted in Graphic EQ mode after switching 
    to/from Draw Curves or after	running the effect then reopening it. 
 * Change Pitch displayed corrupted values when reducing pitch or editing 
    "from" Frequency. Detection was very inaccurate at high sample rates. 
 * Bass Boost no longer clips if the track contains 32-bit audio.   
 * Auto Duck was excessively slow on older machines. 
 * (Windows) Exported MP3 comments tags were not seen by Windows programs.   
 * (Windows and OS X) Audacity crashed if you used system quit before
    file import was complete. 
 * (Linux) Equalization crashed Audacity if the XML file was corrupted.  
 * (Linux) When configuring effect parameters in "Edit Chains", "Preview" 
    (not intended to be functional) caused a crash.
 * (Linux) LICENSE.txt and README.txt were wrongly installed in
     /usr/local/share/doc instead of /usr/local/share/doc/audacity/ .
 * Accessibility: ENTER did not toggle selectedness of a label track unless
    a label was selected.  
 * Numerous other interface fixes.  


Changes and Improvements:

 * New "Reverb" effect to replace GVerb, based on the original "Freeverb".
 * New View > Go to Selection Start and Go to Selection End commands.  
 * New "Align End to End" command to append existing tracks to each other. 
 * Change Tempo now supports fractional BPM.    
 * Plot Spectrum now supports FFT sizes up to 65536.  
 * WAV files now support "Album Title", "Track Number" and "Genre" LIST INFO 
    tags and also support ID3 tags.
 * Handle a bug in older iPods or some OS X applications that cause them to 
    refuse AIFF files whose metadata contains an uneven number of characters,   
 * (Windows) Added support for "Windows WDM-KS" host which can provide very
    low latencies if you reduce "Audio to Buffer" in Recording Preferences.
 * (Windows Vista and later) You can now record computer playback by choosing
    the new "Windows WASAPI" host in Device Toolbar then a "loopback" input.
 * (Windows and Mac OS X): VST scanning dialog now replaced with a dialog
    for choosing which VST effects to load. 
 * (Linux) CTRL + ALT can now be used to smooth samples in Draw Tool. 
 * Modules Preferences replaced with a dialog on launch of Audacity 
    enabling you to choose which modules to load.  


Changes in version 2.0.3:

Bug fixes for:

 * Crash using Undo whilst time-shifting a track.
 * Crash using Repair if the selection extended into an empty track.
 * Export Multiple didn't prevent export if there was no audio or 
    all audio was muted. This allowed export of small invalid files. 

 * Time Track:
   * Loop Play of a speeded-up track inserted silence.
   * Playback and rendering was significantly inaccurate, creating 
      audible and visual glitches. 

 * Accessibility:
   * The mnemonics character "&" was read out by screen readers in 
      most of the Preferences choices.
   * NVDA did not read static text in most dialogs. Text can now be
      read by using INSERT + B.
   * JAWS and Window-eyes misread the "Duration" control in Silence 
      Generator. 
   * Toolbar buttons could not be pressed by ENTER 	

 * Other interface bug fixes.


Changes and Improvements:

 * The SoX Resampler library (libsoxr) has replaced libresample in 
    Audacity releases, offering both higher quality and greater speed. . 

 * Time Tracks new features:
   * "Set Range" now changes only the range of the Time Track,
      preserving the pitch/speed set by any existing warp points.
   * Vertical scale added with options for linear and logarithmic  
      display and interpolation.
   * Upper and lower speed limits will now be remembered when saving 
      and reopening a project in 2.0.3. Warp points in projects saved 
      by previous Audacity versions will be correctly restored in 2.0.3.
   * Warp points saved in a 2.0.3 project will be preserved if opened
      in previous versions but playback and display will be incorrect.

 * New effects: 
   * Studio Fade Out (uses a filtered "S" curve).
   * Adjustable Fade (accessible effect for creating partial fades 
      and adjustable fade shapes).   
   * Bass and Treble (replaces Bass Boost).  
 
 * Real sample rates up to 384000 Hz are now supported for playback 
    and recording in high resolution devices (the maximum is up to 
    192000 Hz for Windows DirectSound host).
 
 * Labeled Regions in Edit Menu is renamed to "Labeled Audio" and now 
    allows splits to be placed at point labels. Labeled audio regions
    that touch without overlapping are treated as separate regions. 
    Overlapping labeled audio regions are treated as a single region.

 * Compilation: cmake is required in order to build libsoxr.

 * New Croatian translation of Audacity.


Changes in version 2.0.2: 

Bug fixes for:

 * Interface:
   * "Retain labels" Interface Preference did not retain labels for
      a region that snapped exactly to both label edges.
   * Projects did not save the track selected state.
   * (OS X, Linux) Timer Record: Interlinking of the Start, End and 
      Duration controls was broken.
   * (Windows) JAWS screen-reader did not read the "Draw curves" and 
      "Graphic EQ" radio buttons in Equalization correctly.  

 * Envelopes and Clips: 
   * Exporting (or any render operation) on a track containing 
      split lines could create clicks at the split lines. 
   * Dragging a clip into another track caused a crash if Sync-Lock
      Tracks was enabled and there was also a label track. 

 * Effects and Analysis:
   * Normalize could crash if the track name contained "%". 


Changes and Improvements:

 * Duration controls when generating at a point now default to 
    hh:mm:ss + milliseconds format. Selection Toolbar also defaults
    to that format on first installation or resetting preferences. 
 * Toolbars visual improvements:
   * "Snap To" in Selection Toolbar now has an explanatory tooltip.  
   * Device Toolbar tooltips now display the selected device. 
   * Increased default width of Device Toolbar and Meter Toolbar.   
 * Improvements and some bug fixes to Nyquist effects, including:
   * Delay (new option to prevent duration change)
   * Sample Data Export (new "L-R on Same Line" layout option)
   * Risset Drum (new "Amplitude" slider). 
 * Importing a labels file writes the file name to the name of the 
    Label Track, and exporting a labels file offers the name of the
    last Label Track in the project.
 * Removed the "Audio cache" option from Directories Preferences 
    due to frequent crash reports. All data operations will now 
    be written to disk and not to RAM. 
 * Removed the FFmpeg "On-Demand" option from Libraries Preferences
    (this fixes Audacity not building if configured --without-ffmpeg). 
 * Compilation: Progress on making the Modules feature mainstream. 
    Modules can now be individually enabled and disabled in Preferences.


Changes in version 2.0.1: 

Bug fixes for:

 * Interface:
   * Selection Toolbar: a value for the previous whole second 
      displayed if the value was close to a whole second. 
   * Finding zero crossings could cause the selection to expand into
      white space at either side of the clip. 
   * Clips did not drag to another track if mouse was over a selection.
   * Mixer Board: Rendering four tracks resulted in a redundant Track
      Strip followed by a crash. 

 * Imports, Exports and Files:
   * Exporting to WAV or AIFF led to a "Libsndfile says" error or
      corrupted output due to order of metadata in imported files. 
   * (Mac) Fixed crashes importing MP3 files on PPC machines.    
   * (Linux Ubuntu) .Aup files could not be associated with Audacity
      (they opened in the web browser instead).

 * Effects and Analysis:
   * Normalize: Fixed issues where normalization could be to wrong
      value if applied with DC offset correction, or if applied to 
      "read-directly" WAV and AIFF files before On-Demand completed. 
   * Sliding Time Scale: fixed an audible discontinuity at the 
      beginning of the processed selection; fixed a serious quality
      problem on Linux 64-bit. 
 
 * Other miscellaneous bug fixes, including fix to prevent zooming
    with mouse wheel or ball scrolling the content off-screen. 


Changes and Improvements:

 * Shortcuts can now be added in Keyboard Preferences to items in the
    Generate, Effect or Analyze menus, including user-added plug-ins.   

 * Nyquist Effect plug-ins can now be added to Chains. 

 * New "Paulstretch" effect for extreme slowdown without pitch change. 

 * New "Sample Data Export" Analyze effect for exporting a file 
    containing amplitude values for each sample in the selection. 

 * New Preference (off by default) to import files On-Demand (without 
    seek ability) when using the optional FFmpeg library. 

 * New Preference (off by default) to retain labels when deleting a
    selection that snaps to the label without extending past it.

 * (Windows installer) New option to reset Preferences on next launch. 

 * (Mac) Audacity now has excellent compatibility with the VoiceOver
     screen reader. For details, please see:
     http://manual.audacityteam.org/help/manual/man/accessibility.html#mac .  

 * CleanSpeech Mode (no longer supported) will not now be enabled 
    even if it was enabled by an earlier version of Audacity.  

 * Added Serbian (Latin and Cyrillic) translations.   


Changes between version 1.3.14 Beta and 2.0.0:

Bug fixes for:

 * Interface:
   * Fixed playback speed and synchronization problems when dragging  
      clips or tracks between tracks having different sample rates.
   * (Windows) Removed a crash risk where shortcuts could be
      used to record or import in one project while importing or 
      exporting in another.    

 * Imports and Exports:
   * Fixed crashes when changing the sample format of read-directly WAV
      or AIFF files using the Track Drop-Down Menu. 
   * Fixed a crash importing MP3 files that had duplicate metadata tags 
      (this is a bug in current libsndfile which has been patched in 
       Audacity; MP3 files mislabeled as WAV which have duplicate tags
       will still crash Audacity on Linux if Audacity has been compiled 
       against an affected version of system libsndfile).
   * Fixed an issue where excessively high or corrupted sample values in
      the audio could corrupt exports from the start of the problem for
      the rest of the file, and could corrupt the rest of the project.  
   * (Linux) Fixed Audacity could not be compiled against FFmpeg
      0.7.x and 0.8.x. 

 * Effects and Analysis:
   * Fixed crash on launch when using "Ambisonic Decoders (PC)" VST  
      plug-ins and other plug-ins that enable additional floating point
      exceptions. 
   * Fixed Plot Spectrum background could be transparent on some machines. 
   * Bug fixes for Click Track, High Pass, Low Pass and Vocal Remover.  
   * Chirp, Tone and Silence generators now remember their settings.
 
 * Other miscellaneous bug fixes.  


Changes and Improvements:

 * New Interface preference to show the track name in the display (this
    is off by default). 
 * Longer default Playback preference for effects preview and preview 
    before cut. 
 * Restored use of Page Up and Page Down to scroll horizontally.  


Summary of bug fixes and new features between 1.2.6 and 2.0.0:

 * Bug Fixes:

   * Labels now accept lower case "z".
   * (Windows Vista/7) Pressing Stop after recording could cause a crash.
   * (Mac OS X) Fixed shortcut keys activating when typing in file open or
      save windows.
   * (Mac OS X) Audacity 2.0.0 fully supports Unicode, which fixes an
      issue where Audacity could not work with folders having non-English
      characters in their name. 
   * (Linux) Playing existing tracks while recording in mono could cause
      recordings to be distorted or low-pitched. 

 * New features: There are dozens of new features in 2.0.0, including: 

   * Many effects significantly improved, especially Equalization, Noise
      Removal and Normalize. Vocal Remover now included plus GVerb on 
      Windows and Mac. VAMP analysis plug-ins now supported.  
   * Improved label tracks with Sync-Lock Tracks feature in the Tracks Menu.
      Multiple clips per track. Tracks and selections can be fully 
      manipulated using the keyboard. Many more keyboard shortcuts. 
   * New Device Toolbar to manage inputs and outputs. Timer Record feature.
      New Mixer Board view with per-track VU meters. 
   * Automatic Crash Recovery in the event of abnormal program termination.  
   * Fast "On-Demand" import of WAV/AIFF files if read directly from source. 
      FLAC now fully supported. Added support for optional FFmpeg library 
      for import/export of AC3/M4A/WMA and import of audio from video files. 

   Please visit http://audacity.sourceforge.net/about/features for a full
   list of features in 2.0.0. You can click Help > Manual (in web browser) 
   in the program to read more. 


Changes in version 1.3.14 Beta: 

Bug fixes for:

 * Interface:
   * Excessive delay occurred when typing into labels in long projects. 
   * Last digit of TimeText controls could not be manipulated in some formats.
   * (Windows, OS X) Play and Record shortcuts did not work after clicking in
      Device Toolbar. 
   * (OS X, Linux) Crash occurred if Toolbars were reset during playback or
      recording. 

 * Imports and Exports:
   * MP2 files were not importable without FFmpeg library or an import rule. 
   * Files that could only be imported using FFmpeg imported as noise with
      no error message if FFmpeg was not available. 
   * Files containing PCM audio but an incorrect extension (such as MP3) 
      caused a freeze. 

 * Effects and Analysis:
   * An empty command could be added to a Chain which then displayed a 
      Nyquist error message when run.  
   * Plot Spectrum didn't preserve signal level if multiple tracks were 
      analyzed.  

 * Other bug fixes:
   * Audacity has been provisionally fixed so that it can no longer create 
      block files longer than the sample format or project format allows, 
      and can no longer delete these, which led to data loss. Any overlong 
      blocks found are preserved but "orphaned", so will appear as silence.
   * Orphan block files were wrongly reported if cutting or copying to 
      the clipboard then reopening the project in the same session. 
   * Fixed some crashes and incorrect movement of audio when dragging tracks.
   * (Windows) Data loss is now prevented when encountering a corrupted
      .aup file created in ANSI builds. 
   * (Linux) Restore building if USE_PORTMIXER is not defined. 
 
Changes and Improvements:

 * Normalize: Faster processing and improved interface. Left-right balance 
    in unsplit stereo tracks is now preserved by default, with a checkbox 
    option provided to process stereo channels independently.   
 * Spectrograms now allow window sizes up to 32768 and frequencies up to
    half the sample rate (the maximum possible).
 * Mix and Render now preserves clip length by not rendering white space 
    between time zero and first audio, and preserves audio before time zero.
    To retain silence before the audio starts, generate silence after render.     
 * Grouped some Edit Menu items into "Remove Audio" and "Clip Boundaries".  
 * CleanSpeech Mode removed from Interface Preferences (it still runs if it
    was enabled in a previous Audacity but can only be turned off there). 
 * (OS X) Added support for AudioUnit MusicEffects (but no MIDI support). 
 * (Linux) Set the per-user files directory per the program name set in 
    configure.
 * (Linux) Changed the default location of the Audacity temporary directory
    to be in /var/tmp not /tmp, so preserving the directory between reboots. 


Changes in version 1.3.13 Beta: 

Bug fixes for:

 * Interface:
   * Cutting or copying from a track at a given sample rate into a track
      at another rate resulted in speed-changed audio. 
   * Generating inside a clip could modify the clip length or create 
      spurious clips.
   * Recorded stereo tracks were only half the height of imported or 
      generated stereo tracks. Imported stereo files had a "1" appended 
      to the track name.  
   * Edit > Region Save did not save the cursor position.  
   * (Windows) Projects crashed when clicking rapidly inside the interface
      or when applying repeated effects towards the end of audio tracks. 
   * (Windows) Some Unicode characters could not be typed into labels,
      or caused a freeze using some input methods.
   * (OS X) Crash when quitting an empty project window, or when closing the
      main project window while a progress dialog was on screen.
   * Numerous other interface fixes including Dependencies dialog, 
      Keyboard Preferences and spurious lines drawn on waveform.

 * Imports and Exports:
   * Support added for later versions of the optional FFmpeg library up to 
      current FFmpeg HEAD. This should significantly improve FFmpeg support
      on Linux. FFmpeg downloads for Windows and Mac updated to v0.6. This
      fixes mono AAC files importing as stereo, though current 0.5 versions
      of FFmpeg will still work.
   * Both FFmpeg and LAME should now be properly detected even when other 
      versions of those libraries exist on the system. 
   * New warning (on by default) for importing uncompressed audio files. 
      Better error messages when read-directly uncompressed files are missing. 
   * Imported ID3v2 metadata tags were removed when exporting without the 
      Metadata Editor appearing (for example, when using an export command
      in Chains). Note: As a result of this fix, ID3v1 tags must now be 
      written by exporting using (external program) and an installed LAME. 
   * U-Law/A-Law files with WAV headers now use the standard 18 byte fmt chunk,
      so should now be recognised by most telephony applications.   
   * Variable bit rate MP3s exported using "MP3 Files" were larger than 
      necessary because using the bit reservoir was disabled.   
   * (OS X) Files imported from iTunes could create invalid characters in the
      .aup project file, causing an error when re-opening the project. Note: 
      An error "reference to invalid character number" will still occur if 
      re-opening a project created in previous Betas that contains such 
      characters. To fix the issue, open a back-up copy of the .aup file in a
      text editor, turn off word wrap, then in the line indicated in the error
      message, remove the string of characters that starts with &# and ends
      with a semi-colon (;).

 * Other bug fixes:
   * Nyquist effects: fixes for crashes, incorrect slider behaviour and better
      support for backslashes, double quotes and Unicode characters. 
   * (Windows and OS X) Processing of VST effects was substantially slower than
      in previous versions of Audacity.
   * (OS X 10.5 PPC) A first-time installation of Audacity Beta would hang
      on launch if VST effects were detected. 
   * (Linux) Recordings made with the pulse device crashed or stalled when
      using overdub and/or software playthrough.  
   * (Linux) Play-at-Speed crashed at 0.08 speed or lower if Audacity was
      configured with libsamplerate. 
 
Changes and Improvements:

 * Control Toolbar renamed to Transport Toolbar.  
 * Device Toolbar (on by default) now contains all input and output device 
    choices, including host and recording channels. Input/output choices are 
    no longer in Mixer Toolbar on Windows XP or some older operating systems.
    New Transport > Rescan Audio Devices menu item to refresh the device list.  
 * New "Sync-Lock Tracks" feature (turned on in the Tracks menu) to allow
    groups of audio and/or label tracks to retain synchronisation when the
    track length changes. 
 * Equalization: New "Manage Curves" dialog for importing and exporting curves.
 * Noise Removal: New "Sensitivity" slider to adjust the noise threshold, and
    new option to isolate noise.
 * New "Extended Import" Preferences for specifying different importers to
    open specific file extensions.
 * Improved Automatic Crash Recovery with all project changes autosaved. 
 * MIDI tracks can be vertically zoomed, time shifted and display bar lines.
    Note: the channel selection buttons are not available in 1.3.13.    
 * (Windows and Linux) The window Close button and other system close or
    shutdown commands now quit on closing the last window. File > Close now 
    always clears to a new, empty project. 
 * (OS X) Simpler installer with top-level "Audacity" folder. 


Changes in version 1.3.12 Beta: 

Bug fixes for:

 * Imports and Exports:
   * AAC files could not be exported at 48000 Hz 
   * When importing multiple native file formats using FFmpeg, all 
      files after the first reverted to using the native importer
   * FFmpeg custom export window too large on 800 x 600 resolution 
      monitors 
   * Projects froze if files imported via On-Demand were no longer 
      available
   * (Linux) WAV/AIFF exports were corrupted if overwriting an aliased 
      file which had been imported using the command line

 * Labels:
   * Cutting or deleting a region in the waveform and label track did 
      not move the labels in advance of the cut 
   * Incorrect behavior snapping to labels and boundaries when Snep To 
      was enabled
   * Labels can now be reversed if included with the audio selection 

 * Other bug fixes:
   * When using non-English languages, Generate effects truncated the 
      selected region
   * Mice with high-precision scroll-wheels could cause a crash   
   * Changing recording preferences using the Transport menu did
      not update the menu in other open projects  
   * (Windows 7) Clicking in a file open or save dialog caused files or 
      folders to disappear from the list, and file filtering was broken

Changes and improvements:

 * A hover tooltip is now provided if the Mixer Toolbar input selector 
    cannot control the system slider for the selected input. 
 * More intuitive behavior when moving and resizing labels by dragging 
 * Support added for importing lists of files (LOF) containing relative 
    paths  
 * Export Multiple: new option to use a numerical prefix before existing 
    label or track names; "Success" dialog now resizable 
 * New Equalization preset "Inverse RIAA", with new button to invert  
    other curves
 * Timer Record now remembers last scheduled duration 
 * Meter Toolbar can now be made much narrower, and so more suitable for
    vertical orientation  
 * New Preferences choice for "System" language which is used on first 
    run instead of asking user to choose language    
 * Warning now provided if WAV/AIFF exports are not successfully
    completed   
 * (Linux) Improved icon set in compliance with freedesktop.org 
    Icon Theme Specification 0.6


Changes in version 1.3.11 Beta: 

Bug fixes for:

 * Imports and Exports:
   * Bug when exporting partial selections caused too much audio to be 
      exported is fixed.
   * Fix corrupt files exported through FFmpeg when metadata included,
      (metadata is now exported correctly in M4A files)
   * Prevent saving a new Audacity Project over an existing one as this 
      could corrupt both projects.
   * Improved help for files that cannot be imported because the relevant
      optional library is missing.

 * Effects:
   * Allow effects which change the length of the audio they work on to also be
      applied to selected label tracks, thus keeping them synchronized.
   * Fixed inability in Nyquist plug-ins to add labels to an existing label track 
   * (Mac) Equalization window was corrupted after Preview  
   * (Linux 64-bit) Fixed crash Generating Click Track

 * Audio Devices:
   * Fixed bug causing recording to stop short when the recording sample rate 
      is not supported by the sound device and libsamplerate is used for 
      resampling.
   * Fix crash when opening Preferences on a machine where there are no
      available audio devices.
   * Fixes for bugs using Timer Record and Sound Activated Recording 

 * User Interface:
   * Sizes of some dialogs adjusted to ensure they fit on the screen.
   * Fix for supposedly "hidden" items appearing on screen with large
      monitors.
   * Various keyboard shortcut and translation fixes.

 * Other bug fixes:
   * Several timing-dependent crashes and minor incorrect behaviors have been
      fixed
   * Windows installer now installs correctly over previous versions of Audacity

Changes and improvements:

 * (Windows) Better icon file with higher resolution and transparency
 * New SoundFinder plug-in to label regions of audio between silences, so 
    allowing silences between tracks to be excluded when exporting multiple 


Changes in version 1.3.10 Beta: 

Bug fixes for:

 * Imports and Exports:
    * Freeze importing audio files when Default View Mode set to 
       Pitch (EAC)
    * Simultaneous On-Demand imports sorted incorrectly
    * WAV or AIFF files imported as noise if Preferences set to 
       copy in the data at 24-bit quality
    * WMA exports corrupted if they contained metadata 
    * Metadata Editor appeared before the Export window when 
       exporting to any format

 * Effects:
    * Crash or hang using Equalization on longer tracks
    * Reverse did not reverse clip boundaries
    * Nyquist: 
       * Excessive memory consumption led to slow processing or
          crashes
       * Values appearing in Nyquist effects text boxes were not 
          always the default or previously entered values
       * Errors running Nyquist effects in European locales where
          comma used as decimal separator
    * VST effects remained in Effect menu even when re-scanned and 
       no longer available 
    * Truncate Silence produced incorrect results if silences 
       spanned a block boundary    

 * Other bug fixes:
    * Spurious "not writable/disk full" errors when saving projects
    * Playing, rendering or exporting multiple tracks led to 
       desynchronised playback or loss of audio data
    * Crash opening Preferences when no recording and/or playback
       devices enabled or connected
    * Preferences window: OK button did not respond to ENTER when
       a tab selected in left-hand panel
    * Mixer Board solo button handling 
    * (Windows) After a period launching correctly, Audacity 
       sometimes did not come up on top at launch
    * (Mac OS X) Correctly installed Help folder could not be found 
    * (Mac OS X and Linux) Output slider could affect VU playback 
        meter which then did not reflect actual waveform volume level 
    * (Linux) Undoing or redoing a label edit could cause a crash 

Changes and improvements:

 * Linked audio and label tracks disabled until a future Beta
    version so they can be bug fixed
 * Input volume slider will be disabled if it doesn't have proper
    control of system slider; use the system slider instead
 * Proper support for copying/pasting label-with-audio including 
    label text; new Edit > Paste Text to New Label menu item to
    paste system clipboard     
 * Contrast Tool now modeless, more intuitive handling of multiple
    project windows, various other minor improvements


Changes in version 1.3.9 Beta:

Bug fixes for:
           * Crash, slow launch or excessive CPU/memory use arising 
              from automatic VST support:
              * VST instrument plug-ins should now be correctly ignored
              * VST effects now scanned only at start of first session
                 that detects them, then cached; effects are now not 
                 loaded or opened until needed
              * New "Effects" tab in Preferences to enable/disable VST 
                 effects and enable VST rescan on next launch
           * Default View Mode now works
           * Chains now always apply their stored parameters rather than 
              those last used in Effect menu  
           * Non-MP3 files imported via drag or Recent Files caused
              crash if filter in file open window set to MP3
           * AAC exports (using the optional FFmpeg library) were 
              silenced 
           * Generating audio always fitted the project in the window; 
              fit now done only if generating in new track  
           * View menu items/shortcuts incorrectly disabled when playing
              or recording 
           * DTMF generator defaulted to zero duration on open
           * Unwanted interactions between linked audio and label tracks  
           * Noise Removal shifted clips if the selection region included
              white space
           * (Windows XP) Failure to launch on some machines due to
              "incorrect configuration" issue 
           * (Windows) Crash importing a stereo file while a screen reader
              such as JAWS is running
           * (Mac OS X):
              * Audio Units effects applied to all tracks in project even
                 if not selected
              * QuickTime importer now handles files greater than 16-bit 
                or 64000 Hz
           * Various other interface bugs 

Improvements:
           * Compressor: new option to compress based on peaks, improved
              attack and decay time support
           * Mixer Board: improved design, more responsive meters and 
              now interacts fully with Track Panel in main window


Changes in version 1.3.8 Beta:

New Features:
           * Effects and Analysis:
              * VST Effects now display GUI by default  
              * Faster Equalization and Noise Removal; improved 
                 Truncate Silence and Click Track
              * Chains applied to files now clear temporary data after 
                 processing each file 
              * Updated Nyquist implementation with support for SAL 
                 syntax and improved memory management 
              * Plot Spectrum now analyzes up to 237.8 seconds of audio,
                 with separate windows for each project and improved 
                 display; new preferences for Spectrograms 
              * Contrast Analysis tool now modeless for easier use    
           * Interface:
              * Draft Manual/Quick Help included in Windows and Mac 
                 installers 
              * New "Mixer Board" view with per-track VU meters 
              * Mute, solo, gain, pan and track height saved in projects
              * More compact Preferences window with easier-to-use Keyboard
                 tab and new toolbars shortcuts   
              * New Screenshot Tools and improved screen reader support  
           * Other:
              * Record more than 16 channels (hardware/drivers permitting)  
              * Improved support for non-mmap ALSA devices such as PulseAudio 
              * 32-bit float data over 0 dB now handled without clipping
              * "Stop" option when importing preserves already imported data
              * AMR NB import/export now supported if the optional FFmpeg library
                 is installed
              * Faster waveform drawing and better response in multi-track 
                 projects 

Bug fixes for:
              * Export Multiple: failed with no export or warning if empty 
                 label encountered; silenced project and exported files if
                 overwriting imported WAV files without copying the data in   
              * Metadata Editor hidden if it was on a now unavailable second
                 monitor
              * Misaligned audio after "Split New" or Noise Removal effect
              * Incorrect label movement and paste with linked audio and label 
                 tracks      
              * Equalization, Cut Preview and Advanced Mixing Options dialog
              * (Linux) Mixer Toolbar should now adjust levels and select input
                 sources properly  
              * "Audio cache" preference caused crashes - data is now only
                 cached in memory if available RAM is above a level defined 
                 in preferences 
              * Various other crashes


Changes in version 1.3.7 Beta:

Cross-platform Bug Fixes:
        * Muting/soloing caused incorrect channel results in exported
           stereo files
        * Noise Removal and all Nyquist effects pasted the original
           unmodified audio at the end of the modified region
        * Noise Removal inserted a tail of low level noise at the end
           of the modified region
        * Nyquist and Compressor plug-ins did not display moving bars
           in progress dialog and over-estimated "Remaining Time"
        * Cancelling Nyquist effects deleted unprocessed audio
        * Change Speed and Change Tempo failed to modify the original
           selection length
        * Cut lines invisible
        * Fixed various bugs importing multi-stream files via FFmpeg
        * File > Export as WAV could be corrupted if overwriting
           an imported WAV read direct from the file
        * Export multiple "Other uncompressed files" choice always
           produced 16-bit PCM audio irrespective of chosen options.
        * MP3 export usually produced a 128 kbps constant bit rate file
           irrespective of chosen options; reported length often
           incorrect
        * MP3 ID3 Genre tag misread on import if the genre list in
           Metadata Editor was opened and saved
        * Exported metadata tags in MP3, OGG and FLAC often not seen by
           player software - now substantially improved
        * WMA exports (via FFmpeg)corrupted if metadata tags included
        * Some multi-channel recording devices that previously recorded
           more than two channels no longer did so
        * Generated audio did not fit in window
        * No warning was given when saving an empty project
        * Beep on completing longer process did not work on many
           systems
        * fixed crashes importing lists of files (.LOF), in Meter Toolbar
           and Change Speed

Platform-specific Bug Fixes:
        * Windows Vista: crash opening Preferences with no sound
           devices enabled and connected
        * Mac OS X and Linux:
           * Spurious clipping at start of playback
           * Labels did not accept certain legal characters
           * Shortcuts did not work after running effects
           * Project Rate did not change to respect rate of first
              imported file if that rate was unsupported
        * Mac OS X only:
           * Crash resizing project window
           * Menu items became inactive or visibly corrupted
           * File > Open dialog did not always work on OS X 10.4
           * Impossible to set independent Command and Control
              shortcuts that shared the same key
           * Freeze importing uncompressed files via On-Demand
              (please report any remaining instances of this to:
               feedback@audacityteam.org)
           * Portable settings were not picked up, instead settings
              were taken from the default location
           * Fixed unavailability of FFmpeg installer

New Features:
           * F11 Full Screen mode
           * High-quality "Sliding Time Scale/Pitch Shift" effect
           * Audio Contrast Analyzer for testing audio on the
              internet for WCAG2 accessibility compliance.
           * Windows: sound devices can now be opened using the
              more efficient DirectSound API

Other changes:
           * Latency correction should be improved for many users
              by employing a fixed rather than variable correction
           * Grouping of Effects into categories turned off until
              a way is added for users to do so themselves
           * Numerous minor interface improvements such as Metadata
              Editor navigation, new "hh:mm:ss + hundredths"
              selection format
           * Note: Windows users wanting to export MP3 files will
              require the latest version of the LAME encoder from
              http://lame.buanzo.com.ar/


Changes in 1.3.6 Beta (since 1.3.6a6 Alpha):

Interface:
        * "Save Compressed Copy of Project" saves in much smaller .OGG
           format to facilitate online transmission of projects
        * Improved MIDI import and export routines, and clearer color
           for selection region
        * Default temporary directory on Mac now accessible in Finder

Import / Export:
        * Stability improvements in on-demand loading
        * FFmpeg: support for latest version of library, improved
           version checks and error messages, stability improvements
           in custom exporter

Bug Fixes:
        * Crash in "Get Noise Profile" step of Noise Removal at project
           rates below 20480 Hz.
        * Underestimation of peak level in tracks with a small number
           of different peaks
        * Truncate Silence could result in repeated or lost audio if
           applied to the whole of a track
        * Other interface, generating, exporting and platform-specific
           fixes

Compilation:
        * Added autoconf macro archive to CVS, enabling *.nix users
           without this archive to build --with -MIDI


Changes in 1.3.6a6:

Interface:
        * Note Track now supports export as a MIDI file
        * Linked audio and label tracks: improved support when source
           and target number of tracks differ and when cross-pasting
           different track types

Import / Export:
        * On-demand now supports project saving during summarising;
           reverts to stripey background; fixed some crashes due to
           threading issues
        * Exports: Single AAC filter (M4A LC profile) with quality
           settings slider; removed FFmpeg formats already supported
           by Audacity; added explicit GSM 6.10 (WAV) filter; current
           project rate now used for all exports, with check for
           format-invalid rates; improvements to metadata support

Effects:
        * LV2 plug-ins: added support (OS X and Linux only) for using
           synths as tone generators, scale point labels, grouped
           controls and i18n translations


Changes in 1.3.6a5:

Interface:
        * Note Track now builds on Windows
        * Fixes/improvements for linked audio and label tracks (one
           desynchronisation bug remains when pasting audio into a
           greater number of tracks than were copied); now supports
           label shifting when changing pitch and tempo
        * Added full label pasting support: now possible to paste
           multiple labels, region labels and labels with audio, and
           correct label text is now pasted

Import / Export:
        * Added full "on-demand" support (now with minimum file
           length of 30 seconds): clicking moves summary calculation
           point; supports split and merge of stereo tracks;
           incompletely summarised tracks resume summary calculation
           automatically; text-based Status Bar progress indication and
           animated dummy waveform instead of embedded progress bar


Effects:
        * Fixed a bug where previewing Equalization curves more
           than once previewed the unmodified audio
        * Improvements to DTMF generator

Miscellaneous:
        * Improved support for working with audio in excess of 2^31
           samples (about 13.5 hours at 44100 Hz); some accessibility
           improvements


Changes in 1.3.6a4:

Interface:
        * New Preference: Default View Mode, to choose type of
           waveform, spectrum or pitch view for new tracks
        * Note Track: experimental support is now enabled by defining
           USE_MIDI in config*, but does not build out-of-the-box
           on Windows
        * Bug fixes for linked audio and label tracks; now supports
           label shifting when changing speed and generating tones

Import / Export:
        * Improvements/fixes for AAC exports including new M4A
           filter for compatibility with iTunes; RealAudio export
           support removed - FFmpeg does not support this properly
        * Improved refresh of on-demand loading; fixed a phantom
           on-demand progress bar when time-shifting clips

Effects:
        * Experimental support for LV2 plug-in architecture on Linux
           and Mac, but operation may be buggy; no LV2 support yet on
           Windows, because the required slv2 library currently does
           not build


Changes in 1.3.6a3:

Import / Export:
        * Experimental support for exporting a much wider range
           of proprietary audio formats via FFmpeg
        * "On-demand" immediate loading of imported PCM WAV or
           AIFF files now has experimental "progress bar" embedded in
           the waveform until fully loaded

Interface:
        * Note Track: experimental support for cut, copy and paste
           using Edit Toolbar; currently not available for Linux, where
           EXPERIMENTAL_NOTE_TRACK must be undefined in order
           to build
        * New Transport menu for alternative access to play and record
           commands and some recording preferences
        * Audio tracks are now linked to label tracks by being positioned
           above a label track, if linkage is enabled in the Tracks menu


Changes in 1.3.6a2:

Import / Export:
        * Experimental support for importing a much wider range
           of audio formats via FFmpeg: support has to be enabled
           in *config when building and requires FFmpeg libraries
        * Experimental support for "on-demand" immediate loading
           of imported PCM WAV or AIFF files (full waveform continues
           to load while you play or edit).

Effects:
        * Built-in plug-ins now grouped into related hierarchical
           categories

Interface:
        * New Debug Log window available in all builds
        * Experimental support for linking a label track with any
           number of audio tracks so that labels shift with cuts and
           inserts in the audio track
        * Default theme now reverted to that of 1.3.5
        * Recording channels preference now defaults to stereo

Miscellaneous:
        * Bug fixes for shortcut availability/tab order in Selection Bar,
           and for window focus issues when previewing effects
        * Improvements in escaping and navigating fields in dialogs,
           and in stability when screen readers are used


Changes in 1.3.6a1:

Interface:
        * Further improvements to menu navigation and wordings.
        * All file dialogs are now resizable, and support "Places"
           sidebar on Windows 2000 or later.
        * Preferences:
          * New "Theme" preference for modifying interface
             colours and images, with experimental new default
             colour scheme.
          * New "Smart Recording" preference automatically pauses
             recordings that fall below a pre-defined input level.

Compilation:
        * Simplified modular builds for Windows, removing
           static-linked configurations.
        * New shared configurations on Mac to support modular
           builds, and all builds are now Unicode.

Miscellaneous:
        * Default auto save interval reduced to 2 minutes.
        * Bug fixes to correct project rate initialisation on Linux, and
           file importing issues on PPC Macs.


Changes in 1.3.5:

Recording  / Playback:
   * Several bugs fixed so that latency correction should be better, and more
      devices work correctly. Problems with invalid sample rates under Linux
      should be much rarer.
   * Newer version of Portaudio library.
   * New feature to record onto the end of an existing track
      (hold Shift while clicking Record).

Import / Export:
   * Updated versions of Libogg, Libvorbis, Libflac, Libsndfile and Twolame
      libraries.
   * Handling of unsupported file formats more informative.
   * Handling of file names with slashes on OS X improved. New dialog
      allows replacement of illegal file name characters on all platforms.

Interface:
   * Improved scaling and layout for rulers and VU meters.
   * Envelope fixes/improvements including full control of undo/redo.
   * New keyboard shortcuts and improved menu navigation.
   * Preferences: More intuitive tab arrangement. New options for
      mute/solo and Metadata Editor behavior. Language can now be
      changed without restart.
   * Expanded Build Information tab.

Effects:
   * New Vocal Remover plug-in, improvements for Generate effects.

Compilation:
   * Fixes when building Audacity with libraries disabled.
   * Improvements to make Mac and Solaris builds easier.

Security:
   * Full fix for issue CVE-2007-6061 on systems where temporary directories
      can be changed by other users (thanks to Michael Schwendt).

Miscellaneous:
   * Updated translations for many locales.
   * Several stability improvements.


Changes in 1.3.4:

New Features
   * New Welcome Screen with introduction to Audacity
   * Enhanced Windows Shell integration, so Audacity shows up in lots of
      Windows places such as "Open With".
   * New keyboard command: 'Mix and Render to New Track'
      (bound to Ctrl+Shift+M).
   * New keyboard shortcut: "Shift-A" starts playback when stopped,
      or performs "Stop and Select" when playing.
   * Added support for VAMP audio analysis plug-ins.
   * Solo button solos only one track at a time, and a track cannot be both
      mute and solo.

Interface:
   * Keyboard shortcuts for making short/long jumps along the timeline.
   * Added 'Snap To' in the Selection Bar.
   * Made keyboard navigation easier when multiple menu items with the
      same first letter exist.
   * Enhanced interface for label editing.
   * Layout of OK/Cancel buttons consistency improved.
   * Preferences:
      * "Select all audio in project, if none selected" (on by default)
      * "Beep on completion of longer activities" (system bell, not
         main output).
      * Other preferences cleaned up and explanations improved.
   * Envelopes: Many fixes when copying / pasting / repeating.
   * Many translation updates.
   * Track height fixed in several cases.
   * CleanSpeech mode switching without closing and re-opening fixed.

Opening/saving formats:
   * Metadata editor added for OGG, FLAC and WAV/AIFF exports, and
      general improvements in this area.
   * Import of metadata improved.
   * Muted tracks are no longer audible in the exported mix.

Effects:
   * Truncate Silence: support for multiple and stereo tracks.
   * Dtmf Generator:
      * added support for keypad letters
      * added an amplitude control.
   * Compressor: variable decay time added.
   * Equalization:
      * Clicks at start / end prevented
      * Improvements to saved curves being found
      * Preview works correctly
   * 'Merge' command appears in Undo history.
   * Clipping detected more reliably.
   * Nyquist plug-ins reviewed and enhanced.
   * Better (and more) progress bars.
   * Cancelling effect always restores previous audio.
   * Several improvement to effects in batch mode.

Recording / Playback:
   * Improvements to latency correction.
   * Updated version of portaudio-v19 library.

Note that Help is no longer built in, but accessible on the Web via links
in Audacity.


Changes in 1.3.3:

Opening/saving formats:
   * Import
      * Import of audio from QuickTime (mov, aac, m4a) files is now
        supported on OS X.
      * Broadcast Wave Format (BWF) wave files can now be imported.
   * Export
      * Metadata can be added to OGG files
      * Improved export option selection
      * Additional export options added to MP3 and FLAC file formats
      * Command line exporter now supported on Windows and OS X

Effects:
   * EQ effect
      * Responsiveness improved.
      * Several enhancements added.
      * Batch support added.
   * New Auto Duck effect
   * Added previewing to AudioUnit effects
   * Much improved Noise Removal effect
   * Effects previewing can now be canceled
   * New DTMF Tone Generator effect
   * Additional options available in Noise effect
   * Improved the Tone Generation effects

Other features:
   * Major speed improvement in Spectrogram rendering
   * Increased support for drag and drop on OS X
   * Support added for building against wxWidgets 2.8
   * Support opening multiple Audacity Projects at once from Explorer on
     Windows
   * Improved main window sliders
   * New support for snapping while selecting and sliding
   * Improved track focus handling and visual feedback
   * Speed improvements and handling of resizing/zooming in tracks
   * Spectrum view can now be zoomed.
   * New internal file cache to improve handling of project files over
     networks

Also:
   * Many improvements to language specific translations
   * Numerous stability improvements


Changes in 1.3.1 and 1.3.2:

o Improved accessibility for the visually impaired
      + Improvements for screen readers, accessibility of
        tracks, and hot keys
o Usability improvements
      + New selection bar
      + New features for label tracks
      + Improved toolbar docking flexibility
      + Menu renaming and reorganization
      + Selection, ruler, and playback control improvements
o Auto-save and automatic crash recovery
o Many bug fixes and stability improvements
o Major improvements to some built-in effects (Repair, Equalization)
  and fixes to others
o New features and bug fixes for Nyquist
o Restructured Preferences dialog
o Improved batch processing
o File format export improvements
o Timer recording
o Intel Mac support


Changes in 1.3.0:

   New features
   The new features in Audacity 1.3 have been grouped into the
   following six major categories.

   1. Collapse/Expand Tracks
      In Audacity 1.3, every track has an upward-pointing triangle at
      the bottom of the label area on the left side of the track.

   2. Multiple clips per track
      In Audacity 1.2, there is one audio 'clip' per track. There is
      no easy way to time-shift part of a track without moving the
      rest. In Audacity 1.3, you can split a single track into multiple
      clips. You can move these clips around between different tracks,
      making it easy to construct complex compositions out of hundreds
      of smaller audio samples.

   3. Selection Bar
      In Audacity 1.2, the current selection is contained in a
      status bar at the bottom of the window. In Audacity 1.3,
      this is replaced by a fully functional Selection Bar, which
      displays and controls the current selection (your choice of
      Start and End, or Start and Length), and the current audio
      position. The selection bar is fully editable - just click
      in any field and type to change the current selection precisely.
      In addition, many formatting options allow you to view times in
      different units, such as samples, CD frames, or NTSC video frames.

   4. Improved Label Tracks
      Label Tracks are Audacity's way for you to create markings 3
      and annotations within your project. In Audacity 1.3, Label
      Tracks are much improved, with support for overlapping labels,
      and support for modifying both the left and right edge of the
      label region just by clicking and dragging.

   5. QuickTime and Audio Units on Mac OS X
      On Mac OS X, Audacity can now import and audio file supported
      by Apple's QuickTime technology. This includes .MOV and .MP4
      (AAC) files. Unfortunately encrypted audio files (such as
      those from the iTunes Music Store) cannot be imported directly
      into Audacity - Apple does not allow this to be done easily
      because it would be too easy to circumvent the encryption this way.

      Also on Mac OS X, Audacity now supports Audio Unit plug-ins.
      Audacity searches for Audio Units in the usual location, in the
      system or user's Library folder.

   6. Other features
      Better performance with large projects

      Project integrity check on open

      Transcription toolbar

      Upload

      Batch

      Cut lines

      CleanSpeech


Changes in 1.2.4:

  * The File menu now includes a list of recent files.

  * The "Generate Silence" effect now prompts for a length.

  * Audacity is now built with Vorbis 1.1, which features better encoding
    quality and file compression.

  * Dragging sound files into the Audacity window now works on Mac OS X
    and Linux, as well as Windows.  (Before, it worked only on Windows.)

  * The "View History" window can now discard old undo levels to save disk
    space on Windows.  (This previously worked only on Linux and Mac.)

  * "Preferences" command is now in Edit menu.

  * "Plot Spectrum" command is now in Analyze menu.

  * Opening a project file saved by a later version of Audacity displays
    an intelligent error message.  Also, trying to import a project file
    (instead of open it) displays an intelligent error message.

  * Audacity now compiles in Visual C++ .NET 2003.

  * Other minor bug fixes.

  * New or updated translations: Arabic (ar), Czech (cs), Finnish (fi),
    Hungarian (hu), Japanese (ja), Norwegian (nb), Slovenian (sl),
    Simplified Chinese (zh_CN), Traditional Chinese (zh_TW).


Changes in 1.2.3:

  * Fixed a bug that caused recording to stop or display incorrectly
    after about 50 minutes on some Windows systems.  (This was partly
    fixed in Audacity 1.2.2, but still didn't work on some systems.)

  * The Change Pitch and Change Tempo effects have been upgraded to
    use a new version of the SoundTouch library by Olli Parviainen,
    with better speed and higher quality.

  * libsndfile has been upgraded to version 1.0.11.

  * Fixed a bug that caused the program to run slowly when using the
    Envelope tool.

  * Shift-clicking on a mute or solo button now un-mutes (or un-solos)
    all other tracks.

  * Nyquist plug-ins can now accept strings as input.  Also, a "Debug"
    button has been added to Nyquist effect dialogs, which allows you
    to see all of the output produced by Nyquist, for aid in debugging.

  * When the audio file referenced ("aliased") by an Audacity project is
    missing, Audacity will now always play silence.  Before, Audacity
    would sometimes repeat the most recent audio that was played previously.

  * VU Meters will now always reset when audio I/O has stopped.

  * Fixed a major Mac-only bug that was causing Audacity to crash at seemingly
    random times, but especially during audio playback and recording.

  * New or updated translations: Italian (it), Hungarian (hu),
    Ukrainian (uk), Spanish (es). Polish (pl), Simplified Chinese (zh),
    Norsk-Bokmal (nb), French (fr).


Changes in 1.2.2:

  * VU Meters added for both playback and recording.  Click on
    the recording meter to monitor the input without recording.

  * Export Multiple - new feature that lets you export multiple
    files at once, either by track, or split based on labels.

  * Attempt to automatically correct latency in full-duplex recordings.
    (This does not work perfectly, and is not yet supported on all
    systems.  It will improve in future versions.)

  * Fixed a serious bug that could cause data loss when you save and
    then reload and re-edit an Audacity project containing repeated
    or duplicate data.

  * MP3 tags dialog will only pop up the first time you export as
    MP3; after that it will not pop up again as long as you have
    filled in at least one tag.

  * You can now add a label at the current playback position - in
    the Project menu, with a shortcut of Ctrl+M.

  * Clicking on a label now selects all of the tracks, making it
    easier to use the label track to recall selections.

  * Windows: Fixed a crash in the Time Track "Set Rate" command.

  * Fixed a bug that caused problems with recordings over 45 minutes
    on some Windows systems.

  * Mac OS X: Improved support for the Griffin iMic by fixing a bug
    that was causing it to always record in mono instead of stereo.

  * Added support for Software Playthrough (listen to what you're
    recording while recording it, or while monitoring using a VU
    meter) - this makes it possible, for example, to record using one
    audio device while listening to it play through a separate device.

  * Unix/Linux: Fixed freeze caused by captured mouse when audio
    device hangs.  (Audacity may not respond, but it will no longer
    freeze all of X.)

  * Fixed a cosmetic bug that caused improper waveform display if
    you tried to open an Audacity project saved on a different
    platform (e.g., copying a project from a Mac to a PC).

  * Fixed bug that could cause instability when pasting, splitting,
    or duplicating a label track.

  * You can now change the font of a label track by choosing "Font..."
    from the label track's pop-up menu.

  * Basic printing support has been added.  Currently it scales the
    entire project to fit exactly on one page.  Try printing in
    landscape orientation for best results.

  * Mac OS X and Windows: Audacity ships with a newer version (1.0.1)
    of the Ogg Vorbis encoder.  Vorbis compression will now have higher
    quality and smaller file sizes.

  * Fix a bug that occasionally caused crashes when applying effects
    to split tracks.

  * Zoom In / Zoom Out now properly disable when they're not available.

  * Fixed disk memory leak in Preview

  * Other minor bug fixes and performance improvements.


Changes in 1.2.1:

  * The following translations have been added or updated:  Finnish,
    French, Hungarian, Italian, Japanese, Norwegian, Polish, Russian.

  * Fix a bug that could cause data to be lost when pasting audio
    from one project into another, after the first project has been
    saved and closed.

  * Fix a possible crash when opening or resizing the Equalization
    window, especially when using large system fonts.

  * Don't allow percentages less than -100% in Change Pitch/Speed/Tempo
    effects (fixes a possible crash).

  * Fix a crash when the temporary directory is not available on startup.

  * Correctly load ID3 tags saved in Audacity project files.

  * On Linux and OS X, store lockfiles in the temp directory instead of
    the user's home directory.  This fixes problems in lab environments
    where users have restricted or network-mounted home directories.

  * Fix a bug that prevented Nyquist effects from running when certain
    regional settings were activated.

  * Fix a bug in the Quick Mix command that could cause old temporary
    files to not be deleted.

  * Linux: Fix endianness problems in playback on PowerPC.

  * Linux: Fix compilation problem in Nyquist on MIPS.

  * Linux: Include a more recent PortAudio v19 snapshot (fixes compilation
    problems when building with the --with-portaudio=v19 option).

  * Two new Nyquist plug-ins: "Cross Fade In" and "Cross Fade Out."

  * Other minor bug-fixes.


Changes in 1.2.0:

  * New cross-fade effects.

  * Fix problem where samples were drawn in the wrong position
    when zoomed all the way in.  This caused the drawing tool
    to move a different sample than the one under the cursor.

  * Don't use id3v2.4 tags, which are not yet supported by
    most players.  (This was fixed in 1.2.0-pre2, but appeared
    again by accident in 1.2.0-pre3.)

  * Correctly display translated messages in the status bar.

  * When the cursor is on-screen, the Zoom In button now zooms
    to the area around the cursor.

  * Mac OS X: Fixed audio problems on the Apple PowerMac G5.

  * Linux/ALSA: Work around a bug in ALSA's OSS emulation that
    caused Audacity's playback cursor to move too quickly.

  * Microsoft Windows: The Audacity source code should now
    compile out of the box on Windows.

  * Many new/updated translations.


Changes in 1.2.0-pre4:

  * Fixed problems that could occur when importing certain
    non-seekable PCM audio files, such as GSM610.

  * Fixed bug that was causing the samples to shift off-screen
    horizontally when zoomed in very far and the track had a
    time-shift offset.

  * Fixed bugs in the new resampler that added noise to resampled
    audio on some systems. If you experienced noise when exporting
    to a WAV, MP3 or OGG file you may have been bitten by this bug.

  * Fixed bug that led to occasional crashes when using the
    time-shift tool in conjunction with high zoom factors.

  * Dithering is now only applied on export when it is really
    necessary (e.g. when converting float samples to 16-bit).

  * Files that only contain mono tracks are now automatically
    exported to stereo files when they contain tracks which are
    panned to the left or the right.

  * The Delete key can now be used to delete the current selection,
    in addition to the Backspace key.

  * Fixed bug where Audacity didn't ask whether to save
    changes if you close the project or exit while recording.

  * Mac OS X: Supports Playthrough (listen to what you're recording
    while recording it) if your hardware device supports it.

  * Mac OS X: Audacity is now a package (you can right-click on
    Audacity.app and select 'Show Package Contents').  Launch time
    has improved significantly.

  * MS Windows: Fixed problem that caused Windows XP to use
    the short name of a file ("TESTFI~1.AUP"), which led to
    problems when the file was later opened again using the
    long file name.

  * MS Windows: Fixed bug that caused file exports to fail
    if the destination directory was the root folder of a
    Windows drive.

  * MS Windows: Audacity's application information which
    is written to the Windows registry now always contains
    the full path to the executable.

  * MS Windows: Fixed problems in trying to set the Windows
    registry as non-admin user, for file-type associations.

  * Make sure the "Save" command is enabled after changing
    gain and pan sliders.

  * Updated translations.  Added translator credits to the
    "About" window in localized versions.


Changes in 1.2.0-pre3:

  * Fixed bug where Export is grayed out when nothing is
    selected.

  * Fixed crash caused by opening Audacity on a computer with
    a high-end sound card with no mixer support.

  * Fixed crash in Import Raw.

  * Fixed New Stereo Track.

  * Cosmetic fixes for Mac OS X.

  * Support for the VST Enabler on Windows added.

  * Fixed crash if you close Audacity while the Preferences
    dialog is open.

  * Fixed duplicate-character bug in Mac OS X Label Tracks.

  * The recording level control on Linux now adjusts the IGAIN,
    rather than the playthrough level of the recording source.

  * Fixed bug that caused corruption to 16-bit stereo recordings.

  * Fixed bug that caused data loss if you deleted all tracks in
    a saved project and then open a new file into the same window.

  * Added support for alternate audio button order (in Interface
    preferences)

  * Added preliminary support for wxX11

  * Added fully transparent Windows XP icon

  * Fixed crash if you try to record (or play) and no audio
    devices exist, or if the audio device doesn't support the
    mode you selected.

  * Audacity no longer sets the process priority to high while
    recording on Windows.  Users can still do this manually
    using the Task Manager.

  * Fixed bug that caused last ~100 ms of the selection to get
    cut off on Windows.

  * Fixed FFT Filter and Equalization effects dialogs.

  * Fixed bugs in Unix build system (DESTDIR in locale directory,
    choosing libsamplerate instead of libresample)

  * Support for LADSPA plug-ins on Windows added, and
    three open source LADSPA plug-ins ported to Windows
    (GVerb reverb, SC4 compressor, and Hard Limiter)


Changes in 1.2.0-pre2:

  * Online help completed.  The full manual is nearly complete
    and will be posted to the web site for online browsing shortly.

  * Audacity will no longer let you do unsafe editing operations
    while playing or recording.  This eliminates many potential
    crashes.

  * Fixed ability to cancel Quit button.

  * New resampling library, with no restrictions on the maximum or
    minimum rate of resampling.

  * Audacity now supports LADSPA plug-ins on all platforms, and
    supports VST plug-ins through an optional LADSPA plug-in
    called the "VST Enabler", which you can download separately.
    Because of licensing issues, Audacity cannot be distributed
    with VST support built-in.

  * Mac OS X keyboard shortcut problems have been fixed.

  * Mac OS X audio muting problems have been fixed.

  * Mac OS X playback/recording cursor sync problems have been fixed.

  * Silence now displays a straight line again, instead of nothing.

  * Added a vertical ruler to the Waveform dB display.

  * Fixed crash in Change Pitch.

  * You can now Paste if nothing is selected.

  * Canceling an Import operation doesn't cause an extra error
    dialog to appear.

  * Audacity now handles filenames with international characters
    correctly.

  * Now outputs ID3v2.3 tags (instead of ID3v2.4), to be
    compatible with more MP3 players.

  * Minor improvements to build system on Unix systems.


New features in Audacity 1.2:
  * User Interface
    - Vertical zooming of tracks.
    - Improved look and placement of toolbars.
    - New custom mouse cursors.
    - Complete implementation of editable keyboard shortcuts.
    - Find zero-crossings.
    - Mouse wheel can be used to zoom in and out.
    - Multi-Tool mode.
    - Amplify using envelope.
    - Labels can store selections (like Audacity 1.0.0).

  * Effects
    - Repeat Last Effect command
    - Improved VST plug-in support
    - Most effects now have a Preview button
    - Compressor (Dynamic Range Compressor)
    - Change Pitch (without changing tempo)
    - Change Tempo (without changing pitch)
    - Change Speed (changing both pitch and tempo)
    - Repeat (useful for creating loops)
    - Normalize (adjust volume and DC bias)

  * Audio I/O
    - 1-second preview command.
    - Looped play.

  * File I/O
    - Audacity 1.2.0 opens project files from all previous versions
      of Audacity from 0.98 through 1.1.3.
    - Open multiple files from the same dialog.
    - Use a text file to specify a list of audio files to open with offsets.

  * Updated user manual

  * Bug fixes
    - Project files with special characters are no longer invalid.
    - "Scratchy" noises caused by bad clipping are fixed.
    - Audacity no longer exports invalid Ogg files, and does not cut off the
      last few seconds of exported Ogg files.
    - Mono MP3 files now export at the correct speed.
    - Many incorrect results from the Envelope tool have been fixed.
    - The "Export Labels" command now overwrites existing files correctly.
    - The "Plot Spectrum" window displays the correct octave numbers for
      notes.
    - Several memory leaks are fixed.


New features in Audacity 1.1.3:
  * User Interface
    - New Mixer toolbar allows you to control the output
      volume, input volume, and input source directly
      from Audacity.
    - Every track now has its own gain and pan controls.

  * File I/O
    - Uses improved project file format.  (Unfortunately reading
      previous formats, including 1.1.1, is not supported.)
    - Block files (stored in Audacity project directories) now
      use the standard AU format.  Though some Audacity
      meta-information is in these files, they can now be
      read by many other popular audio programs as well.
    - Fixed some bugs relating to reading/writing audio
      files with more than 16 bits per sample.
    - Import RAW is functional again, with a simpler GUI
      but support for far more file formats.  The
      autodetection algorithms are much more accurate than
      in 1.0.

  * Audio I/O
    - Completely rewritten audio I/O, with lower latency
      and minimal chance of buffer underruns while
      recording.

  * Resampling
    - Using high quality resampling algorithms, with the
      option of better quality for mixing than for real-time
      playback

    - Preliminary support for Time Tracks, for changing
      playback speed over time.

  * Many more bug fixes and new features


New features in Audacity 1.1.2:
  * User Interface
    - Fixed bug in Windows version, for track menu commands
     "Name..." and "Split Stereo Track"/"Make Stereo Track".
  * Effects
    - Nyquist support on Windows (supports plug-ins written
     in Nyquist, an interpreted functional language based
     on Lisp).


New features in Audacity 1.1.1:

  * User Interface
    - Tooltips appear in Status Bar.
    - Vertical cursor follows play/record
    - Pause button
    - Drawing tool (with three different modes)
    - Vertical Resizing of stereo tracks is more fun.
    - Adjust selection by click-dragging selection boundary
    - Toolbar button context-sensitive enabling/disabling
    - Better zooming functionality (centers region)
    - Multiple ways to display the cursor position and selection
    - Snap-to selection mode
    - Drag tracks up and down
    - Align and group align functions
    - Cursor save/restore
    - Working history window
  * Effects
    - Effects broken down into three menus: Generate, Effect, and
      Analyze
    - Generate menu lets you generate silence, noise, or a tone
    - Nyquist support (supports plug-ins written in Nyquist,
      an interpreted functional language based on Lisp)
  * Localization
    - Improved localization support
    - More languages available
    - Language selection dialog on startup
  * Mac OS X
    - Support for more audio hardware
    - Support for full-duplex (play while recording)
    - Support for MP3 exporting using LameLib Carbon
  * Unix
    - Audacity now has a man page (it describes command-line
      options and how to set the search path)
  * File Formats
    - Uses libsndfile 1.0, which fixes some bugs and
      improves performance
  * Searching for Files:
    - On Windows and Mac OS, Audacity now looks for
      translations in the "Languages" folder and all plug-ins
      in the "Plug-ins" folder, relative to the program.
    - On Unix, Audacity looks for translations in
      <prefix>/share/locale and looks for everything else
      in <prefix>/share/audacity and also in any paths in
      the AUDACITY_PATH environment variable


New features in Audacity 1.1.0:

  * Core audio processing:
    - Support for 24-bit and 32-bit sample formats
    - Automatic real-time resampling (using linear
        interpolation)
  * Effects:
    - Support LADSPA plug-ins on Linux / Unix
  * File formats:
    - New XML-based Audacity project format
    - Full Ogg Vorbis support now (importing and exporting)
    - Export to any command-line programs on Unix
    - Support for reading and writing many more types of
        uncompressed audio files, including ADPCM WAV files.
  * Toolbars
    - New toolbar drawing code; automatically adopts your
        operating system's colors
    - New toolbar buttons (Skip to Start, Skip to End)
    - New Edit toolbar
    - Toolbar buttons disable when they're not available
  * User Interface
    - Fully customizable keyboard commands
    - Autoscroll while playing or recording
    - New Ruler, used in main view and in
        FFT Filter effect
    - The waveform now displays the average value in a lighter
        color inside the peak values
  * Localization
    - Audacity can now be localized to different foreign
      languages.


New libraries in Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
