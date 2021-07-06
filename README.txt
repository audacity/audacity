Audacity(R): free, open source, cross-platform audio software for 
multi-track recording and editing: https://www.audacityteam.org/ . 

User support is provided on Audacity Forum:
https://forum.audacityteam.org/ .

We welcome feedback on Audacity, suggestions for new or improved features, 
and bug reports. Please visit 
https://forum.audacityteam.org/viewforum.php?f=25 .

Audacity is copyright (c) 1999-2021 by Audacity Team. This copyright 
notice applies to all documents in the Audacity source code archive, 
except as otherwise noted (mostly in the lib-src subdirectories). 
"Audacity" is a registered trademark. 

The Audacity documentation is licensed under the Creative Commons
Attribution 3.0 license: https://creativecommons.org/licenses/by/3.0/legalcode .

Compilation instructions for Audacity are provided in the source code:
* Windows: win\build.txt
* macOS: mac/build.txt
* GNU/Linux: linux/build.txt 

You can ask for help with compilation problems at:
https://forum.audacityteam.org/viewforum.php?f=19 .

If you want to suggest some simple text change in our code, please submit a 
pull request on https://github.com/audacity/audacity/pulls . It's usually 
best to discuss functional code changes with us first on audacity-devel: 
https://lists.sourceforge.net/lists/listinfo/audacity-devel . 

Version 3.0.3

Contents of this README:

1.  Licensing
2.  Changes since version 3.0.2
3.  Known Issues at Release
4.  Source Code, Libraries and Additional Copyright Information

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
to https://www.gnu.org/licenses/old-licenses/gpl-2.0.html or write to

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA


--------------------------------------------------------------------------------

2. Changes since version 3.0.0: 


Improvements

 * More information to help Audacity developers is now provided 
   with most error reports.
 * Manage Macros dialog is now clearer about how to save and
   close the dialog, thanks to changes in buttons.
 * In MP3 export we removed a legacy option for a slower encoding
   that is no better than the faster one.

See also: https://wiki.audacityteam.org/wiki/New_features_in_Audacity_3.0.2


Bug Fixes

 Over 30 bugs in 3.0.0 fixed, including:


 * 2676 - Export: Incorrect handling over 0 dB
 * 2692 - Mac: VI users get trapped in radio buttons of Keyboard Preferences
 * 2694 - Coloring of tracks in Mixer Board are awry
 * 2696 - Metadata Editor: changed size is not remembered/restored on next use
 * 2697 - Linux: AUP3 projects use incorrect char size on "differently"
          configured wxWidgets
 * 2698 - Noise Gate fails silently if "hold" is non-zero
 * 2699 - Windows: Cannot edit labels with emoji in them correctly
 * 2701 - Progress dialog interferes with scripting
 * 2703 - Mac: EGAT preview dialog is hidden behind the EGAT dialog
 * 2704 - Mac: Stop button in EGATs with Preview does nor stop the preview
 * 2706 - Win: Vocal Reduction and Isolation most options fail
 * 2707 - Opened projects not being removed from "audacity.cfg"
 * 2708 - Nyquist effects fail silently with One hour plus stereo
 * 2709 - Mac: Missing sub-menu check marks in French
 * 2710 - Compressor with audio before T=0 crashes Audacity
 * 2714 - (Japanese & Korean) "Unable to parse project information"
 * 2716 - Cleared "Recent Files" are restored on next launch
 * 2718 - Failed to execute a project file command (on server)
 * 2720 - Cannot open project that's in a read only folder - error message vague
 * 2722 - Noise Gate fails on a one hour stereo selection

See also: https://wiki.audacityteam.org/wiki/Release_Notes_3.0.2


-------------------------------------------------------------------------------


3. Some Known Issues in 3.0.2:

We have two significant bugs we would like help in tracking down:

1) Audacity may occasionally report "Failed to open the project 
   database".  We don't think this is causing people to lose data.
2) Audacity may offer to 'recover' a project at start up, when
   there is nothing to recover.

If there are repeatable steps to make these bugs happen, this
could help us track down what is causing them.

For best workarounds and other known issues in 3.0.2, please see:
  https://wiki.audacityteam.org/wiki/Release_Notes_3.0.2/Issues 


-------------------------------------------------------------------------------

4.  Source Code, Libraries and Additional Copyright Information

Source code to this program is always available; for more information visit
our web site at:

  https://www.audacityteam.org/download/source

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

  libmp3lame: LGPL
    Encodes MP3 files.

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

  SQLite: Public Domain
    Small and fast SQL database engine.
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

Copyright (c) 2000-2021, by Roger B. Dannenberg
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
