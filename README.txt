DarkAudacity: A Free, Cross-Platform Digital Audio Editor
  WWW: http://www.darkaudacity.com/

DarkAudacity is a branch of Audacity(R).  Audacity is maintained by the 
audacity team.

I welcome feedback on DarkAudacity, suggestions for new or improved features, 
bug reports and patches at:
  james@audacityteam.org .

Audacity is copyright (c) 1999-2019 by Audacity Team. This copyright notice
applies to all documents in the Audacity source code archive, except as
otherwise noted (mostly in the lib-src subdirectories).

The additions and changes to Audacity for DarkAudacity are (c) 2019 by
James Crook.

The documentation for Audacity is licensed under the Creative Commons
Attribution 3.0 license:
http://creativecommons.org/licenses/by/3.0/legalcode .


"Audacity" is a registered trademark of Dominic Mazzoni.

DarkAudacity Version 2.3.2x 

The x,y, and z suffixes indicate experimental releases.  DarkAudacity releases 
are cutting edge releases.  Whilst I am careful about the changes I make, if 
you want assurances about the degree of testing done, use an official Audacity 
release that has been through the official Audacity QA process.



Contents of this README:

1.  Licensing
2.  Changes since earlier versions.
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
to http://www.gnu.org/licenses/old-licenses/gpl-2.0.html or write to

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA


-------------------------------------------------------------------------------

2. DarkAudacity 2.3.2x, changes since earlier versions.

 Many of the changes pioneered in Dark Audacity 2.1.3x have now ben integrated 
 into mainstream Audacity 2.3.2.  Audacity has updated its standard (light) 
 theme to use much of the new design.  Audacity now offers our dark theme as 
 a new option.  Audacity has reorganised its menus to be more compact.

 There isn't a lot of difference between DarkAudacity and Audacity.  That's
 mainly because I've been doing the work of modifying Audacity to have the
 new features.

 Dark Audacity is working well as a way to blaze new trails.  These are
 some key differences between DarkAudacity 2.3.2x and Audacity 2.3.2.


 * Interface:  
   * Dark is the default theme.
   * Mute and Solo buttons are stacked rather than side-by-side.
   * Larger cursor position indicator in Selection Toolbar.
   * Combined Device, Meter and Mixer toolbars into one simpler toolbar.
   * Option of LED meters (it's the new default).
   * Different Logos.
   * Clutter on left of track panel removed rather than moved to the bottom.
   
 * Other Changes:   
   * Installer ships without manual; Translations not installed; accessibility 
     support not enabled.  Only Windows.  [These save me a lot of time in 
     preparing a release - e.g 3 platforms = 3x the work.].

   
Bug fixes:
 * 82 bugs we knew about in 2.1.2x addressed, 66 closed, 16 had some residual 
   problems after the fixes.  These issues included a dramatic slowdown when 
   using Lyrics Window with lots of lyrics (fully fixed), and labels 
   being written at the wrong position in some circumstances (fully fixed). I'm
   not listing all these issues, but this gives an idea of the kind of issue.
 * At least as much work has gone into other issues which we did not know about 
   when we released 2.1.2x.  These were for example caused by work on new 
   features, and usually were in those features, or came to light as a result 
   of code review (mostly code review by Paul).

 
-------------------------------------------------------------------------------

3. Known Issues in 2.3.2x:

Please see the facebook page for DarkAudacity for discussion of issues.

Many issues that affect Audacity 2.3.2 will affect DarkAudacity 2.3.2x too, 
since DarkAudacity and Audacity have a lot of code in common.  Many Audacity 
2.3.2 issues are listed here:

  http://wiki.audacityteam.org/wiki/Release_Notes_2.3.2



-------------------------------------------------------------------------------

4.  Source Code, Libraries and Additional Copyright Information

Source code to this program is always available; for more information visit:

  http://audacityteam.org/download/source - for Audacity
  https://github.com/JamesCrook/audacity - for DarkAudacity branch.
  
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
