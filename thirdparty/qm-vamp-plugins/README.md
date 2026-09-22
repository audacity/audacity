
QM Vamp Plugins
===============

Audio feature extraction plugins in [Vamp format](https://vamp-plugins.org/)
from the [Centre for Digital Music](http://c4dm.eecs.qmul.ac.uk) at
Queen Mary, University of London.


Plugins included
----------------

This plugin set includes the following plugins:

   * Note onset detector

   * Beat tracker and tempo estimator

   * Key estimator and tonal change detector

   * Adaptive multi-resolution FFT spectrogram

   * Polyphonic note transcription estimator

   * Segmenter, to divide a track into a consistent sequence of segments

   * Timbral and rhythmic similarity between audio tracks

   * Wavelet scalogram (discrete wavelet transform)

   * Chromagram, constant-Q spectrogram, and MFCC calculation plugins

For full details about the plugins, with references, please see

  https://vamp-plugins.org/plugin-doc/qm-vamp-plugins.html


Changes for this release
------------------------

See the CHANGELOG.md file included.


Compiling the plugins
---------------------

Ready-to-use binary builds of these plugins are provided for common
platforms, but it's also possible to build them from source code.

To compile your own from source, first run `./repoint install` to
gather the necessary library code, and then

 - Linux: `make -f build/linux/Makefile.linux64`

 - Mac: `make -f build/osx/Makefile.osx`

 - Windows (MSVC): Use the solution `build/msvc/QMVampPlugins.sln`


Licence
-------

These plugins are provided under the terms of the GNU General Public
License.  You may install and use the plugin binaries without fee for
any purpose commercial or non-commercial.  You may also modify and
redistribute the plugins in source or binary form, provided you comply
with the terms given by the GNU General Public License.  See the file
COPYING for more details.

Further copyrights apply to the qm-dsp library used in these
plugins. See the README for that library for details.

If you wish to use this code in a proprietary application or product
for which the terms of the GPL are not appropriate, please contact QM
Innovation https://www.qminnovation.co.uk/ for licensing terms.

Copyright (c) 2006-2020 Queen Mary, University of London.
