/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  *
 *   USA                                                                   *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_H
#define TAGLIB_H

#define TAGLIB_MAJOR_VERSION 1
#define TAGLIB_MINOR_VERSION 5
#define TAGLIB_PATCH_VERSION 0

#include <string>

//! A namespace for all TagLib related classes and functions

/*!
 * This namespace contains everything in TagLib.  For projects working with
 * TagLib extensively it may be conveniten to add a
 * \code
 * using namespace TagLib;
 * \endcode
 */

namespace TagLib {

  class String;

  typedef wchar_t wchar;
  typedef unsigned char uchar;
  typedef unsigned int  uint;
  typedef unsigned long ulong;

  /*!
   * Unfortunately std::wstring isn't defined on some systems, (i.e. GCC < 3)
   * so I'm providing something here that should be constant.
   */
  typedef std::basic_string<wchar> wstring;

#ifndef DO_NOT_DOCUMENT // Tell Doxygen to skip this class.
  /*!
   * \internal
   * This is just used as a base class for shared classes in TagLib.
   *
   * \warning This <b>is not</b> part of the TagLib public API!
   */

  class RefCounter
  {
  public:
    RefCounter() : refCount(1) {}
    void ref() { refCount++; }
    bool deref() { return ! --refCount ; }
    int count() { return refCount; }
  private:
    uint refCount;
  };

#endif // DO_NOT_DOCUMENT

}

/*!
 * \mainpage TagLib
 *
 * \section intro Introduction
 *
 * TagLib is a library for reading and editing audio meta data, commonly know as \e tags.
 *
 * Features:
 * - A clean, high level, C++ API to handling audio meta data.
 * - Format specific APIs for advanced API users.
 * - ID3v1, ID3v2, APE, FLAC and Xiph tag formats.
 * - MP3, MPC, FLAC, Ogg FLAC, Ogg Vorbis and Speex file formats.
 * - Basic audio file properties such as length, sample rate, etc.
 * - Long term binary and source compatibility.
 * - Extensible design, notably the ability to add other formats or extend current formats as a library user.
 * - Full support for unicode and internationalized tags.
 * - Dual <a href="http://www.mozilla.org/MPL/MPL-1.1.html">MPL</a> and
 *   <a href="http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html">LGPL</a> licenses.
 * - No external toolkit dependancies.
 *
 * \section why Why TagLib?
 *
 * TagLib originally was written to provide an updated and improved ID3v2 implementation in C++ for use
 * in a variety of Open Source projects.  Since development began in 2002 and the 1.0 release in 2004
 * it has expanded to cover a wide variety of tag and file formats and is used in a wide variety of
 * Open Source and proprietary applications.  It now supports a variety of UNIXes, including Apple's OS
 * X, as well as Microsoft Windows.
 *
 * \section commercial Usage in Commercial Applications
 *
 * TagLib's licenses \e do allow usage within propriety (\e closed) applications, however TagLib is \e not
 * public domain.  Please note the requirements of the LGPL or MPL, and adhere to at least one of them.
 * In simple terms, you must at a minimum note your usage of TagLib, note the licensing terms of TagLib and
 * if you make changes to TagLib publish them.  Please review the licenses above before using TagLib in your
 * software.  Note that you may choose either the MPL or the LGPL, you do not have to fulfill the
 * requirements of both.
 *
 * \section installing Installing TagLib
 *
 * Please see the <a href="http://developer.kde.org/~wheeler/taglib.html">TagLib website</a> for the latest
 * downloads.
 *
 * Instructions for installing TagLib vary per platform, but generally speaking on UNIX standard configure and
 * make commands are provided.  TagLib installs a taglib-config and package-config file to make it easier to
 * integrate into various build systems.  Note that TagLib's include install directory \e must be included in
 * the header include path.  Simply adding <taglib/tag.h> will \e not work.
 *
 * On Windows, TagLib can be built using the CMake build systems.
 *
 * \section start Getting Started
 *
 * TagLib provides both simple, abstract APIs which make it possible to ignore the differences between tagging
 * formats and format specific APIs which allow programmers to work with the features of specific tagging
 * schemes.  There is a similar abstraction mechanism for AudioProperties.
 *
 * The best place to start is with the <b>Class Hierarchy</b> linked at the top of the page.  The File and
 * AudioProperties classes and their subclasses are the core of TagLib.  The FileRef class is also a convenient
 * way for using a value-based handle.
 *
 * \note When working with FileRef please consider that it has only the most basic (extension-based) file
 * type resolution.  Please see its documentation on how to plug in more advanced file type resolution.  (Such
 * resolution may be part of later TagLib releases by default.)
 *
 * Here's a very simple example with TagLib:
 *
 * \code
 *
 * TagLib::FileRef f("Latex Solar Beef.mp3");
 * TagLib::String artist = f.tag()->artist(); // artist == "Frank Zappa"
 *
 * f.tag()->setAlbum("Fillmore East");
 * f.save();
 *
 * TagLib::FileRef g("Free City Rhymes.ogg");
 * TagLib::String album = g.tag()->album(); // album == "NYC Ghosts & Flowers"
 *
 * g.tag()->setTrack(1);
 * g.save();
 *
 * \endcode
 *
 * More examples can be found in the \e examples directory of the source distribution.
 *
 * \section Contact
 *
 * Questions about TagLib should be directed to the TagLib mailing list, not directly to the author.
 *
 *  - <a href="http://developer.kde.org/~wheeler/taglib/">TagLib Homepage</a>
 *  - <a href="https://mail.kde.org/mailman/listinfo/taglib-devel">TagLib Mailing List (taglib-devel@kde.org)</a>
 *
 * \author Scott Wheeler <wheeler@kde.org> et al.
 *
 */

#endif
