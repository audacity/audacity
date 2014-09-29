/***************************************************************************
    copyright            : (C) 2002 - 2008 by Scott Wheeler
    email                : wheeler@kde.org

    copyright            : (C) 2010 by Alex Novichkov
    email                : novichko@atnet.ru
                           (added APE file support)
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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#include <tfile.h>
#include <tstring.h>
#include <tdebug.h>
#include "trefcounter.h"

#include "fileref.h"
#include "asffile.h"
#include "mpegfile.h"
#include "vorbisfile.h"
#include "flacfile.h"
#include "oggflacfile.h"
#include "mpcfile.h"
#include "mp4file.h"
#include "wavpackfile.h"
#include "speexfile.h"
#include "opusfile.h"
#include "trueaudiofile.h"
#include "aifffile.h"
#include "wavfile.h"
#include "apefile.h"
#include "modfile.h"
#include "s3mfile.h"
#include "itfile.h"
#include "xmfile.h"

using namespace TagLib;

class FileRef::FileRefPrivate : public RefCounter
{
public:
  FileRefPrivate(File *f) : RefCounter(), file(f) {}
  ~FileRefPrivate() {
    delete file;
  }

  File *file;
  static List<const FileTypeResolver *> fileTypeResolvers;
};

List<const FileRef::FileTypeResolver *> FileRef::FileRefPrivate::fileTypeResolvers;

////////////////////////////////////////////////////////////////////////////////
// public members
////////////////////////////////////////////////////////////////////////////////

FileRef::FileRef()
{
  d = new FileRefPrivate(0);
}

FileRef::FileRef(FileName fileName, bool readAudioProperties,
                 AudioProperties::ReadStyle audioPropertiesStyle)
{
  d = new FileRefPrivate(create(fileName, readAudioProperties, audioPropertiesStyle));
}

FileRef::FileRef(File *file)
{
  d = new FileRefPrivate(file);
}

FileRef::FileRef(const FileRef &ref) : d(ref.d)
{
  d->ref();
}

FileRef::~FileRef()
{
  if(d->deref())
    delete d;
}

Tag *FileRef::tag() const
{
  if(isNull()) {
    debug("FileRef::tag() - Called without a valid file.");
    return 0;
  }
  return d->file->tag();
}

AudioProperties *FileRef::audioProperties() const
{
  if(isNull()) {
    debug("FileRef::audioProperties() - Called without a valid file.");
    return 0;
  }
  return d->file->audioProperties();
}

File *FileRef::file() const
{
  return d->file;
}

bool FileRef::save()
{
  if(isNull()) {
    debug("FileRef::save() - Called without a valid file.");
    return false;
  }
  return d->file->save();
}

const FileRef::FileTypeResolver *FileRef::addFileTypeResolver(const FileRef::FileTypeResolver *resolver) // static
{
  FileRefPrivate::fileTypeResolvers.prepend(resolver);
  return resolver;
}

StringList FileRef::defaultFileExtensions()
{
  StringList l;

  l.append("ogg");
  l.append("flac");
  l.append("oga");
  l.append("mp3");
  l.append("mpc");
  l.append("wv");
  l.append("spx");
  l.append("tta");
  l.append("m4a");
  l.append("m4r");
  l.append("m4b");
  l.append("m4p");
  l.append("3g2");
  l.append("mp4");
  l.append("wma");
  l.append("asf");
  l.append("aif");
  l.append("aiff");
  l.append("wav");
  l.append("ape");
  l.append("mod");
  l.append("module"); // alias for "mod"
  l.append("nst"); // alias for "mod"
  l.append("wow"); // alias for "mod"
  l.append("s3m");
  l.append("it");
  l.append("xm");

  return l;
}

bool FileRef::isNull() const
{
  return !d->file || !d->file->isValid();
}

FileRef &FileRef::operator=(const FileRef &ref)
{
  if(&ref == this)
    return *this;

  if(d->deref())
    delete d;

  d = ref.d;
  d->ref();

  return *this;
}

bool FileRef::operator==(const FileRef &ref) const
{
  return ref.d->file == d->file;
}

bool FileRef::operator!=(const FileRef &ref) const
{
  return ref.d->file != d->file;
}

File *FileRef::create(FileName fileName, bool readAudioProperties,
                      AudioProperties::ReadStyle audioPropertiesStyle) // static
{

  List<const FileTypeResolver *>::ConstIterator it = FileRefPrivate::fileTypeResolvers.begin();

  for(; it != FileRefPrivate::fileTypeResolvers.end(); ++it) {
    File *file = (*it)->createFile(fileName, readAudioProperties, audioPropertiesStyle);
    if(file)
      return file;
  }

  // Ok, this is really dumb for now, but it works for testing.

  String ext;
  {
#ifdef _WIN32

    String s = fileName.toString();

#else

    String s = fileName;

 #endif

    const int pos = s.rfind(".");
    if(pos != -1)
      ext = s.substr(pos + 1).upper();
  }

  // If this list is updated, the method defaultFileExtensions() should also be
  // updated.  However at some point that list should be created at the same time
  // that a default file type resolver is created.

  if(!ext.isEmpty()) {
    if(ext == "MP3")
      return new MPEG::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "OGG")
      return new Ogg::Vorbis::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "OGA") {
      /* .oga can be any audio in the Ogg container. First try FLAC, then Vorbis. */
      File *file = new Ogg::FLAC::File(fileName, readAudioProperties, audioPropertiesStyle);
      if (file->isValid())
        return file;
      delete file;
      return new Ogg::Vorbis::File(fileName, readAudioProperties, audioPropertiesStyle);
    }
    if(ext == "FLAC")
      return new FLAC::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "MPC")
      return new MPC::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "WV")
      return new WavPack::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "SPX")
      return new Ogg::Speex::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "OPUS")
      return new Ogg::Opus::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "TTA")
      return new TrueAudio::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "M4A" || ext == "M4R" || ext == "M4B" || ext == "M4P" || ext == "MP4" || ext == "3G2")
      return new MP4::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "WMA" || ext == "ASF")
      return new ASF::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "AIF" || ext == "AIFF")
      return new RIFF::AIFF::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "WAV")
      return new RIFF::WAV::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "APE")
      return new APE::File(fileName, readAudioProperties, audioPropertiesStyle);
    // module, nst and wow are possible but uncommon extensions
    if(ext == "MOD" || ext == "MODULE" || ext == "NST" || ext == "WOW")
      return new Mod::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "S3M")
      return new S3M::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "IT")
      return new IT::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(ext == "XM")
      return new XM::File(fileName, readAudioProperties, audioPropertiesStyle);
  }

  return 0;
}
