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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <tfile.h>
#include <tstring.h>

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
#include "trueaudiofile.h"
#include "aifffile.h"
#include "wavfile.h"

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
  return d->file->tag();
}

AudioProperties *FileRef::audioProperties() const
{
  return d->file->audioProperties();
}

File *FileRef::file() const
{
  return d->file;
}

bool FileRef::save()
{
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
#ifdef WITH_MP4
  l.append("m4a");
  l.append("m4b");
  l.append("m4p");
  l.append("3g2");
#endif
#ifdef WITH_ASF
  l.append("wma");
#endif
  l.append("aif");
  l.append("aiff");
  l.append("wav");

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

  String s;

#ifdef _WIN32
  s = (wcslen((const wchar_t *) fileName) > 0) ? String((const wchar_t *) fileName) : String((const char *) fileName);
#else
  s = fileName;
#endif

  // If this list is updated, the method defaultFileExtensions() should also be
  // updated.  However at some point that list should be created at the same time
  // that a default file type resolver is created.

  if(s.size() > 4) {
    if(s.substr(s.size() - 4, 4).upper() == ".OGG")
      return new Ogg::Vorbis::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".MP3")
      return new MPEG::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".OGA")
      return new Ogg::FLAC::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 5, 5).upper() == ".FLAC")
      return new FLAC::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".MPC")
      return new MPC::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 3, 3).upper() == ".WV")
      return new WavPack::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".SPX")
      return new Ogg::Speex::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".TTA")
      return new TrueAudio::File(fileName, readAudioProperties, audioPropertiesStyle);
#ifdef WITH_MP4
    if(s.substr(s.size() - 4, 4).upper() == ".M4A" ||
       s.substr(s.size() - 4, 4).upper() == ".M4B" ||
       s.substr(s.size() - 4, 4).upper() == ".M4P" ||
       s.substr(s.size() - 4, 4).upper() == ".3G2")
      return new MP4::File(fileName, readAudioProperties, audioPropertiesStyle);
#endif
#ifdef WITH_ASF
    if(s.substr(s.size() - 4, 4).upper() == ".WMA")
      return new ASF::File(fileName, readAudioProperties, audioPropertiesStyle);
#endif
    if(s.substr(s.size() - 4, 4).upper() == ".AIF")
      return new RIFF::AIFF::File(fileName, readAudioProperties, audioPropertiesStyle);
    if(s.substr(s.size() - 4, 4).upper() == ".WAV")
      return new RIFF::WAV::File(fileName, readAudioProperties, audioPropertiesStyle);
  }
  if(s.size() > 5) {
    if(s.substr(s.size() - 5, 5).upper() == ".AIFF")
      return new RIFF::AIFF::File(fileName, readAudioProperties, audioPropertiesStyle);
  }

  return 0;
}
