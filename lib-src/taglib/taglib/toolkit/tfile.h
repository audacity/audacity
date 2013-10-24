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
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_FILE_H
#define TAGLIB_FILE_H

#include "taglib_export.h"
#include "taglib.h"
#include "tag.h"
#include "tbytevector.h"
#include "tiostream.h"

namespace TagLib {

  class String;
  class Tag;
  class AudioProperties;
  class PropertyMap;

  //! A file class with some useful methods for tag manipulation

  /*!
   * This class is a basic file class with some methods that are particularly
   * useful for tag editors.  It has methods to take advantage of
   * ByteVector and a binary search method for finding patterns in a file.
   */

  class TAGLIB_EXPORT File
  {
  public:
    /*!
     * Position in the file used for seeking.
     */
    enum Position {
      //! Seek from the beginning of the file.
      Beginning,
      //! Seek from the current position in the file.
      Current,
      //! Seek from the end of the file.
      End
    };

    /*!
     * Destroys this File instance.
     */
    virtual ~File();

    /*!
     * Returns the file name in the local file system encoding.
     */
    FileName name() const;

    /*!
     * Returns a pointer to this file's tag.  This should be reimplemented in
     * the concrete subclasses.
     */
    virtual Tag *tag() const = 0;

    /*!
     * Exports the tags of the file as dictionary mapping (human readable) tag
     * names (uppercase Strings) to StringLists of tag values. Calls the according
     * specialization in the File subclasses.
     * For each metadata object of the file that could not be parsed into the PropertyMap
     * format, the returend map's unsupportedData() list will contain one entry identifying
     * that object (e.g. the frame type for ID3v2 tags). Use removeUnsupportedProperties()
     * to remove (a subset of) them.
     * For files that contain more than one tag (e.g. an MP3 with both an ID3v2 and an ID3v2
     * tag) only the most "modern" one will be exported (ID3v2 in this case).
     * BIC: Will be made virtual in future releases.
     */
    PropertyMap properties() const;

    /*!
     * Removes unsupported properties, or a subset of them, from the file's metadata.
     * The parameter \a properties must contain only entries from
     * properties().unsupportedData().
     * BIC: Will be mad virtual in future releases.
     */
    void removeUnsupportedProperties(const StringList& properties);

    /*!
     * Sets the tags of this File to those specified in \a properties. Calls the
     * according specialization method in the subclasses of File to do the translation
     * into the format-specific details.
     * If some value(s) could not be written imported to the specific metadata format,
     * the returned PropertyMap will contain those value(s). Otherwise it will be empty,
     * indicating that no problems occured.
     * With file types that support several tag formats (for instance, MP3 files can have
     * ID3v1, ID3v2, and APEv2 tags), this function will create the most appropriate one
     * (ID3v2 for MP3 files). Older formats will be updated as well, if they exist, but won't
     * be taken into account for the return value of this function.
     * See the documentation of the subclass implementations for detailed descriptions.
     * BIC: will become pure virtual in the future
     */
    PropertyMap setProperties(const PropertyMap &properties);
    
    /*!
     * Returns a pointer to this file's audio properties.  This should be
     * reimplemented in the concrete subclasses.  If no audio properties were
     * read then this will return a null pointer.
     */
    virtual AudioProperties *audioProperties() const = 0;

    /*!
     * Save the file and its associated tags.  This should be reimplemented in
     * the concrete subclasses.  Returns true if the save succeeds.
     *
     * \warning On UNIX multiple processes are able to write to the same file at
     * the same time.  This can result in serious file corruption.  If you are
     * developing a program that makes use of TagLib from multiple processes you
     * must insure that you are only doing writes to a particular file from one
     * of them.
     */
    virtual bool save() = 0;

    /*!
     * Reads a block of size \a length at the current get pointer.
     */
    ByteVector readBlock(ulong length);

    /*!
     * Attempts to write the block \a data at the current get pointer.  If the
     * file is currently only opened read only -- i.e. readOnly() returns true --
     * this attempts to reopen the file in read/write mode.
     *
     * \note This should be used instead of using the streaming output operator
     * for a ByteVector.  And even this function is significantly slower than
     * doing output with a char[].
     */
    void writeBlock(const ByteVector &data);

    /*!
     * Returns the offset in the file that \a pattern occurs at or -1 if it can
     * not be found.  If \a before is set, the search will only continue until the
     * pattern \a before is found.  This is useful for tagging purposes to search
     * for a tag before the synch frame.
     *
     * Searching starts at \a fromOffset, which defaults to the beginning of the
     * file.
     *
     * \note This has the practial limitation that \a pattern can not be longer
     * than the buffer size used by readBlock().  Currently this is 1024 bytes.
     */
    long find(const ByteVector &pattern,
              long fromOffset = 0,
              const ByteVector &before = ByteVector::null);

    /*!
     * Returns the offset in the file that \a pattern occurs at or -1 if it can
     * not be found.  If \a before is set, the search will only continue until the
     * pattern \a before is found.  This is useful for tagging purposes to search
     * for a tag before the synch frame.
     *
     * Searching starts at \a fromOffset and proceeds from the that point to the
     * beginning of the file and defaults to the end of the file.
     *
     * \note This has the practial limitation that \a pattern can not be longer
     * than the buffer size used by readBlock().  Currently this is 1024 bytes.
     */
    long rfind(const ByteVector &pattern,
               long fromOffset = 0,
               const ByteVector &before = ByteVector::null);

    /*!
     * Insert \a data at position \a start in the file overwriting \a replace
     * bytes of the original content.
     *
     * \note This method is slow since it requires rewriting all of the file
     * after the insertion point.
     */
    void insert(const ByteVector &data, ulong start = 0, ulong replace = 0);

    /*!
     * Removes a block of the file starting a \a start and continuing for
     * \a length bytes.
     *
     * \note This method is slow since it involves rewriting all of the file
     * after the removed portion.
     */
    void removeBlock(ulong start = 0, ulong length = 0);

    /*!
     * Returns true if the file is read only (or if the file can not be opened).
     */
    bool readOnly() const;

    /*!
     * Since the file can currently only be opened as an argument to the
     * constructor (sort-of by design), this returns if that open succeeded.
     */
    bool isOpen() const;

    /*!
     * Returns true if the file is open and readable.
     */
    bool isValid() const;

    /*!
     * Move the I/O pointer to \a offset in the file from position \a p.  This
     * defaults to seeking from the beginning of the file.
     *
     * \see Position
     */
    void seek(long offset, Position p = Beginning);

    /*!
     * Reset the end-of-file and error flags on the file.
     */
    void clear();

    /*!
     * Returns the current offset within the file.
     */
    long tell() const;

    /*!
     * Returns the length of the file.
     */
    long length();

    /*!
     * Returns true if \a file can be opened for reading.  If the file does not
     * exist, this will return false.
     *
     * \deprecated
     */
    static bool isReadable(const char *file);

    /*!
     * Returns true if \a file can be opened for writing.
     *
     * \deprecated
     */
    static bool isWritable(const char *name);

  protected:
    /*!
     * Construct a File object and opens the \a file.  \a file should be a
     * be a C-string in the local file system encoding.
     *
     * \note Constructor is protected since this class should only be
     * instantiated through subclasses.
     */
    File(FileName file);

    /*!
     * Construct a File object and use the \a stream instance.
     *
     * \note TagLib will *not* take ownership of the stream, the caller is
     * responsible for deleting it after the File object.
     *
     * \note Constructor is protected since this class should only be
     * instantiated through subclasses.
     */
    File(IOStream *stream);

    /*!
     * Marks the file as valid or invalid.
     *
     * \see isValid()
     */
    void setValid(bool valid);

    /*!
     * Truncates the file to a \a length.
     */
    void truncate(long length);

    /*!
     * Returns the buffer size that is used for internal buffering.
     */
    static uint bufferSize();

  private:
    File(const File &);
    File &operator=(const File &);

    class FilePrivate;
    FilePrivate *d;
  };

}

#endif
