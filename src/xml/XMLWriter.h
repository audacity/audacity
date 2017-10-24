/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLWriter.h

  Leland Lucius

**********************************************************************/
#ifndef __AUDACITY_XML_XML_FILE_WRITER__
#define __AUDACITY_XML_XML_FILE_WRITER__

#include <wx/arrstr.h>
#include <wx/dynarray.h>
#include <wx/ffile.h>

#include "../FileException.h"

///
/// XMLWriter
///
class AUDACITY_DLL_API XMLWriter /* not final */ {

 public:

   XMLWriter();
   virtual ~XMLWriter();

   virtual void StartTag(const wxString &name);
   virtual void EndTag(const wxString &name);

   virtual void WriteAttr(const wxString &name, const wxString &value);
   virtual void WriteAttr(const wxString &name, const wxChar *value);

   virtual void WriteAttr(const wxString &name, int value);
   virtual void WriteAttr(const wxString &name, bool value);
   virtual void WriteAttr(const wxString &name, long value);
   virtual void WriteAttr(const wxString &name, long long value);
   virtual void WriteAttr(const wxString &name, size_t value);
   virtual void WriteAttr(const wxString &name, float value, int digits = -1);
   virtual void WriteAttr(const wxString &name, double value, int digits = -1);

   virtual void WriteData(const wxString &value);

   virtual void WriteSubTree(const wxString &value);

   virtual void Write(const wxString &data) = 0;

   // Escape a string, replacing certain characters with their
   // XML encoding, i.e. '<' becomes '&lt;'
   wxString XMLEsc(const wxString & s);

 protected:

   bool mInTag;
   int mDepth;
   wxArrayString mTagstack;
   wxArrayInt mHasKids;

};

///
/// XMLFileWriter
///

/// This writes to a provisional file, and replaces the previously existing
/// contents by a file rename in Commit() only after all writes succeed.
/// The original contents may also be retained at a backup path name, as
/// directed by the optional constructor argument.
/// If it is destroyed before Commit(), then the provisional file is removed.
/// If the construction and all operations are inside a GuardedCall or event
/// handler, then the default delayed handler action in case of exceptions will
/// notify the user of problems.
class AUDACITY_DLL_API XMLFileWriter final : private wxFFile, public XMLWriter {

 public:

   /// The caption is for message boxes to show in case of errors.
   /// Might throw.
   XMLFileWriter
      ( const wxString &outputPath, const wxString &caption,
        bool keepBackup = false );

   virtual ~XMLFileWriter();

   /// Close all tags and then close the file.
   /// Might throw.  If not, then create
   /// or modify the file at the output path.
   void Commit();

   /// Write to file. Might throw.
   void Write(const wxString &data) override;

   wxString GetBackupName() const { return mBackupName; }

 private:

   void ThrowException(
      const wxFileName &fileName, const wxString &caption)
   {
      throw FileException{ FileException::Cause::Write, fileName, caption };
   }

   /// Close file without automatically ending tags.
   /// Might throw.
   void CloseWithoutEndingTags(); // for auto-save files

   const wxString mOutputPath;
   const wxString mCaption;
   wxString mBackupName;
   const bool mKeepBackup;

   wxFFile mBackupFile;
};

///
/// XMLStringWriter
///
class XMLStringWriter final : public wxString, public XMLWriter {

 public:

   XMLStringWriter(size_t initialSize = 0);
   virtual ~XMLStringWriter();

   void Write(const wxString &data) override;

   wxString Get();

 private:

};

#endif
