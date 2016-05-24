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
class AUDACITY_DLL_API XMLFileWriter final : public wxFFile, public XMLWriter {

 public:

   XMLFileWriter();
   virtual ~XMLFileWriter();

   /// Open the file. Might throw XMLFileWriterException.
   void Open(const wxString &name, const wxString &mode);

   /// Close file. Might throw XMLFileWriterException.
   void Close();

   /// Close file without automatically ending tags.
   /// Might throw XMLFileWriterException.
   void CloseWithoutEndingTags(); // for auto-save files

   /// Write to file. Might throw XMLFileWriterException.
   void Write(const wxString &data) override;

 private:

};

///
/// Exception thrown by various XMLFileWriter methods
///
class XMLFileWriterException
{
public:
   XMLFileWriterException(const wxString& message) { mMessage = message; }
   wxString GetMessage() const { return mMessage; }

protected:
   wxString mMessage;
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
