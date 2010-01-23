/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLWriter.h

  Leland Lucius

**********************************************************************/
#ifndef __AUDACITY_XML_XML_FILE_WRITER__
#define __AUDACITY_XML_XML_FILE_WRITER__

#include <wx/ffile.h>
#include <wx/dynarray.h>

///
/// XMLWriter
///
class XMLWriter {

 public:

   XMLWriter();
   virtual ~XMLWriter();

   void StartTag(const wxString &name);
   void EndTag(const wxString &name);

   void WriteAttr(const wxString &name, const wxString &value);
   void WriteAttr(const wxChar *name, const wxChar *value);
   void WriteAttr(const wxString &name, const wxChar *value);
   void WriteAttr(const wxChar *name, const wxString &value);

   void WriteAttr(const wxString &name, int value);
   void WriteAttr(const wxChar *name, int value);
   void WriteAttr(const wxString &name, bool value);
   void WriteAttr(const wxChar *name, bool value);
   void WriteAttr(const wxString &name, long value);
   void WriteAttr(const wxChar *name, long value);
   void WriteAttr(const wxString &name, long long value);
   void WriteAttr(const wxChar *name, long long value);
   void WriteAttr(const wxString &name, size_t value);
   void WriteAttr(const wxChar *name, size_t value);
   void WriteAttr(const wxString &name, float value, int digits = -1);
   void WriteAttr(const wxChar *name, float value, int digits = -1);
   void WriteAttr(const wxString &name, double value, int digits = -1);
   void WriteAttr(const wxChar *name, double value, int digits = -1);

   void WriteData(const wxString &value);
   void WriteData(const wxChar *value);

   void WriteSubTree(const wxString &value);
   void WriteSubTree(const wxChar *value);

   void Write(const wxChar *data);

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
class XMLFileWriter:public wxFFile, public XMLWriter {

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
   void Write(const wxString &data);

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
class XMLStringWriter:public wxString, public XMLWriter {

 public:

   XMLStringWriter();
   virtual ~XMLStringWriter();

   void Write(const wxString &data);

   wxString Get();

 private:

};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 391b08e6-61f4-43ea-8431-c835c31ba86d
