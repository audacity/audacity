/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLTagHandler.h

  Dominic Mazzoni
  Vaughan Johnson
  Dmitry Vedenko

  The XMLTagHandler class is an interface which should be implemented by
  classes which wish to be able to load and save themselves
  using XML files.

  The XMLValueChecker class implements static bool methods for checking
  input values from XML files.

**********************************************************************/
#ifndef __AUDACITY_XML_TAG_HANDLER__
#define __AUDACITY_XML_TAG_HANDLER__

#include <string_view>

#include "XMLWriter.h"


class XML_API XMLValueChecker
{
public:
   // "Good" means well-formed and for the file-related functions, names an existing file or folder.
   // These are used in HandleXMLTag and BuildFomXML methods to check the input for
   // security vulnerabilities, per the NGS report for UmixIt.
   static bool IsGoodString(const wxString & str);
   // Labels are allowed to be very long.  At some future date we will format long labels nicely.
   static bool IsGoodLongString(const wxString & str);

   static bool IsGoodFileName(const FilePath & strFileName, const FilePath & strDirName = {});
   static bool IsGoodFileString(const FilePath &str);
   static bool IsGoodSubdirName(const FilePath & strSubdirName, const FilePath & strDirName = {});
   static bool IsGoodPathName(const FilePath & strPathName);
   static bool IsGoodPathString(const FilePath &str);

   /** @brief Check that the supplied string can be converted to a long (32bit)
	* integer.
	*
	* Note that because wxString::ToLong does additional testing, IsGoodInt doesn't
	* duplicate that testing, so use wxString::ToLong after IsGoodInt, not just
	* atoi.
	* @param strInt The string to test
	* @return true if the string is convertible, false if not
	*/
   static bool IsGoodInt(const wxString & strInt);
   /** @brief Check that the supplied string can be converted to a 64bit
	* integer.
	*
	* Note that because wxString::ToLongLong does additional testing, IsGoodInt64
	* doesn't duplicate that testing, so use wxString::ToLongLong after IsGoodInt64
	* not just atoll.
	* @param strInt The string to test
	* @return true if the string is convertible, false if not
	*/
   static bool IsGoodInt64(const wxString & strInt);
   static bool IsGoodIntForRange(const wxString & strInt, const wxString & strMAXABS);
};


class XML_API XMLTagHandler /* not final */ {
 public:
   XMLTagHandler(){};
   virtual ~XMLTagHandler(){};
   //
   // Methods to override
   //

   // This method will be called on your class if your class has
   // been registered to handle this particular tag.  Parse the
   // tag and the attribute-value pairs (null-terminated), and
   // return true on success, and false on failure.  If you return
   // false, you will not get any calls about children.
   virtual bool HandleXMLTag(const std::string_view& tag, const wxChar **attrs) = 0;

   // This method will be called when a closing tag is encountered.
   // It is optional to override this method.
   virtual void HandleXMLEndTag(const std::string_view& WXUNUSED(tag)) {}

   // This method will be called when element content has been
   // encountered.
   // It is optional to override this method.
   virtual void HandleXMLContent(const std::string_view& WXUNUSED(content)) {}

   // If the XML document has children of your tag, this method
   // should be called.  Typically you should construct a NEW
   // object for the child, insert it into your own local data
   // structures, and then return it.  If you do not wish to
   // handle this child, return NULL and it will be ignored.
   virtual XMLTagHandler *HandleXMLChild(const std::string_view& tag) = 0;

   // These functions receive data from expat.  They do charset
   // conversion and then pass the data to the handlers above.
   bool ReadXMLTag(const char *tag, const char **attrs);
   void ReadXMLEndTag(const char *tag);
   void ReadXMLContent(const char *s, int len);
   XMLTagHandler *ReadXMLChild(const char *tag);
};

#endif // define __AUDACITY_XML_TAG_HANDLER__

