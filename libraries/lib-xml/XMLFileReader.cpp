/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.cpp

  Dominic Mazzoni

*******************************************************************//**

\class XMLFileReader
\brief Reads a file and passes the results through an XMLTagHandler.

*//*******************************************************************/

#include "XMLFileReader.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>
#include <wx/log.h>

#include <string.h>

#include "expat.h"

XMLFileReader::XMLFileReader()
{
   mParser = XML_ParserCreate(NULL);
   XML_SetUserData(mParser, (void *)this);
   XML_SetElementHandler(mParser, startElement, endElement);
   XML_SetCharacterDataHandler(mParser, charHandler);
   mBaseHandler = NULL;
   mHandler.reserve(128);
}

XMLFileReader::~XMLFileReader()
{
   XML_ParserFree(mParser);
}

bool XMLFileReader::Parse(XMLTagHandler *baseHandler,
                          const FilePath &fname)
{
   wxFFile theXMLFile(fname, wxT("rb"));
   if (!theXMLFile.IsOpened()) {
      mErrorStr = XO("Could not open file: \"%s\"").Format( fname );
      return false;
   }

   mBaseHandler = baseHandler;

   const size_t bufferSize = 16384;
   char buffer[16384];
   int done = 0;
   do {
      size_t len = fread(buffer, 1, bufferSize, theXMLFile.fp());
      done = (len < bufferSize);
      if (!XML_Parse(mParser, buffer, len, done)) {

         // Embedded error string from expat doesn't translate (yet)
         // We could make a table of XOs if we wanted so that it could
         // If we do, uncomment the second constructor argument so it's not
         // a verbatim string
         mLibraryErrorStr = Verbatim(
            XML_ErrorString(XML_GetErrorCode(mParser)) // , {}
         );

         mErrorStr = XO("Error: %s at line %lu").Format(
            mLibraryErrorStr,
            (long unsigned int)XML_GetCurrentLineNumber(mParser)
         );

         theXMLFile.Close();
         return false;

// If we did want to handle every single parse error, these are they....
/*
    XML_L("out of memory"),
    XML_L("syntax error"),
    XML_L("no element found"),
    XML_L("not well-formed (invalid token)"),
    XML_L("unclosed token"),
    XML_L("partial character"),
    XML_L("mismatched tag"),
    XML_L("duplicate attribute"),
    XML_L("junk after document element"),
    XML_L("illegal parameter entity reference"),
    XML_L("undefined entity"),
    XML_L("recursive entity reference"),
    XML_L("asynchronous entity"),
    XML_L("reference to invalid character number"),
    XML_L("reference to binary entity"),
    XML_L("reference to external entity in attribute"),
    XML_L("XML or text declaration not at start of entity"),
    XML_L("unknown encoding"),
    XML_L("encoding specified in XML declaration is incorrect"),
    XML_L("unclosed CDATA section"),
    XML_L("error in processing external entity reference"),
    XML_L("document is not standalone"),
    XML_L("unexpected parser state - please send a bug report"),
    XML_L("entity declared in parameter entity"),
    XML_L("requested feature requires XML_DTD support in Expat"),
    XML_L("cannot change setting once parsing has begun"),
    XML_L("unbound prefix"),
    XML_L("must not undeclare prefix"),
    XML_L("incomplete markup in parameter entity"),
    XML_L("XML declaration not well-formed"),
    XML_L("text declaration not well-formed"),
    XML_L("illegal character(s) in public id"),
    XML_L("parser suspended"),
    XML_L("parser not suspended"),
    XML_L("parsing aborted"),
    XML_L("parsing finished"),
    XML_L("cannot suspend in external parameter entity"),
    XML_L("reserved prefix (xml) must not be undeclared or bound to another namespace name"),
    XML_L("reserved prefix (xmlns) must not be declared or undeclared"),
    XML_L("prefix must not be bound to one of the reserved namespace names")
*/
      }
   } while (!done);

   theXMLFile.Close();

   // Even though there were no parse errors, we only succeed if
   // the first-level handler actually got called, and didn't
   // return false.
   if (mBaseHandler)
      return true;
   else {
      mErrorStr = XO("Could not load file: \"%s\"").Format( fname );
      return false;
   }
}

bool XMLFileReader::ParseString(XMLTagHandler *baseHandler,
                                const wxString &xmldata)
{
   auto utf8 = xmldata.ToUTF8();
   const char *buffer = utf8.data();
   int len = utf8.length();

   mBaseHandler = baseHandler;

   if (!ParseBuffer(baseHandler, utf8.data(), utf8.length(), true))
      return false;

   // Even though there were no parse errors, we only succeed if
   // the first-level handler actually got called, and didn't
   // return false.
   if (!mBaseHandler)
   {
      mErrorStr = XO("Could not parse XML");
      return false;
   }

   return true;
}

bool XMLFileReader::ParseMemoryStream(
   XMLTagHandler* baseHandler, const MemoryStream& xmldata)
{
   mBaseHandler = baseHandler;

   for (auto chunk : xmldata)
   {
      if (!ParseBuffer(baseHandler, static_cast<const char*>(chunk.first), chunk.second, false))
         return false;
   }

   if (!ParseBuffer(baseHandler, nullptr, 0, true))
      return false;

   if (!mBaseHandler)
   {
      mErrorStr = XO("Could not parse XML");
      return false;
   }

   return true;
}

const TranslatableString &XMLFileReader::GetErrorStr() const
{
   return mErrorStr;
}

const TranslatableString &XMLFileReader::GetLibraryErrorStr() const
{
   return mLibraryErrorStr;
}

// static
void XMLFileReader::startElement(void *userData, const char *name,
                                 const char **atts)
{
   XMLFileReader *This = (XMLFileReader *)userData;
   Handlers &handlers = This->mHandler;

   if (handlers.empty()) {
      handlers.push_back(This->mBaseHandler);
   }
   else {
      if (XMLTagHandler *const handler = handlers.back())
         handlers.push_back(handler->ReadXMLChild(name));
      else
         handlers.push_back(NULL);
   }

   if (XMLTagHandler *& handler = handlers.back()) {
      if (!handler->ReadXMLTag(name, atts)) {
         handler = nullptr;
         if (handlers.size() == 1)
            This->mBaseHandler = nullptr;
      }
   }
}

// static
void XMLFileReader::endElement(void *userData, const char *name)
{
   XMLFileReader *This = (XMLFileReader *)userData;
   Handlers &handlers = This->mHandler;

   if (XMLTagHandler *const handler = handlers.back())
      handler->ReadXMLEndTag(name);

   handlers.pop_back();
}

// static
void XMLFileReader::charHandler(void *userData, const char *s, int len)
{
   XMLFileReader *This = (XMLFileReader *)userData;
   Handlers &handlers = This->mHandler;

   if (XMLTagHandler *const handler = handlers.back())
      handler->ReadXMLContent(s, len);
}

bool XMLFileReader::ParseBuffer(
   XMLTagHandler* baseHandler, const char* buffer, size_t len, bool isFinal)
{
   if (!XML_Parse(mParser, buffer, len, isFinal))
   {

      // Embedded error string from expat doesn't translate (yet)
      // We could make a table of XOs if we wanted so that it could
      // If we do, uncomment the second constructor argument so it's not
      // a verbatim string
      mLibraryErrorStr =
         Verbatim(XML_ErrorString(XML_GetErrorCode(mParser)) // , {}
         );

      mErrorStr = XO("Error: %s at line %lu")
                     .Format(
                        mLibraryErrorStr,
                        (long unsigned int)XML_GetCurrentLineNumber(mParser));

      wxLogMessage(
         wxT("ParseString error: %s\n===begin===%s\n===end==="),
         mErrorStr.Debug(), buffer);

      return false;
   }

   return true;
}
