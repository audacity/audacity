/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.cpp

  Dominic Mazzoni

*******************************************************************//**

\class XMLFileReader
\brief Reads a file and passes the results through an XMLTagHandler.

*//*******************************************************************/

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>

#include <string.h>

#include "../Internat.h"
#include "XMLFileReader.h"

XMLFileReader::XMLFileReader()
{
   mParser = XML_ParserCreate(NULL);
   XML_SetUserData(mParser, (void *)this);
   XML_SetElementHandler(mParser, startElement, endElement);
   XML_SetCharacterDataHandler(mParser, charHandler);
   mBaseHandler = NULL;
   mErrorStr = wxT("");
   mHandler.reserve(128);
}

XMLFileReader::~XMLFileReader()
{
   XML_ParserFree(mParser);
}

bool XMLFileReader::Parse(XMLTagHandler *baseHandler,
                          const wxString &fname)
{
   wxFFile theXMLFile(fname, wxT("rb"));
   if (!theXMLFile.IsOpened()) {
      mErrorStr.Printf(_("Could not open file: \"%s\""), fname.c_str());
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
         mErrorStr.Printf(_("Error: %hs at line %lu"),
                          XML_ErrorString(XML_GetErrorCode(mParser)),
                          (long unsigned int)XML_GetCurrentLineNumber(mParser));
         theXMLFile.Close();
         return false;
      }
   } while (!done);

   theXMLFile.Close();

   // Even though there were no parse errors, we only succeed if
   // the first-level handler actually got called, and didn't
   // return false.
   if (mBaseHandler)
      return true;
   else {
      mErrorStr.Printf(_("Could not load file: \"%s\""), fname.c_str());
      return false;
   }
}

wxString XMLFileReader::GetErrorStr()
{
   return mErrorStr;
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
