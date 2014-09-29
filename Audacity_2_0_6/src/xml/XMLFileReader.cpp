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
   mMaxDepth = 128;
   mHandler = new XMLTagHandler*[mMaxDepth];
   mDepth = -1;
   mErrorStr = wxT("");
}

XMLFileReader::~XMLFileReader()
{
   delete[] mHandler;
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
   mHandler[0] = NULL;

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
   if (mHandler[0])
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

   This->mDepth++;

   if (This->mDepth >= This->mMaxDepth) {
      XMLTagHandler  **newHandler = new XMLTagHandler*[This->mMaxDepth*2];
      for(int i=0; i<This->mMaxDepth; i++)
         newHandler[i] = This->mHandler[i];
      delete[] This->mHandler;
      This->mHandler = newHandler;
      This->mMaxDepth *= 2;
   }

   if (This->mDepth==0)
      This->mHandler[This->mDepth] = This->mBaseHandler;
   else {
      if (This->mHandler[This->mDepth-1])
         This->mHandler[This->mDepth] =
            This->mHandler[This->mDepth-1]->ReadXMLChild(name);
      else
         This->mHandler[This->mDepth] = NULL;
   }

   if (This->mHandler[This->mDepth]) {
      if (!This->mHandler[This->mDepth]->ReadXMLTag(name, atts))
         This->mHandler[This->mDepth] = 0;
   }
}

// static
void XMLFileReader::endElement(void *userData, const char *name)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   if (This->mHandler[This->mDepth])
      This->mHandler[This->mDepth]->ReadXMLEndTag(name);

   This->mDepth--;
}

// static
void XMLFileReader::charHandler(void *userData, const char *s, int len)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   if (This->mHandler[This->mDepth])
      This->mHandler[This->mDepth]->ReadXMLContent(s, len);
}
