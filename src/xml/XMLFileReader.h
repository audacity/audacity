/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.h

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <vector>
#include "expat.h"

#include "XMLTagHandler.h"

class AUDACITY_DLL_API XMLFileReader final {
 public:
   XMLFileReader();
   ~XMLFileReader();

   bool Parse(XMLTagHandler *baseHandler,
              const wxString &fname);

   wxString GetErrorStr();

   // Callback functions for expat

   static void startElement(void *userData, const char *name,
                            const char **atts);

   static void endElement(void *userData, const char *name);

   static void charHandler(void *userData, const char *s, int len);

 private:
   XML_Parser       mParser;
   XMLTagHandler   *mBaseHandler;
   using Handlers = std::vector<XMLTagHandler*>;
   Handlers mHandler;
   wxString         mErrorStr;
};
