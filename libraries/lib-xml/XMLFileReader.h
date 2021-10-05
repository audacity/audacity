/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.h

  Dominic Mazzoni

**********************************************************************/

#include <vector>
struct XML_ParserStruct;
typedef struct XML_ParserStruct *XML_Parser;

#include "XMLTagHandler.h"
#include "Internat.h" // for TranslatableString

class XML_API XMLFileReader final {
 public:
   XMLFileReader();
   ~XMLFileReader();

   bool Parse(XMLTagHandler *baseHandler,
              const FilePath &fname);
   bool ParseString(XMLTagHandler *baseHandler,
                    const wxString &xmldata);

   const TranslatableString &GetErrorStr() const;
   const TranslatableString &GetLibraryErrorStr() const;

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
   TranslatableString mErrorStr;
   TranslatableString mLibraryErrorStr;
};
