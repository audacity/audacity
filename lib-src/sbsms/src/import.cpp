#include <iostream>
#include "sbsms.h"
#include "import.h"
#include "config.h"

#include "pcm.h"
#ifdef HAVE_MAD
#include "mp3.h"
#endif

#include <map>
#include <string>

using namespace std;

namespace _sbsms_ {
string lower(string strToConvert)
{
  for(unsigned int i=0;i<strToConvert.length();i++) {
    strToConvert[i] = tolower(strToConvert[i]);
  }
  return strToConvert;
}
 
AudioDecoder *import(const char *filename)
{
  string fname(filename);
  int i = fname.find(".");
  AudioDecoder *decoder = NULL;

  if(i) {
    string ext = fname.substr(i+1);
    string extl = lower(ext);
  
    if(!extl.compare("wav") || !extl.compare("aif") || !extl.compare("aiff")) {
      decoder = new PcmReader(filename);
#ifdef HAVE_MAD
    } else if (!extl.compare("mp3")) {
      decoder = new MP3Reader(filename);
#endif
    } else {
      perror("Error importing file");
      return NULL;
    }
  }
  return decoder;
}
}
