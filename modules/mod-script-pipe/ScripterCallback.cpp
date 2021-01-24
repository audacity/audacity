// ScripterCallback.cpp : 
//
// A loadable module that connects a windows named pipe
// to a registered service function that is able to
// process a single command at a time.
//
// The service function is provided by the application
// and not by libscript.  mod_script_pipe was developed for
// Audacity.  Because it forwards commands
// rather than handling them itself it can be used in
// other projects too.  
//
// Enabling other programs to connect to Audacity via a pipe is a potential 
// security risk.  Use at your own risk.

#include <wx/wx.h>
#include "ScripterCallback.h"
#include "../../src/Audacity.h"

/*
There are several functions that can be used in a GUI module.

//#define versionFnName   "GetVersionString"
If the version is wrong, the module will be rejected.
That is it will be loaded and then unloaded.

//#define ModuleDispatchName "ModuleDispatch"
The most useful function.  See the example in this 
file.  It has several cases/options in it.

//#define scriptFnName    "RegScriptServerFunc"
This function is run from a non gui thread.  It was originally 
created for the benefit of mod-script-pipe.

//#define mainPanelFnName "MainPanelFunc"
This function is the hijacking function, to take over Audacity
and replace the main project window with our own wxFrame.

*/

#ifdef _MSC_VER
   #define DLL_API _declspec(dllexport)
   #define DLL_IMPORT _declspec(dllimport)
#else
   #define DLL_API __attribute__ ((visibility("default")))
   #define DLL_IMPORT
#endif


typedef enum
{
   ModuleInitialize,
   ModuleTerminate,
   AppInitialized,
   AppQuiting,
   ProjectInitialized,
   ProjectClosing
} ModuleDispatchTypes;


extern void PipeServer();
typedef DLL_IMPORT int (*tpExecScriptServerFunc)( wxString * pIn, wxString * pOut);
static tpExecScriptServerFunc pScriptServerFn=NULL;


extern "C" {


DLL_API const wxChar * GetVersionString()
{
   // Make sure that this version of the module requires the version 
   // of Audacity it is built with. 
   // For now the versions must match exactly for Audacity to 
   // agree to load the module.
   return AUDACITY_VERSION_STRING;
}

extern int DLL_API  ModuleDispatch(ModuleDispatchTypes type);
// ModuleDispatch
// is called by Audacity to initialize/terminate the module
// We don't (yet) do anything in this, since we have a special function for the scripter
// all we need to do is return 1.
int ModuleDispatch(ModuleDispatchTypes type){
   switch (type){
      case AppInitialized:{
      }
      break;
      case AppQuiting: {
      }
      break;
      case ProjectInitialized: {
      }
      break;
      default:
      break;
   }
   return 1;
}   

// And here is our special registration function.
int DLL_API RegScriptServerFunc( tpExecScriptServerFunc pFn )
{
   if( pFn )
   {
      pScriptServerFn = pFn;
      PipeServer();
   }

   return 4;
}


wxString Str2;
wxArrayString aStr;
unsigned int currentLine;
size_t currentPosition;

// Send the received command to Audacity and build an array of response lines.
// The response lines can be retrieved by calling DoSrvMore repeatedly.
int DoSrv(char *pIn)
{
   // Interpret string as unicode.
   // wxWidgets (now) uses unicode internally.
   // Scripts must send unicode strings (if going beyond 7-bit ASCII).
   // Important for filenames in commands.
   wxString Str1(pIn, wxConvUTF8); 
   Str1.Replace( wxT("\r"), wxT(""));
   Str1.Replace( wxT("\n"), wxT(""));
   Str2 = wxEmptyString;
   (*pScriptServerFn)( &Str1 , &Str2);

   Str2 += wxT('\n');
   size_t outputLength = Str2.Length();
   aStr.Clear();
   size_t iStart = 0;
   size_t i;
   for(i = 0; i < outputLength; ++i)
   {
      if( Str2[i] == wxT('\n') )
      {
         aStr.Add( Str2.Mid( iStart, i-iStart) + wxT("\n") );
         iStart = i+1;
      }
   }

   currentLine     = 0;
   currentPosition = 0;

   return 1;
}

size_t smin(size_t a, size_t b) { return a < b ? a : b; }

// Write up to nMax characters of the prepared (by DoSrv) response lines.
// Returns the number of characters sent, including null.
// Zero returned if and only if there's nothing else to send.
int DoSrvMore(char *pOut, size_t nMax)
{
   wxASSERT(currentLine >= 0);
   wxASSERT(currentPosition >= 0);

   size_t totalLines = aStr.GetCount();
   while (currentLine < totalLines)
   {
      wxCharBuffer lineString    = aStr[currentLine].ToUTF8();
      size_t lineLength      = lineString.length();
      size_t charsLeftInLine = lineLength - currentPosition;

      wxASSERT(charsLeftInLine >= 0);

      if (charsLeftInLine == 0)
      {
         // Move to next line
         ++currentLine;
         currentPosition = 0;
      }
      else
      {
         // Write as much of the rest of the line as will fit in the buffer
         size_t charsToWrite = smin(charsLeftInLine, nMax - 1);
         memcpy(pOut, 
                &(lineString.data()[currentPosition]),
                charsToWrite);
         pOut[charsToWrite] = '\0';
         currentPosition    += charsToWrite;
         // Need to cast to prevent compiler warnings
         int charsWritten = static_cast<int>(charsToWrite + 1);
         // (Check cast was safe)
         wxASSERT(static_cast<size_t>(charsWritten) == charsToWrite + 1);
         return charsWritten;
      }
   }
   return 0;
}

} // End extern "C"
