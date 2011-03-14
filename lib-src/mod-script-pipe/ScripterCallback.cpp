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

#include <wx/wx.h>
#include "ScripterCallback.h"
//#include "../lib_widget_extra/ShuttleGuiBase.h"
#include "../../src/Audacity.h"
#include "../../src/ShuttleGui.h"

#ifdef NOT_DEFINED

// This is 'traditional code' for DLLs, which we're not
// using.

//#include "stdafx.h"

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
    }
    return TRUE;
}

#endif


extern void PipeServer();

extern "C" {

typedef SCRIPT_PIPE_DLL_IMPORT int (*tpExecScriptServerFunc)( wxString * pIn, wxString * pOut);

static tpExecScriptServerFunc pScriptServerFn=NULL;

wxString Str2;
wxArrayString aStr;
unsigned int currentLine;
size_t currentPosition;

// Send the received command to Audacity and build an array of response lines.
// The response lines can be retrieved by calling DoSrvMore repeatedly.
int DoSrv(char *pIn)
{
   wxString Str1(pIn, wxConvISO8859_1);
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
      wxString lineString    = aStr[currentLine];
      size_t lineLength      = lineString.Length();
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
                lineString.Mid(currentPosition, 
                               currentPosition + charsToWrite).mb_str(), 
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

int SCRIPT_PIPE_DLL_API RegScriptServerFunc( tpExecScriptServerFunc pFn )
{
   if( pFn )
   {
      pScriptServerFn = pFn;
      PipeServer();
   }
   return 4;
}

// This is an example of an exported function.
int SCRIPT_PIPE_DLL_API ExtensionModuleInit(int ix)
{
//   pExecFunc = NULL;
   ix;// compiler food.

#if defined(_DEBUG)
   wxLogDebug(wxT("Got into DLL"));
#endif

   // Here is proof that the DLL was dynamically loaded and this Init function
   // called.
   //wxDialog Dlg( (wxWindow*)NULL, (wxWindowID)-1, wxT("mod-script-pipe - Dialog Loaded by Plug In"), wxPoint(0,0));


#if 0
   ShuttleGui S( &Dlg, eIsCreating );
   S.StartStatic( "Scripter Initialising" );
   S.StartHorizontalLay();
   S.Id(wxID_CANCEL).AddButton( "Cancel" ); 
   S.Id(wxID_OK).AddButton( "OK" )->SetFocus(); 
   S.EndHorizontalLay();
   S.EndStatic();
#endif

   /*
   Dlg.Fit();
   Dlg.Move( 100,100 );
   int id = Dlg.ShowModal();
   */
//printf("id = %d\n", id);
   // return -1 for cancel, anything else for OK.
//   return (id==wxID_CANCEL)?-1:42;
   return 0;
}

wxString SCRIPT_PIPE_DLL_API GetVersionString()
{
   // Make sure that this version of the module requires the version 
   // of Audacity it is built with. 
   // For now, the versions must match exactly for Audacity to 
   // agree to load the module.

   return AUDACITY_VERSION_STRING;
}

} // End extern "C"
