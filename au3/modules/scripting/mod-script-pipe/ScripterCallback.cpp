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
#include "commands/ScriptCommandRelay.h"

/*
//#define ModuleDispatchName "ModuleDispatch"
See the example in this file.  It has several cases/options in it.
*/

#include "ModuleConstants.h"

extern void PipeServer();
typedef DLL_IMPORT int (* tpExecScriptServerFunc)(wxString* pIn, wxString* pOut);
static tpExecScriptServerFunc pScriptServerFn=NULL;

extern "C" {
// And here is our special registration function.
int DLL_API RegScriptServerFunc(tpExecScriptServerFunc pFn)
{
    if (pFn) {
        pScriptServerFn = pFn;
        PipeServer();
    }

    return 4;
}

DEFINE_VERSION_CHECK
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
    switch (type) {
    case ModuleInitialize:
        ScriptCommandRelay::StartScriptServer(RegScriptServerFunc);
        break;
    default:
        break;
    }
    return 1;
}

wxString Str2;
wxArrayString aStr;
unsigned int currentLine;
size_t currentPosition;

// Send the received command to Audacity and build an array of response lines.
// The response lines can be retrieved by calling DoSrvMore repeatedly.
int DoSrv(char* pIn)
{
    // Interpret string as unicode.
    // wxWidgets (now) uses unicode internally.
    // Scripts must send unicode strings (if going beyond 7-bit ASCII).
    // Important for filenames in commands.
    wxString Str1(pIn, wxConvUTF8);
    Str1.Replace(wxT("\r"), wxT(""));
    Str1.Replace(wxT("\n"), wxT(""));
    Str2 = wxEmptyString;
    (*pScriptServerFn)(&Str1, &Str2);

    Str2 += wxT('\n');
    size_t outputLength = Str2.Length();
    aStr.Clear();
    size_t iStart = 0;
    size_t i;
    for (i = 0; i < outputLength; ++i) {
        if (Str2[i] == wxT('\n')) {
            aStr.Add(Str2.Mid(iStart, i - iStart) + wxT("\n"));
            iStart = i + 1;
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
int DoSrvMore(char* pOut, size_t nMax)
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

        if (charsLeftInLine == 0) {
            // Move to next line
            ++currentLine;
            currentPosition = 0;
        } else {
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
