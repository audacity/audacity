/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalOutput.cpp

  Paul Licameli

**********************************************************************/

#include "JournalOutput.h"

#include <wx/textfile.h>
#include "wxArrayStringEx.h"

namespace Journal {
namespace {
struct FlushingTextFile : wxTextFile {
    // Flush output when the program quits, even if that makes an incomplete
    // journal file without an exit
    ~FlushingTextFile()
    {
        if (IsOpened()) {
            Write();
            Close();
        }
    }
} sFileOut;
}

bool IsRecording()
{
    return sFileOut.IsOpened();
}

bool OpenOut(const wxString& fullPath)
{
    sFileOut.Open(fullPath);
    if (sFileOut.IsOpened()) {
        sFileOut.Clear();
    } else {
        sFileOut.Create();
        sFileOut.Open(fullPath);
    }
    return sFileOut.IsOpened();
}

void Output(const wxString& string)
{
    if (IsRecording()) {
        sFileOut.AddLine(string);
    }
}

void Output(const wxArrayString& strings)
{
    if (IsRecording()) {
        Output(::wxJoin(strings, SeparatorCharacter, EscapeCharacter));
    }
}

void Output(std::initializer_list< const wxString > strings)
{
    return Output(wxArrayStringEx(strings));
}

void Comment(const wxString& string)
{
    if (IsRecording()) {
        sFileOut.AddLine(CommentCharacter + string);
    }
}
}
