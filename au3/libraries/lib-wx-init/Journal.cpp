/**********************************************************************

  Audacity: A Digital Audio Editor

  Journal.cpp

  Paul Licameli

*******************************************************************//*!

\namespace Journal
\brief Facilities for recording and playback of sequences of user interaction

*//*******************************************************************/

#include "Journal.h"
#include "JournalOutput.h"
#include "JournalRegistry.h"

#include <algorithm>
#include <wx/app.h>
#include <wx/filename.h>
#include <wx/ffile.h>

#include <string>
#include <string_view>

#include "MemoryX.h"
#include "Prefs.h"
#include "FileNames.h"

namespace Journal {
namespace {
wxString sFileNameIn;
wxTextFile sFileIn;

wxString sLine;
// Invariant:  the input file has not been opened, or else sLineNumber counts
// the number of lines consumed by the tokenizer
int sLineNumber = -1;

BoolSetting JournalEnabled{ L"/Journal/Enabled", false };

class JournalLogger final
{
public:
    JournalLogger()
    {
        wxFileName logFile(FileNames::DataDir(), L"journallog.txt");
        mLogFile.Open(logFile.GetFullPath(wxPATH_NATIVE), L"w");
    }

    void WriteString(std::string_view str)
    {
        mLogFile.Write(str.data(), str.size());
    }

    void FinalizeMessge()
    {
        mLogFile.Write("\n");
        mLogFile.Flush();
    }

private:
    wxFFile mLogFile;
};

JournalLogger& GetLogger()
{
    static JournalLogger logger;
    return logger;
}

std::string ToString(const wxString& str)
{
    return str.ToStdString();
}

template<typename T>
std::string ToString(const T& arg)
{
    return std::to_string(arg);
}

template<typename ... Args>
void Log(std::string_view message, const Args&... args)
{
    if (message.empty()) {
        return;
    }

    constexpr auto n = sizeof...(Args);

    std::string strings[n];
    std::size_t i = 0;
    ((strings[i++] = ToString(args)), ...);
    i = 0;

    auto& logger = GetLogger();

    while (!message.empty())
    {
        const auto placeholderPos = message.find("{}");

        if (placeholderPos == std::string_view::npos || i == n) {
            logger.WriteString(message);
            break;
        }

        std::string_view arg = strings[i++];

        logger.WriteString(message.substr(0, placeholderPos));
        logger.WriteString(arg);

        message = message.substr(placeholderPos + 2);
    }

    logger.FinalizeMessge();
}

inline void NextIn()
{
    if (!sFileIn.Eof()) {
        sLine = sFileIn.GetNextLine();
        ++sLineNumber;

        Log("Journal: line {} is '{}'", sLineNumber, sLine);
    }
}

wxArrayStringEx PeekTokens()
{
    wxArrayStringEx tokens;
    if (Journal::IsReplaying()) {
        for (; !sFileIn.Eof(); NextIn()) {
            if (sLine.StartsWith(CommentCharacter)) {
                continue;
            }

            tokens = wxSplit(sLine, SeparatorCharacter, EscapeCharacter);
            if (tokens.empty()) {
                // Ignore blank lines
                continue;
            }

            break;
        }
    }
    return tokens;
}

constexpr auto VersionToken = wxT("Version");

// Numbers identifying the journal format version
int journalVersionNumbers[] = {
    1
};

wxString VersionString()
{
    wxString result;
    for ( auto number : journalVersionNumbers ) {
        auto str = wxString::Format("%d", number);
        result += (result.empty() ? str : ('.' + str));
    }
    return result;
}

//! True if value is an acceptable journal version number to be rerun
bool VersionCheck(const wxString& value)
{
    auto strings = wxSplit(value, '.');
    std::vector<int> numbers;
    for ( auto& string : strings ) {
        long value;
        if (!string.ToCLong(&value)) {
            return false;
        }
        numbers.push_back(value);
    }
    // OK if the static version number is not less than the given value
    // Maybe in the future there will be a compatibility break
    return !std::lexicographical_compare(
        std::begin(journalVersionNumbers), std::end(journalVersionNumbers),
        numbers.begin(), numbers.end());
}
}

SyncException::SyncException(const wxString& string)
{
    // If the exception is ever constructed, cause nonzero program exit code
    SetError();

    Log("Journal sync failed: {}", string);
}

SyncException::~SyncException() {}

void SyncException::DelayedHandlerAction()
{
    // Simulate the application Exit menu item
    wxCommandEvent evt{ wxEVT_MENU, wxID_EXIT };
    wxTheApp->AddPendingEvent(evt);
}

bool RecordEnabled()
{
    return JournalEnabled.Read();
}

bool SetRecordEnabled(bool value)
{
    auto result = JournalEnabled.Write(value);
    gPrefs->Flush();
    return result;
}

bool IsReplaying()
{
    return sFileIn.IsOpened();
}

void SetInputFileName(const wxString& path)
{
    sFileNameIn = path;
}

bool Begin(const FilePath& dataDir)
{
    if (!GetError() && !sFileNameIn.empty()) {
        wxFileName fName{ sFileNameIn };
        fName.MakeAbsolute(dataDir);
        const auto path = fName.GetFullPath();
        sFileIn.Open(path);
        if (!sFileIn.IsOpened()) {
            Log("Journal: failed to open journal file \"{}\"", path);
            SetError();
        } else {
            sLine = sFileIn.GetFirstLine();
            sLineNumber = 0;

            auto tokens = PeekTokens();
            NextIn();

            if (!(tokens.size() == 2 && tokens[0] == VersionToken
                  && VersionCheck(tokens[1]))) {
                Log("Journal: invalid journal version: \"{}\"", tokens[1]);
                SetError();
            }
        }
    }

    if (!GetError() && RecordEnabled()) {
        wxFileName fName{ dataDir, "journal", "txt" };
        const auto path = fName.GetFullPath();
        if (!OpenOut(path)) {
            SetError();
        } else {
            // Generate a header
            Comment(wxString::Format(
                        wxT("Journal recorded by %s on %s"),
                        wxGetUserName(),
                        wxDateTime::Now().Format()
                        ));
            Output({ VersionToken, VersionString() });
        }
    }

    // Call other registered initialization steps
    for (auto& initializer : GetInitializers()) {
        if (initializer && !initializer()) {
            SetError();
            break;
        }
    }

    return !GetError();
}

wxArrayStringEx GetTokens()
{
    auto result = PeekTokens();
    if (!result.empty()) {
        NextIn();
        return result;
    }
    throw SyncException("unexpected end of stream");
}

bool Dispatch()
{
    if (GetError()) {
        // Don't repeatedly indicate error
        // Do nothing
        return false;
    }

    if (!IsReplaying()) {
        return false;
    }

    // This will throw if no lines remain.  A proper journal should exit the
    // program before that happens.
    auto words = GetTokens();

    // Lookup dispatch function by the first field of the line
    auto& table = GetDictionary();
    auto& name = words[0];
    auto iter = table.find(name);
    if (iter == table.end()) {
        throw SyncException(
                  wxString::Format("unknown command: %s", name.ToStdString().c_str()));
    }

    // Pass all the fields including the command name to the function
    if (!iter->second(words)) {
        throw SyncException(wxString::Format(
                                "command '%s' has failed", wxJoin(words, ',').ToStdString().c_str()));
    }

    return true;
}

void Sync(const wxString& string)
{
    if (IsRecording() || IsReplaying()) {
        if (IsRecording()) {
            Output(string);
        }
        if (IsReplaying()) {
            if (sFileIn.Eof() || sLine != string) {
                throw SyncException(wxString::Format(
                                        "sync failed. Expected '%s', got '%s'",
                                        string.ToStdString().c_str(), sLine.ToStdString().c_str()));
            }

            NextIn();
        }
    }
}

void Sync(const wxArrayString& strings)
{
    if (IsRecording() || IsReplaying()) {
        auto string = ::wxJoin(strings, SeparatorCharacter, EscapeCharacter);
        Sync(string);
    }
}

void Sync(std::initializer_list< const wxString > strings)
{
    return Sync(wxArrayStringEx(strings));
}

int IfNotPlaying(
    const wxString& string, const InteractiveAction& action)
{
    // Special journal word
    Sync(string);

    // Then read or write the return value on another journal line
    if (IsReplaying()) {
        auto tokens = GetTokens();
        if (tokens.size() == 1) {
            try {
                std::wstring str{ tokens[0].wc_str() };
                size_t length = 0;
                auto result = std::stoi(str, &length);
                if (length == str.length()) {
                    if (IsRecording()) {
                        Journal::Output(std::to_wstring(result));
                    }
                    return result;
                }
            }
            catch (const std::exception&) {}
        }

        throw SyncException(wxString::Format(
                                "unexpected tokens: %s", wxJoin(tokens, ',').ToStdString().c_str()));
    } else {
        auto result = action ? action() : 0;
        if (IsRecording()) {
            Output(std::to_wstring(result));
        }
        return result;
    }
}

int GetExitCode()
{
    // Unconsumed commands remaining in the input file is also an error condition.
    if (!GetError() && !PeekTokens().empty()) {
        NextIn();
        SetError();
    }
    if (GetError()) {
        // Return nonzero
        // Returning the (1-based) line number at which the script failed is a
        // simple way to communicate that information to the test driver script.
        return sLineNumber ? sLineNumber : -1;
    }

    // Return zero to mean all is well, the convention for command-line tools
    return 0;
}
}
