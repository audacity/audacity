/**********************************************************************

  Audacity: A Digital Audio Editor

  @file FFmpegPrefs.cpp
  @brief adds controls for FFmpeg import/export to Library preferences

  Paul Licameli split from LibraryPrefs.cpp

**********************************************************************/

#include "../FFmpeg.h"
#include "Internat.h"
#include "ShuttleGui.h"
#include "LibraryPrefs.h"
#include "AudacityMessageBox.h"
#include "HelpSystem.h"
#include "ReadOnlyText.h"
#include <wx/stattext.h>

namespace {
struct State {
    wxWindow* parent = nullptr;
    ReadOnlyText* FFmpegVersion = nullptr;
};

void OnFFmpegFindButton(State& state);

void SetFFmpegVersionText(State& state)
{
    auto FFmpegVersion = state.FFmpegVersion;
    FFmpegVersion->SetValue(GetFFmpegVersion());
}

void AddControls(ShuttleGui& S)
{
    auto pState = std::make_shared<State>();
    pState->parent = S.GetParent();

    S.StartStatic(XO("FFmpeg Import/Export Library"));
    {
        S.StartTwoColumn();
        {
            auto version
                =XO("No compatible FFmpeg library was found");

            pState->FFmpegVersion = S
                                    .Position(wxALIGN_CENTRE_VERTICAL)
                                    .AddReadOnlyText(XO("FFmpeg Library Version:"), version.Translation());

            S.AddVariableText(XO("FFmpeg Library:"),
                              true, wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);

            auto pFindButton
                =S
#if defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
                  .Disable()
#endif
                  .AddButton(XXO("Loca&te..."),
                             wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
            if (pFindButton) {
                pFindButton->Bind(wxEVT_BUTTON, [pState](wxCommandEvent&){
                    OnFFmpegFindButton(*pState);
                });
            }

            S.AddVariableText(XO("FFmpeg Library:"),
                              true, wxALL | wxALIGN_RIGHT | wxALIGN_CENTRE_VERTICAL);

            auto pDownButton
                =S
#if defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
                  .Disable()
#endif
                  .AddButton(XXO("Dow&nload"),
                             wxALL | wxALIGN_LEFT | wxALIGN_CENTRE_VERTICAL);
            if (pDownButton) {
                pDownButton->Bind(wxEVT_BUTTON, [pState](wxCommandEvent&){
                    HelpSystem::ShowHelp(pState->parent,
                                         wxT("FAQ:Installing_the_FFmpeg_Import_Export_Library"), true);
                });
            }
        }
        S.EndTwoColumn();
    }
    S.EndStatic();

    SetFFmpegVersionText(*pState);
}

void OnFFmpegFindButton(State& state)
{
    bool showerrs =
#if defined(_DEBUG)
        true;
#else
        false;
#endif
    // Load the libs ('true' means that all errors will be shown)
    bool locate = !LoadFFmpeg(showerrs);

    // Libs are fine, don't show "locate" dialog unless user really wants it
    if (!locate) {
        int response = AudacityMessageBox(
            XO(
                "Audacity has automatically detected valid FFmpeg libraries.\nDo you still want to locate them manually?"),
            XO("Success"),
            wxCENTRE | wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION);
        if (response == wxYES) {
            locate = true;
        }
    }

    if (locate) {
        // Show "Locate FFmpeg" dialog
        FindFFmpegLibs(state.parent);
        LoadFFmpeg(showerrs);
    }
    SetFFmpegVersionText(state);
}

LibraryPrefs::RegisteredControls reg{ wxT("FFmpeg"), AddControls };
}
