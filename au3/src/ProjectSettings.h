/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSettings.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SETTINGS__
#define __AUDACITY_PROJECT_SETTINGS__

#include <atomic>
#include <wx/event.h> // to declare custom event type

#include "ClientData.h" // to inherit
#include "Observer.h"
#include "Prefs.h" // to inherit
#include "audacity/Types.h"

class AudacityProject;

namespace ToolCodes {
enum : int {
    // The buttons that are in the Tools toolbar must be in correspondence
    // with the first few
    selectTool,
    envelopeTool,
    drawTool,
    multiTool,

#ifdef EXPERIMENTAL_BRUSH_TOOL
    brushTool,
#endif

    numTools,
    firstTool = selectTool,
};
}

struct ProjectSettingsEvent {
    const enum Type : int {
        ChangedTool,
    } type;
    const int oldValue;
    const int newValue;
};

///\brief Holds various per-project settings values,
/// and sends events to the project when certain values change
class AUDACITY_DLL_API ProjectSettings final : public ClientData::Base, public Observer::Publisher<ProjectSettingsEvent>,
    private PrefsListener
{
public:
    static ProjectSettings& Get(AudacityProject& project);
    static const ProjectSettings& Get(const AudacityProject& project);

    explicit ProjectSettings(AudacityProject& project);
    ProjectSettings(const ProjectSettings&) = delete;
    ProjectSettings& operator=(const ProjectSettings&) = delete;

    // Current tool

    void SetTool(int tool);
    int GetTool() const { return mCurrentTool; }

    // Current brush radius
    void SetBrushRadius(int brushRadius) { mCurrentBrushRadius = brushRadius; }
    int GetBrushRadius() const { return mCurrentBrushRadius; }

    void SetSmartSelection(bool isSelected) { mbSmartSelection = isSelected; }
    bool IsSmartSelection() const { return mbSmartSelection; }

    void SetOvertones(bool isSelected) { mbOvertones = isSelected; }
    bool IsOvertones() const { return mbOvertones; }

    bool GetShowSplashScreen() const { return mShowSplashScreen; }

private:
    void UpdatePrefs() override;

    AudacityProject& mProject;

    int mCurrentTool;
    int mCurrentBrushRadius;
    int mCurrentBrushHop;
    bool mbSmartSelection { false };
    bool mbOvertones { false };

    bool mShowSplashScreen;
};

#endif
