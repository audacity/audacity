/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSelectionManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SELECTION_MANAGER__
#define __AUDACITY_PROJECT_SELECTION_MANAGER__

#include "ClientData.h" // to inherit
#include "Observer.h"
#include "ComponentInterfaceSymbol.h"
#include "Observer.h"

class AudacityProject;
struct ProjectNumericFormatsEvent;

//! This object is useful mostly as an observer of others in the project
/*!
 It listens for changes of selection formats and snap-to choices, and reacts
 by updating persistent preferences, and updating the time selection to be
 consistent with those choices.
 */
class TIME_FREQUENCY_SELECTION_API ProjectSelectionManager final : public ClientData::Base
{
public:
    static ProjectSelectionManager& Get(AudacityProject& project);
    static const ProjectSelectionManager& Get(const AudacityProject& project);

    explicit ProjectSelectionManager(AudacityProject& project);
    ProjectSelectionManager(const ProjectSelectionManager&) = delete;
    ProjectSelectionManager& operator=(
        const ProjectSelectionManager&) = delete;
    ~ProjectSelectionManager() override;

    void ModifySelection(double& start, double& end, bool done);
    void ModifySpectralSelection(double nyquist, double& bottom, double& top, bool done);

private:
    void OnFormatsChanged(ProjectNumericFormatsEvent);

    void SetSelectionFormat(const NumericFormatID& format);
    void SetAudioTimeFormat(const NumericFormatID& format);
    void SetFrequencySelectionFormatName(
        const NumericFormatID& formatName);
    void SetBandwidthSelectionFormatName(
        const NumericFormatID& formatName);

    void SnapSelection();

    Observer::Subscription mFormatsSubscription;
    AudacityProject& mProject;

    Observer::Subscription mSnappingChangedSubscription;
    Observer::Subscription mTimeSignatureChangedSubscription;
    Observer::Subscription mProjectRateChangedSubscription;
};

#endif
