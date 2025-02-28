/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ProjectNumericFormats.cpp

 Paul Licameli split from ProjectSettings.cpp

 **********************************************************************/
#ifndef __AUDACITY_PROJECT_NUMERIC_FORMATS__
#define __AUDACITY_PROJECT_NUMERIC_FORMATS__

#include "ClientData.h"
#include "ComponentInterfaceSymbol.h"
#include "NumericConverterType.h"
#include "Observer.h"

class AudacityProject;

struct ProjectNumericFormatsEvent {
    const enum Type : int {
        ChangedSelectionFormat,
        ChangedAudioTimeFormat,
        ChangedFrequencyFormat,
        ChangedBandwidthFormat,
    } type;
    const NumericFormatID oldValue;
    const NumericFormatID newValue;
};

class NUMERIC_FORMATS_API ProjectNumericFormats final : public ClientData::Base, public Observer::Publisher<ProjectNumericFormatsEvent>
{
public:
    static ProjectNumericFormats& Get(AudacityProject& project);
    static const ProjectNumericFormats& Get(const AudacityProject& project);

    explicit ProjectNumericFormats(const AudacityProject& project);
    ~ProjectNumericFormats() override;

    // Selection Format
    void SetSelectionFormat(const NumericFormatID& format);
    NumericFormatID GetSelectionFormat() const;

    // AudioTime format
    void SetAudioTimeFormat(const NumericFormatID& format);
    NumericFormatID GetAudioTimeFormat() const;

    // Spectral Selection Formats
    void SetFrequencySelectionFormatName(const NumericFormatID& format);
    NumericFormatID GetFrequencySelectionFormatName() const;

    void SetBandwidthSelectionFormatName(const NumericFormatID& format);
    NumericFormatID GetBandwidthSelectionFormatName() const;

    NumericFormatSymbol LookupFormat(const NumericConverterType& type, const wxString& identifier);

private:
    const AudacityProject& mProject;

    NumericFormatID mSelectionFormat;
    NumericFormatID mFrequencySelectionFormatName;
    NumericFormatID mBandwidthSelectionFormatName;
    NumericFormatID mAudioTimeFormat;
};

#endif
