/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewInfo.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_VIEWINFO__
#define __AUDACITY_VIEWINFO__

#include <utility>
#include <vector>
#include <wx/weakref.h> // member variable
#include "ClientData.h"
#include "SelectedRegion.h"
#include <memory>
#include "Observer.h"
#include "Prefs.h"
#include "XMLMethodRegistry.h"
#include "ZoomInfo.h" // to inherit

struct NotifyingSelectedRegionMessage : Observer::Message {};

// This heavyweight wrapper of the SelectedRegion structure emits events
// on mutating operations, that other classes can listen for.
class TIME_FREQUENCY_SELECTION_API NotifyingSelectedRegion : public Observer::Publisher<NotifyingSelectedRegionMessage>, public wxTrackable
{
public:
    // Expose SelectedRegion's const accessors
    double t0() const { return mRegion.t0(); }
    double t1() const { return mRegion.t1(); }
    double f0() const { return mRegion.f0(); }
    double f1() const { return mRegion.f1(); }
    double fc() const { return mRegion.fc(); }
    bool isPoint() const { return mRegion.isPoint(); }
    double duration() const { return mRegion.duration(); }

    // Writing and reading of persistent fields -- the read is mutating but
    // does not emit events
    void WriteXMLAttributes
        (XMLWriter& xmlFile,
        const char* legacyT0Name, const char* legacyT1Name) const
    { mRegion.WriteXMLAttributes(xmlFile, legacyT0Name, legacyT1Name); }

    //! Return some information used for deserialization purposes by ViewInfo
    static XMLMethodRegistryBase::Mutators<NotifyingSelectedRegion>
    Mutators(const char* legacyT0Name, const char* legacyT1Name);

    // const-only access allows assignment from this into a SelectedRegion
    // or otherwise passing it into a function taking const SelectedRegion&
    operator const SelectedRegion&() const {
        return mRegion;
    }

    // These are the event-emitting operations
    NotifyingSelectedRegion& operator =(const SelectedRegion& other);

    // Returns true iff the bounds got swapped
    bool setTimes(double t0, double t1);

    // Returns true iff the bounds got swapped
    bool setT0(double t, bool maySwap = true);

    // Returns true iff the bounds got swapped
    bool setT1(double t, bool maySwap = true);

    void collapseToT0();

    void collapseToT1();

    void move(double delta);

    // Returns true iff the bounds got swapped
    bool setFrequencies(double f0, double f1);

    // Returns true iff the bounds got swapped
    bool setF0(double f, bool maySwap = true);

    // Returns true iff the bounds got swapped
    bool setF1(double f, bool maySwap = true);

private:
    void Notify(bool delayed = false);

    SelectedRegion mRegion;
};

enum : int {
    kVerticalPadding = 6, /*!< Width of padding below each channel group */
};

enum : int {
    kTrackInfoTitleHeight = 16,
    kTrackInfoTitleExtra = 2,
    kTrackInfoBtnSize = 18, // widely used dimension, usually height
    kTrackInfoSliderHeight = 25,
    kTrackInfoSliderWidth = 139,
    kTrackInfoSliderAllowance = 5,
    kTrackInfoSliderExtra = 5,
};

struct PlayRegionMessage : Observer::Message {};

class TIME_FREQUENCY_SELECTION_API PlayRegion : public Observer::Publisher<PlayRegionMessage>
{
public:
    PlayRegion() = default;

    PlayRegion(const PlayRegion&) = delete;
    PlayRegion& operator=(const PlayRegion& that)
    {
        mActive = that.mActive;
        // Guarantee the equivalent un-swapped order of endpoints
        mStart = that.GetStart();
        mEnd = that.GetEnd();
        mLastActiveStart = that.GetLastActiveStart();
        mLastActiveEnd = that.GetLastActiveEnd();
        return *this;
    }

    bool Active() const { return mActive; }
    void SetActive(bool active);

    bool Empty() const { return GetStart() == GetEnd(); }
    double GetStart() const
    {
        if (mEnd < 0) {
            return mStart;
        } else {
            return std::min(mStart, mEnd);
        }
    }

    double GetEnd() const
    {
        if (mStart < 0) {
            return mEnd;
        } else {
            return std::max(mStart, mEnd);
        }
    }

    double GetLastActiveStart() const
    {
        if (mLastActiveEnd < 0) {
            return mLastActiveStart;
        } else {
            return std::min(mLastActiveStart, mLastActiveEnd);
        }
    }

    double GetLastActiveEnd() const
    {
        if (mLastActiveStart < 0) {
            return mLastActiveEnd;
        } else {
            return std::max(mLastActiveStart, mLastActiveEnd);
        }
    }

    void SetStart(double start);
    void SetEnd(double end);
    void SetTimes(double start, double end);
    // Set current and last active times the same regardless of activation:
    void SetAllTimes(double start, double end);

    //! Set to an invalid state
    void Clear();
    //! Test whether in invalid state
    bool IsClear() const;
    //! Test whether last active region is in invalid state
    bool IsLastActiveRegionClear() const;

    void Order();

private:
    void Notify();

    // Times:
    static constexpr auto invalidValue = -std::numeric_limits<double>::infinity();

    double mStart { invalidValue };
    double mEnd { invalidValue };
    double mLastActiveStart { invalidValue };
    double mLastActiveEnd { invalidValue };

    bool mActive{ false };
};

extern TIME_FREQUENCY_SELECTION_API const TranslatableString LoopToggleText;

class TIME_FREQUENCY_SELECTION_API ViewInfo final : public ZoomInfo, public PrefsListener, public ClientData::Base
{
public:
    static ViewInfo& Get(AudacityProject& project);
    static const ViewInfo& Get(const AudacityProject& project);

    ViewInfo(double start, double pixelsPerSecond);
    ViewInfo(const ViewInfo&) = delete;
    ViewInfo& operator=(const ViewInfo&) = delete;

    int GetHeight() const { return mHeight; }
    void SetHeight(int height) { mHeight = height; }

    static int UpdateScrollPrefsID();
    void UpdatePrefs() override;
    void UpdateSelectedPrefs(int id) override;

    double GetBeforeScreenWidth() const
    {
        return hpos * zoom;
    }

    // Current selection

    NotifyingSelectedRegion selectedRegion;
    PlayRegion playRegion;

    // Scroll info

    //! Pixel distance from top of tracks to top of visible scrolled area
    int vpos{ 0 };

    // Other stuff, mainly states (true or false) related to autoscroll and
    // drawing the waveform. Maybe this should be put somewhere else?

    bool bUpdateTrackIndicator;

    void WriteXMLAttributes(XMLWriter& xmlFile) const;

private:
    int mHeight{ 0 };

    struct ProjectFileIORegistration;
};

#endif
