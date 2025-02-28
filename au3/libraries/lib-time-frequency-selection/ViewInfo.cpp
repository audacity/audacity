/**********************************************************************

Audacity: A Digital Audio Editor

ViewInfo.cpp

Paul Licameli

**********************************************************************/

#include "ViewInfo.h"

#include <algorithm>
#include "XMLAttributeValueView.h"

#include "BasicUI.h"
#include "Project.h"
#include "UndoManager.h"
#include "XMLWriter.h"

XMLMethodRegistryBase::Mutators<NotifyingSelectedRegion>
NotifyingSelectedRegion::Mutators(
    const char* legacyT0Name, const char* legacyT1Name)
{
    XMLMethodRegistryBase::Mutators<NotifyingSelectedRegion> results;
    // Get serialization methods of contained SelectedRegion, and wrap each
    for (auto& delegate: SelectedRegion::Mutators(legacyT0Name, legacyT1Name)) {
        results.emplace_back(
            delegate.first,
            [fn = std::move(delegate.second)](auto& region, auto value) {
            fn(region.mRegion, value);
            region.Notify(true);
        }
            );
    }
    return results;
}

NotifyingSelectedRegion& NotifyingSelectedRegion::operator =
    (const SelectedRegion& other)
{
    if (mRegion != other) {
        mRegion = other;
        Notify();
    }
    return *this;
}

bool NotifyingSelectedRegion::setTimes(double t0, double t1)
{
    bool result = false;
    if (mRegion.t0() != t0 || mRegion.t1() != t1) {
        result = mRegion.setTimes(t0, t1);
        Notify();
    }
    return result;
}

bool NotifyingSelectedRegion::setT0(double t, bool maySwap)
{
    bool result = false;
    if (mRegion.t0() != t) {
        result = mRegion.setT0(t, maySwap);
        Notify();
    }
    return result;
}

bool NotifyingSelectedRegion::setT1(double t, bool maySwap)
{
    bool result = false;
    if (mRegion.t1() != t) {
        result = mRegion.setT1(t, maySwap);
        Notify();
    }
    return result;
}

void NotifyingSelectedRegion::collapseToT0()
{
    if (mRegion.t0() != mRegion.t1()) {
        mRegion.collapseToT0();
        Notify();
    }
}

void NotifyingSelectedRegion::collapseToT1()
{
    if (mRegion.t0() != mRegion.t1()) {
        mRegion.collapseToT1();
        Notify();
    }
}

void NotifyingSelectedRegion::move(double delta)
{
    if (delta != 0) {
        mRegion.move(delta);
        Notify();
    }
}

bool NotifyingSelectedRegion::setFrequencies(double f0, double f1)
{
    bool result = false;
    if (mRegion.f0() != f0 || mRegion.f1() != f1) {
        result = mRegion.setFrequencies(f0, f1);
        Notify();
    }
    return result;
}

bool NotifyingSelectedRegion::setF0(double f, bool maySwap)
{
    bool result = false;
    if (mRegion.f0() != f) {
        result = mRegion.setF0(f, maySwap);
        Notify();
    }
    return result;
}

bool NotifyingSelectedRegion::setF1(double f, bool maySwap)
{
    bool result = false;
    if (mRegion.f1() != f) {
        result = mRegion.setF1(f, maySwap);
        Notify();
    }
    return result;
}

void NotifyingSelectedRegion::Notify(bool delayed)
{
    if (delayed) {
        BasicUI::CallAfter([This = wxWeakRef(this)]{
            if (This) {
                This->Publish({});
            }
        });
    } else {
        Publish({});
    }
}

void PlayRegion::SetActive(bool active)
{
    if (mActive != active) {
        mActive = active;
        if (mActive) {
            // Restore values
            if (mStart != mLastActiveStart || mEnd != mLastActiveEnd) {
                mStart = mLastActiveStart;
                mEnd = mLastActiveEnd;
            }
        }
        Notify();
    }
}

void PlayRegion::SetStart(double start)
{
    if (mStart != start) {
        if (mActive) {
            mLastActiveStart = start;
        }
        mStart = start;
        Notify();
    }
}

void PlayRegion::SetEnd(double end)
{
    if (mEnd != end) {
        if (mActive) {
            mLastActiveEnd = end;
        }
        mEnd = end;
        Notify();
    }
}

void PlayRegion::SetTimes(double start, double end)
{
    if (mStart != start || mEnd != end) {
        if (mActive) {
            mLastActiveStart = start, mLastActiveEnd = end;
        }
        mStart = start, mEnd = end;
        Notify();
    }
}

void PlayRegion::SetAllTimes(double start, double end)
{
    SetTimes(start, end);
    mLastActiveStart = start, mLastActiveEnd = end;
}

void PlayRegion::Clear()
{
    SetAllTimes(invalidValue, invalidValue);
}

bool PlayRegion::IsClear() const
{
    return GetStart() == invalidValue && GetEnd() == invalidValue;
}

bool PlayRegion::IsLastActiveRegionClear() const
{
    return GetLastActiveStart() == invalidValue && GetLastActiveEnd() == invalidValue;
}

void PlayRegion::Order()
{
    if (mStart >= 0 && mEnd >= 0 && mStart > mEnd) {
        std::swap(mStart, mEnd);
        if (mActive) {
            mLastActiveStart = mStart, mLastActiveEnd = mEnd;
        }
        Notify();
    }
}

void PlayRegion::Notify()
{
    Publish({});
}

const TranslatableString LoopToggleText = XXO("Enable &Looping");

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    []( AudacityProject& project ) {
        return std::make_unique<ViewInfo>(0.0, ZoomInfo::GetDefaultZoom());
    }
};

ViewInfo& ViewInfo::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< ViewInfo >(key);
}

const ViewInfo& ViewInfo::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

ViewInfo::ViewInfo(double start, double pixelsPerSecond)
    : ZoomInfo(start, pixelsPerSecond)
    , selectedRegion()
    , bUpdateTrackIndicator(true)
{
    UpdatePrefs();
}

void ViewInfo::UpdateSelectedPrefs(int id)
{
    if (id == UpdateScrollPrefsID()) {
        gPrefs->Read(wxT("/GUI/AutoScroll"), &bUpdateTrackIndicator,
                     true);
    }
}

void ViewInfo::UpdatePrefs()
{
    UpdateSelectedPrefs(UpdateScrollPrefsID());
}

void ViewInfo::WriteXMLAttributes(XMLWriter& xmlFile) const
// may throw
{
    selectedRegion.WriteXMLAttributes(xmlFile, "sel0", "sel1");
    xmlFile.WriteAttr(wxT("vpos"), vpos);
    xmlFile.WriteAttr(wxT("h"), hpos, 10);
    xmlFile.WriteAttr(wxT("zoom"), zoom, 10);
}

//! Construct once at static initialization time to hook project file IO
static struct ViewInfo::ProjectFileIORegistration {
    ProjectFileIORegistry::AttributeReaderEntries entries {
        [](AudacityProject& project) -> NotifyingSelectedRegion&
        {
            return ViewInfo::Get(project).selectedRegion;
        },
        NotifyingSelectedRegion::Mutators("sel0", "sel1")
    };

    ProjectFileIORegistry::AttributeReaderEntries entries2 {
// Just a pointer to function, but needing overload resolution as non-const:
        (ViewInfo & (*)(AudacityProject&)) & ViewInfo::Get,
        {
            { "vpos", [](auto& viewInfo, auto value){
                    viewInfo.vpos = value.Get(viewInfo.vpos);
                    // Note that (other than in import of old .aup files) there is no other
                    // reassignment of vpos, except in handling the vertical scroll.
                } },
            { "h", [](auto& viewInfo, auto value){
                    viewInfo.hpos = value.Get(viewInfo.hpos);
                } },
            { "zoom", [](auto& viewInfo, auto value){
                    viewInfo.zoom = value.Get(viewInfo.zoom);
                } },
        } };
} projectFileIORegistration;

int ViewInfo::UpdateScrollPrefsID()
{
    return 10000;
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        ViewInfo::Get(project).WriteXMLAttributes(xmlFile);
    }
};

// Undo/redo handling of selection changes
namespace {
struct SelectedRegionRestorer final : UndoStateExtension {
    SelectedRegionRestorer(AudacityProject& project)
        : mSelectedRegion{ViewInfo::Get(project).selectedRegion}
    {}
    void RestoreUndoRedoState(AudacityProject& project) override
    {
        ViewInfo::Get(project).selectedRegion = mSelectedRegion;
    }

    SelectedRegion mSelectedRegion;
};

UndoRedoExtensionRegistry::Entry sEntry {
    [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension> {
        return std::make_shared<SelectedRegionRestorer>(project);
    }
};
}
