#include "projectsnap.h"

#include "XMLAttributeValueView.h"
#include "XMLWriter.h"
#include "Prefs.h"

using namespace au::au3;

static const AttachedProjectObjects::RegisteredFactory key
{
    [](AudacityProject&) {
        return std::make_shared<ProjectSnap>();
    }
};

ProjectSnap& ProjectSnap::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectSnap&>(key);
}

const ProjectSnap& ProjectSnap::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ProjectSnap::ProjectSnap() {}

ProjectSnap::~ProjectSnap() {}

bool ProjectSnap::isSnapEnabled() const
{
    return m_snapEnabled;
}

void ProjectSnap::enableSnap(bool enable)
{
    if (m_snapEnabled != enable) {
        m_snapEnabled = enable;
    }
}

int ProjectSnap::snapType() const
{
    return m_snapType;
}

void ProjectSnap::setSnapType(unsigned int type)
{
    if (m_snapType != type) {
        m_snapType = type;
    }
}

bool ProjectSnap::isSnapTriplets() const
{
    return m_snapTriplets;
}

void ProjectSnap::setSnapTriplets(bool triplets)
{
    if (m_snapTriplets != triplets) {
        m_snapTriplets = triplets;
    }
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        auto& snap = ProjectSnap::Get(project);
        xmlFile.WriteAttr(wxT("snap_enabled"), snap.isSnapEnabled());
        xmlFile.WriteAttr(wxT("snap_type"), snap.snapType());
        xmlFile.WriteAttr(wxT("snap_triplets"), snap.isSnapTriplets());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
    (ProjectSnap & (*)(AudacityProject&)) & ProjectSnap::Get, {
        { "snap_enabled", [](auto& snap, auto value) {
                const bool enabled = value.Get(snap.isSnapEnabled());
                snap.enableSnap(enabled);
            } },
        { "snap_type", [](auto& snap, auto value) {
                const int type = value.Get(snap.snapType());
                snap.setSnapType(type);
            } },
        { "snap_triplets", [](auto& snap, auto value) {
                const bool triplets = value.Get(snap.isSnapTriplets());
                snap.setSnapTriplets(triplets);
            } }
    }
};
