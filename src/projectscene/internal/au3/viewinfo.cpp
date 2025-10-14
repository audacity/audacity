#include "viewinfo.h"

#include "XMLAttributeValueView.h"
#include "XMLWriter.h"
#include "Prefs.h"
#include "UndoManager.h"

using namespace au::au3;

static const AttachedProjectObjects::RegisteredFactory key
{
    [](AudacityProject&) {
        return std::make_shared<ViewInfo>();
    }
};

ViewInfo& ViewInfo::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ViewInfo&>(key);
}

const ViewInfo& ViewInfo::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

ViewInfo::ViewInfo() {}

ViewInfo::~ViewInfo() {}

double ViewInfo::zoom() const
{
    return m_zoom;
}

void ViewInfo::setZoom(double zoom)
{
    if (m_zoom != zoom) {
        m_zoom = zoom;
    }
}

int ViewInfo::vPos() const
{
    return m_vpos;
}

void ViewInfo::setVPos(int pos)
{
    if (m_vpos != pos) {
        m_vpos = pos;
    }
}

double ViewInfo::hPos() const
{
    return m_hpos;
}

void ViewInfo::setHPos(double pos)
{
    if (m_hpos != pos) {
        m_hpos = pos;
    }
}

static ProjectFileIORegistry::AttributeWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        auto& viewInfo = ViewInfo::Get(project);
        xmlFile.WriteAttr(wxT("viewstate_zoom"), viewInfo.zoom());
        xmlFile.WriteAttr(wxT("viewstate_vpos"), viewInfo.vPos());
        xmlFile.WriteAttr(wxT("viewstate_hpos"), viewInfo.hPos());
    }
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
    (ViewInfo & (*)(AudacityProject&)) & ViewInfo::Get, {
        { "viewstate_zoom", [](auto& viewInfo, auto value) {
                const double zoom = value.Get(viewInfo.zoom());
                viewInfo.setZoom(zoom);
            } },
        { "viewstate_vpos", [](auto& viewInfo, auto value) {
                const int vpos = value.Get(viewInfo.vPos());
                viewInfo.setVPos(vpos);
            } },
        { "viewstate_hpos", [](auto& viewInfo, auto value) {
                const double hpos = value.Get(viewInfo.hPos());
                viewInfo.setHPos(hpos);
            } }
    }
};

namespace {
struct RestorerNotification : public ClientData::Base
{
    static RestorerNotification& Get(AudacityProject& project);
    muse::async::Notification notification;
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{ [](AudacityProject&) {
        return std::make_shared<RestorerNotification>();
    } };

RestorerNotification& RestorerNotification::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<RestorerNotification>(key);
}
}

namespace au::au3 {
struct ViewStateRestorer final : UndoStateExtension {
    ViewStateRestorer(AudacityProject& project)
    {
        m_viewInfo = ViewInfo::Get(project);
    }

    void RestoreUndoRedoState(AudacityProject& project) override
    {
        ViewInfo::Get(project) = m_viewInfo;
        RestorerNotification::Get(project).notification.notify();
    }

    ViewInfo m_viewInfo;
};
}

namespace {
UndoRedoExtensionRegistry::Entry<ViewStateRestorer> sEntry { [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension> {
        return std::make_shared<ViewStateRestorer>(project);
    }
};
}

void au::au3::setViewStateRestorerNotification(AudacityProject& project, muse::async::Notification notification)
{
    RestorerNotification::Get(project).notification = notification;
}
