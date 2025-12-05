/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-registries/ClientData.h"
#include "au3-utility/Observer.h"
#include "au3-project/Project.h"

#include "framework/global/async/notification.h"

class AudacityProject;

namespace au::au3 {
class ViewInfo : public ClientData::Base
{
public:
    static ViewInfo& Get(AudacityProject& project);
    static const ViewInfo& Get(const AudacityProject& project);

    ViewInfo();
    ~ViewInfo();

    void setZoom(double zoom);
    double zoom() const;
    void setVPos(int pos);
    int vPos() const;
    void setHPos(double hPos);
    double hPos() const;

private:
    friend struct ViewStateRestorer;
    ViewInfo& operator=(const ViewInfo& other)
    {
        m_zoom = other.m_zoom;
        m_vpos = other.m_vpos;
        m_hpos = other.m_hpos;
        return *this;
    }

    double m_zoom{ 0.0 };
    int m_vpos{ 0 };
    double m_hpos{ 0.0 };
};

void setViewStateRestorerNotification(AudacityProject&, muse::async::Notification);
}
