/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ClientData.h"
#include "Observer.h"
#include "Project.h"

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
    double m_zoom{ 0.0 };
    int m_vpos{ 0 };
    double m_hpos{ 0.0 };
};
}
