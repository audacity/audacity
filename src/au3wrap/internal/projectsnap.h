/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ClientData.h"
#include "Observer.h"
#include "Project.h"

class AudacityProject;

namespace au::au3 {
class ProjectSnap : public ClientData::Base
{
public:
    static ProjectSnap& Get(AudacityProject& project);
    static const ProjectSnap& Get(const AudacityProject& project);

    ProjectSnap();
    ~ProjectSnap();

    void enableSnap(bool enable);
    bool isSnapEnabled() const;
    int snapType() const;
    void setSnapType(unsigned int type);
    bool isSnapTriplets() const;
    void setSnapTriplets(bool triplets);

private:
    bool m_snapEnabled{ false };
    int m_snapType{ 0 };
    bool m_snapTriplets{ false };
};
}
