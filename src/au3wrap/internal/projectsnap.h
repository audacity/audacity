/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-registries/ClientData.h"
#include "au3-utility/Observer.h"
#include "au3-project/Project.h"

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
