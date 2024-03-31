#ifndef AU_AU3WRAP_AUDACITY3PROJECT_H
#define AU_AU3WRAP_AUDACITY3PROJECT_H

#include <memory>

namespace au::au3 {
struct Audacity3ProjectData;
class Audacity3Project
{
public:

    Audacity3Project();

    static std::shared_ptr<Audacity3Project> create();

private:

    std::shared_ptr<Audacity3ProjectData> m_data;
};
}

#endif // AU_AU3WRAP_AUDACITY3PROJECT_H
