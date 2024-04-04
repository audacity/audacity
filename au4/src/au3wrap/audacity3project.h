#ifndef AU_AU3WRAP_AUDACITY3PROJECT_H
#define AU_AU3WRAP_AUDACITY3PROJECT_H

#include <memory>
#include <string>

#include "global/io/path.h"

namespace au::au3 {
struct Audacity3ProjectData;
class Audacity3Project
{
public:

    Audacity3Project();

    static std::shared_ptr<Audacity3Project> create();

    bool load(const mu::io::path_t& filePath);

    std::string title() const;

private:

    std::shared_ptr<Audacity3ProjectData> m_data;
};
}

#endif // AU_AU3WRAP_AUDACITY3PROJECT_H
