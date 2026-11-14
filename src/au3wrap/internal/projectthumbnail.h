#pragma once

#include <vector>
#include <cstdint>

#include "au3-project/Project.h"
#include "au3-xml/XMLTagHandler.h"

namespace au::au3 {
class ProjectThumbnail final : public ClientData::Base, public XMLTagHandler
{
public:
    static ProjectThumbnail& Get(AudacityProject& project);
    static const ProjectThumbnail& Get(const AudacityProject& project);

    void SetData(std::vector<uint8_t> data);
    const std::vector<uint8_t>& GetData() const;
    bool HasData() const;

    void WriteXML(XMLWriter& xmlFile) const;

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void HandleXMLBlob(const std::string_view& name, const void* data, size_t len) override;

private:
    std::vector<uint8_t> m_data;
};
}
