#include "projectthumbnail.h"

#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-project-file-io/ProjectSerializer.h"
#include "au3-project/Project.h"

using namespace au::au3;

static const AudacityProject::AttachedObjects::RegisteredFactory key {
    [](AudacityProject&) {
        return std::make_shared<ProjectThumbnail>();
    }
};

static ProjectFileIORegistry::ObjectWriterEntry writerEntry {
    [](const AudacityProject& project, XMLWriter& xmlFile) {
        ProjectThumbnail::Get(const_cast<AudacityProject&>(project)).WriteXML(xmlFile);
    }
};

static ProjectFileIORegistry::ObjectReaderEntry readerEntry {
    "thumbnail",
    [](AudacityProject& project) -> XMLTagHandler* {
        return &ProjectThumbnail::Get(project);
    }
};

ProjectThumbnail& ProjectThumbnail::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectThumbnail>(key);
}

const ProjectThumbnail& ProjectThumbnail::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

void ProjectThumbnail::SetData(std::vector<uint8_t> data)
{
    m_data = std::move(data);
}

const std::vector<uint8_t>& ProjectThumbnail::GetData() const
{
    return m_data;
}

bool ProjectThumbnail::HasData() const
{
    return !m_data.empty();
}

void ProjectThumbnail::WriteXML(XMLWriter& xmlFile) const
{
    if (!HasData()) {
        return;
    }

    auto* serializer = dynamic_cast<ProjectSerializer*>(&xmlFile);
    if (serializer == nullptr) {
        return;
    }

    xmlFile.StartTag(wxT("thumbnail"));
    serializer->WriteBlob(wxT("data"), m_data.data(), m_data.size());
    xmlFile.EndTag(wxT("thumbnail"));
}

bool ProjectThumbnail::HandleXMLTag(const std::string_view& tag, const AttributesList&)
{
    return tag == "thumbnail";
}

XMLTagHandler* ProjectThumbnail::HandleXMLChild(const std::string_view&)
{
    return nullptr;
}

void ProjectThumbnail::HandleXMLBlob(const std::string_view& name, const void* data, size_t len)
{
    if (name == "data") {
        const auto* bytes = static_cast<const uint8_t*>(data);
        m_data.assign(bytes, bytes + len);
    }
}
