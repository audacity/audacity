/*
* Audacity: A Digital Audio Editor
*/

#include "serialization/xmlstreamwriter.h"
#include "serialization/xmlstreamreader.h"
#include "io/buffer.h"

#include "au3metadata.h"
#include "project/types/projectmeta.h"

#include "au3wrap/au3types.h"

using namespace au::project;
using namespace muse;
using namespace muse::io;

ProjectMeta Au3Metadata::tags() const
{
    auto project = reinterpret_cast<au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    ProjectMeta au4Tags;
    Tags& au3Tags = Tags::Get(*project);

    for (auto row : au3Tags.GetRange()) {
        auto tag = row.first.ToStdString();
        if (tag == TRACK_TITLE_TAG) {
            au4Tags.trackTitle = row.second.ToStdString();
        }

        const std::string key = row.first.ToStdString();
        const std::string val = row.second.ToStdString();

        bool assigned = false;
        // assign to standard fields if matched
        for (size_t i = 0; i < project::standardTags.size(); ++i) {
            if (key == project::standardTags[i]) {
                au4Tags.*(kStdMembers[i]) = val;
                assigned = true;
                break;
            }
        }

        if (!assigned) {
            au4Tags.additionalTags.insert(QString::fromStdString(key), QString::fromStdString(val));
        }
    }

    return au4Tags;
}

void Au3Metadata::setTags(ProjectMeta au4Tags)
{
    auto project = reinterpret_cast<au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    Tags& au3Tags = Tags::Get(*project);
    au3Tags.Clear();

    for (size_t i = 0; i < project::standardTags.size(); ++i) {
        const std::string& key = project::standardTags[i];
        const std::string& val = au4Tags.*(kStdMembers[i]);

        au3Tags.SetTag(key, val);
    }

    for (auto it = au4Tags.additionalTags.cbegin(); it != au4Tags.additionalTags.cend(); ++it) {
        const QString& key = it.key();
        if (key.isEmpty()) {
            continue;
        }

        const QString val = it.value().toString();

        au3Tags.SetTag(key.toStdString(), val.toStdString());
    }
}

std::string Au3Metadata::buildXml(const project::ProjectMeta& meta) const
{
    ByteArray data;
    Buffer buf(&data);
    buf.open(muse::io::IODevice::WriteOnly);

    XmlStreamWriter xml(&buf);

    xml.startElement("tags");

    for (size_t i = 0; i < kStdTags.size(); ++i) {
        const std::string& name = kStdTags[i];
        const std::string& val  = meta.*(project::kStdMembers[i]);

        XmlStreamWriter::Attributes attrs;
        attrs.emplace_back("name",  muse::String::fromStdString(name));
        attrs.emplace_back("value", muse::String::fromStdString(val));

        xml.element("tag", attrs);
    }

    for (auto it = meta.additionalTags.cbegin(); it != meta.additionalTags.cend(); ++it) {
        const std::string& name = it.key().toStdString();
        const std::string val  = it.value().toString().toStdString();

        XmlStreamWriter::Attributes attrs;
        attrs.emplace_back("name",  muse::String::fromStdString(name));
        attrs.emplace_back("value", muse::String::fromStdString(val));

        xml.element("tag", attrs);
    }

    xml.endElement();

    xml.flush();
    buf.close();

    return data.toQByteArray().toStdString();
}

ProjectMeta Au3Metadata::parseXml(const std::string& xml) const
{
    ByteArray data = ByteArray::fromQByteArray(QByteArray(xml));
    XmlStreamReader r(data);

    au::project::ProjectMeta loaded = project::ProjectMeta();

    for (size_t i = 0; i < kStdTags.size(); ++i) {
        loaded.*(project::kStdMembers[i]) = std::string();
    }
    loaded.additionalTags.clear();

    if (!r.readNextStartElement()) {
        interactive()->errorSync(
            muse::trc("metadata", "Error loading template"),
            muse::trc("metadata", "Unable to load metadata template from given file.")
            );
        return {};
    }

    if (r.name() != muse::AsciiStringView("tags")) {
        r.raiseError(u"Root element <tags> expected");
    }

    while (!r.isError() && r.readNextStartElement()) {
        if (r.name() == muse::AsciiStringView("tag")) {
            const muse::String nameAttr = r.attribute("name");
            const muse::String valueAttr = r.attribute("value");

            const std::string name = nameAttr.toStdString();
            const std::string val  = valueAttr.toStdString();

            bool assigned = false;
            for (size_t i = 0; i < kStdTags.size(); ++i) {
                if (name == kStdTags[i]) {
                    loaded.*(project::kStdMembers[i]) = val;
                    assigned = true;
                    break;
                }
            }

            if (!assigned && !name.empty()) {
                loaded.additionalTags.insert(QString::fromStdString(name), QString::fromStdString(val));
            }

            r.skipCurrentElement();
        } else {
            r.skipCurrentElement();
        }
    }

    if (r.isError()) {
        interactive()->errorSync(
            muse::trc("metadata", "Error loading template"),
            muse::trc("metadata", "Unable to load metadata template from given file.")
            );
        return {};
    }

    return loaded;
}
