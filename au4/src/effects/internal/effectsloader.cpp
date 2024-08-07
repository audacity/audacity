/*
* Audacity: A Digital Audio Editor
*/
#include "effectsloader.h"

#include "global/io/dir.h"
#include "global/io/file.h"
#include "global/io/fileinfo.h"
#include "global/serialization/json.h"

#include "log.h"

using namespace au::effects;
using namespace muse;

const std::string MANIFEST("manifest.json");

ManifestList EffectsLoader::loadManifestList(const io::path_t& path) const
{
    TRACEFUNC;

    LOGD() << "try load extensions: " << path;

    ManifestList defaultManifests = manifestList(path);

    ManifestList retList;
    for (const Manifest& m : defaultManifests) {
        if (!m.isValid()) {
            continue;
        }

        retList.push_back(m);
    }

    return retList;
}

ManifestList EffectsLoader::manifestList(const io::path_t& rootPath) const
{
    ManifestList manifests;
    io::paths_t paths = manifestPaths(rootPath);
    for (const io::path_t& path : paths) {
        Manifest manifest = parseManifest(path);
        resolvePaths(manifest, muse::io::FileInfo(path).dirPath());
        manifests.push_back(manifest);
    }

    return manifests;
}

io::paths_t EffectsLoader::manifestPaths(const io::path_t& rootPath) const
{
    RetVal<io::paths_t> paths = io::Dir::scanFiles(rootPath, { MANIFEST });
    if (!paths.ret) {
        LOGE() << "failed scan files, err: " << paths.ret.toString();
    }
    return paths.val;
}

Manifest EffectsLoader::parseManifest(const io::path_t& path) const
{
    ByteArray data;
    Ret ret = io::File::readFile(path, data);
    if (!ret) {
        LOGE() << "failed read file: " << path << ", err: " << ret.toString();
        return Manifest();
    }

    return parseManifest(data);
}

Manifest EffectsLoader::parseManifest(const ByteArray& data) const
{
    std::string jsonErr;
    JsonObject obj = JsonDocument::fromJson(data, &jsonErr).rootObject();
    if (!jsonErr.empty()) {
        LOGE() << "failed parse json, err: " << jsonErr;
        return Manifest();
    }

    Manifest m;
    m.id = obj.value("id").toString();
    m.url = obj.value("url").toStdString();
    m.title = obj.value("title").toString();
    m.description = obj.value("description").toString();
    m.version = obj.value("version").toString();
    m.vendor = obj.value("vendor").toString();

    return m;
}

void EffectsLoader::resolvePaths(Manifest& manifest, const muse::io::path_t& rootDirPath) const
{
    manifest.url = rootDirPath + "/" + manifest.url;
}
