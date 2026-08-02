/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#include "nativeapi.h"

#include <limits>

#include <QJSEngine>
#include <QStringList>

#include "extensions/native/bytebuffer.h"
#include "extensions/native/nativelibrary.h"
#include "framework/extensions/extensionbundle.h"
#include "log.h"

namespace au::extensions {
namespace {
constexpr auto EXTENSION_DISPATCH_SYMBOL = "extension_dispatch_v0";

QString platformName()
{
#if defined(Q_OS_MACOS)
    return QStringLiteral("macos");
#elif defined(Q_OS_WIN)
    return QStringLiteral("windows");
#elif defined(Q_OS_LINUX)
    return QStringLiteral("linux");
#else
    return {};
#endif
}

QString architectureName()
{
#if defined(Q_PROCESSOR_ARM_64)
    return QStringLiteral("arm64");
#elif defined(Q_PROCESSOR_X86_64)
    return QStringLiteral("x86_64");
#else
    return {};
#endif
}

QString librarySuffix()
{
#if defined(Q_OS_MACOS)
    return QStringLiteral(".dylib");
#elif defined(Q_OS_WIN)
    return QStringLiteral(".dll");
#elif defined(Q_OS_LINUX)
    return QStringLiteral(".so");
#else
    return {};
#endif
}

QString resolveLibrary(muse::api::IApiEngine* engine, const QString& name)
{
    const muse::io::path_t bundlePath = engine ? engine->apiContext().bundlePath : muse::io::path_t{};
    if (bundlePath.empty() || name.isEmpty()) {
        return {};
    }

    const QString platform = platformName();
    const QString architecture = architectureName();
    const QString suffix = librarySuffix();
    if (platform.isEmpty() || architecture.isEmpty() || suffix.isEmpty()) {
        return {};
    }

    QStringList architectures{ architecture };
#if defined(Q_OS_MACOS)
    architectures.push_back(QStringLiteral("universal"));
#endif
    for (const QString& candidateArchitecture : architectures) {
        const QString relative = QStringLiteral("platform/%1/%2/%3%4").arg(platform, candidateArchitecture, name, suffix);
        if (const auto path = muse::extensions::resolveBundleFile(bundlePath, muse::io::path_t(relative))) {
            return path->toQString();
        }
    }
    return {};
}
} // namespace

NativeApi::NativeApi(muse::api::IApiEngine* engine)
    : ApiObject(engine)
{
}

QObject* NativeApi::open(const QString& name)
{
    const QString path = resolveLibrary(engine(), name);
    if (path.isEmpty()) {
        return nullptr;
    }
    auto* library = new muse::extensions::NativeLibrary(path, EXTENSION_DISPATCH_SYMBOL, nullptr);
    if (!library->isLoaded()) {
        LOGE() << "failed to load native extension library: " << path << ", error: " << library->errorString();
        delete library;
        return nullptr;
    }
    QJSEngine::setObjectOwnership(library, QJSEngine::JavaScriptOwnership);
    return library;
}

QObject* NativeApi::createBuffer(qulonglong byteLength)
{
    if (byteLength > static_cast<qulonglong>(std::numeric_limits<qsizetype>::max())) {
        return nullptr;
    }
    auto* buffer = new muse::extensions::ByteBuffer(static_cast<size_t>(byteLength));
    QJSEngine::setObjectOwnership(buffer, QJSEngine::JavaScriptOwnership);
    return buffer;
}
} // namespace au::extensions
