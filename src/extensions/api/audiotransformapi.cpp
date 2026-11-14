/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#include "audiotransformapi.h"

#include <cstdint>
#include <utility>
#include <vector>

#include <QJSEngine>
#include <QPointer>

#include "extensions/native/nativeextension.h"
#include "extensions/native/nativelibrary.h"
#include "trackedit/api/audiotransform.h"

namespace au::extensions {
namespace {
constexpr quint32 MAX_CHANNEL_COUNT = 2;

class NativeAudioTransform final : public trackedit::api::AudioTransform
{
public:
    NativeAudioTransform(muse::extensions::NativeLibrary& library, QByteArray operation, std::vector<void*> contexts)
        : m_library(&library), m_operation(std::move(operation)), m_contexts(std::move(contexts))
    {
    }

    bool apply(float* const channels[], uint32_t channelCount, uint64_t sampleCount) override
    {
        if (!m_library || !channels || (!m_contexts.empty() && m_contexts.size() != channelCount)) {
            return false;
        }
        for (uint32_t channel = 0; channel < channelCount; ++channel) {
            ext_value arguments[2]{};
            if (!m_contexts.empty()) {
                arguments[0].type = EXT_VALUE_OBJECT;
                arguments[0].as_object = m_contexts[channel];
            }
            arguments[1].type = EXT_VALUE_BUFFER;
            arguments[1].as_buffer.data = channels[channel];
            arguments[1].as_buffer.size = sampleCount * sizeof(float);
            ext_value result{};
            if (m_library->dispatch(m_operation, arguments, 2, &result) != EXT_STATUS_OK) {
                return false;
            }
        }
        return true;
    }

private:
    QPointer<muse::extensions::NativeLibrary> m_library;
    QByteArray m_operation;
    std::vector<void*> m_contexts;
};
} // namespace

AudioTransformApi::AudioTransformApi(muse::api::IApiEngine* engine)
    : ApiObject(engine)
{
}

QObject* AudioTransformApi::create(QObject* declaredLibrary, const QString& operation, const QJSValue& declaredContexts)
{
    auto* library = qobject_cast<muse::extensions::NativeLibrary*>(declaredLibrary);
    if (!library || operation.isEmpty() || !declaredContexts.isArray()) {
        return nullptr;
    }
    const quint32 count = declaredContexts.property(QStringLiteral("length")).toUInt();
    if (count > MAX_CHANNEL_COUNT) {
        return nullptr;
    }
    std::vector<void*> contexts;
    contexts.reserve(count);
    for (quint32 index = 0; index < count; ++index) {
        auto* handle = qobject_cast<muse::extensions::NativeHandle*>(declaredContexts.property(index).toQObject());
        if (!handle || handle->library() != library) {
            return nullptr;
        }
        contexts.push_back(handle->value());
    }
    auto* transform = new NativeAudioTransform(*library, operation.toUtf8(), std::move(contexts));
    QJSEngine::setObjectOwnership(transform, QJSEngine::JavaScriptOwnership);
    return transform;
}
} // namespace au::extensions
