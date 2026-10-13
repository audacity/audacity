/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QByteArray>
#include <QJSValue>

#include "framework/global/api/apiobject.h"
#include "framework/global/modularity/ioc.h"

#include "importexport/import/iimporter.h"

namespace au::extensions {
//! `require("Audacity.AudioCodec")`: audio files to and from samples, e.g. for
//! audio exchanged with a web service
class AudioCodecApi final : public muse::api::ApiObject
{
    Q_OBJECT

    muse::ContextInject<importexport::IImporter> importer = { this };

public:
    explicit AudioCodecApi(muse::api::IApiEngine* engine);

    //! `data`: the bytes of an audio file, in any format Audacity imports.
    //! `options` (optional): `format`, the file extension (e.g. "m4a"), as a
    //! hint, and `sampleRate`, to resample to.
    //! Returns `{ sampleRate, sampleCount, channels }`, `channels` holding one
    //! ArrayBuffer of float32 samples per channel.
    Q_INVOKABLE QJSValue decode(const QByteArray& data, const QJSValue& options = QJSValue());
};
} // namespace au::extensions
