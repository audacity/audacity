/*
 * Audacity: A Digital Audio Editor
 */
#include "audiocodecapi.h"

#include <QDir>
#include <QJSEngine>
#include <QTemporaryFile>

namespace au::extensions {
namespace {
QJSValue throwError(QObject* object, const QString& message)
{
    if (auto* engine = qjsEngine(object)) {
        engine->throwError(message);
    }
    return {};
}
}

AudioCodecApi::AudioCodecApi(muse::api::IApiEngine* engine)
    : ApiObject(engine)
{
}

QJSValue AudioCodecApi::decode(const QByteArray& data, const QJSValue& options)
{
    const QJSValue formatValue = options.property(QStringLiteral("format"));
    const QString format = formatValue.isString() ? formatValue.toString() : QStringLiteral("audio");
    const QJSValue rateValue = options.property(QStringLiteral("sampleRate"));
    const std::optional<double> sampleRate = rateValue.isNumber() ? std::make_optional(rateValue.toNumber()) : std::nullopt;

    // The importer tries the format the file extension suggests first
    QTemporaryFile file(QDir::tempPath() + QStringLiteral("/audacity-decode-XXXXXX.") + format);
    if (!file.open() || file.write(data) != data.size() || !file.flush()) {
        return throwError(this, QStringLiteral("decode: could not write a temporary file"));
    }
    file.close();

    const auto decoded = importer()->decode(muse::io::path_t(file.fileName()), sampleRate);
    if (!decoded.ret) {
        return throwError(this, QStringLiteral("decode: ") + QString::fromStdString(decoded.ret.text()));
    }

    const auto& audio = decoded.val;
    QJSValue channels = engine()->newArray(audio.channels.size());
    for (size_t channel = 0; channel < audio.channels.size(); ++channel) {
        const auto& samples = audio.channels[channel];
        channels.setProperty(static_cast<quint32>(channel),
                             engine()->newArrayBuffer(QByteArray(reinterpret_cast<const char*>(samples.data()),
                                                                 static_cast<qsizetype>(samples.size() * sizeof(float)))));
    }
    QJSValue result = engine()->newObject();
    result.setProperty(QStringLiteral("sampleRate"), audio.sampleRate);
    result.setProperty(QStringLiteral("sampleCount"), static_cast<double>(audio.channels.empty() ? 0 : audio.channels.front().size()));
    result.setProperty(QStringLiteral("channels"), channels);
    return result;
}
} // namespace au::extensions
