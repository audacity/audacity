/*
 * Audacity: A Digital Audio Editor
 */
#include "nyquistpromptviewmodel.h"
#include "nyquistprompteffect.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/log.h"
#include "framework/global/translation.h"

#include "effects/nyquist/internal/nyquistparameterextractorservice.h"

#include <QDir>

using namespace au::effects;
using namespace muse;

NyquistPromptViewModel::NyquistPromptViewModel(QObject* parent, int instanceId)
    : BuiltinEffectModel(parent, instanceId)
{
}

QString NyquistPromptViewModel::commandText() const
{
    return m_commandText;
}

void NyquistPromptViewModel::setCommandText(const QString& text)
{
    if (m_commandText == text) {
        return;
    }

    effect<NyquistPromptEffect>().commandText() = au3::wxFromString(text);

    m_commandText = text;
    emit commandTextChanged();
}

QString NyquistPromptViewModel::title() const
{
    return muse::qtrc("effects", "Nyquist prompt");
}

void NyquistPromptViewModel::loadScript()
{
    const std::vector<std::string> filter {
        muse::trc("effects", "Nyquist scripts") + " (*.ny)",
        muse::trc("effects", "Lisp scripts") + " (*.lsp)",
        muse::trc("effects", "All files") + " (*)"
    };

    const io::path_t dir = m_lastFilePath.isEmpty()
                           ? io::path_t(QDir::homePath().toStdString())
                           : io::dirpath(io::path_t(m_lastFilePath.toStdString()));

    const io::path_t filePath = interactive()->selectOpeningFileSync(
        muse::trc("effects", "Load Nyquist script"),
        dir,
        filter
        );

    if (filePath.empty()) {
        return;
    }

    // Load the file (overriding current text)
    Ret ret = fileSystem()->exists(filePath);
    if (!ret) {
        LOGE() << "File does not exist: " << filePath;
        return;
    }

    ByteArray data;
    ret = fileSystem()->readFile(filePath, data);
    if (!ret) {
        LOGE() << "Failed to read file: " << filePath;
        return;
    }

    m_lastFilePath = QString::fromStdString(filePath.toStdString());

    // Find the actual end of the text (before any null bytes)
    size_t actualSize = data.size();
    const char* dataPtr = data.constChar();
    for (size_t i = 0; i < data.size(); ++i) {
        if (dataPtr[i] == '\0') {
            actualSize = i;
            break;
        }
    }

    // Convert only the actual text content, excluding null padding
    const QString loadedText = QString::fromUtf8(dataPtr, static_cast<int>(actualSize));
    setCommandText(loadedText);
}

void NyquistPromptViewModel::saveScript()
{
    const std::vector<std::string> filter {
        muse::trc("effects", "Nyquist scripts") + " (*.ny)",
        muse::trc("effects", "Lisp scripts") + " (*.lsp)",
        muse::trc("effects", "All files") + " (*)"
    };

    const io::path_t defaultPath = m_lastFilePath.isEmpty()
                                   ? io::path_t(QDir::homePath().toStdString())
                                   : io::path_t(m_lastFilePath.toStdString());

    const io::path_t filePath = interactive()->selectSavingFileSync(
        muse::trc("effects", "Save Nyquist script"),
        defaultPath,
        filter
        );

    if (filePath.empty()) {
        return;
    }

    // Save the file
    const QByteArray qdata = m_commandText.toUtf8();
    const ByteArray data = ByteArray::fromQByteArrayNoCopy(qdata);

    const Ret ret = fileSystem()->writeFile(filePath, data);
    if (!ret) {
        LOGE() << "Failed to write file: " << filePath;
        return;
    }

    m_lastFilePath = QString::fromStdString(filePath.toStdString());
}

void NyquistPromptViewModel::debugEffect()
{
    // Get the Nyquist parameter extractor
    NyquistParameterExtractorService* extractor = getExtractor();
    if (!extractor) {
        return;
    }

    EffectInstance* instance = instancesRegister()->instanceById(m_instanceId).get();
    if (!instance) {
        return;
    }

    // Get the settings access
    EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(m_instanceId);
    if (!settingsAccess) {
        return;
    }

    // Execute the Nyquist code for debugging (without starting audio playback)
    String debugOutput;
    settingsAccess->ModifySettings([extractor, instance, &debugOutput](EffectSettings& settings) {
        // Execute the effect and capture debug output
        debugOutput = extractor->executeForDebug(instance, settings);
        return nullptr;
    });

    // Show the debug output in a dialog
    if (!debugOutput.isEmpty()) {
        interactive()->info(
            muse::trc("effects", "Nyquist debug output"),
            debugOutput.toStdString()
            );
    } else {
        interactive()->info(
            muse::trc("effects", "Nyquist debug output"),
            muse::mtrc("effects", "No debug output generated.").toStdString()
            );
    }
}

void NyquistPromptViewModel::doReload()
{
    // Load the current command text from the effect instance
    m_commandText = QString::fromStdString(au3::wxToStdString(effect<NyquistPromptEffect>().commandText()));
    emit commandTextChanged();
}

NyquistParameterExtractorService* NyquistPromptViewModel::getExtractor() const
{
    // Get the Nyquist parameter extractor
    // We know this is a Nyquist effect, so we can safely cast
    return dynamic_cast<NyquistParameterExtractorService*>(
        parameterExtractorRegistry()
        ? parameterExtractorRegistry()->extractorForFamily(EffectFamily::Nyquist)
        : nullptr
        );
}
