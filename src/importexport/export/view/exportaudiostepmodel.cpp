/*
* Audacity: A Digital Audio Editor
*/
#include "exportaudiostepmodel.h"

#include <algorithm>
#include <array>
#include <ranges>
#include <string>
#include <type_traits>
#include <utility>

#include "framework/global/translation.h"
#include "framework/global/types/val.h"

#include "../internal/exportcommandoptions.h"
#include "../internal/exportstephandlers.h"
#include "exportstepoutput.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
template<class ... Ts>
struct Overloaded : Ts ... {
    using Ts::operator() ...;
};

struct Choice {
    std::string_view key;
    const char* text;
};

constexpr std::array<Choice, 4> FORMATS { {
    { "wav", QT_TRANSLATE_NOOP("export", "WAV") },
    { "mp3", QT_TRANSLATE_NOOP("export", "MP3") },
    { "flac", QT_TRANSLATE_NOOP("export", "FLAC") },
    { "ogg", QT_TRANSLATE_NOOP("export", "Ogg Vorbis") },
} };

constexpr std::array<Choice, 4> MP3_MODES { {
    { "preset", QT_TRANSLATE_NOOP("export", "Preset") },
    { "vbr", QT_TRANSLATE_NOOP("export", "Variable") },
    { "abr", QT_TRANSLATE_NOOP("export", "Average") },
    { "cbr", QT_TRANSLATE_NOOP("export", "Constant") },
} };

QString toQString(std::string_view text)
{
    return QString::fromUtf8(text.data(), static_cast<qsizetype>(text.size()));
}

QVariantMap option(const QString& text, std::string_view value)
{
    return { { "text", text }, { "value", toQString(value) } };
}

template<std::size_t N>
QVariantList choiceOptions(const std::array<Choice, N>& choices)
{
    QVariantList list;
    for (const Choice& choice : choices) {
        list << option(qtrc("export", choice.text), choice.key);
    }
    return list;
}

template<typename C>
QVariantList contractOptions()
{
    QVariantList list;
    for (const auto& entry : C::table) {
        list << option(qtrc("export", std::string(entry.description).c_str()), entry.sval);
    }
    return list;
}

template<typename E>
bool isSupportedRate(E rate, const std::optional<std::vector<int> >& supported)
{
    return rate == E::ProjectRate || !supported || std::ranges::find(*supported, static_cast<int>(rate)) != supported->end();
}

template<typename C>
QVariantList sampleRateOptions(const std::optional<std::vector<int> >& supported = std::nullopt)
{
    QVariantList list;
    for (const auto& entry : C::table) {
        if (!isSupportedRate(entry.eval, supported)) {
            continue;
        }

        const QString text = entry.eval == decltype(entry.eval)::ProjectRate
                             ? qtrc("export", "Project rate")
                             : qtrc("export", "%1 Hz").arg(static_cast<int>(entry.eval));
        list << option(text, entry.sval);
    }
    return list;
}

template<typename C>
QVariantList exporterOptions(const IExporter& exporter)
{
    QVariantList list;
    for (const auto& entry : C::table) {
        const std::string label = exporter.optionLabel(entry.eval).value_or(std::string(entry.sval));
        list << option(QString::fromStdString(label), entry.sval);
    }
    return list;
}

template<typename C>
std::string supportedOrDefault(const std::string& value)
{
    using E = std::remove_cv_t<decltype(C::def)>;
    return EnumContract<E>::enum_val(value) ? value : EnumContract<E>::str_val(C::def);
}

template<typename E>
QString keyOf(E value)
{
    return QString::fromStdString(EnumContract<E>::str_val(value));
}

std::string_view mp3ModeOf(const ExportFormatSettings& settings)
{
    return std::visit(Overloaded {
            [](const Mp3VbrSettings&) { return std::string_view("vbr"); },
            [](const Mp3AbrSettings&) { return std::string_view("abr"); },
            [](const Mp3CbrSettings&) { return std::string_view("cbr"); },
            [](const auto&) { return std::string_view("preset"); },
        }, settings);
}

ExportFormatSettings formatDefaults(std::string_view format)
{
    if (format == "mp3") {
        return mp3Settings(Mp3QualityContract::def);
    }
    if (format == "flac") {
        return FlacSettings {};
    }
    if (format == "ogg") {
        return OggSettings {};
    }
    return WavSettings {};
}

ExportFormatSettings mp3ModeDefaults(std::string_view mode)
{
    if (mode == "vbr") {
        return Mp3VbrSettings {};
    }
    if (mode == "abr") {
        return Mp3AbrSettings {};
    }
    if (mode == "cbr") {
        return Mp3CbrSettings {};
    }
    return Mp3PresetSettings {};
}

template<typename E, typename Apply>
void applyEnum(const std::string& value, Apply apply)
{
    if (const std::optional<E> parsed = EnumContract<E>::enum_val(value)) {
        apply(*parsed);
    }
}

bool isMp3(const ExportFormatSettings& settings)
{
    return mp3QualityOf(settings).has_value();
}
}

ExportAudioStepModel::ExportAudioStepModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QString ExportAudioStepModel::title() const
{
    return EXPORT_AUDIO_STEP_TITLE.qTranslated();
}

QString ExportAudioStepModel::fileName() const
{
    return QString::fromStdString(m_fileName);
}

void ExportAudioStepModel::setFileName(const QString& fileName)
{
    const std::string value = fileName.toStdString();
    if (m_fileName == value) {
        return;
    }

    m_fileName = value;
    emit fileNameChanged();
    emit outputChanged();
}

QString ExportAudioStepModel::folder() const
{
    return QString::fromStdString(m_folder);
}

void ExportAudioStepModel::setFolder(const QString& folder)
{
    const std::string value = folder.toStdString();
    if (m_folder == value) {
        return;
    }

    m_folder = value;
    emit folderChanged();
    emit outputChanged();
}

QString ExportAudioStepModel::defaultFolder() const
{
    return stepDefaultFolder(exportConfiguration()->directoryPath());
}

QString ExportAudioStepModel::preview() const
{
    return stepOutputPreview(exportConfiguration()->directoryPath(), globalContext()->currentProject(), m_folder, m_fileName,
                             extensionOf(m_settings));
}

QString ExportAudioStepModel::fileNameWarning() const
{
    return stepFileNameWarning(m_fileName);
}

void ExportAudioStepModel::load(const QVariantMap& data)
{
    const RetVal<ExportFormatCommand> command = formatCommandFromVal(stepDataFromMap(data));
    const ExportFormatCommand loaded = command.ret ? command.val : ExportFormatCommand(ExportWavCommand {});
    m_settings = formatSettings(loaded);
    std::visit([this](const auto& c) {
        m_sampleRate = EnumContract<std::decay_t<decltype(c.sampleRate)> >::str_val(c.sampleRate);
        m_channels = c.channels;
        m_folder = c.folder;
        m_fileName = c.fileName;
    }, loaded);

    beginResetModel();
    m_rows = buildRows();
    endResetModel();

    emit fileNameChanged();
    emit folderChanged();
    emit outputChanged();
}

void ExportAudioStepModel::setValue(int row, const QVariant& value)
{
    if (row < 0 || row >= rowCount()) {
        return;
    }

    apply(m_rows.at(row).field, value.toString().toStdString());
    rebuild();
}

QVariantMap ExportAudioStepModel::stepData() const
{
    return stepDataToMap(toVal(formatCommand(m_settings, m_sampleRate, m_channels, m_folder, m_fileName)));
}

int ExportAudioStepModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_rows.size());
}

QVariant ExportAudioStepModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return {};
    }

    const Row& row = m_rows.at(index.row());
    switch (role) {
    case TitleRole: return row.title;
    case SectionRole: return QVariant::fromValue(row.section);
    case OptionsRole: return row.options;
    case ValueRole: return row.value;
    default: return {};
    }
}

QHash<int, QByteArray> ExportAudioStepModel::roleNames() const
{
    return {
        { TitleRole, "title" },
        { SectionRole, "section" },
        { OptionsRole, "options" },
        { ValueRole, "value" },
    };
}

std::vector<ExportAudioStepModel::Row> ExportAudioStepModel::buildRows() const
{
    std::vector<Row> rows {
        { Field::Format, Section::Format, qtrc("export", "Format"), choiceOptions(FORMATS), toQString(extensionOf(m_settings)) },
    };
    std::ranges::move(formatRows(), std::back_inserter(rows));
    rows.push_back({ Field::Channels, Section::Audio, qtrc("export", "Channels"), contractOptions<ExportChannelLayoutContract>(),
                     keyOf(m_channels) });
    rows.push_back(sampleRateRow());
    return rows;
}

std::vector<ExportAudioStepModel::Row> ExportAudioStepModel::formatRows() const
{
    const Row mp3Mode { Field::Mp3Mode, Section::Format, qtrc("export", "Bit rate mode"), choiceOptions(MP3_MODES),
                        toQString(mp3ModeOf(m_settings)) };

    return std::visit(Overloaded {
        [this](const WavSettings& s) {
            return std::vector<Row> {
                { Field::WavEncoding, Section::Format, qtrc("export", "Encoding"), exporterOptions<WavEncodingContract>(*exporter()), keyOf(
                      s.encoding) },
            };
        },
        [this, &mp3Mode](const Mp3PresetSettings& s) {
            return std::vector<Row> {
                mp3Mode,
                { Field::Mp3Value, Section::Format, qtrc("export", "Quality"), exporterOptions<Mp3PresetContract>(*exporter()), keyOf(
                      s.preset) },
            };
        },
        [this, &mp3Mode](const Mp3VbrSettings& s) {
            return std::vector<Row> {
                mp3Mode,
                { Field::Mp3Value, Section::Format, qtrc("export", "Quality"), exporterOptions<Mp3VbrQualityContract>(*exporter()),
                  keyOf(s.quality) },
            };
        },
        [this, &mp3Mode](const Mp3AbrSettings& s) {
            return std::vector<Row> {
                mp3Mode,
                { Field::Mp3Value, Section::Format, qtrc("export", "Bit rate"), exporterOptions<Mp3BitRateContract>(*exporter()),
                  keyOf(s.bitRate) },
            };
        },
        [this, &mp3Mode](const Mp3CbrSettings& s) {
            return std::vector<Row> {
                mp3Mode,
                { Field::Mp3Value, Section::Format, qtrc("export", "Bit rate"), exporterOptions<Mp3BitRateContract>(*exporter()),
                  keyOf(s.bitRate) },
            };
        },
        [this](const FlacSettings& s) {
            return std::vector<Row> {
                { Field::FlacBitDepth, Section::Format, qtrc("export", "Bit depth"), exporterOptions<FlacBitDepthContract>(*exporter()),
                  keyOf(s.bitDepth) },
                { Field::FlacLevel, Section::Format, qtrc("export", "Level"), exporterOptions<FlacLevelContract>(*exporter()), keyOf(
                      s.level) },
            };
        },
        [this](const OggSettings& s) {
            return std::vector<Row> {
                { Field::OggQuality, Section::Format, qtrc("export", "Quality"), exporterOptions<OggQualityContract>(*exporter()),
                  keyOf(s.quality) },
            };
        },
    }, m_settings);
}

ExportAudioStepModel::Row ExportAudioStepModel::sampleRateRow() const
{
    const QVariantList options = isMp3(m_settings)
                                 ? sampleRateOptions<Mp3SampleRateContract>(exporter()->supportedSampleRates(m_settings))
                                 : sampleRateOptions<ExportSampleRateContract>();

    return { Field::SampleRate, Section::Audio, qtrc("export", "Sample rate"), options, QString::fromStdString(m_sampleRate) };
}

void ExportAudioStepModel::apply(Field field, const std::string& value)
{
    switch (field) {
    case Field::Format:
        m_settings = formatDefaults(value);
        m_sampleRate = isMp3(m_settings) ? supportedOrDefault<Mp3SampleRateContract>(m_sampleRate)
                       : supportedOrDefault<ExportSampleRateContract>(m_sampleRate);
        resetUnsupportedMp3SampleRate();
        break;
    case Field::Mp3Mode:
        m_settings = mp3ModeDefaults(value);
        resetUnsupportedMp3SampleRate();
        break;
    case Field::WavEncoding:
        applyEnum<WavEncoding>(value, [this](WavEncoding e) { m_settings = WavSettings { e }; });
        break;
    case Field::Mp3Value:
        std::visit(Overloaded {
            [&value](Mp3PresetSettings& s) { applyEnum<Mp3Preset>(value, [&s](Mp3Preset e) { s.preset = e; }); },
            [&value](Mp3VbrSettings& s) { applyEnum<Mp3VbrQuality>(value, [&s](Mp3VbrQuality e) { s.quality = e; }); },
            [&value](Mp3AbrSettings& s) { applyEnum<Mp3BitRate>(value, [&s](Mp3BitRate e) { s.bitRate = e; }); },
            [&value](Mp3CbrSettings& s) { applyEnum<Mp3BitRate>(value, [&s](Mp3BitRate e) { s.bitRate = e; }); },
            [](auto&) {},
        }, m_settings);
        resetUnsupportedMp3SampleRate();
        break;
    case Field::FlacBitDepth:
        if (auto* flac = std::get_if<FlacSettings>(&m_settings)) {
            applyEnum<FlacBitDepth>(value, [flac](FlacBitDepth e) { flac->bitDepth = e; });
        }
        break;
    case Field::FlacLevel:
        if (auto* flac = std::get_if<FlacSettings>(&m_settings)) {
            applyEnum<FlacLevel>(value, [flac](FlacLevel e) { flac->level = e; });
        }
        break;
    case Field::OggQuality:
        applyEnum<OggQuality>(value, [this](OggQuality e) { m_settings = OggSettings { e }; });
        break;
    case Field::SampleRate:
        m_sampleRate = value;
        break;
    case Field::Channels:
        applyEnum<ExportChannelLayout>(value, [this](ExportChannelLayout e) { m_channels = e; });
        break;
    }
}

void ExportAudioStepModel::resetUnsupportedMp3SampleRate()
{
    const std::optional<Mp3SampleRate> rate = EnumContract<Mp3SampleRate>::enum_val(m_sampleRate);
    if (!isMp3(m_settings) || !rate || isSupportedRate(*rate, exporter()->supportedSampleRates(m_settings))) {
        return;
    }

    m_sampleRate = EnumContract<Mp3SampleRate>::str_val(Mp3SampleRate::ProjectRate);
}

void ExportAudioStepModel::rebuild()
{
    std::vector<Row> rows = buildRows();
    const bool sameFields = std::ranges::equal(rows, m_rows, {}, &Row::field, &Row::field);

    if (!sameFields) {
        beginResetModel();
        m_rows = std::move(rows);
        endResetModel();
    } else {
        m_rows = std::move(rows);
        emit dataChanged(index(0), index(rowCount() - 1), { TitleRole, OptionsRole, ValueRole });
    }

    emit outputChanged();
}
