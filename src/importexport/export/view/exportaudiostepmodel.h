/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include <QAbstractListModel>
#include <QStringList>
#include <QVariantList>
#include <QVariantMap>

#include "framework/global/modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../exportcommands.h"
#include "../iexportconfiguration.h"
#include "../iexporter.h"
#include "../types/exportformatsettings.h"

namespace au::importexport {
class ExportAudioStepModel : public QAbstractListModel, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title CONSTANT FINAL)
    Q_PROPERTY(QString fileName READ fileName WRITE setFileName NOTIFY fileNameChanged FINAL)
    Q_PROPERTY(QString folder READ folder WRITE setFolder NOTIFY folderChanged FINAL)
    Q_PROPERTY(QString defaultFolder READ defaultFolder CONSTANT FINAL)
    Q_PROPERTY(QString preview READ preview NOTIFY outputChanged FINAL)
    Q_PROPERTY(QString fileNameWarning READ fileNameWarning NOTIFY fileNameChanged FINAL)

    muse::GlobalInject<IExportConfiguration> exportConfiguration;
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IExporter> exporter{ this };

public:
    enum class Section {
        Format,
        Audio
    };
    Q_ENUM(Section)

    enum Roles {
        TitleRole = Qt::UserRole + 1,
        SectionRole,
        OptionsRole,
        ValueRole
    };

    explicit ExportAudioStepModel(QObject* parent = nullptr);

    QString title() const;
    QString fileName() const;
    void setFileName(const QString& fileName);
    QString folder() const;
    void setFolder(const QString& folder);
    QString defaultFolder() const;
    QString preview() const;
    QString fileNameWarning() const;

    Q_INVOKABLE void load(const QVariantMap& data);
    Q_INVOKABLE void setValue(int row, const QVariant& value);
    Q_INVOKABLE QVariantMap stepData() const;

    int rowCount(const QModelIndex& parent = {}) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

signals:
    void fileNameChanged();
    void folderChanged();
    void outputChanged();

private:
    enum class Field {
        Format,
        Mp3Mode,
        WavEncoding,
        Mp3Value,
        FlacBitDepth,
        FlacLevel,
        OggQuality,
        SampleRate,
        Channels
    };

    struct Row {
        Field field = Field::Format;
        Section section = Section::Format;
        QString title;
        QVariantList options;
        QString value;
    };

    std::vector<Row> buildRows() const;
    std::vector<Row> formatRows() const;
    Row sampleRateRow() const;
    void apply(Field field, const std::string& value);
    void resetUnsupportedMp3SampleRate();
    void rebuild();

    ExportFormatSettings m_settings = WavSettings {};
    std::string m_sampleRate;
    ExportChannelLayout m_channels = ExportChannelLayoutContract::def;
    std::string m_folder;
    std::string m_fileName;
    std::vector<Row> m_rows;
};
}
