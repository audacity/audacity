/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include <QObject>
#include <QString>
#include <QStringList>
#include <QVariantList>
#include <QVariantMap>

#include "framework/global/modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../exportcommands.h"
#include "../iexportconfiguration.h"

namespace au::importexport {
class ExportSimpleStepModel : public QObject, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(QString title READ title CONSTANT FINAL)
    Q_PROPERTY(QVariantList presetOptions READ presetOptions CONSTANT FINAL)
    Q_PROPERTY(QString preset READ preset NOTIFY presetChanged FINAL)
    Q_PROPERTY(QString presetResult READ presetResult NOTIFY presetChanged FINAL)
    Q_PROPERTY(QString fileName READ fileName WRITE setFileName NOTIFY fileNameChanged FINAL)
    Q_PROPERTY(QString folder READ folder WRITE setFolder NOTIFY folderChanged FINAL)
    Q_PROPERTY(QString defaultFolder READ defaultFolder CONSTANT FINAL)
    Q_PROPERTY(QString preview READ preview NOTIFY outputChanged FINAL)
    Q_PROPERTY(QString fileNameWarning READ fileNameWarning NOTIFY fileNameChanged FINAL)
    Q_PROPERTY(QStringList fileFilter READ fileFilter NOTIFY outputChanged FINAL)

    muse::GlobalInject<IExportConfiguration> exportConfiguration;
    muse::ContextInject<context::IGlobalContext> globalContext{ this };

public:
    explicit ExportSimpleStepModel(QObject* parent = nullptr);

    QString title() const;
    QVariantList presetOptions() const;
    QString preset() const;
    QString presetResult() const;
    QString fileName() const;
    void setFileName(const QString& fileName);
    QString folder() const;
    void setFolder(const QString& folder);
    QString defaultFolder() const;
    QString preview() const;
    QString fileNameWarning() const;
    QStringList fileFilter() const;

    Q_INVOKABLE void load(const QVariantMap& data);
    Q_INVOKABLE void setPreset(const QString& preset);
    Q_INVOKABLE void setPickedFile(const QString& path);
    Q_INVOKABLE QVariantMap stepData() const;

signals:
    void presetChanged();
    void fileNameChanged();
    void folderChanged();
    void outputChanged();

private:
    ExportPreset m_preset = ExportPresetContract::def;
    std::string m_folder;
    std::string m_fileName;
};
}
