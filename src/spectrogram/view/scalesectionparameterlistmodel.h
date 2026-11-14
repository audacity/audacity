/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractsectionparameterslistmodel.h"
#include "ispectrogramservice.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QQmlParserStatus>

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel;

class ScaleSectionParameterListModel : public AbstractSectionParametersListModel, public QQmlParserStatus, public muse::Contextable,
    public muse::async::Asyncable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

protected:
    muse::ContextInject<ISpectrogramService> spectrogramService{ this };

public:
    explicit ScaleSectionParameterListModel(QObject* parent = nullptr);
    ~ScaleSectionParameterListModel() override = default;

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    int trackId() const { return m_trackId; }
    void setTrackId(int);

signals:
    void trackIdChanged();

private:
    enum Control {
        MaxFreq = 0, // Max freq first to be consistent with spectrogram UI
        MinFreq,
        _count
    };

    int rowCount(const QModelIndex&) const override { return static_cast<int>(Control::_count); }
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;

    void onSettingsModelSet(AbstractSpectrogramSettingsModel& model) override;

    void classBegin() override {}
    void componentComplete() override;

    enum Roles {
        ControlLabelRole = Qt::UserRole + 1,
        ShortControlLabelRole,
        ControlUnitsRole,
        ControlMinValueRole,
        ControlMaxValueRole,
        ControlCurrentValueRole,
    };

    QString controlLabel(Control) const;
    QString shortControlLabel(Control) const;
    int controlMinValue(Control) const;
    int controlMaxValue(Control) const;
    int controlCurrentValue(Control) const;

    int m_trackId = -1;
};
}
