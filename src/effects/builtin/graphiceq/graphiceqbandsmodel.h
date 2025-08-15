/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "equalizationbandsliders.h"

#include <QAbstractListModel>

namespace au::effects {
class GraphicEq;

class GraphicEqBandsModel : public QAbstractListModel
{
    Q_OBJECT

public:
    GraphicEqBandsModel(QObject* parent, GraphicEq& eq);

    static constexpr auto NUM_BANDS = 31;
    void reload();

    Q_INVOKABLE void flatten();
    Q_INVOKABLE void invert();

private:
    enum EqDataRoles
    {
        rDbGain = Qt::UserRole + 1,
        rCenterFreq,
    };

    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;

    void doReload();
    void updateGraphic();

    GraphicEq& m_eq;
    EqualizationBandSliders mSliders;
    bool m_inited = false;
};
}
