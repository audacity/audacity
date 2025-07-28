/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "equalizationbandsliders.h"

#include <QAbstractListModel>

#include <functional>

namespace au::effects {
class GraphicEq;

class GraphicEqBandsModel : public QAbstractListModel
{
    Q_OBJECT

public:
    GraphicEqBandsModel(QObject* parent, std::function<GraphicEq* ()> eqGetter);

    static constexpr auto NUM_BANDS = 31;
    void reload();

    Q_INVOKABLE void flatten();
    Q_INVOKABLE void invert();

private:
    static constexpr std::array<double, NUM_BANDS> kThirdOct =
    {
        20., 25., 31., 40., 50., 63., 80., 100., 125., 160., 200.,
        250., 315., 400., 500., 630., 800., 1000., 1250., 1600., 2000.,
        2500., 3150., 4000., 5000., 6300., 8000., 10000., 12500., 16000., 20000.,
    };

    enum RoleNames
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

    const std::function<GraphicEq* ()> m_getEq;
    EqualizationBandSliders mSliders;
    bool m_inited = false;
};
}
