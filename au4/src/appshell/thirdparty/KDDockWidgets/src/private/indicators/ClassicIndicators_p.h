/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_INDICATORS_CLASSICINDICATORS_P_H
#define KD_INDICATORS_CLASSICINDICATORS_P_H

#include "../DropIndicatorOverlayInterface_p.h"

namespace KDDockWidgets {

class IndicatorWindow;
class Indicator;

class DOCKS_EXPORT ClassicIndicators : public DropIndicatorOverlayInterface
{
    Q_OBJECT

    Q_PROPERTY(bool innerIndicatorsVisible READ innerIndicatorsVisible NOTIFY innerIndicatorsVisibleChanged)
    Q_PROPERTY(bool outterIndicatorsVisible READ outterIndicatorsVisible NOTIFY outterIndicatorsVisibleChanged)
    Q_PROPERTY(bool tabIndicatorVisible READ tabIndicatorVisible NOTIFY tabIndicatorVisibleChanged)

public:
    explicit ClassicIndicators(DropArea *dropArea);
    ~ClassicIndicators() override;
    DropLocation hover_impl(QPoint globalPos) override;
    QPoint posForIndicator(DropLocation) const override;

    bool innerIndicatorsVisible() const;
    bool outterIndicatorsVisible() const;

    // The tab/center indicator
    bool tabIndicatorVisible() const;

protected:
    bool onResize(QSize newSize) override;
    void updateVisibility() override;
Q_SIGNALS:
    void innerIndicatorsVisibleChanged();
    void outterIndicatorsVisibleChanged();
    void tabIndicatorVisibleChanged();

private:
    friend class KDDockWidgets::Indicator;
    friend class KDDockWidgets::IndicatorWindow;
    bool rubberBandIsTopLevel() const;
    void updateIndicatorsVisibility(bool visible);
    void raiseIndicators();
    QRect geometryForRubberband(QRect localRect) const;
    void setDropLocation(DropLocation);
    void updateWindowPosition();

    bool m_innerIndicatorsVisible = false;
    bool m_outterIndicatorsVisible = false;
    bool m_tabIndicatorVisible = false;
    QWidgetOrQuick *const m_rubberBand;
    IndicatorWindow *const m_indicatorWindow;
};

}

#endif
