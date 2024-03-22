/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_NULL_INDICATORS_P_H
#define KD_NULL_INDICATORS_P_H

#include "../DropIndicatorOverlayInterface_p.h"

namespace KDDockWidgets {

/**
 * @brief A dummy DropIndicatorOverlayInterface implementation which doesn't do anything.
 *
 * Used for debugging purposes or if someone doesn't want the drop indicators.
 */
class DOCKS_EXPORT NullIndicators : public DropIndicatorOverlayInterface
{
    Q_OBJECT
public:
    explicit NullIndicators(DropArea *);
    ~NullIndicators() override;
    DropIndicatorOverlayInterface::DropLocation hover_impl(QPoint) override
    {
        return {};
    };

    DropLocation dropLocationForPos(QPoint) const
    {
        return {};
    }

protected:
    QPoint posForIndicator(DropLocation) const override
    {
        return {};
    }
};

}

#endif
