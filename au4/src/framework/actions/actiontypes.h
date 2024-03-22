/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_ACTIONS_ACTIONTYPES_H
#define MU_ACTIONS_ACTIONTYPES_H

#include <string>
#include <vector>
#include <memory>
#include <QString>

namespace mu::actions {
using ActionCode = std::string;
using ActionCodeList = std::vector<ActionCode>;

inline bool containsAction(const ActionCodeList& list, const ActionCode& actionCode)
{
    return std::find(list.cbegin(), list.cend(), actionCode) != list.cend();
}

inline static ActionCode codeFromQString(const QString& s)
{
    return s.toStdString();
}

class ActionData
{
public:

    template<typename T>
    static ActionData make_arg1(const T& val)
    {
        ActionData d;
        d.setArg<T>(0, val);
        return d;
    }

    template<typename T1, typename T2>
    static ActionData make_arg2(const T1& val1, const T2& val2)
    {
        ActionData d;
        d.setArg<T1>(0, val1);
        d.setArg<T2>(1, val2);
        return d;
    }

    template<typename T1, typename T2, typename T3>
    static ActionData make_arg3(const T1& val1, const T2& val2, const T3& val3)
    {
        ActionData d;
        d.setArg<T1>(0, val1);
        d.setArg<T2>(1, val2);
        d.setArg<T3>(2, val3);
        return d;
    }

    template<typename T>
    void setArg(int i, const T& val)
    {
        IArg* p = new Arg<T>(val);
        m_args.insert(m_args.begin() + i, std::shared_ptr<IArg>(p));
    }

    template<typename T>
    T arg(int i = 0) const
    {
        IArg* p = m_args.at(i).get();
        if (!p) {
            return T();
        }
        Arg<T>* d = reinterpret_cast<Arg<T>*>(p);
        return d->val;
    }

    int count() const
    {
        return int(m_args.size());
    }

    bool empty() const
    {
        return count() == 0;
    }

    struct IArg {
        virtual ~IArg() = default;
    };

    template<typename T>
    struct Arg : public IArg {
        T val;
        Arg(const T& v)
            : IArg(), val(v) {}
    };

private:
    std::vector<std::shared_ptr<IArg> > m_args;
};
}

#endif // MU_ACTIONS_ACTIONTYPES_H
