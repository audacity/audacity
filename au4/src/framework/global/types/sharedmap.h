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

#ifndef MU_GLOBAL_SHAREDMAP_H
#define MU_GLOBAL_SHAREDMAP_H

#include <memory>
#include <map>

namespace mu {
template<typename KeyType, typename ValType>
class SharedMap
{
public:
    using PairType = std::pair<KeyType, ValType>;
    using Data = std::map<KeyType, ValType>;
    using DataPtr = std::shared_ptr<Data>;
    typedef typename Data::iterator iterator;
    typedef typename Data::const_iterator const_iterator;
    typedef typename Data::reverse_iterator reverse_iterator;
    typedef typename Data::const_reverse_iterator const_reverse_iterator;

    SharedMap()
    {
        m_dataPtr = std::make_shared<Data>();
    }

    SharedMap(std::initializer_list<PairType> initList)
    {
        m_dataPtr = std::make_shared<Data>(initList);
    }

    SharedMap& operator=(std::initializer_list<PairType> initList)
    {
        m_dataPtr = std::make_shared<Data>(initList);
        return *this;
    }

    SharedMap(const SharedMap&) = default;
    SharedMap(SharedMap&&) = default;
    SharedMap& operator=(const SharedMap&) = default;
    SharedMap& operator=(SharedMap&&) = default;

    const ValType& at(const KeyType& key) const
    {
        return m_dataPtr->at(key);
    }

    ValType& at(const KeyType& key)
    {
        ensureDetach();
        return m_dataPtr->at(key);
    }

    ValType& operator[](const KeyType& key)
    {
        ensureDetach();
        return m_dataPtr->operator [](key);
    }

    ValType& operator[](KeyType&& key)
    {
        ensureDetach();
        return m_dataPtr->operator [](std::forward<KeyType>(key));
    }

    iterator begin() noexcept
    {
        ensureDetach();
        return m_dataPtr->begin();
    }

    iterator end() noexcept
    {
        ensureDetach();
        return m_dataPtr->end();
    }

    const_iterator begin() const noexcept
    {
        return m_dataPtr->cbegin();
    }

    const_iterator end() const noexcept
    {
        return m_dataPtr->cend();
    }

    const_iterator cbegin() const noexcept
    {
        return m_dataPtr->cbegin();
    }

    const_iterator cend() const noexcept
    {
        return m_dataPtr->cend();
    }

    const_reverse_iterator rbegin() const noexcept
    {
        return m_dataPtr->rbegin();
    }

    const_reverse_iterator rend() const noexcept
    {
        return m_dataPtr->rend();
    }

    const_iterator find(const KeyType& key) const noexcept
    {
        return m_dataPtr->find(key);
    }

    const_iterator lower_bound(const KeyType& key) const
    {
        return m_dataPtr->lower_bound(key);
    }

    const_iterator upper_bound(const KeyType& key) const
    {
        return m_dataPtr->upper_bound(key);
    }

    bool contains(const KeyType& key) const noexcept
    {
        return find(key) != end();
    }

    bool empty() const noexcept
    {
        return m_dataPtr->empty();
    }

    size_t size() const noexcept
    {
        return m_dataPtr->size();
    }

    void insert(const PairType& pair)
    {
        ensureDetach();
        m_dataPtr->insert(pair);
    }

    void insert(PairType&& pair)
    {
        ensureDetach();
        m_dataPtr->insert(std::forward<PairType>(pair));
    }

    void insert_or_assign(const KeyType& key, ValType&& val)
    {
        ensureDetach();
        m_dataPtr->insert_or_assign(key, std::forward<ValType>(val));
    }

    void insert_or_assign(const KeyType& key, const ValType& val)
    {
        ensureDetach();
        m_dataPtr->insert_or_assign(key, val);
    }

    template<typename ... Args>
    void emplace(Args&& ... args)
    {
        ensureDetach();
        m_dataPtr->emplace(std::forward<Args>(args)...);
    }

    void clear() noexcept
    {
        ensureDetach();
        m_dataPtr->clear();
    }

    void erase(const KeyType& key)
    {
        ensureDetach();
        m_dataPtr->erase(key);
    }

    void erase(const_iterator first, const_iterator last)
    {
        ensureDetach();
        m_dataPtr->erase(first, last);
    }

    bool operator ==(const SharedMap& another) const noexcept
    {
        return *m_dataPtr == *another.m_dataPtr;
    }

    bool operator !=(const SharedMap& another) const noexcept
    {
        return !this->operator ==(another);
    }

    bool operator <(const SharedMap& another) const noexcept
    {
        return m_dataPtr->operator <(another.m_dataPtr);
    }

    bool operator >(const SharedMap& another) const noexcept
    {
        return m_dataPtr->operator >(another.m_dataPtr);
    }

protected:
    void ensureDetach()
    {
        if (!m_dataPtr) {
            return;
        }

        if (m_dataPtr.use_count() == 1) {
            return;
        }

        m_dataPtr = std::make_shared<Data>(*m_dataPtr);
    }

    DataPtr m_dataPtr = nullptr;
};
}

#endif // MU_GLOBAL_SHAREDMAP_H
