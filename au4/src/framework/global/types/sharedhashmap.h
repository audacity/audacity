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

#ifndef MU_GLOBAL_SHAREDHASHMAP_H
#define MU_GLOBAL_SHAREDHASHMAP_H

#include <memory>
#include <unordered_map>

namespace mu {
template<typename KeyType, typename ValType>
class SharedHashMap
{
public:
    using PairType = std::pair<KeyType, ValType>;
    using Data = std::unordered_map<KeyType, ValType>;
    using DataPtr = std::shared_ptr<Data>;
    typedef typename Data::iterator iterator;
    typedef typename Data::const_iterator const_iterator;

    SharedHashMap()
    {
        m_dataPtr = std::make_shared<Data>();
    }

    SharedHashMap(const size_t reserveSize)
    {
        m_dataPtr = std::make_shared<Data>(reserveSize);
    }

    SharedHashMap(std::initializer_list<PairType> initList)
    {
        m_dataPtr = std::make_shared<Data>(initList);
    }

    SharedHashMap& operator=(std::initializer_list<PairType> initList)
    {
        m_dataPtr = std::make_shared<Data>(initList);
        return *this;
    }

    SharedHashMap(const SharedHashMap&) = default;
    SharedHashMap(SharedHashMap&&) = default;
    SharedHashMap& operator=(const SharedHashMap&) = default;
    SharedHashMap& operator=(SharedHashMap&&) = default;

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

    const_iterator find(const KeyType& key) const noexcept
    {
        return m_dataPtr->find(key);
    }

    bool contains(const KeyType& key) const noexcept
    {
        return find(key) != end();
    }

    template<typename BeginIt, typename EndIt>
    bool containsAnyOf(BeginIt beginIt, EndIt endIt) const noexcept
    {
        for (auto it = beginIt; it != endIt; ++it) {
            if (this->contains(*it)) {
                return true;
            }
        }

        return false;
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

    bool operator ==(const SharedHashMap& another) const noexcept
    {
        return *m_dataPtr == *another.m_dataPtr;
    }

    bool operator !=(const SharedHashMap& another) const noexcept
    {
        return !this->operator ==(another);
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

#endif // MU_GLOBAL_SHAREDHASHMAP_H
