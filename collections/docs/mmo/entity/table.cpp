///
/// @file table.cpp
/// @author Kevin Lynx
///
#include "table.h"

ParamTable::ParamTable (Getter g, Setter s, void *u) {
    SetAccessor (g, s, u);
}

const Value *ParamTable::Get (const std::string &name) const {
    ValTable::const_iterator it = m_vals.find (name);
    if (it != m_vals.end()) {
        return &it->second;
    }
    return m_getter ? m_getter(m_udata, *this, name) : NULL;
}

bool ParamTable::Set (const std::string &name, const Value &val) {
    ValTable::iterator it = m_vals.find (name);
    if (it != m_vals.end()) {
        it->second = val;
        return true;
    }
    Add (name, val);
    return m_setter ? m_setter(m_udata, *this, name, val) : false;
}

void ParamTable::Add (const std::string &name, const Value &val) {
    m_vals[name] = val;
}

void ParamTable::Dump () {
    if (!m_setter) return;
    for (ValTable::iterator it = m_vals.begin(); it != m_vals.end(); ++it) {
        m_setter (m_udata, *this, it->first, it->second);
    }
    m_vals.clear();
}

void ParamTable::Clear () {
    m_vals.clear();
}

void ParamTable::SetAccessor (Getter g, Setter s, void *u) {
    m_getter = g;
    m_setter = s;
    m_udata = u;
}

