///
/// @file gvalue.cpp
/// @author Kevin Lynx
/// @date 5.6.2011
///
#include <memory.h>
#include <stdlib.h>
#include <assert.h>
#include "gvalue.h"

#define IS_RAW(t) (t >= RAW)

GValue::~GValue() {
    DecRef ();
}

GValue::GValue (const GValue &other) {
    Copy (other);
}

GValue &GValue::operator = (const GValue &other) {
    if (m_ref == other.m_ref) return *this;
    DecRef ();
    Copy (other);
    return *this;
}

void GValue::Copy (const GValue &other) {
    other.IncRef ();
    m_ref = other.m_ref;
    m_val = other.m_val;
    m_type = other.m_type;
}

bool GValue::operator == (const GValue &other) const {
    if (this == &other) return true;
    if (m_type != other.m_type) return false;
    if (IS_RAW(m_type)) {
        return m_val.r.p == other.m_val.r.p ||
            (m_val.r.size == other.m_val.r.size &&
             !memcmp (m_val.r.p, other.m_val.r.p, m_val.r.size));
    }
    if (m_type == NIL) return true;
    if (m_type == BOOLEAN) return m_val.n == other.m_val.n;
    return m_val.d == other.m_val.d;
}

void GValue::Set () {
    if (*m_ref > 1) {
        Split ();
    }
    else {
        FreeVal ();
    }
    m_type = NIL;
}

void GValue::Set (double d) {
    Set ();
    m_type = NUMBER;
    m_val.d = d;
}

void GValue::Set (bool b) {
    Set ();
    m_type = BOOLEAN;
    m_val.n = b ? 1 : 0;
}

void GValue::Set (const void *p, size_t size, int t) {
    Set ();
    assert (IS_RAW(t));
    m_type = t;
    m_val.r.p = malloc(size);
    m_val.r.size = size;
    memcpy (m_val.r.p, p, size);
}

bool GValue::GetBool() const {
    assert (m_type == BOOLEAN);
    if (m_type != BOOLEAN) return false;
    return m_val.n != 0;
}

double GValue::GetNumber() const {
    assert (m_type == NUMBER);
    if (m_type != NUMBER) return 0.0;
    return m_val.d;
}

const GValue::Raw *GValue::GetRaw() const {
    assert (IS_RAW(m_type));
    if (!IS_RAW(m_type)) return NULL;
    return &m_val.r;
}

void GValue::Init () {
    m_ref = (int*) malloc (sizeof(*m_ref));
    *m_ref = 1;
    m_type = NIL;
}

void GValue::Free () {
    FreeVal ();
    free (m_ref);
}

void GValue::FreeVal () {
    if (IS_RAW(m_type)) {
        free (m_val.r.p);
    }
}

void GValue::IncRef () const {
    *m_ref = *m_ref + 1;
}

void GValue::DecRef () {
    *m_ref = *m_ref - 1;
    if (*m_ref <= 0) {
        Free ();
    }
}

void GValue::Split () {
    DecRef ();
    Init ();
}

