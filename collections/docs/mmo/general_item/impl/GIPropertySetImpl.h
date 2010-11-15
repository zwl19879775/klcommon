/// Internal use only, template class implementation for PropertySet.
template <typename Key, typename Value, typename Table>
PropertySet<Key, Value, Table>::PropertySet( PListenerType *listener ) :
    m_proListener( listener )
{
}

template <typename Key, typename Value, typename Table>
PropertySet<Key, Value, Table>::~PropertySet()
{
    SetListener( NULL );
}

template <typename Key, typename Value, typename Table>
void PropertySet<Key, Value, Table>::SetListener( PListenerType *listener )
{
    if( m_proListener ) delete m_proListener;
    m_proListener = listener;
    if( m_proListener )
    {
        m_proListener->SetOwner( this );
    }
}

template <typename Key, typename Value, typename Table>
bool PropertySet<Key, Value, Table>::AddProperty( Key key, Value val )
{
    if( HasProperty( key ) ) return false;
    m_properties[key] = val;
    if( m_proListener ) m_proListener->OnAdd( key, val );
    return true;
}

template <typename Key, typename Value, typename Table>
void PropertySet<Key, Value, Table>::RemoveProperty( Key key )
{
    if( !HasProperty( key ) ) return;
    if( m_proListener ) m_proListener->OnRemove( key );
    m_properties.erase( key );
}

template <typename Key, typename Value, typename Table>
void PropertySet<Key, Value, Table>::Clear()
{
    m_properties.clear();
}

template <typename Key, typename Value, typename Table>
bool PropertySet<Key, Value, Table>::HasProperty( Key key ) const
{
    return m_properties.find( key ) != m_properties.end();
}

template <typename Key, typename Value, typename Table>
typename PropertySet<Key, Value, Table>::ValueType 
    PropertySet<Key, Value, Table>::GetValue( Key key ) const
{
    typename TableType::const_iterator it = m_properties.find( key );
    return it == m_properties.end() ? ValueType() : it->second;
}

template <typename Key, typename Value, typename Table>
bool PropertySet<Key, Value, Table>::SetValue( Key key, Value val )
{
    typename TableType::iterator it = m_properties.find( key );
    if( it == m_properties.end() ) return false;
    if( m_proListener ) m_proListener->OnSet( key, it->second, val );
    it->second = val;
    return true;    
}

template <typename Key, typename Value, typename Table>
template <typename Fn>
void PropertySet<Key, Value, Table>::Traverse( Fn op ) const
{
    for( typename TableType::const_iterator it = m_properties.begin();
            it != m_properties.end(); ++ it )
    {
        if( op( it->first, it->second ) ) return;
    }
}


