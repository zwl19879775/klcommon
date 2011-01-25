/**
  
*/
#include "Utils.h"
#include "UserList.h"

UserList UserList::m_inst;

bool UserList::Load()
{
    const char *file = "userlist.ini";
    FILE *fp = fopen( file, "r" );
    if( !fp ) 
    {
        Log( "Open config file %s failed.\n", file );
        return false;
    }
    while( !feof( fp ) )
    {
        User user;
        char port[8];
        fscanf( fp, "%s%s", user.ip, port );
        user.port = (unsigned short) atoi( port );
        m_users.push_back( user );
    }
    fclose( fp );
    return true;
}

const UserList::UListT &UserList::GetList() const
{
    return m_users;
}

UserList &UserList::Inst() 
{
    return m_inst;
}

void UserList::Dump() 
{
    Log( "=====Dump user list====\n" );
    for( UListT::const_iterator it = m_users.begin();
        it != m_users.end(); ++ it )
    {
        Log( "%s:%d\n", it->ip, it->port );
    }
    Log( "=======================\n" );
}
