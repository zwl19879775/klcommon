
#ifndef __USERLIST_H_
#define __USERLIST_H_

#include <vector>


class UserList
{
public:
    struct User 
    {
        char ip[32];
        unsigned short port;
    };
    typedef std::vector<User> UListT;
public:
    bool Load();

    const UListT &GetList() const;

    void Dump();

    static UserList &Inst();
private:
    UListT m_users;
    static UserList m_inst;
};

#endif
