///
/// @file kl_guard.h
/// @author Kevin Lynx
/// @date 3.6.2008
///
#ifndef ___KL_GUARD_H_
#define ___KL_GUARD_H_

#include "kl_compiler_cfg.h"

KL_COMMON_NAMESPACE_BEGIN

///
/// make the mutex's 'acquire' 'release' operation automatically
///
template <typename _MutexType>
class guard
{
public:
	/// mutex type
	typedef _MutexType mutex_type;
public:
	/// constructor, acquire the mutex
	guard( mutex_type &mutex ) : _mutex( mutex )
	{
		_mutex.acquire();
	}

	/// destructor, release the mutex
	~guard()
	{
		_mutex.release();
	}

private:
	/// mutex
	mutex_type &_mutex;
};

KL_COMMON_NAMESPACE_END

#endif // end ___KL_GUARD_H_