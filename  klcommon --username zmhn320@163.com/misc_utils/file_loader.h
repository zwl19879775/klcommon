///
///
///
#ifndef ___FILE_LOADER_H_
#define ___FILE_LOADER_H_

#include <stddef.h>

class FileLoader
{
public:
	struct RawData
	{
		void *buf;
		size_t size;
	};
	enum ReadMode
	{
		TEXT, BINARY
	};
public:
	FileLoader()
	{
		_data.buf = 0;
		_data.size = 0;
	}

	~FileLoader()
	{
		Destroy();
	}

	bool Load( const char *file, int mode );

	void Destroy();

	RawData GetRawData()
	{
		return _data;
	}
private:
	RawData _data;
};

#endif
