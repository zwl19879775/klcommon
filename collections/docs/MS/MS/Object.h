///
/// 所有SHAPE的基类，抽象出各种SHAPE的共同属性
///
class CObject
{
public:
	/// object的类型
	enum 
	{
		OBJ_ALL = -1, ///< monster && player
		OBJ_INVALID = 0,
		OBJ_REGION,
		OBJ_PLAYER,
		OBJ_MONSTER
	};
public:
	Object() : 
		m_type( OBJ_INVALID )
	{
	}

	virtual ~Object() { }

	/// 解码
	virtual void Decode( DBReadSet &db );
protected:
	long m_type;
	CGUID m_guid;
};

