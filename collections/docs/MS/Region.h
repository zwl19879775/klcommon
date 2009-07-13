///
/// 代表场景，主要保存场景阻挡
/// 
class CRegion : public CObject
{
public:
	/// 阻挡类型
	enum BlockType
	{
		BLOCK_NONE, 
		BLOCK_CANFLY,
		BLOCK_UNFLY,
		BLOCK_MONSTER,
		BLOCK_SHAPE,
		BLOCK_AIM
	};

	/// 格子结构
	struct tagCell
	{
		WORD Block; ///< 预留一些空间保存GS上的信息
		WORD Switch; 
	};

	typedef stdext::hash_map<CGUID, CMonster*, hash_guid_compare>
		MonsterListType;
	typedef stdext::hash_map<CGUID, CPlayer*, hash_guid_compare>
		PlayerListType;
public:
	CRegion();

	/// 释放资源（内存）
	~CRegion();

	/// 解码
	virtual void Decode( DBReadSet &db );

	/// 获取指定格子阻挡类型
	unsigned char GetBlock( long x, long y );

	/// 设置阻挡
	void SetBlock( long x, long y, unsigned char b );

	/// 获取指定ID的SHAPE
	CShape *FindShape( const CGUID &guid, long type );

	/// 获取指定SHPAE周围的特定类型SHAPE列表
	/// type等于CObject::OBJ_ALL时会忽略类型
	size_t FindAroundShape( CShape *self, long type, std::vector<CShape*> &retShapes );
	
	/// 添加一个SHAPE
	void AddShape( long type, CShape *shape );

	/// 移除一个SHAPE
	void RemoveShape( long type, const CGUID &shape_id );
private:
	long m_RgnType; ///< 场景类型
	long m_Width;
	long m_Height;
	unsigned char m_Country;
	tagCell *m_CellArray; ///< 阻挡数据
	CArea *m_AreaArray; ///< 区域数据
	MonsterListType m_Monsters;
	PlayerListType m_Players;
};

