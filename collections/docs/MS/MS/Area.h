///
/// 主要提供在场景上查找指定SHAPE周围SHAPE的能力
///
class CArea
{
public:
	typedef std::vector<CGUID> GuidSetType;
public:
	/// 添加SHAPE到此区域内
	void AddShape( long type, const CGUID &guid );

	/// 删除一个SHAPE
	void RemoveShape( long type, const CGUID &guid );

	/// 获取该区域指定类型的所有SHAPE
	size_t GetShapes( long type, std::vector<CShape*> &shapelist );
private:
	/// 该区域上的怪物
	GuidSetType m_Monsters;

	/// 该区域上的玩家
	GuidSetType m_Players;

	/// 该区域在场景上的坐标
	long m_X;
	long m_Y;

	/// 该区域所在场景
	CRegion *m_pFather;
};

