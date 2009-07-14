///
/// 缓存发给MS的局部数据的管理类。
/// 发给MS的数据包括：
/// 同步数据：动态更新
/// 静态数据：通常只需要一次更新，又包括：
/// 全局数据：所有MS都需要的数据，如怪物静态属性配置、技能静态配置；
/// 局部数据：单个MS拥有的数据，如特定的场景、该场景上的怪物实例。
///
namespace MServer
{
	///
	/// 所有添加操作都会立即向指定MS发送数据，如果MS未连接，就会
	/// 缓存数据。删除操作会立即发送删除消息。
	///
	class CCacheData
	{
	private:
		typedef std::vector<CGUID> RgnListType;
		/// MS划分的场景列表<id, rgn_list>
		typedef std::map<long, RgnListType> RgnTableType;
		typedef std::vector<CGUID> MonsterListType;
		/// MS划分的怪物列表<id, monster_list>
		typedef std::map<long, MonsterListType> MonsterTableType;
		/// 建立场景到MS ID的映射
		typedef std::map<CGUID, long> Guid2Long;
	public:
		/// 添加缓存场景
		void AddRegion( long id, const CGUID &rgn );

		/// 不指定ID时则为动态添加场景，主要是添加副本，
		/// 该副本会被放置在负载场景最少的MS
		long AddRegion( const CGUID &rgn );

		/// 删除场景
		void DelRegion( const CGUID &rgn );

		/// 添加缓存怪物
		void AddMonster( long id, const CGUID &monster );

		// 删除怪物
		void DelMonster( const CGUID &monster );

		/// 发送缓存场景给指定MS，发送后并不清除缓存数据
		void SendRegions( long socketid );

		/// 发送缓存怪物给指定MS
		void SendMonsters( long id );	
	private:
		void SendRegion( long socketid, const CGUID &rgn );
		void SendMonster( long socketid, const CGUID &monster );
	private:
		RgnTableType m_RgnTable;
		MonsterTableType m_MonsterTable;
		Guid2Long m_Rgn2ID;
		Guid2Long m_Monster2ID;
	};
}

