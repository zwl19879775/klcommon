///
/// 提供同步操作函数集合
///
namespace MServer
{
	namespace SyncOper
	{
		/// 同步场景数据，一般用于场景创建时的编码
		/* 该函数大致实现：
		 	long ms_id = GetInst( CRgnDisConfiger ).GetDisMS( pRegion->GetID() );
			if( ms_id == NOT_DIS ) return ;
			if( ms_id == DYNA_DIS )
			{
				GetInst( CCacheData ).AddRegion( ms_id, pRegion->GetExID() );	
			}
			else
			{
				GetInst( CCacheData ).AddRegion( pRegion->GetExID() );
			}
		 */
		void SyncRegion( CServerRegion *pRegion );

		/// 同步怪物数据，用于怪物创建时的编码，实现同SyncRegion
		void SyncMonster( CMonster *pMonster );

		/// 同步技能静态配置
		void SyncSkillCfg( /*实现待定*/ );

		/// 同步玩家移动
		void SyncPlayerPos( const CGUID &player, float pos_x, float pos_y, long dir );

		/// 同步玩家离开场景
		void SyncPlayerLeaveRgn( const CGUID &player, bool del );

		/// 同步玩家进入场景
		void SyncPlayerEnterRgn( const CGUID &player, const CGUID &rgn, bool add );	

		/// 辅助函数，同步玩家切换场景，该函数会先同步离开场景，再同步进入场景
		/// 并涉及到部分情况的判断，上层可直接调用该函数完成玩家切换场景时对MS
		/// 的影响
		/* 该函数大致实现为：
		 	bool del ;
			if( 新旧场景在同一MS ) del = false;
			else del = true;
			if( 旧场景存在 )
				SyncPlayerLeaveRgn( player->GetExID(), del );
			SyncPlayerEnterRgn( player->GetExID(), new_rgn->GetExID(), del );
		 */
		void SyncPlayerChangeRgn( CPlayer *player, CServerRegion *old_rgn,
				CServerRegion *new_rgn );
	}
}

