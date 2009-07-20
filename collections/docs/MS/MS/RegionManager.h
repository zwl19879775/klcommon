///
/// 全局管理场景的类
///
class CRegionManager : public kl_common::ObjManager
						<CGUID, CRegion*, stdext::hash_map<CGUID, CRegion*, hash_map_compare> >
{
};

