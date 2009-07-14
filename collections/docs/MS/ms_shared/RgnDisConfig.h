///
/// 场景静态分配配置结构
///
namespace MServer
{
	/// 配置单个MS上的场景列表
	struct MSRgnInfo
	{
		long ms_id;
		std::vector<long> rgn_list;
	};

	/// 配置单个GS上对应的MS列表
	struct GSRgnInfo
	{
		std::map<long, MSRgnInfo> ms_rgn_table;
	};
}
