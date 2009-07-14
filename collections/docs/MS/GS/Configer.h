///
/// 负责维护一些配置信息
///
namespace MServer
{
	class CConfiger
	{
	public:
		struct Addr
		{
			std::string ip;
			unsigned short port;
		};
		struct ServerParam
		{
			Addr addr;
			long nMaxFreeSockOperNum;
			// 其他开启网络环境的参数
		};
	};
}
