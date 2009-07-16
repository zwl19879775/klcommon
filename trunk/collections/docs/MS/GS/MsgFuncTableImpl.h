///
/// 建立处理MS请求的函数表
///
namespace MServer
{
	class CReqRecvMgr : public MsgFuncTable
	{
	public:
		void BuildFuncTable();
	};
}
