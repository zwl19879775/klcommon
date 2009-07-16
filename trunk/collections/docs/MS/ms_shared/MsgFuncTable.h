///
/// 建立处理消息的函数表
/// 
namespace MServer
{
	class CMsgFuncTable
	{
		public:
			/// 处理消息的函数原型
			typedef void (*FuncType)( CMessage *msg );
		private:
			/// 函数表类型
			typedef std::map<long, FuncType> FuncTableType;
		public:
			/// 添加函数到函数表
			void AddFunc( long id, FuncType func );

			/// 默认是取消息第一个LONG型数据，然后在函数表中查找
			/// 对应的函数并调用之。
			virtual void Execute( CMessage *msg );

			/// 建立函数表
			virtual void BuildFuncTable() = 0;
		protected:
			FuncTableType m_Funcs;
	};
}

