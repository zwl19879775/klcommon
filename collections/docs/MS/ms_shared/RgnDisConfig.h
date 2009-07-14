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

		/// 编码该结构，编码结构为：
		/*
		  ms_id(long),
		  rgn_list_size(long),
		  rgn_id1,
		  rgn_id2,
		  ...
		*/
		static void Encode( const MSRgnInfo &ms_rgn, DBWriteSet &db );

		/// 解码该结构
		static void Decode( DBReadSet &db, MSRgnInfo &ms_rgn );
	};

	/// 配置单个GS上对应的MS列表
	struct GSRgnInfo
	{
		std::map<long, MSRgnInfo> ms_rgn_table;

		/// 编码该结构，编码结构为：
		/*
		 ms_rgn_table_size(long),
		 ms_rgn_info1,
		 ms_rgn_info2,
		 ...
		*/
		static void Encode( const GSRgnInfo &gs_rgn, DBWriteSet &db );

		/// 解码该结构，在解码过程中，根据MSRgnInfo里的MS ID作为map的key
		static void Decode( DBReadSet &db, GSRgnInfo &gs_rgn );
	};
}

