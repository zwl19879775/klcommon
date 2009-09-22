///
/// a simple program to dump pe file.
///
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

struct FileData
{
	void *buf;
	size_t size;
};

bool LoadFile( const char *file, FileData *fd )
{
	FILE *fp = fopen( file, "rb" );
	if( fp == NULL )
	{
		return false;
	}

	fseek( fp, 0, SEEK_END );
	fd->size = (size_t) ftell( fp );
	fseek( fp, 0, SEEK_SET );
	fd->buf = malloc( fd->size );
	fread( fd->buf, sizeof( char ), fd->size, fp );
	fclose( fp );
	return true;
}

void FreeFile( FileData *fd )
{
	fd->size = 0;
	free( fd->buf); 
}

#define CAST_PTR( t, buf, pos ) ((t*)&buf[pos])

DWORD RVAToOffset( IMAGE_SECTION_HEADER *sections, long sec_count, DWORD rva )
{
	DWORD offset = 0;
	for( long i = 0; i < sec_count; ++ i )
	{
		if( rva >= sections[i].VirtualAddress &&
			rva < sections[i].VirtualAddress + sections[i].SizeOfRawData )
		{
			// the address is located in this section.
			offset = rva - sections[i].VirtualAddress;
			offset += sections[i].PointerToRawData;
			return offset;
		}
	}
	return offset;
}

void DumpSections( long sec_count, IMAGE_SECTION_HEADER *sections )
{
	printf( "Sections:\n" );
	for( long i = 0; i < sec_count; ++ i )
	{
		printf( "\tName:%s\tVirtualAddress:0x%08x\tSizeOfRawData:0x%08x\n", sections[i].Name,
			sections[i].VirtualAddress,
			sections[i].SizeOfRawData );
	}
}

void DumpImports( IMAGE_NT_HEADERS *pe_header, const char *buf, IMAGE_SECTION_HEADER *sections )
{
	printf( "Import:\n" );
	IMAGE_DATA_DIRECTORY *import_dic = &pe_header->OptionalHeader.DataDirectory[1];

	long offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, import_dic->VirtualAddress );
	if( offset == 0 )
	{
		printf( "\tNo import symbols.\n" );
		return ;
	}
	IMAGE_IMPORT_DESCRIPTOR *import_descs = CAST_PTR( IMAGE_IMPORT_DESCRIPTOR, buf, offset );
	for( IMAGE_IMPORT_DESCRIPTOR *import = import_descs; import->FirstThunk != 0 && import->OriginalFirstThunk != 0;
		import ++ )
	{
		offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, import->Name ); 
		printf( "\t%s\n", &buf[offset] );

		if( !( import->OriginalFirstThunk & IMAGE_ORDINAL_FLAG32 ) )
		{
			// import by name
			long offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, import->OriginalFirstThunk );
			IMAGE_THUNK_DATA *trunk = CAST_PTR( IMAGE_THUNK_DATA , buf, offset );
			for( ; trunk->u1.AddressOfData != 0; trunk ++ )
			{
				long offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, trunk->u1.AddressOfData );
				IMAGE_IMPORT_BY_NAME *names = CAST_PTR( IMAGE_IMPORT_BY_NAME, buf, offset );
				printf( "\t\t%s\n", (char*)names->Name );
			}
		}
	}
}

void DumpExports( IMAGE_NT_HEADERS *pe_header, const char *buf, IMAGE_SECTION_HEADER *sections )
{
	printf( "Export:\n" );
	IMAGE_DATA_DIRECTORY *export_dic = &pe_header->OptionalHeader.DataDirectory[0];

	long offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, export_dic->VirtualAddress );
	if( offset == 0 )
	{
		printf( "\tNo export symbols.\n" );
		return ;
	}
	IMAGE_EXPORT_DIRECTORY *exports = CAST_PTR( IMAGE_EXPORT_DIRECTORY, buf, offset );
	offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, exports->AddressOfNames );
	DWORD *name_rva = (DWORD*)&buf[offset];
	for( DWORD i = 0; i < exports->NumberOfNames; ++ i )
	{
		offset = RVAToOffset( sections, pe_header->FileHeader.NumberOfSections, name_rva[i] );
		printf( "\t\t%s\n", &buf[offset] );
	} 
}

void DumpPE( const char *buf, size_t size )
{
	// DOS MZ header
	IMAGE_DOS_HEADER *dos_header = CAST_PTR( IMAGE_DOS_HEADER, buf, 0 );
	if( dos_header->e_magic != IMAGE_DOS_SIGNATURE )
	{
		fprintf( stderr, "Invalid DOS MZ header.\n" );
		return ;
	}
	// skip to PE header
	IMAGE_NT_HEADERS *pe_header = CAST_PTR( IMAGE_NT_HEADERS, buf, dos_header->e_lfanew );
	IMAGE_SECTION_HEADER *sections = CAST_PTR( IMAGE_SECTION_HEADER , pe_header, 1 );

	if( pe_header->Signature != IMAGE_NT_SIGNATURE )
	{
		fprintf( stderr, "Invalid NT signature.\n" );
		return ;
	}
	DumpSections( pe_header->FileHeader.NumberOfSections, sections );
	DumpImports( pe_header, buf, sections );
	DumpExports( pe_header, buf, sections );
}

int main( int argc, char **argv )
{
	if( argc != 2 )
	{
		fprintf( stderr, "Usage : %s filename.\n", argv[0] );
		exit( -1 );
	}
	FileData fd;
	if( !LoadFile( argv[1], &fd ) )
	{
		fprintf( stderr, "Load %s failed.\n", argv[0] );
		exit( -1 );
	}
	DumpPE( (char*)fd.buf, fd.size );
	FreeFile( &fd );
	return 0;
}
