/**
  @file monster_counter.c
  @author Kevin Lynx
  @date 9.1.2008
*/
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>
#include "queue.h"

#define M_COUNT_FACT 1

/** 
  region monster
*/
struct region_monster
{
	/** region id, i.e : 10009 */
	int region_id;
	/** monster count */
	int m_count;
	LIST_ENTRY( region_monster ) next;
};

/**
  region monster list head 
*/
LIST_HEAD( region_m_head, region_monster );

/**
  read to the specified token.It will change the file position indicator.
*/
int raw_read_to( FILE *fp, const char *token )
{
	size_t len = strlen( token );
	char *buf = (char*) malloc( len + 1 );
	buf[0] = '\0';
	while( !feof( fp ) && strcmp( buf, token ) != 0 )
	{
		fread( buf, len, 1, fp );
		if( len > 1 )
		{
			fseek( fp, -(long)len + 1, SEEK_CUR );
		}
		buf[len] = '\0';
	}
	free( buf );

	return !feof( fp );
}

/**
  add a monster item to the list.
*/
void region_monster_add( struct region_m_head *rmh, int region_id, int m_count )
{
	struct region_monster *rm = (struct region_monster*) malloc( sizeof( struct region_monster ) );
	rm->m_count = m_count;
	rm->region_id = region_id;
	LIST_INSERT_HEAD( rmh, rm, next );
}

/**
  free the monster list.
*/
void region_monster_free( struct region_m_head *rmh )
{
	struct region_monster *rm, *prev = 0;
	LIST_FOREACH( rm, rmh, next )
	{
		if( prev != 0 ) free( prev );
		prev = rm;
	}
}

/**
  dump the monster list to the specified file.
*/
int region_monster_dump(  struct region_m_head *rmh, FILE *fp )
{
	struct region_monster *rm;
	int i = 0;
	LIST_FOREACH( rm, rmh, next )
	{
		fprintf( fp, "%d\t%d\n", rm->region_id, rm->m_count );		
		++ i;
	}
	return i;
}

/**
  read the monster config file for one region.
*/
int read_region_monster( struct region_m_head *rmh, int region_id )
{
	FILE *fp;
	int m_count = 0 ;
	char file[256];
	int index, left, top, right, bottom, count;
	sprintf( file, "regions/%d.monster", region_id );
	
	fp = fopen( file, "r" );
	if( fp == NULL )
	{
		return 0;
	}

	while( raw_read_to( fp, "#" ) )
	{
		count = 0;
		fscanf( fp, "%d%d%d%d%d%d", &index, &left, &top,&right, &bottom, &count );
		count *= M_COUNT_FACT;
		m_count += ( count == 0 ? 1 : count );

		{
			/* check the fucking <end> tag */
			long init_pos = ftell( fp );
			if( raw_read_to( fp, "<end>" ) )
			{
				long end_pos = ftell( fp );
				fseek( fp, init_pos, SEEK_SET );
				if( raw_read_to( fp, "#" ) )
				{
					long sharp_pos = ftell( fp );
					if( sharp_pos > end_pos ) 
					{
						break;
					}
					else
					{
						fseek( fp, init_pos, SEEK_SET );
					}
				}
			}
		}
	}

	region_monster_add( rmh, region_id, m_count );

	fclose( fp );
	return 1;
}

/**
  read region list config file.
*/
int read_region_list( struct region_m_head *rmh, const char *region_list )
{
	int region_id;
	int type;
	FILE *fp = fopen( region_list, "r" );
	if( fp == 0 )
	{
		return 0;
	}

	while( raw_read_to( fp, "#" ) )
	{
		/* id type */
		fscanf( fp, "%d%d", &type, &region_id );
		/* read the config file for this region */
		read_region_monster( rmh, region_id );
	}

	fclose( fp );
	return 1;
}

/** 
  the fucking c program entry.
*/
int main( int argc, char **argv )
{
	struct region_m_head rmh;
	char *dump_file;
	LIST_INIT( &rmh );
	if( !read_region_list( &rmh, "setup/regionlist.ini" ) )
	{
		fprintf( stderr, "read failed.\n" );
		exit( -1 );
	}
	
	if( argc < 2 )
	{
		dump_file = "DispatchMonsterInfo.conf";
	}
	else
	{
		dump_file = argv[1];
	}
	printf( "read ok...dump to [%s] file.\n", dump_file );

	{
		/* dump to the output file */
		FILE *fp = fopen( dump_file, "w" );
		int m_count;
		if( fp == NULL )
		{
			fprintf( stderr, "open the dump file [%s] failed!\n", dump_file );
			exit( -1 );
		}

		fprintf( fp, "Region\tValue\n" );
		m_count = region_monster_dump( &rmh, fp );

		printf( "dump %d region monsters.\n", m_count );
		fclose( fp );
	}

	region_monster_free( &rmh );
	return 0;
}

