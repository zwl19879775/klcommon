content = "Hello " .. cgi_query_value( "name" )
cgi_write( "text/plain", content )

