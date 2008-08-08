-- lua script file to handle cgi query
-- this file is used to handle 'feedback' query
print( "lua script : lua cgi query begin." )
content = "This is cgi response from lua script, your feedback is : "
content = content .. cgi_query_value( "feedback" )
cgi_write( "text/plain", content )
print( "lua script : lua cgi query end." )
