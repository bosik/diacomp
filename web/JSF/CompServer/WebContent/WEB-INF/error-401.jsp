<%@ page import="org.bosik.diacomp.utils.ResponseBuilder" contentType="text/plain"%><%
	out.print(ResponseBuilder.build(ResponseBuilder.CODE_UNAUTHORIZED, "Not authorized"));
	// TODO: remove it
%>