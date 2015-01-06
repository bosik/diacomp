<%@ page import="org.bosik.diacomp.core.rest.ResponseBuilder"
	contentType="text/plain"%>
<%
	out.print(ResponseBuilder.build(ResponseBuilder.CODE_NOTFOUND, "Method not found"));
%>