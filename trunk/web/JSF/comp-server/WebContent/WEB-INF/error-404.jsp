<%@ page import="org.bosik.diacomp.web.backend.common.ResponseBuilder" contentType="text/plain"%><%
	out.print(ResponseBuilder.build(ResponseBuilder.CODE_NOTFOUND, "Method not found"));
%>