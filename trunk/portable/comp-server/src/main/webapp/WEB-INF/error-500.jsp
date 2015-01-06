<%@ page isErrorPage="true"
	import="java.io.*,org.bosik.diacomp.core.rest.ResponseBuilder"
	contentType="text/plain"%>
<%
	//StringWriter stringWriter = new StringWriter();
	//PrintWriter printWriter = new PrintWriter(stringWriter);
	//exception.printStackTrace(printWriter);

	out.print(ResponseBuilder.build(ResponseBuilder.CODE_FAIL, "Internal error"/* + stringWriter*/));
%>