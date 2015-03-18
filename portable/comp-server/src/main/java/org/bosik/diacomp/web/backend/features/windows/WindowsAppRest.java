package org.bosik.diacomp.web.backend.features.windows;

import java.io.InputStream;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.rest.ResponseBuilder;

@Path("windows/")
@SuppressWarnings("static-method")
public class WindowsAppRest
{
	private static final int	LATEST_VERSION	= 201;

	@GET
	@Path("version/")
	@Produces(MediaType.TEXT_PLAIN)
	public Response getVersion()
	{
		try
		{
			return Response.ok(String.valueOf(LATEST_VERSION)).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("file/{fileName}")
	@Produces(MediaType.APPLICATION_OCTET_STREAM)
	public Response getFile(@PathParam("fileName") String fileName)
	{
		try
		{
			if (fileName == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("File name is not specified").build();
			}

			String[] files = new String[] { "compensation.exe", "mathan.dll", "restart.exe", "demofoodbase.xml",
					"demodishbase.xml" };

			for (String f : files)
			{
				if (f.equals(fileName))
				{
					return getFileFromResource(fileName);
				}
			}

			return Response.status(Status.NOT_FOUND).entity("File not found: " + fileName).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	private Response getFileFromResource(String fileName)
	{
		ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		InputStream stream = classloader.getResourceAsStream("/win32/" + fileName);
		return Response.ok(stream).header("Content-Disposition", "attachment; filename=" + fileName).build();
	}
}
