package org.bosik.diacomp.web.backend.features.windows;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
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
	private static final String	DOWNLOAD_FOLDER	= "/download/win32/";
	private static final String	FILE_VERSION	= "version.txt";

	private List<String>		files			= new ArrayList<String>();
	{
		File folder = new File(DOWNLOAD_FOLDER);
		File[] listFiles = folder.listFiles();

		// null returned if the path is invalid
		if (listFiles != null)
		{
			for (final File fileEntry : listFiles)
			{
				if (!fileEntry.isDirectory())
				{
					files.add(fileEntry.getName());
				}
			}
		}
	}

	@GET
	@Path("version/")
	@Produces(MediaType.TEXT_PLAIN)
	public Response getVersion()
	{
		try
		{
			File file = new File(DOWNLOAD_FOLDER + FILE_VERSION);
			return Response.ok(file).build();
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

			for (String f : files)
			{
				if (f.equals(fileName))
				{
					System.out.println("Requested file to download: " + fileName);
					File file = new File(DOWNLOAD_FOLDER + fileName);
					return Response.ok(file).build();
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
}
