package org.bosik.diacomp.web.backend.features.preferences;

import java.util.List;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import org.bosik.diacomp.core.persistence.parsers.Parser;
import org.bosik.diacomp.core.persistence.parsers.ParserPreferenceEntry;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.utils.SerializerAdapter;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.bosik.diacomp.core.services.preferences.Preference;
import org.bosik.diacomp.core.services.preferences.PreferenceEntry;
import org.bosik.diacomp.core.services.preferences.PreferencesService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Path("preferences/")
public class PreferencesRest
{
	@Autowired
	private PreferencesService							preferencesService;

	private final Parser<PreferenceEntry<String>>		parser		= new ParserPreferenceEntry();
	private final Serializer<PreferenceEntry<String>>	serializer	= new SerializerAdapter<PreferenceEntry<String>>(
																			parser);

	@GET
	@Path("hash")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getHash() throws CommonServiceException
	{
		try
		{
			String s = preferencesService.getHash();
			String response = s != null ? s : "";
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getAll() throws CommonServiceException
	{
		try
		{
			List<PreferenceEntry<String>> item = preferencesService.getAll();
			String response = serializer.writeAll(item);
			return Response.ok(response).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("{key}")
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response getPreference(@PathParam("key") String parKey) throws CommonServiceException
	{
		try
		{
			PreferenceEntry<String> entity = preferencesService.getString(Preference.parse(parKey));
			String response = serializer.write(entity);
			return Response.ok(response).build();
		}
		catch (IllegalArgumentException e)
		{
			return Response.status(Status.NOT_FOUND).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@PUT
	@Produces(MediaType.APPLICATION_JSON + ";charset=utf-8")
	public Response update(@FormParam("data") String parData) throws CommonServiceException
	{
		try
		{
			if (parData == null)
			{
				return Response.status(Status.BAD_REQUEST).entity("Missing parameter: data").build();
			}

			List<PreferenceEntry<String>> entries = serializer.readAll(parData);
			preferencesService.update(entries);

			return Response.ok("Saved OK").build();
		}
		catch (IllegalArgumentException e)
		{
			return Response.status(Status.BAD_REQUEST).entity(e.getMessage()).build();
		}
		catch (NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity(ResponseBuilder.buildNotAuthorized()).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
