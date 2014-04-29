package org.bosik.diacomp.web.frontend.features.dishbase;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerDishItem;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.dishbase.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.common.AuthorizedRestClient;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.representation.Form;

public class DishbaseRestClient extends AuthorizedRestClient implements DishBaseService
{
	private static final long						serialVersionUID	= 1L;

	private final Serializer<Versioned<DishItem>>	serializer			= new SerializerDishItem();

	public DishbaseRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		super(authService, login, pass, apiVersion);
	}

	@Override
	public void add(Versioned<DishItem> item) throws PersistenceException
	{
		// TODO: current implementation doesn't fail for duplicates
		save(Arrays.asList(item));
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<DishItem> item = findById(id);

		if (item == null)
		{
			throw new NotFoundException(id);
		}

		if (item.isDeleted())
		{
			throw new AlreadyDeletedException(id);
		}

		item.setDeleted(true);
		save(Arrays.asList(item));
	}

	@Override
	public List<Versioned<DishItem>> findAll(boolean includeRemoved)
	{
		try
		{
			WebResource resource = getResource("api/dish/all");
			resource = resource.queryParam("show_rem", Utils.formatBooleanInt(includeRemoved));
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			handleUniformInterfaceException(e);
			return null; // previous method will throw exception anyway
		}
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		try
		{
			WebResource resource = getResource("api/dish/search");
			resource = resource.queryParam("q", filter);
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			handleUniformInterfaceException(e);
			return null; // previous method will throw exception anyway
		}
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		try
		{
			WebResource resource = getResource(String.format("api/dish/guid/%s", id));
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return resp.getCode() != ResponseBuilder.CODE_NOTFOUND ? serializer.read(resp.getResponse()) : null;
		}
		catch (UniformInterfaceException e)
		{
			handleUniformInterfaceException(e);
			return null; // previous method will throw exception anyway
		}
	}

	@Override
	public List<Versioned<DishItem>> findChanged(Date since)
	{
		try
		{
			WebResource resource = getResource("api/dish/changes");
			resource = resource.queryParam("since", Utils.formatTimeUTC(since));

			String str = authGet(resource);
			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			handleUniformInterfaceException(e);
			return null; // previous method will throw exception anyway
		}
	}

	@Override
	public void save(List<Versioned<DishItem>> items) throws NotFoundException, PersistenceException
	{
		WebResource resource = getResource("api/dish/");
		try
		{
			Form form = new Form();
			form.add("items", serializer.writeAll(items));
			String str = authPut(resource, form);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);
		}
		catch (UniformInterfaceException e)
		{
			System.err.println(e.getResponse().getEntity(String.class));
			handleUniformInterfaceException(e);
		}
	}
}
