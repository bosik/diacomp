package org.bosik.diacomp.web.frontend.features.foodbase;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.bosik.diacomp.core.entities.business.foodbase.FoodItem;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodItem;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.services.AuthService;
import org.bosik.diacomp.core.services.exceptions.AlreadyDeletedException;
import org.bosik.diacomp.core.services.exceptions.CommonServiceException;
import org.bosik.diacomp.core.services.exceptions.NotFoundException;
import org.bosik.diacomp.core.services.exceptions.PersistenceException;
import org.bosik.diacomp.core.services.foodbase.FoodBaseService;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.frontend.common.AuthorizedRestClient;
import com.sun.jersey.api.client.UniformInterfaceException;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.representation.Form;

public class FoodbaseRestClient extends AuthorizedRestClient implements FoodBaseService
{
	private final Serializer<Versioned<FoodItem>>	serializer	= new SerializerFoodItem();

	public FoodbaseRestClient(AuthService authService, String login, String pass, int apiVersion)
	{
		super(authService, login, pass, apiVersion);
	}

	@Override
	public void add(Versioned<FoodItem> item) throws PersistenceException
	{
		// TODO: current implementation doesn't fail for duplicates
		save(Arrays.asList(item));
	}

	@Override
	public void delete(String id) throws NotFoundException, AlreadyDeletedException
	{
		Versioned<FoodItem> item = findById(id);

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
	public List<Versioned<FoodItem>> findAll(boolean includeRemoved)
	{
		try
		{
			WebResource resource = getResource("api/food/all");
			resource = resource.queryParam("show_rem", Utils.formatBooleanInt(includeRemoved));
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		try
		{
			WebResource resource = getResource("api/food/search");
			resource = resource.queryParam("q", filter);
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findById(String guid)
	{
		try
		{
			WebResource resource = getResource(String.format("api/food/guid/%s", guid));
			String str = authGet(resource);

			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			Versioned<FoodItem> item = resp.getCode() != ResponseBuilder.CODE_NOTFOUND ? serializer.read(resp
					.getResponse()) : null;
			return item;
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public List<Versioned<FoodItem>> findChanged(Date since)
	{
		try
		{
			WebResource resource = getResource("api/food/changes");
			resource = resource.queryParam("since", Utils.formatTimeUTC(since));

			String str = authGet(resource);
			StdResponse resp = new StdResponse(str);
			checkResponse(resp);

			return serializer.readAll(resp.getResponse());
		}
		catch (UniformInterfaceException e)
		{
			throw new CommonServiceException(e);
		}
	}

	@Override
	public void save(List<Versioned<FoodItem>> items) throws NotFoundException, PersistenceException
	{
		WebResource resource = getResource("api/food/");
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
			throw new CommonServiceException("URL: " + resource.getURI(), e);
		}
	}
}
