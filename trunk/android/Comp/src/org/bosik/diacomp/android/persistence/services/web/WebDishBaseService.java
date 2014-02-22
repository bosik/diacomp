package org.bosik.diacomp.android.persistence.services.web;

import java.util.List;
import org.bosik.diacomp.android.persistence.services.web.utils.client.WebClient;
import org.bosik.diacomp.core.bo.dishbase.DishItem;
import org.bosik.diacomp.core.persistence.common.Versioned;
import org.bosik.diacomp.core.persistence.serializers.Serializer;
import org.bosik.diacomp.core.services.DishBaseService;
import org.bosik.diacomp.core.services.exceptions.DuplicateException;
import org.bosik.diacomp.core.services.exceptions.ItemNotFoundException;

public class WebDishBaseService implements DishBaseService
{
	private WebClient						webClient;
	private Serializer<Versioned<DishItem>>	serializer;

	public WebDishBaseService(WebClient webClient, Serializer<Versioned<DishItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	@Override
	public String add(Versioned<DishItem> item) throws DuplicateException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public List<Versioned<DishItem>> findAll()
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public List<Versioned<DishItem>> findAny(String filter)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Versioned<DishItem> findOne(String exactName)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Versioned<DishItem> findById(String id)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public void update(Versioned<DishItem> item) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public List<Versioned<DishItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<DishItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}
}
