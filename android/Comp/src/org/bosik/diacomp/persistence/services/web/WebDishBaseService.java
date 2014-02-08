package org.bosik.diacomp.persistence.services.web;

import java.util.List;
import org.bosik.diacomp.bo.dishbase.DishItem;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.exceptions.DuplicateException;
import org.bosik.diacomp.persistence.exceptions.ItemNotFoundException;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.services.web.utils.client.WebClient;
import services.DishBaseService;

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
