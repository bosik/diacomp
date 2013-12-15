package org.bosik.compensation.persistence.dao.web;

import java.util.List;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.dishbase.DishItem;
import org.bosik.compensation.persistence.dao.DishBaseDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.serializers.Serializer;

public class WebDishBaseDAO implements DishBaseDAO
{
	private WebClient						webClient;
	private Serializer<Unique<DishItem>>	serializer;

	public WebDishBaseDAO(WebClient webClient, Serializer<Unique<DishItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	@Override
	public String add(Unique<DishItem> item) throws DuplicateException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public List<Unique<DishItem>> findAll()
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public List<Unique<DishItem>> findAny(String filter)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Unique<DishItem> findOne(String exactName)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public Unique<DishItem> findById(String id)
	{
		throw new UnsupportedOperationException("Not implemented");
	}

	@Override
	public void update(Unique<DishItem> item) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException("Not implemented");
	}
}
