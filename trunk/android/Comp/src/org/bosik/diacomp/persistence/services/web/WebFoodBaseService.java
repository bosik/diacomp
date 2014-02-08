package org.bosik.diacomp.persistence.services.web;

import java.util.List;
import org.bosik.diacomp.bo.foodbase.FoodItem;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.serializers.Serializer;
import org.bosik.diacomp.persistence.services.web.utils.client.WebClient;
import org.bosik.diacomp.services.FoodBaseService;
import org.bosik.diacomp.services.exceptions.DuplicateException;
import org.bosik.diacomp.services.exceptions.ItemNotFoundException;

public class WebFoodBaseService implements FoodBaseService
{
	// private static final String TAG = WebFoodBaseService.class.getSimpleName();

	private WebClient						webClient;
	private Serializer<Versioned<FoodItem>>	serializer;

	public WebFoodBaseService(WebClient webClient, Serializer<Versioned<FoodItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	@Override
	public String add(Versioned<FoodItem> item) throws DuplicateException
	{
		throw new UnsupportedOperationException();
		// item.updateTimeStamp();
		//
		// MemoryBase<Versioned<FoodItem>> base = load();
		// base.add(item);
		// save(base);
		// return item.getId();
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// base.remove(id);
		// save(base);
	}

	@Override
	public List<Versioned<FoodItem>> findAll()
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findAll();
	}

	@Override
	public List<Versioned<FoodItem>> findAny(String filter)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findAny(filter);
	}

	@Override
	public Versioned<FoodItem> findById(String id)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findById(id);
	}

	@Override
	public Versioned<FoodItem> findOne(String exactName)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findOne(exactName);
	}

	@Override
	public void update(Versioned<FoodItem> item) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// base.update(item);
		// save(base);
	}

	@Override
	public List<Versioned<FoodItem>> findSysAll()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Versioned<FoodItem> findSysById(String id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	// ----------------------------------- Web I/O -----------------------------------

	// private MemoryBase<FoodItem> load()
	// {
	// String source = webClient.getFoodBase();
	// return serializer.read(source);
	// }

	// private void save(MemoryBase<FoodItem> base)
	// {
	// String source = serializer.write(base);
	// webClient.postFoodBase(base.getVersion(), source);
	// }
}
