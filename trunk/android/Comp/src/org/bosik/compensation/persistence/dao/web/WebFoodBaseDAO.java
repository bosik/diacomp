package org.bosik.compensation.persistence.dao.web;

import java.util.List;
import org.bosik.compensation.bo.basic.Unique;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.exceptions.DuplicateException;
import org.bosik.compensation.persistence.exceptions.ItemNotFoundException;
import org.bosik.compensation.persistence.serializers.Serializer;

public class WebFoodBaseDAO implements FoodBaseDAO
{
	// private static final String TAG = WebFoodBaseDAO.class.getSimpleName();

	private WebClient						webClient;
	private Serializer<Unique<FoodItem>>	serializer;

	public WebFoodBaseDAO(WebClient webClient, Serializer<Unique<FoodItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
	}

	@Override
	public String add(Unique<FoodItem> item) throws DuplicateException
	{
		throw new UnsupportedOperationException();
		// item.updateTimeStamp();
		//
		// MemoryBase<Unique<FoodItem>> base = load();
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
	public List<Unique<FoodItem>> findAll()
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findAll();
	}

	@Override
	public List<Unique<FoodItem>> findAny(String filter)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findAny(filter);
	}

	@Override
	public Unique<FoodItem> findById(String id)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findById(id);
	}

	@Override
	public Unique<FoodItem> findOne(String exactName)
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// return base.findOne(exactName);
	}

	@Override
	public void update(Unique<FoodItem> item) throws ItemNotFoundException
	{
		throw new UnsupportedOperationException();
		// MemoryBase<FoodItem> base = load();
		// base.update(item);
		// save(base);
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
