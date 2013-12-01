package org.bosik.compensation.persistence.dao.web;

import java.util.List;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.common.MemoryBase;
import org.bosik.compensation.persistence.dao.BaseDAO;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.serializers.Serializer;

public class WebFoodBaseDAO implements BaseDAO<FoodItem>
{
	// private static final String					TAG	= WebFoodBaseDAO.class.getSimpleName();

	private WebClient							webClient;
	private Serializer<MemoryBase<FoodItem>>	serializer;
	private MemoryBase<FoodItem>				base;

	public WebFoodBaseDAO(WebClient webClient, Serializer<MemoryBase<FoodItem>> serializer)
	{
		this.webClient = webClient;
		this.serializer = serializer;
		base = load();
	}

	@Override
	public String add(FoodItem item) throws DuplicateException
	{
		// base = load();
		base.add(item);
		save(base);
		return item.getId();
	}

	@Override
	public void delete(String id) throws ItemNotFoundException
	{
		// base = load();
		base.remove(id);
		save(base);
	}

	@Override
	public List<FoodItem> findAll()
	{
		base = load();
		return base.findAll();
	}

	@Override
	public List<FoodItem> findAny(String filter)
	{
		base = load();
		return base.findAny(filter);
	}

	@Override
	public FoodItem findById(String id)
	{
		base = load();
		return base.findById(id);
	}

	@Override
	public FoodItem findOne(String exactName)
	{
		base = load();
		return base.findOne(exactName);
	}

	@Override
	public void replaceAll(List<FoodItem> newList, int newVersion)
	{
		// base = new MemoryBase<FoodItem>();
		base.replaceAll(newList, newVersion);
		save(base);
	}

	@Override
	public void update(FoodItem item) throws ItemNotFoundException
	{
		// base = load();
		base.update(item);
		save(base);
	}

	@Override
	public int getVersion()
	{
		return webClient.getFoodBaseVersion();
	}

	// ----------------------------------- Web I/O -----------------------------------

	private MemoryBase<FoodItem> load()
	{
		String source = webClient.getFoodBase();
		return serializer.read(source);
	}

	private void save(MemoryBase<FoodItem> base)
	{
		String source = serializer.write(base);
		webClient.postFoodBase(base.getVersion(), source);
	}
}
