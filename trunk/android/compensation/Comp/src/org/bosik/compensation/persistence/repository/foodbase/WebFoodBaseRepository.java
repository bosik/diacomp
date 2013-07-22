package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.Base;
import org.bosik.compensation.persistence.repository.common.Interchangeable;
import org.bosik.compensation.persistence.repository.providers.WebClient;

public class WebFoodBaseRepository implements Interchangeable
{
	// private static String TAG = WebFoodBaseRepository.class.getSimpleName();
	private static FoodBaseXMLSerializer formatter = new FoodBaseXMLSerializer();

	private WebClient webClient;

	public WebFoodBaseRepository(WebClient webClient)
	{
		if (webClient == null)
			throw new IllegalArgumentException("WebClient can't be null");

		this.webClient = webClient;
	}

	// ================================ API ================================

	@Override
	public int getVersion()
	{
		String resp = webClient.getFoodBaseVersion();
		return Integer.parseInt(resp);
	}

	@Override
	public String write()
	{
		return webClient.getFoodBase();
		// Base<Food> base = new Base<Food>();
		// formatter.read(base, resp);
		// return base;
	}

	@Override
	public void read(String data)
	{
		// TODO: optimize if need

		Base<Food> base = new Base<Food>();
		formatter.read(base, data);
		String version = String.valueOf(base.getVersion());

		webClient.postFoodBase(version, data);
	}
}
