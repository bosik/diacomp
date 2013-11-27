package org.bosik.compensation.persistence.dao.web;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.common.Interchangeable;
import org.bosik.compensation.persistence.common.MemoryBase;
import org.bosik.compensation.persistence.dao.web.utils.client.WebClient;
import org.bosik.compensation.persistence.serializers.foodbase.FoodBaseXMLSerializer;

public class WebFoodBaseRepository implements Interchangeable<MemoryBase<FoodItem>>
{
	private static FoodBaseXMLSerializer	serializer	= new FoodBaseXMLSerializer();

	private WebClient						webClient;

	public WebFoodBaseRepository(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new NullPointerException("WebClient can't be null");
		}

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
	public MemoryBase<FoodItem> getData()
	{
		String source = webClient.getFoodBase();
		return serializer.read(source);
	}

	@Override
	public void postData(MemoryBase<FoodItem> base)
	{
		String source = serializer.write(base);
		String version = String.valueOf(base.getVersion());
		webClient.postFoodBase(version, source);
	}
}
