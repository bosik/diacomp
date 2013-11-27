package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.repository.common.Base;
import org.bosik.compensation.persistence.repository.common.Interchangeable;
import org.bosik.compensation.persistence.repository.providers.web.WebClient;

public class WebFoodBaseRepository implements Interchangeable<Base<FoodItem>>
{
	// private static String TAG = WebFoodBaseRepository.class.getSimpleName();
	private static FoodBaseXMLSerializer	serializer	= new FoodBaseXMLSerializer();

	private WebClient						webClient;

	public WebFoodBaseRepository(WebClient webClient)
	{
		if (webClient == null)
		{
			throw new IllegalArgumentException("WebClient can't be null");
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
	public Base<FoodItem> getData()
	{
		String source = webClient.getFoodBase();
		return serializer.read(source);
	}

	@Override
	public void postData(Base<FoodItem> base)
	{
		String source = serializer.write(base);
		String version = String.valueOf(base.getVersion());
		webClient.postFoodBase(version, source);
	}
}
