package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.Base;
import org.bosik.compensation.persistence.repository.common.BaseRepository;
import org.bosik.compensation.persistence.repository.providers.WebClient;
import android.util.Log;

public class WebFoodBaseRepository implements BaseRepository<Base<Food>>
{
	// @SuppressWarnings("unused")
	private static String TAG = WebFoodBaseRepository.class.getSimpleName();
	private static FoodBaseXMLSerializer formatter = new FoodBaseXMLSerializer();

	private WebClient webClient;

	public WebFoodBaseRepository(WebClient webClient)
	{
		if (webClient == null)
			throw new NullPointerException("WebClient can't be null");

		this.webClient = webClient;
	}

	@Override
	public int getVersion()
	{
		String resp = webClient.getFoodBaseVersion();
		return Integer.parseInt(resp);
	}

	@Override
	public Base<Food> getBase()
	{
		// String resp = webClient.doGetSmart(webClient.getServer() + WebClient.URL_CONSOLE +
		// "?foodbase:download");
		String resp = webClient.getFoodBase();
		Log.d(TAG, "Web response: " + resp);
		return formatter.read(resp);
	}

	@Override
	public void postBase(Base<Food> base)
	{
		String version = String.valueOf(base.getVersion());
		String data = formatter.write(base);
		webClient.postFoodBase(version, data);
	}
}
