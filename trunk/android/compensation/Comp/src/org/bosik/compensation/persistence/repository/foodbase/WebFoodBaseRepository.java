package org.bosik.compensation.persistence.repository.foodbase;

import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.repository.common.Base;
import org.bosik.compensation.persistence.repository.common.BaseRepository;
import org.bosik.compensation.persistence.repository.providers.WebClient;

public class WebFoodBaseRepository implements BaseRepository<Base<Food>>
{
	@SuppressWarnings("unused")
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
		String resp = webClient.doGetSmart(webClient.getServer() + WebClient.URL_CONSOLE + "?foodbase:getVersion");
		return Integer.parseInt(resp);
	}

	@Override
	public Base<Food> getBase()
	{
		String resp = webClient.doGetSmart(webClient.getServer() + WebClient.URL_CONSOLE + "?foodbase:download");
		return formatter.read(resp);
	}

	@Override
	public void postBase(Base<Food> base)
	{
		// TODO: uncomment when tested

		/*
		 * 
		 * // конструируем запрос List<NameValuePair> p = new ArrayList<NameValuePair>(); p.add(new
		 * BasicNameValuePair("foodbase:upload", "")); p.add(new BasicNameValuePair("version",
		 * String.valueOf(base.getVersion()))); p.add(new BasicNameValuePair("data",
		 * formatter.write(base)));
		 * 
		 * // отправляем на сервер webClient.doPostSmart(webClient.getServer() +
		 * WebClient.URL_CONSOLE, p);
		 */
	}
}
