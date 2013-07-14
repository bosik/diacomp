package org.bosik.compensation.persistence.repository.foodbase;

import java.util.ArrayList;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.bosik.compensation.persistence.repository.common.BaseRepository;
import org.bosik.compensation.persistence.repository.providers.WebClient;

public class WebFoodBaseRepository implements BaseRepository<FoodBase>
{
	private static String TAG = WebFoodBaseRepository.class.getSimpleName();
	private static FoodBaseXMLFormatter formatter = new FoodBaseXMLFormatter();

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
	public FoodBase getBase()
	{
		String resp = webClient.doGetSmart(webClient.getServer() + WebClient.URL_CONSOLE + "?foodbase:download");
		return formatter.read(resp);
	}

	@Override
	public void postBase(FoodBase base)
	{
		// конструируем запрос
		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("foodbase:upload", ""));
		p.add(new BasicNameValuePair("version", String.valueOf(base.getVersion())));
		p.add(new BasicNameValuePair("data", formatter.write(base)));

		// отправляем на сервер
		String resp = webClient.doPostSmart(webClient.getServer() + WebClient.URL_CONSOLE, p);
	}

}
