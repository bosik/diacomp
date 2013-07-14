package org.bosik.compensation.persistence.repository.diary;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.bosik.compensation.face.BuildConfig;
import org.bosik.compensation.persistence.entity.diary.DiaryPage;
import org.bosik.compensation.persistence.providers.WebClient;
import org.bosik.compensation.utils.Utils;
import android.util.Log;

public class WebDiaryRepository implements DiaryRepository
{
	private static String TAG()
	{
		return WebDiaryRepository.class.getName();
	}

	private WebClient webClient;

	public WebDiaryRepository(WebClient webClient)
	{
		if (webClient == null)
			throw new NullPointerException("WebClient can't be null");

		this.webClient = webClient;
	}

	/* ================ ВНУТРЕННИЕ ================ */

	/**
	 * Преобразует timeStamp всех переданных страниц в серверное время.
	 * 
	 * @param pages
	 *            Страницы
	 */
	private void localToServer(List<DiaryPage> pages)
	{
		for (DiaryPage page : pages)
		{
			page.setTimeStamp(webClient.localToServer(page.getTimeStamp()));
		}
	}

	/**
	 * Преобразует timeStamp всех переданных страниц в локальное время.
	 * 
	 * @param pages
	 *            Страницы
	 */
	private void serverToLocal(List<DiaryPage> pages)
	{
		for (DiaryPage page : pages)
		{
			page.setTimeStamp(webClient.serverToLocal(page.getTimeStamp()));
		}
	}

	private List<DiaryPage> getPagesNaive(List<Date> dates)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();

		// проверки
		// if (null == dates) throw new NullPointerException("Dates can't be null");
		if (dates.isEmpty())
			return result;

		// конструируем запрос
		String query = webClient.getServer() + WebClient.URL_CONSOLE + "?diary:download&dates=";
		for (Date date : dates)
		{
			query += Utils.formatDate(date) + ",";
		}

		// отправляем на сервер
		String resp = webClient.doGetSmart(query);

		// обрабатываем результат
		if (resp == null)
			return null;
		else
		{
			result = DiaryPage.multiRead(resp);
			serverToLocal(result);
			return result;
		}
	}

	/* ================ ВНЕШНИЕ ================ */

	@Override
	public List<PageVersion> getModList(Date time)
	{
		List<PageVersion> result = new ArrayList<PageVersion>();

		// обращаемся к серверу
		String resp = webClient.doGetSmart(webClient.getServer() + WebClient.URL_CONSOLE + "?diary:getModList&time="
				+ Utils.formatTime(webClient.localToServer(time)));

		// разбираем результат

		String[] lines = resp.split("\n");

		for (int i = 0; i < lines.length; i++)
		{
			String[] item = lines[i].split("\\|");

			if (item.length == 2)
			{
				try
				{
					Date date = Utils.parseDate(item[0]);
					int version = Integer.parseInt(item[1]);
					PageVersion info = new PageVersion(date, version);
					result.add(info);
				} catch (ParseException e)
				{
					if (BuildConfig.DEBUG)
						throw new WebClient.ResponseFormatException("Incorrect line: " + lines[i], e);
					else
						Log.e(TAG(), "getModList(): Incorrect line: " + lines[i]);
				}
			} else
			{
				if (BuildConfig.DEBUG)
					throw new WebClient.ResponseFormatException("Incorrect line: " + lines[i]);
				else
					Log.e(TAG(), "getModList(): Incorrect line: " + lines[i]);
			}
		}
		return result;
	}

	@Override
	public List<DiaryPage> getPages(List<Date> dates)
	{
		List<DiaryPage> result = new ArrayList<DiaryPage>();

		int block = 10;
		int start = 0;
		while (start < dates.size())
		{
			if (start + block >= dates.size())
				block = dates.size() - start;
			result.addAll(getPagesNaive((List<Date>) dates.subList(start, start + block)));
			start += block;
		}

		return result;
	}

	@Override
	/**
	 * <b>Modifies pages time to server's one</b>
	 * 
	 * @param pages
	 *            Страницы
	 * @return Успешность отправки
	 */
	public boolean postPages(List<DiaryPage> pages)
	{
		// проверки
		if (null == pages)
			throw new NullPointerException("Pages can't be null");
		if (pages.isEmpty())
			return true;

		localToServer(pages);

		// конструируем запрос
		List<NameValuePair> p = new ArrayList<NameValuePair>();
		p.add(new BasicNameValuePair("diary:upload", ""));
		p.add(new BasicNameValuePair("pages", DiaryPage.multiWrite(pages)));

		// отправляем на сервер
		String resp = webClient.doPostSmart(webClient.getServer() + WebClient.URL_CONSOLE, p);

		// обрабатываем результат
		if (!WebClient.RESPONSE_DONE.equals(resp))
		{
			throw new WebClient.ServerException("Uploading pages to the server failed, response is \"" + resp + "\"");
		}

		return true;
	}
}
